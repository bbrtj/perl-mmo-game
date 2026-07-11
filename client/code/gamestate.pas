unit GameState;

interface

uses SysUtils, Classes, FGL,
	CastleVectors, CastleTransform, CastleViewport, CastleScene, CastleTiledMap,
	GameMaps, GameTypes, GameNetwork, GameLog, GameTranslations,
	GameActors, GameProjectiles, GameConfig,
	GameModels.Discovery, GameModels.Move, GameModels.Actors, GameModels.Projectiles,
	GamePipelines;

type
	EActorNotFound = class(EGameException);

	TActorMap = specialize TFPGMap<TUlid, TGameActor>;
	TProjectileMap = specialize TFPGMap<TUlid, TGameProjectile>;

	TGameState = class
	private
		FUIViewport: TCastleViewport;
		FUIBoardObjects: TCastleTransform;
		FUIPlayerLight: TCastlePointLight;
	private
		FActors: TActorMap;
		FProjectiles: TProjectileMap;
		FThisPlayer: TUlid;
		FMapData: TMapData;
	private
		FActorFactory: TGameActorFactory;
		FProjectileFactory: TGameProjectileFactory;
	private
		FCleanupTime: Single;
		FPipelineCleanupTime: Single;
	private
		function FindActor(const Id: TUlid): TGameActor;
		function FindProjectile(const Id: TUlid): TGameProjectile;
		procedure SetBoardObjects(BoardObjects: TCastleTransform);
	public
		constructor Create(Viewport: TCastleViewport);
		destructor Destroy; override;
	public
		procedure Update(const secondsPassed: Single);
		procedure SetMapData(Board: TCastleTiledMap; MapData: TMapData);
	public
		procedure CreatePlayer(ActorInfo: TGameActorRepositoryRecord; PosX, PosY: Single);
		procedure CreateActor(ActorInfo: TGameActorRepositoryRecord);
		procedure AddActor(Actor: TGameActor);
		procedure RemoveActor(const Id: TUlid);
	public
		procedure ProcessMovement(Movement: TMsgFeedActorMovement);
		procedure ProcessPosition(Stop: TMsgFeedActorPosition);
		procedure ProcessActorEvent(Event: TMsgFeedActorEvent);
		procedure ProcessActorState(Event: TMsgFeedActorState);
		procedure ProcessActorAction(Event: TMsgFeedActorAction);
		procedure ProcessProjectile(Event: TMsgFeedProjectile);
		procedure ProcessProjectileStop(Event: TMsgFeedProjectileStop);
	public
		property BoardObjects: TCastleTransform write SetBoardObjects;
		property PlayerLight: TCastlePointLight write FUIPlayerLight;
	end;

implementation

constructor TGameState.Create(Viewport: TCastleViewport);
begin
	FUIViewport := Viewport;
	FActorFactory := nil;
	FProjectileFactory := nil;

	FActors := TActorMap.Create;
	FProjectiles := TProjectileMap.Create;
end;

destructor TGameState.Destroy;
begin
	inherited;
	FActorFactory.Free;
	FActors.Free;

	FProjectileFactory.Free;
	FProjectiles.Free;
end;

procedure TGameState.Update(const secondsPassed: Single);
const
	CCleanupInterval = 0.1;
	CPipelineCleanupInterval = 5;
var
	I: Integer;
begin
	FCleanupTime += secondsPassed;
	if FCleanupTime >= CCleanupInterval then begin
		FCleanupTime := 0;

		// NOTE: only global update here. Game objects are updated automatically when added to the scene

		for I := FProjectiles.Count - 1 downto 0 do begin
			if not FProjectiles.Data[I].Finished then continue;
			FProjectileFactory.RemoveProjectile(FProjectiles.Data[I]);
			FProjectiles.Remove(FProjectiles.Data[I].Id);
		end;
	end;

	FPipelineCleanupTime += secondsPassed;
	if FPipelineCleanupTime >= CPipelineCleanupInterval then begin
		FPipelineCleanupTime := 0;

		GlobalPipelineManager.Cleanup;
	end;
end;

procedure TGameState.SetMapData(Board: TCastleTiledMap; MapData: TMapData);
var
	LProportionX: Single;
	LProportionY: Single;
begin
	FMapData := MapData;

	LProportionX := FMapData.Map.SizeX / Board.Data.Width / Board.Data.TileWidth;
	LProportionY := FMapData.Map.SizeY / Board.Data.Height / Board.Data.TileHeight;
	Board.Scale := Vector3(LProportionX, LProportionY, 1);
	Board.LayersZDistance := GlobalConfig.LayerDistance;

	FActorFactory.DrawLayer := Board.Data.Layers.Count * Board.LayersZDistance;
	FProjectileFactory.DrawLayer := FActorFactory.DrawLayer + Board.LayersZDistance;
end;

procedure TGameState.CreatePlayer(ActorInfo: TGameActorRepositoryRecord; PosX, PosY: Single);
var
	LNewObject: TMsgFeedActorPosition;
	LPlayer: TGameActor;
	LPlayerBehavior: TPlayerBehavior;
begin
	FThisPlayer := ActorInfo.Id;
	self.CreateActor(ActorInfo);
	LPlayer := self.FindActor(ActorInfo.Id);

	// pretty artificial, but does the trick...
	LNewObject := TMsgFeedActorPosition.Create;
	LNewObject.id := ActorInfo.Id;
	LNewObject.x := PosX;
	LNewObject.y := PosY;
	self.ProcessPosition(LNewObject);
	LNewObject.Free;

	LPlayerBehavior := TPlayerBehavior.Create(LPlayer);
	LPlayerBehavior.Camera := FUIViewport.Camera;
	LPlayerBehavior.Light := FUIPlayerLight;
	LPlayer.AddBehavior(LPlayerBehavior);
	// TODO: this behavior must be freed
end;

procedure TGameState.CreateActor(ActorInfo: TGameActorRepositoryRecord);
begin
	self.AddActor(FActorFactory.CreateActor(ActorInfo));
end;

procedure TGameState.AddActor(Actor: TGameActor);
begin
	FActors.Add(Actor.Id, Actor);
end;

procedure TGameState.RemoveActor(const Id: TUlid);
var
	LActor: TGameActor;
begin
	LActor := FindActor(Id);
	if LActor <> nil then begin
		FActors.Remove(Id);
		FActorFactory.RemoveActor(LActor);
	end;
end;

function TGameState.FindActor(const Id: TUlid): TGameActor;
begin
	if not FActors.TryGetData(Id, result) then
		result := nil;
end;

function TGameState.FindProjectile(const Id: TUlid): TGameProjectile;
begin
	if not FProjectiles.TryGetData(Id, result) then
		result := nil;
end;

procedure TGameState.SetBoardObjects(BoardObjects: TCastleTransform);
begin
	FUIBoardObjects := BoardObjects;
	if FActorFactory <> nil then
		FActorFactory.Free;
	FActorFactory := TGameActorFactory.Create(FUIViewport, FUIBoardObjects);

	if FProjectileFactory <> nil then
		FProjectileFactory.Free;
	FProjectileFactory := TGameProjectileFactory.Create(FUIViewport, FUIBoardObjects);
end;

procedure TGameState.ProcessMovement(Movement: TMsgFeedActorMovement);
var
	LActor: TGameActor;
begin
	LActor := self.FindActor(Movement.id);
	if LActor = nil then
		raise EActorNotFound.Create;

	LActor.SetPosition(Movement.x, Movement.y);
	LActor.Move(Movement.to_x, Movement.to_y, Movement.speed);
end;

procedure TGameState.ProcessPosition(Stop: TMsgFeedActorPosition);
var
	LActor: TGameActor;
begin
	LActor := self.FindActor(Stop.id);
	if LActor = nil then
		raise EActorNotFound.Create;

	LActor.SetPosition(Stop.x, Stop.y);
	LActor.Stop();
end;

procedure TGameState.ProcessActorEvent(Event: TMsgFeedActorEvent);
const
	CUnknownCharacter = 'msg.character.unknown';
	CMessageDamage = 'msg.combat.damage[]';
	CMessageHealing = 'msg.combat.healing[]';
var
	LActor: TGameActor;
	LSourceActorName: String;
	LMessageId: String;
	LLogHealth: Integer;
begin
	LActor := self.FindActor(Event.Id);
	if LActor = nil then
		raise EActorNotFound.Create;

	LActor.ModifyHealth(Event.Health);

	if GlobalActorRepository.HasActorInfo(Event.EventSource) then
		LSourceActorName := GlobalActorRepository.GetActorInfo(Event.EventSource).ActorName
	else
		LSourceActorName := _(CUnknownCharacter);

	if Event.HealthChange > 0 then LMessageId := CMessageHealing
	else LMessageId := CMessageDamage;

	// TODO: health may be an ugly float
	LLogHealth := Round(Abs(Event.HealthChange));
	LogCombat(cltCombat, _(LMessageId, [LSourceActorName, LActor.ActorRecord.ActorName, LLogHealth.ToString]));


	// TODO: animate damage / healing (HealthChange)
end;

procedure TGameState.ProcessActorState(Event: TMsgFeedActorState);
var
	LActor: TGameActor;
begin
	LActor := self.FindActor(Event.Id);
	if LActor = nil then
		raise EActorNotFound.Create;

	LActor.SetHealth(Event.Health, Event.MaxHealth);
	LActor.SetEnergy(Event.Energy, Event.MaxEnergy);
	LActor.SetRegeneration(Event.HealthRegeneration, Event.EnergyRegeneration);
	LActor.SetSize(Event.Size);
end;

procedure TGameState.ProcessActorAction(Event: TMsgFeedActorAction);
var
	LActor: TGameActor;
begin
	LActor := self.FindActor(Event.Id);
	if LActor = nil then
		raise EActorNotFound.Create;

	LActor.SetAction(Event.LoreId, Event.Duration);
end;

procedure TGameState.ProcessProjectile(Event: TMsgFeedProjectile);
var
	LProjectile: TGameProjectile;
begin
	LProjectile := FProjectileFactory.CreateProjectile(Event.Id, Event.LoreId);
	LProjectile.SetPosition(Event.X, Event.Y);
	LProjectile.Move(Event.Angle, Event.Speed, Event.MaxDistance);

	FProjectiles.Add(LProjectile.Id, LProjectile);
end;

procedure TGameState.ProcessProjectileStop(Event: TMsgFeedProjectileStop);
var
	LProjectile: TGameProjectile;
begin
	LProjectile := self.FindProjectile(Event.Id);
	if LProjectile <> nil then begin
		FProjectiles.Remove(LProjectile.Id);
		FProjectileFactory.RemoveProjectile(LProjectile);
	end;
end;

end.

