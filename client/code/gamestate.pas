unit GameState;

interface

uses Classes, FGL,
	CastleVectors, CastleTransform, CastleViewport, CastleScene, CastleTiledMap,
	GameMaps, GameTypes, GameNetwork,
	GameActors,
	GameModels.Discovery, GameModels.Move, GameModels.Actors;

type
	TActorMap = specialize TFPGMap<TUlid, TGameActor>;

	TGameState = class
	private
		FUIViewport: TCastleViewport;
		FUIBoard: TCastleTiledMap;

		FActors: TActorMap;
		FThisPlayer: TUlid;
		FMapData: TMapData;

		FActorFactory: TGameActorFactory;

		function FindActor(const Id: TUlid): TGameActor;
		procedure SetBoard(Board: TCastleTiledMap);

	public
		constructor Create(Viewport: TCastleViewport);
		destructor Destroy; override;

		procedure Update(const SecondsPassed: Single);
		procedure SetMapData(const MapData: TMapData);

		procedure CreatePlayer(const Id: TUlid; const PosX, PosY: Single);
		procedure AddActor(const Id: TUlid);
		procedure RemoveActor(const Id: TUlid);
		procedure ProcessMovement(Movement: TMsgFeedActorMovement);
		procedure ProcessPosition(Stop: TMsgFeedActorPosition);
		procedure ProcessEvent(Event: TMsgFeedActorEvent);

		property Board: TCastleTiledMap write SetBoard;
	end;

implementation

constructor TGameState.Create(Viewport: TCastleViewport);
begin
	FUIViewport := Viewport;
	FActorFactory := nil;

	FActors := TActorMap.Create;
end;

destructor TGameState.Destroy;
begin
	inherited;
	FActorFactory.Free;
	FActors.Free;
end;

procedure TGameState.Update(const SecondsPassed: Single);
begin
end;

procedure TGameState.SetMapData(const MapData: TMapData);
var
	LProportionX: Single;
	LProportionY: Single;
begin
	FMapData := MapData;

	LProportionX := FMapData.Map.SizeX / FUIBoard.Data.Width / FUIBoard.Data.TileWidth;
	LProportionY := FMapData.Map.SizeY / FUIBoard.Data.Height / FUIBoard.Data.TileHeight;
	FUIBoard.Scale := Vector3(LProportionX, LProportionY, 1);
	// FUIBoard.Translation := Vector3(FMapData.Map.SizeX / 2, FMapData.Map.SizeY / 2, 0);
end;

procedure TGameState.CreatePlayer(const Id: TUlid; const PosX, PosY: Single);
var
	LNewObject: TMsgFeedActorPosition;
	LPlayer: TGameActor;
	LPlayerBehavior: TPlayerBehavior;
begin
	FThisPlayer := Id;
	self.AddActor(Id);
	LPlayer := FindActor(Id);

	// pretty artificial, but does the trick...
	LNewObject := TMsgFeedActorPosition.Create;
	LNewObject.id := Id;
	LNewObject.x := PosX;
	LNewObject.y := PosY;
	self.ProcessPosition(LNewObject);
	LNewObject.Free;

	LPlayerBehavior := TPlayerBehavior.Create(LPlayer);
	LPlayerBehavior.Camera := FUIViewport.Camera;
	LPlayer.AddBehavior(LPlayerBehavior);
	// TODO: this behavior must be freed
end;

procedure TGameState.AddActor(const Id: TUlid);
begin
	FActors.Add(Id, FActorFactory.CreateActor(Id));
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

procedure TGameState.SetBoard(Board: TCastleTiledMap);
begin
	FUIBoard := Board;
	if FActorFactory <> nil then
		FActorFactory.Free;
	FActorFactory := TGameActorFactory.Create(FUIViewport, FUIBoard);
end;

procedure TGameState.ProcessMovement(Movement: TMsgFeedActorMovement);
var
	LActor: TGameActor;
begin
	LActor := self.FindActor(Movement.id);
	if LActor <> nil then begin
		LActor.SetPosition(Movement.x, Movement.y);
		LActor.Move(Movement.to_x, Movement.to_y, Movement.speed);
	end
end;

procedure TGameState.ProcessPosition(Stop: TMsgFeedActorPosition);
var
	LActor: TGameActor;
begin
	LActor := self.FindActor(Stop.id);
	if LActor <> nil then begin
		LActor.SetPosition(Stop.x, Stop.y);
		LActor.Stop();
	end
end;

procedure TGameState.ProcessEvent(Event: TMsgFeedActorEvent);
var
	LActor: TGameActor;
begin
	LActor := self.FindActor(Event.Id);
	if LActor <> nil then begin
		LActor.SetHealth(Event.Health, Event.MaxHealth);
		LActor.SetEnergy(Event.Energy, Event.MaxEnergy);
		// TODO: event data not handled
	end
end;

end.

