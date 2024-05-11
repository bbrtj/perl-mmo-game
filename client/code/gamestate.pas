unit GameState;

interface

uses Classes, FGL,
	CastleVectors, CastleTransform, CastleScene, CastleTiledMap, CastleRectangles,
	GameMaps, GameTypes, GameNetwork,
	GameActors,
	GameModels.Discovery, GameModels.Move, GameModels.Actors;

type
	TActorMap = specialize TFPGMap<TUlid, TGameActor>;

	TGameState = class
	private
	const
		cCameraDistance = 10;

	var
		FUIBoard: TCastleTiledMap;
		FUICamera: TCastleCamera;

		FActors: TActorMap;
		FThisPlayer: TUlid;
		FMapData: TMapData;

		FActorFactory: TGameActorFactory;

		function FindActor(const Id: TUlid): TGameActor;

	public
		constructor Create(const Board: TCastleTiledMap; const Camera: TCastleCamera);
		destructor Destroy; override;

		procedure Update(const SecondsPassed: Single);
		procedure SetMapData(const MapData: TMapData);

		procedure CreatePlayer(const Id: TUlid; const PosX, PosY: Single);
		procedure AddActor(const Id: TUlid);
		procedure RemoveActor(const Id: TUlid);
		procedure ProcessMovement(Movement: TMsgFeedActorMovement);
		procedure ProcessPosition(Stop: TMsgFeedActorPosition);
		procedure ProcessEvent(Event: TMsgFeedActorEvent);
	end;

implementation

constructor TGameState.Create(const Board: TCastleTiledMap; const Camera: TCastleCamera);
begin
	FUIBoard := Board;
	FUICamera := Camera;
	FActorFactory := TGameActorFactory.Create(FUIBoard);

	// FUICamera.Translation := Vector3(0, 0, cCameraDistance);
	FActors := TActorMap.Create;
end;

destructor TGameState.Destroy;
begin
	inherited;
	FActorFactory.Free;
	FActors.Free;
end;

procedure TGameState.Update(const SecondsPassed: Single);
var
	LPlayer: TGameActor;
	LRect: TFloatRectangle;
begin
	LPlayer := FindActor(FThisPlayer);
	if LPlayer <> nil then begin
		LRect := FUICamera.Orthographic.EffectiveRect;
		FUICamera.Translation := Vector3(LPlayer.GetPosition.X - LRect.Width / 2, LPlayer.GetPosition.Y - LRect.Height / 2, cCameraDistance);
	end;
end;

procedure TGameState.SetMapData(const MapData: TMapData);
var
	LProportionX: Single;
	LProportionY: Single;
begin
	FMapData := MapData;

	LProportionX := FMapData.Map.SizeX / FUIBoard.Map.Width / FUIBoard.Map.TileWidth;
	LProportionY := FMapData.Map.SizeY / FUIBoard.Map.Height / FUIBoard.Map.TileHeight;
	FUIBoard.Scale := Vector3(LProportionX, LProportionY, 1);
	// FUIBoard.Translation := Vector3(FMapData.Map.SizeX / 2, FMapData.Map.SizeY / 2, 0);
end;

procedure TGameState.CreatePlayer(const Id: TUlid; const PosX, PosY: Single);
var
	LNewObject: TMsgFeedActorPosition;
begin
	FThisPlayer := Id;
	self.AddActor(Id);

	// pretty artificial, but does the trick...
	LNewObject := TMsgFeedActorPosition.Create;
	LNewObject.id := Id;
	LNewObject.x := PosX;
	LNewObject.y := PosY;

	self.ProcessPosition(LNewObject);
	LNewObject.Free;
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

