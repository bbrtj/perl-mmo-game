unit GameState;

interface

uses Classes, FGL,
	CastleVectors, CastleTransform, CastleScene, CastleTiledMap, CastleRectangles,
	GameMaps, GameTypes, GameNetwork,
	GameActors,
	GameModels.Discovery, GameModels.Move;

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

		function FindActor(const vId: TUlid): TGameActor;

	public
		constructor Create(const vBoard: TCastleTiledMap; const vCamera: TCastleCamera);
		destructor Destroy; override;

		procedure Update(const vSecondsPassed: Single);
		procedure SetMapData(const vMapData: TMapData);

		procedure CreatePlayer(const vId: TUlid; const vPosX, vPosY: Single);
		procedure AddActor(const vObject: TMsgFeedNewObject);
		procedure RemoveActor(const vId: TUlid);
		procedure ProcessMovement(const vMovement: TMsgFeedActorMovement);
		procedure ProcessPosition(const vStop: TMsgFeedActorPosition);
	end;

implementation

constructor TGameState.Create(const vBoard: TCastleTiledMap; const vCamera: TCastleCamera);
begin
	FUIBoard := vBoard;
	FUICamera := vCamera;
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

procedure TGameState.Update(const vSecondsPassed: Single);
var
	vPlayer: TGameActor;
	vRect: TFloatRectangle;
begin
	vPlayer := FindActor(FThisPlayer);
	if vPlayer <> nil then begin
		vRect := FUICamera.Orthographic.EffectiveRect;
		FUICamera.Translation := Vector3(vPlayer.GetPosition.X - vRect.Width / 2, vPlayer.GetPosition.Y - vRect.Height / 2, cCameraDistance);
	end;
end;

procedure TGameState.SetMapData(const vMapData: TMapData);
var
	vProportionX: Single;
	vProportionY: Single;
begin
	FMapData := vMapData;

	vProportionX := FMapData.Map.SizeX / FUIBoard.Map.Width / FUIBoard.Map.TileWidth;
	vProportionY := FMapData.Map.SizeY / FUIBoard.Map.Height / FUIBoard.Map.TileHeight;
	FUIBoard.Scale := Vector3(vProportionX, vProportionY, 0);
	// FUIBoard.Translation := Vector3(FMapData.Map.SizeX / 2, FMapData.Map.SizeY / 2, 0);
end;

procedure TGameState.CreatePlayer(const vId: TUlid; const vPosX, vPosY: Single);
var
	vNewObject: TMsgFeedNewObject;
begin
	FThisPlayer := vId;

	// pretty artificial, but does the trick...
	vNewObject := TMsgFeedNewObject.Create;
	vNewObject.id := vId;
	vNewObject.x := vPosX;
	vNewObject.y := vPosY;

	AddActor(vNewObject);
	vNewObject.Free;
end;

procedure TGameState.AddActor(const vObject: TMsgFeedNewObject);
begin
	FActors.Add(vObject.id, FActorFactory.CreateActor(vObject.id));
	ProcessPosition(vObject);
end;

procedure TGameState.RemoveActor(const vId: TUlid);
var
	vActor: TGameActor;
begin
	vActor := FindActor(vId);
	if vActor <> nil then begin
		FActors.Remove(vId);
		FActorFactory.RemoveActor(vActor);
	end;
end;

function TGameState.FindActor(const vId: TUlid): TGameActor;
begin
	if not FActors.TryGetData(vId, result) then
		result := nil;
end;

procedure TGameState.ProcessMovement(const vMovement: TMsgFeedActorMovement);
var
	vActor: TGameActor;
begin
	vActor := FindActor(vMovement.id);
	if vActor <> nil then begin
		vActor.SetPosition(vMovement.x, vMovement.y); // TODO: take latency into account? This is from the past
		vActor.Move(vMovement.to_x, vMovement.to_y, vMovement.speed);
	end
end;

procedure TGameState.ProcessPosition(const vStop: TMsgFeedActorPosition);
var
	vActor: TGameActor;
begin
	vActor := FindActor(vStop.id);
	if vActor <> nil then begin
		vActor.SetPosition(vStop.x, vStop.y); // TODO: take latency into account? This is from the past
		vActor.Stop();
	end
end;

{ implementation end }

end.

