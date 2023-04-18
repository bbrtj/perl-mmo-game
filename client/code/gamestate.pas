unit GameState;

interface

uses Classes, FGL,
	CastleVectors, CastleTransform, CastleScene,
	GameMaps, GameTypes, GameNetwork,
	GameActors,
	GameModels.Move;

type
	TActorMap = specialize TFPGMapObject<TUlid, TGameActor>;

	TGameState = class
	private
	const
		cCameraDistance = 10;

	var
		Board: TCastlePlane;
		Camera: TCastleTransform;

		FActors: TActorMap;
		FThisPlayer: TUlid;
		FMapData: TMapData;

		FActorFactory: TGameActorFactory;

		function FindActor(const vId: TUlid): TGameActor;

	public
		constructor Create(const vBoard: TCastlePlane; const vCamera: TCastleTransform);
		destructor Destroy; override;

		procedure Update(const vSecondsPassed: Single);
		procedure SetMapData(const vMapData: TMapData);

		procedure CreatePlayer(const vId: TUlid);
		procedure ProcessMovement(const vMovement: TMsgFeedActorMovement);
	end;

implementation

constructor TGameState.Create(const vBoard: TCastlePlane; const vCamera: TCastleTransform);
begin
	Board := vBoard;
	Camera := vCamera;
	FActorFactory := TGameActorFactory.Create(Board);

	Camera.Translation := Vector3(0, 0, cCameraDistance);
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
begin
	vPlayer := FindActor(FThisPlayer);
	if vPlayer <> nil then begin
		Camera.Translation := Vector3(vPlayer.GetPosition.X, vPlayer.GetPosition.Y, cCameraDistance);
	end;
end;

procedure TGameState.SetMapData(const vMapData: TMapData);
begin
	FMapData := vMapData;

	Board.Size := Vector2(FMapData.Map.SizeX, FMapData.Map.SizeY);
	Board.Translation := Vector3(FMapData.Map.SizeX / 2, FMapData.Map.SizeY / 2, 0);
end;

procedure TGameState.CreatePlayer(const vId: TUlid);
begin
	FThisPlayer := vId;
	FActors.Add(vId, FActorFactory.CreateActor(vId));
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
	vActor.SetPosition(vMovement.x, vMovement.y); // TODO: take latency into account? This is from the past
	vActor.Move(vMovement.to_x, vMovement.to_y, vMovement.speed);
end;

{ implementation end }

end.

