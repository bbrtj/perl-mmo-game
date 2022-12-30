unit GameState;

interface

uses Classes,
	CastleVectors, CastleTransform, CastleScene,
	GameMaps, GameTypes, GameNetwork,
	GameActors;

type
	TGameState = class
	private
	const
		cCameraDistance = 10;

	var
		Board: TCastlePlane;
		Camera: TCastleTransform;

		FThisPlayer: TGameActor;
		FMapData: TMapData;

		FActorFactory: TGameActorFactory;

	public
		constructor Create(const vBoard: TCastlePlane; const vCamera: TCastleTransform);
		destructor Destroy; override;

		procedure Update(const vSecondsPassed: Single);
		procedure SetMapData(const vMapData: TMapData);

		procedure CreatePlayer(const vId: TUlid);
	end;

implementation

constructor TGameState.Create(const vBoard: TCastlePlane; const vCamera: TCastleTransform);
begin
	Board := vBoard;
	Camera := vCamera;
	FActorFactory := TGameActorFactory.Create(Board);

	Camera.Translation := Vector3(0, 0, cCameraDistance);
	FThisPlayer := nil;
end;

destructor TGameState.Destroy;
begin
	inherited;
	FActorFactory.Free;
	if FThisPlayer <> nil then FThisPlayer.Free;
end;

procedure TGameState.Update(const vSecondsPassed: Single);
begin
	Camera.Translation := Camera.Translation + Vector3(0.001, 0.001, 0);
end;

procedure TGameState.SetMapData(const vMapData: TMapData);
begin
	FMapData := vMapData;

	Board.Size := Vector2(FMapData.Map.SizeX, FMapData.Map.SizeY);
	Board.Translation := Vector3(FMapData.Map.SizeX / 2, FMapData.Map.SizeY / 2, 0);
end;

procedure TGameState.CreatePlayer(const vId: TUlid);
begin
	FThisPlayer := FActorFactory.CreateActor(vId);
end;

{ implementation end }

end.

