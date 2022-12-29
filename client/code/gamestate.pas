unit GameState;

interface

uses Classes,
	CastleVectors, CastleTransform, CastleScene,
	GameMaps, GameTypes, GameNetwork;

type
	TGameState = class
	private
	const
		cCameraDistance = 10;

	var
		Board: TCastlePlane;
		Camera: TCastleCamera;

		FThisPlayer: TUlid;
		FMapData: TMapData;

	public
		constructor Create(const vBoard: TCastlePlane; const vCamera: TCastleCamera);

		procedure Update(const vSecondsPassed: Single);
		procedure SetMapData(const vMapData: TMapData);

		property ThisPlayer: TUlid read FThisPlayer write FThisPlayer;
	end;

implementation

constructor TGameState.Create(const vBoard: TCastlePlane; const vCamera: TCastleCamera);
begin
	Board := vBoard;
	Camera := vCamera;

	Camera.Translation := Vector3(0, 0, cCameraDistance);
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

{ implementation end }

end.

