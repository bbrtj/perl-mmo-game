unit GameStatePlay;

interface

uses Classes,
	CastleVectors, CastleUIState, CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform, CastleScene,
	GameMaps;

type
	TStatePlay = class(TUIState)
	private
		Board: TCastlePlane;
		PlayerCamera: TCastleCamera;
		AmbientLight: TCastleDirectionalLight;

		FMapData: TMapData;
		FMapImagePath: String;

	public
		constructor Create(vOwner: TComponent); override;
		procedure Start; override;

		property MapData: TMapData write FMapData;
		property MapImagePath: String write FMapImagePath;

	end;

var
	StatePlay: TStatePlay;

implementation

constructor TStatePlay.Create(vOwner: TComponent);
begin
	inherited;
	DesignUrl := 'castle-data:/gamestateplay.castle-user-interface';
end;

procedure TStatePlay.Start;
begin
	inherited;

	Board := DesignedComponent('Board') as TCastlePlane;
	PlayerCamera := DesignedComponent('PlayerCamera') as TCastleCamera;
	AmbientLight := DesignedComponent('AmbientLight') as TCastleDirectionalLight;

	// NOTE: X coord is reversed (goes to left), since direction is -1
	Board.Size := Vector2(FMapData.Map.SizeX, FMapData.Map.SizeY);
	Board.Translation := Vector3(-1 * FMapData.Map.SizeX / 2, 0, FMapData.Map.SizeY / 2);
	Board.Texture := FMapImagePath;

	PlayerCamera.Translation := Vector3(0, 10, 0);
end;

end.

