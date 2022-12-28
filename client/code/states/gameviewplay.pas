unit GameViewPlay;

interface

uses Classes,
	CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform, CastleScene,
	GameMaps;

type
	TViewPlay = class(TCastleView)
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
	ViewPlay: TViewPlay;

implementation

constructor TViewPlay.Create(vOwner: TComponent);
begin
	inherited;
	DesignUrl := 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TViewPlay.Start;
begin
	inherited;

	Board := DesignedComponent('Board') as TCastlePlane;
	PlayerCamera := DesignedComponent('PlayerCamera') as TCastleCamera;
	AmbientLight := DesignedComponent('AmbientLight') as TCastleDirectionalLight;

	Board.Size := Vector2(FMapData.Map.SizeX, FMapData.Map.SizeY);
	Board.Translation := Vector3(FMapData.Map.SizeX / 2, FMapData.Map.SizeY / 2, 0);
	Board.Texture := FMapImagePath;

	PlayerCamera.Translation := Vector3(0, 0, 10);
end;

end.

