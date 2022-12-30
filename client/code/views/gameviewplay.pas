unit GameViewPlay;

interface

uses Classes,
	CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform, CastleScene,
	GameState;

type
	TViewPlay = class(TCastleView)
	private
		Board: TCastlePlane;
		PlayerCamera: TCastleCamera;
		AmbientLight: TCastleDirectionalLight;

		FGameState: TGameState;
		FMapImagePath: String;
		FPlaying: Boolean;

	public
		constructor Create(vOwner: TComponent); override;
		procedure Start; override;
		procedure Stop; override;

		procedure Update(const vSecondsPassed: Single; var vHandleInput: Boolean); override;

		procedure SetMapImagePath(vMapImagePath: String);

		property GameState: TGameState read FGameState write FGameState;
		property Playing: Boolean read FPlaying write FPlaying;

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
	FPlaying := false;

	FGameState := TGameState.Create(Board, PlayerCamera);
end;

procedure TViewPlay.Stop;
begin
	FGameState.Free;
end;

procedure TViewPlay.Update(const vSecondsPassed: Single; var vHandleInput: Boolean);
begin
	inherited;

	if not FPlaying then exit;
	FGameState.Update(vSecondsPassed);
end;

procedure TViewPlay.SetMapImagePath(vMapImagePath: String);
begin
	FMapImagePath := vMapImagePath;

	Board.Texture := FMapImagePath;
end;

{ implementation end }

end.

