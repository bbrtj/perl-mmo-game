unit GameStateLoading;

interface

uses Classes,
	CastleVectors, CastleUIState, CastleUIControls, CastleControls, CastleKeysMouse;

type
	TStateLoading = class(TUIState)
	private
		{ Components designed using CGE editor, loaded from the castle-user-interface file. }
		// ButtonXxx: TCastleButton;
	public
		constructor Create(AOwner: TComponent); override;
		procedure Start; override;
	end;

var
	StateLoading: TStateLoading;

implementation

constructor TStateLoading.Create(AOwner: TComponent);
begin
	inherited;
	DesignUrl := 'castle-data:/gamestateloading.castle-user-interface';
end;

procedure TStateLoading.Start;
begin
	inherited;
	{ Find components, by name, that we need to access from code }
	// ButtonXxx := DesignedComponent('ButtonXxx') as TCastleButton;
end;

end.

