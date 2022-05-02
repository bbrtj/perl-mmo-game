unit GameUIComponents;

interface

uses
	Classes,
	CastleControls, CastleComponentSerialize, CastleColors;

type

	TGameButton = class(TCastleButton)
	public
		const
			GameDefaultPaddingHorizontal = 15;
			GameDefaultTextColor: TCastleColor = (X: 1; Y: 1; Z: 1; W: 1.0);
			GameDefaultBgColor: TCastleColor = (X: 0.0; Y: 0.0; Z: 0.0; W: 1.0);
			GameDefaultFocusedBgColor: TCastleColor = (X: 0.12; Y: 0.12; Z: 0.12; W: 1.0);

		constructor Create(AOwner: TComponent); override;

	published
		property CustomTextColorUse default true;
		property CustomBackground default true;
		property PaddingHorizontal default GameDefaultPaddingHorizontal;
	end;

implementation

constructor TGameButton.Create(AOwner: TComponent);
begin
	inherited;

	CustomTextColorUse := true;
	CustomBackground := true;
	PaddingHorizontal := GameDefaultPaddingHorizontal;

	CustomTextColor := GameDefaultTextColor;

	CustomColorDisabled := GameDefaultBgColor;
	CustomColorFocused := GameDefaultFocusedBgColor;
	CustomColorNormal := GameDefaultBgColor;
	CustomColorPressed := GameDefaultBgColor;
end;

initialization
	RegisterSerializableComponent(TGameButton, 'XX Game Button');

end.

