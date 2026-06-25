unit GameMessageLog;

interface

uses Classes, SysUtils,
	CastleControls,
	GameTypes;

type

	TGameMessageLog = class
	private
		FUILabel: TCastleLabel;
		FUIScroll: TCastleScrollView;
	public
		constructor Create(UILabel: TCastleLabel; UIScroll: TCastleScrollView);
	public
		procedure AddLine(const Text: String);
	end;

implementation

constructor TGameMessageLog.Create(UILabel: TCastleLabel; UIScroll: TCastleScrollView);
begin
	FUILabel := UILabel;
	FUIScroll := UIScroll;
end;

procedure TGameMessageLog.AddLine(const Text: String);
var
	LScrolling: Boolean;
begin
	LScrolling := FUIScroll.Scroll < FUIScroll.ScrollMax;
	FUILabel.Text.Append(Text);

	FUIScroll.Scroll := FUIScroll.ScrollMax;
end;

end.

