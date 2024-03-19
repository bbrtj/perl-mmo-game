unit GameTranslations;

interface

uses Classes,
	CastleClassUtils, CastleLocalizationGetText, CastleComponentSerialize;

type
	TGameMOFile = class(TCastleMOFile)
	private
		const
			DesignUrl = 'castle-data:/translations.mo';

	public
		constructor Create();

		procedure TranslateCallback(const Sender: TCastleComponent; const PropertyName: String; var PropertyValue: String);
	end;

var
	GlobalTranslations: TGameMOFile;

procedure TranslateAllGameDesigns();
function _(const TranslatedString: String): String;

implementation

{ not exported }
procedure TranslateGameDesignCallback(const Component: TComponent; const GroupName: String);
begin
	TranslateProperties(Component, @GlobalTranslations.TranslateCallback);
end;

procedure TranslateAllGameDesigns();
begin
	OnInternalTranslateDesign := @TranslateGameDesignCallback;
end;

constructor TGameMOFile.Create();
begin
	inherited Create(DesignUrl);
end;

procedure TGameMOFile.TranslateCallback(const Sender: TCastleComponent; const PropertyName: String; var PropertyValue: String);
var
	LOrigValue: String;
begin
	LOrigValue := PropertyValue;
	PropertyValue := self.Translate(LOrigValue);
	if PropertyValue = '' then
		PropertyValue := LOrigValue + ' [!!]';
end;

function _(const TranslatedString: String): String;
begin
	result := GlobalTranslations.Translate(TranslatedString);
end;

{ implementation end }

finalization
	if GlobalTranslations <> nil then
		GlobalTranslations.Free;

end.

