unit GameTranslations;

interface

uses SysUtils, Classes,
	CastleClassUtils, CastleLocalizationGetText, CastleComponentSerialize;

type
	TTranslationPlaceholders = Array of String;

	TGameMOFile = class(TCastleMOFile)
	private const
		CDesignUrl = 'castle-data:/translations.mo';
	public
		constructor Create();
	public
		procedure TranslateCallback(const Sender: TCastleComponent; const PropertyName: String; var PropertyValue: String);
		function TranslatePlaceholders(const TranslatedString: String; Ph: TTranslationPlaceholders): String;
	end;

var
	GlobalTranslations: TGameMOFile;

procedure TranslateAllGameDesigns();
function _(const TranslatedString: String; Ph: TTranslationPlaceholders): String;
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
	inherited Create(CDesignUrl);
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

function TGameMOFile.TranslatePlaceholders(const TranslatedString: String; Ph: TTranslationPlaceholders): String;
var
	I, PhI: Integer;
begin
	result := self.Translate(TranslatedString);

	// NOTE: we really want to make sure the array starts with 0 here for replacement to work
	for I := 0 to High(Ph) do begin
		PhI := I + 1;
		result := StringReplace(result, '[_' + PhI.ToString() + ']', Ph[I], [rfReplaceAll]);
	end;
end;

function _(const TranslatedString: String; Ph: TTranslationPlaceholders): String;
begin
	result := GlobalTranslations.TranslatePlaceholders(TranslatedString, Ph);
	if (result = '') and (TranslatedString <> '') then
		result := 'MISSING TRANSLATION ' + TranslatedString;
end;

function _(const TranslatedString: String): String;
begin
	result := _(TranslatedString, []);
end;

finalization
	FreeAndNil(GlobalTranslations);

end.

