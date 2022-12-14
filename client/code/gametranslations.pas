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

		procedure TranslateCallback(const vSender: TCastleComponent; const vPropertyName: String; var vPropertyValue: String);
	end;

var
	GlobalTranslations: TGameMOFile;

procedure TranslateAllGameDesigns();

implementation

{ not exported }
procedure TranslateGameDesignCallback(const vComponent: TComponent; const vGroupName: String);
begin
	TranslateProperties(vComponent, @GlobalTranslations.TranslateCallback);
end;

procedure TranslateAllGameDesigns();
begin
	OnInternalTranslateDesign := @TranslateGameDesignCallback;
end;

constructor TGameMOFile.Create();
begin
	inherited Create(DesignUrl);
end;

procedure TGameMOFile.TranslateCallback(const vSender: TCastleComponent; const vPropertyName: String; var vPropertyValue: String);
var
	vOrigValue: String;
begin
	vOrigValue := vPropertyValue;
	vPropertyValue := Translate(vOrigValue);
	if vPropertyValue = '' then
		vPropertyValue := vOrigValue + ' [!!]';
end;

{ implementation end }

finalization
	if GlobalTranslations <> nil then
		GlobalTranslations.Free;

end.

