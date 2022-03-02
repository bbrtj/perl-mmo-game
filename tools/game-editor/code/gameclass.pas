unit gameclass;

{$mode objfpc}{$H+}

interface

uses fpjsonrtti, FPJSON,
	translationdialog, loreiddialog, editorcommon, serialization;

type
	TClass = class
	private
		FLoreId: TLoreId;
		FTranslations: TTranslations;

	public
		constructor Create;
		destructor Destroy; override;

		procedure Import(const vContent: String);
		function Export(): String;

	published

		property LoreId: TLoreId read FLoreId write FLoreId;
		property Translations: TTranslations read FTranslations write FTranslations;
	end;

implementation

{}
constructor TClass.Create;
begin
	FTranslations := TTranslations.Create;
end;

{}
destructor TClass.Destroy;
begin
	FTranslations.Free;
	inherited;
end;

{}
procedure TClass.Import(const vContent: String);
var
	streamer: TGameStreamer;
begin
	streamer := TGameStreamer.Create();
	streamer.DeStreamer.JSONToObject(vContent, self);
	streamer.Free;
end;

{}
function TClass.Export(): String;
var
	jsonResult: TJSONObject;
	streamer: TGameStreamer;
begin
	streamer := TGameStreamer.Create();
	jsonResult := streamer.Streamer.ObjectToJSON(self);
	result := jsonResult.FormatJSON([foUseTabchar], 1);

	streamer.Free;
end;

{ implementation end }

initialization
	// ListSerializationMap.Add(TSerializedList.Create(TMarkers, TMapMarker));

end.

