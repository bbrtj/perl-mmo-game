unit GameConfig;

interface

uses FGL, Classes, SysUtils,
	CastleDownload,
	GameTypes, Serialization;

type
	TGameConfig = class
	private
		FNetworkSeparatorCharacter: String;
		FNetworkControlCharacter: String;
		FNetworkMaxLength: Integer;
		FNetworkPort: Integer;
		FNetworkTransportFloatPrecision: Integer;

		FLayerDistance: Single;
		FCameraDistance: Single;
		FLightsDistance: Single;
		FFormatSettings: TFormatSettings;
	public
		procedure Initialize();

	published
		property NetworkSeparatorCharacter: String read FNetworkSeparatorCharacter write FNetworkSeparatorCharacter;
		property NetworkControlCharacter: String read FNetworkControlCharacter write FNetworkControlCharacter;
		property NetworkMaxLength: Integer read FNetworkMaxLength write FNetworkMaxLength;
		property NetworkPort: Integer read FNetworkPort write FNetworkPort;
		property NetworkTransportFloatPrecision: Integer read FNetworkTransportFloatPrecision write FNetworkTransportFloatPrecision;

	public
		property LayerDistance: Single read FLayerDistance write FLayerDistance;
		property CameraDistance: Single read FCameraDistance write FCameraDistance;
		property LightsDistance: Single read FLightsDistance write FLightsDistance;
		property FormatSettings: TFormatSettings read FFormatSettings;

	end;

var
	GlobalConfig: TGameConfig;

implementation

procedure TGameConfig.Initialize();
var
	LStreamer: TGameStreamer;
	LLines: TStringList;
	LStream: TStream;
begin
	LStreamer := TGameStreamer.Create;
	LLines := TStringList.Create;

	LStream := Download('castle-data:/config.json');
	LLines.LoadFromStream(LStream);
	LStreamer.DeStreamer.JSONToObject(LLines.Text, self);

	LStreamer.Free;
	LLines.Free;
	LStream.Free;

	FLayerDistance := 1;
	FCameraDistance := FLayerDistance * 10;
	FLightsDistance := FLayerDistance * 100;
	FFormatSettings.DecimalSeparator := '.';
end;

initialization
	GlobalConfig := TGameConfig.Create;

finalization
	FreeAndNil(GlobalConfig);

end.

