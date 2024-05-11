unit GameModels;

interface

uses SysUtils, FPJSON, GameConfig, Serialization;

type

	TModelBase = class abstract(TSerialized)
	public
		class function MessageType(): String; virtual;
	end;

	TModelClass = class of TModelBase;

	TEmptyModel = class (TModelBase);

	TPlaintextModel = class (TModelBase)
	private
		FValueParts: TStringArray;

	protected
		procedure SetValue(const Value: String); virtual;
		function GetValue(): String; virtual;

		function GetValueIndex(Index: Integer): String;
		function GetValueIndexReal(Index: Integer): Single;
		procedure SetValueIndex(Index: Integer; const Value: String);
		procedure SetValueIndexReal(Index: Integer; Value: Single);

	public
		constructor Create();
		constructor Create(Parts: TStringArray);

		property RawValue: TStringArray read FValueParts write FValueParts;
		property Value: String read GetValue write SetValue;

	end;

	TModelSerializationBase = class abstract
	public
		function Serialize(const Model: TModelBase): String; virtual; abstract;
		function DeSerialize(const Serialized: String; const ModelClass: TModelClass): TModelBase; virtual; abstract;
	end;

	TJSONModelSerialization = class (TModelSerializationBase)
	const
		cArrayWrapKey = 'list';

	private

		FStreamer: TGameStreamer;

	public
		constructor Create();
		destructor Destroy; override;

		function Serialize(const Model: TModelBase): String; override;
		function DeSerialize(const Serialized: String; const ModelClass: TModelClass): TModelBase; override;
	end;

var
	DummyModel: TEmptyModel;

implementation

{ this should be reimplemented for more complex models }
class function TModelBase.MessageType(): String;
begin
	result := '';
end;

constructor TPlaintextModel.Create();
begin
	FValueParts := [];
end;

constructor TPlaintextModel.Create(Parts: TStringArray);
begin
	FValueParts := Parts;
end;

procedure TPlaintextModel.SetValue(const Value: String);
begin
	FValueParts := Value.Split(GlobalConfig.NetworkSeparatorCharacter);
end;

function TPlaintextModel.GetValue(): String;
begin
	result := String.Join(GlobalConfig.NetworkSeparatorCharacter, FValueParts);
end;

function TPlaintextModel.GetValueIndex(Index: Integer): String;
begin
	result := FValueParts[Index];
end;

function TPlaintextModel.GetValueIndexReal(Index: Integer): Single;
begin
	result := StrToFloat(self.GetValueIndex(Index), GlobalConfig.FormatSettings);
end;

procedure TPlaintextModel.SetValueIndex(Index: Integer; const Value: String);
begin
	if Length(FValueParts) <= Index then SetLength(FValueParts, Index + 1);
	FValueParts[Index] := Value;
end;

procedure TPlaintextModel.SetValueIndexReal(Index: Integer; Value: Single);
begin
	self.SetValueIndex(Index, FloatToStr(Value, GlobalConfig.FormatSettings));
end;

constructor TJSONModelSerialization.Create();
begin
	FStreamer := TGameStreamer.Create;
end;

destructor TJSONModelSerialization.Destroy;
begin
	FStreamer.Free;
	inherited;
end;

function TJSONModelSerialization.Serialize(const Model: TModelBase): String;
begin
	if Model is TEmptyModel then
		result := ''
	else if Model is TPlaintextModel then
		result := (Model as TPlaintextModel).Value
	else
		result := FStreamer.Streamer.ObjectToJSONString(Model);
end;

function TJSONModelSerialization.DeSerialize(const Serialized: String; const ModelClass: TModelClass): TModelBase;

	function WrappedJson(): String;
	var
		LJsonMaybeArray: TJSONData;
		LNewObject: TJSONObject;
	begin
		result := Serialized;
		LJsonMaybeArray := GetJSON(Serialized);

		if LJsonMaybeArray.JsonType = jtArray then begin
			LNewObject := TJSONObject.Create;
			LNewObject.Add(cArrayWrapKey, LJsonMaybeArray);

			result := LNewObject.AsJson;
			FreeAndNil(LNewObject);
		end else
			FreeAndNil(LJsonMaybeArray);
	end;

begin
	result := ModelClass.Create;

	if ModelClass.InheritsFrom(TPlaintextModel) then
		(result as TPlaintextModel).Value := Serialized
	else
		FStreamer.DeStreamer.JSONToObject(WrappedJson(), result);
end;

initialization
	DummyModel := TEmptyModel.Create;

finalization
	DummyModel.Free;

end.

