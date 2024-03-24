unit GameModels;

interface

uses SysUtils, FPJSON, Serialization;

type

	TModelBase = class abstract(TSerialized)
	public
		class function MessageType(): String; virtual;
	end;

	TModelClass = class of TModelBase;

	TEmptyModel = class (TModelBase);

	TPlaintextModel = class (TModelBase)
	private
		FValue: Variant;

	public
		constructor Create();

	published
		property value: Variant read FValue write FValue;

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
	FValue := '';
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

