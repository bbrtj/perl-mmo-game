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

	published
		property value: Variant read FValue write FValue;

	end;

	TModelSerializationBase = class abstract
	public
		function Serialize(const vModel: TModelBase): String; virtual; abstract;
		function DeSerialize(const vSerialized: String; const vModelClass: TModelClass): TModelBase; virtual; abstract;
	end;

	TJSONModelSerialization = class (TModelSerializationBase)
	const
		cArrayWrapKey = 'list';

	private

		FStreamer: TGameStreamer;

	public
		constructor Create();
		destructor Destroy; override;

		function Serialize(const vModel: TModelBase): String; override;
		function DeSerialize(const vSerialized: String; const vModelClass: TModelClass): TModelBase; override;
	end;

var
	DummyModel: TEmptyModel;

implementation

{ this should be reimplemented for more complex models }
class function TModelBase.MessageType(): String;
begin
	result := '';
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

function TJSONModelSerialization.Serialize(const vModel: TModelBase): String;
begin
	if vModel is TEmptyModel then
		result := ''
	else if vModel is TPlaintextModel then
		result := (vModel as TPlaintextModel).Value
	else
		result := FStreamer.Streamer.ObjectToJSONString(vModel);
end;

function TJSONModelSerialization.DeSerialize(const vSerialized: String; const vModelClass: TModelClass): TModelBase;

	function WrappedJson(): String;
	var
		vJsonMaybeArray: TJSONData;
		vNewObject: TJSONObject;
	begin
		result := vSerialized;
		vJsonMaybeArray := GetJSON(vSerialized);

		if vJsonMaybeArray.JsonType = jtArray then begin
			vNewObject := TJSONObject.Create;
			vNewObject.Add(cArrayWrapKey, vJsonMaybeArray);

			result := vNewObject.AsJson;
		end;
	end;

begin
	result := vModelClass.Create;

	if vModelClass.InheritsFrom(TPlaintextModel) then
		(result as TPlaintextModel).Value := vSerialized
	else
		FStreamer.DeStreamer.JSONToObject(WrappedJson(), result);
end;

{ implementation end }

initialization
	DummyModel := TEmptyModel.Create;

finalization
	DummyModel.Free;

end.

