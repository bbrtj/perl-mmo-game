unit GameModels;

interface

uses SysUtils, Serialization;

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
		property Value: Variant read FValue write FValue;

	end;

	TModelSerializationBase = class abstract
	public
		function Serialize(const vModel: TModelBase): String; virtual; abstract;
		function DeSerialize(const vSerialized: String; const vModelClass: TModelClass): TModelBase; virtual; abstract;
	end;

	TJSONModelSerialization = class (TModelSerializationBase)
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

{ this should be reimplemented for more complex models}
class function TModelBase.MessageType(): String;
begin
	result := '';
end;

{}
constructor TJSONModelSerialization.Create();
begin
	FStreamer := TGameStreamer.Create;
end;

{}
destructor TJSONModelSerialization.Destroy;
begin
	FStreamer.Free;
	inherited;
end;

{}
function TJSONModelSerialization.Serialize(const vModel: TModelBase): String;
begin
	if vModel is TEmptyModel then
		result := ''
	else if vModel is TPlaintextModel then
		result := (vModel as TPlaintextModel).Value
	else
		result := FStreamer.Streamer.ObjectToJSONString(vModel);
end;

{}
function TJSONModelSerialization.DeSerialize(const vSerialized: String; const vModelClass: TModelClass): TModelBase;
begin
	result := vModelClass.Create;

	if vModelClass.InheritsFrom(TPlaintextModel) then
		(result as TPlaintextModel).Value := vSerialized
	else
		FStreamer.DeStreamer.JSONToObject(vSerialized, result);
end;

{ implementation end }

initialization
	DummyModel := TEmptyModel.Create;

finalization
	DummyModel.Free;

end.

