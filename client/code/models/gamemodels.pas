unit GameModels;

interface

uses SysUtils, Serialization;

type

	TModelBase = class abstract(TSerialized);

	TModelClass = class of TModelBase;

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

implementation

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
	result := FStreamer.Streamer.ObjectToJSONString(vModel);
end;

{}
function TJSONModelSerialization.DeSerialize(const vSerialized: String; const vModelClass: TModelClass): TModelBase;
begin
	result := vModelClass.Create;
	FStreamer.DeStreamer.JSONToObject(vSerialized, result);
end;

{ implementation end }

end.

