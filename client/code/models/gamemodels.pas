unit GameModels;

interface

uses Serialization;

type

	TModelBase = TSerialized;

	TModelClass = class of TModelBase;

	TMessageBase = class;
	TMessageClass = class of TMessageBase;

	TMessageMeta = class(TSerialized)
	protected
		FId: Integer;
		FType: String;

	published
		property n: Integer read FId write FId;
		property t: String read FType write FType;
	end;

	TMessageBase = class abstract(TMessageMeta)
	protected
		FModel: TModelBase;

	public
		destructor Destroy(); override;
		function ResultClass(): TMessageClass; virtual; abstract;

	published
		property d: TModelBase read FModel write FModel;
	end;

implementation

destructor TMessageBase.Destroy();
begin
	if FModel <> nil then
		FModel.Free;
	inherited;
end;

end.
