unit GameModels.General;

interface

uses GameModels;

type
	TMsgResSuccess = class(TPlaintextModel)
	public
		class function MessageType(): String; override;
	end;

	TMsgResError = class(TPlaintextModel)
	public
		class function MessageType(): String; override;
	published
		property Msg: String index 0 read GetValueIndex write SetValueIndex;
	end;

implementation

class function TMsgResSuccess.MessageType(): String;
begin
	result := 'success';
end;

class function TMsgResError.MessageType(): String;
begin
	result := 'error';
end;

end.

