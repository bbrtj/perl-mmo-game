unit GameNetworkMessages;

interface

uses FGL, SysUtils,
	GameModels, GameModels.General;

type
	TMessage = class
	const
		SEPARATOR = ';';

	protected

		FId: Integer;
		FData: String;

		procedure SetBody(vBody: String); virtual;
		function GetBody(): String; virtual;

	public
		property Id: Integer read FId write FId;
		property Data: String read FData write FData;
		property Body: String read GetBody write SetBody;
	end;


	TOutMessage = class(TMessage)
	protected
		FType: String;

		procedure SetBody(vBody: String); override;
		function GetBody(): String; override;

	public
		property Typ: String read FType write FType;

	end;


	TMessageType = class
	public
		MessageType: String;
		MessageModel: TModelClass;
		MessageCallbackModel: TModelClass;

		constructor Create(const vType: String; const vModel: TModelClass);
		constructor Create(const vType: String; const vModel: TModelClass; vCallbackModel: TModelClass);
	end;


	TMessageTypes = specialize TFPGObjectList<TMessageType>;

	var
		MessageTypesMap: TMessageTypes;

	function FindMessageType(const vName: String): TMessageType;

implementation

{}
function FindMessageType(const vName: String): TMessageType;
var
	vMessageType: TMessageType;
begin
	for vMessageType in MessageTypesMap do begin
		if vMessageType.MessageType = vName then
			exit(vMessageType);
	end;

	raise Exception.Create('No such network message type: ' + vName);
end;

{}
procedure TMessage.SetBody(vBody: String);
var
	parts: TStringArray;
begin
	// TODO: handle non-existing parts of this split
	parts := vBody.Split([SEPARATOR], 2);
	FId := StrToInt(parts[0]);
	FData := parts[1];
end;

{}
function TMessage.GetBody(): String;
begin
	result := FId.ToString() + SEPARATOR + FData;
end;

{}
procedure TOutMessage.SetBody(vBody: String);
var
	parts: TStringArray;
begin
	inherited;
	// TODO: handle non-existing parts of this split
	parts := FData.Split([SEPARATOR], 2);
	FType := parts[0];
	FData := parts[1];
end;

{}
function TOutMessage.GetBody(): String;
begin
	result := FId.ToString() + SEPARATOR + FType;
	if FData <> '' then
		result += SEPARATOR + FData;
end;

{}
constructor TMessageType.Create(const vType: String; const vModel: TModelClass);
begin
	MessageType := vType;
	MessageModel := vModel;
end;

{}
constructor TMessageType.Create(const vType: String; const vModel: TModelClass; vCallbackModel: TModelClass);
begin
	MessageType := vType;
	MessageModel := vModel;
	MessageCallbackModel := vCallbackModel;
end;

{ implementation end }

initialization
	MessageTypesMap := TMessageTypes.Create;

	MessageTypesMap.Add(TMessageType.Create('login', TLoginMessage, TSuccessResultMessage));
	MessageTypesMap.Add(TMessageType.Create('logout', TEmptyModel));
	MessageTypesMap.Add(TMessageType.Create('list_characters', TEmptyModel, TCharacterListResultMessage));
	MessageTypesMap.Add(TMessageType.Create('enter_game', TPlaintextModel, TSuccessResultMessage));

finalization
	MessageTypesMap.Free;

end.

