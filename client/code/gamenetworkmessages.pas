unit GameNetworkMessages;

interface

uses FGL, SysUtils,
	GameModels, GameModels.General;

type
	TMessage = class
	const
		// TODO: make this configurable in an xml
		cSeparator = ';';

	protected

		FId: Integer;
		FData: String;
		FType: String;

		procedure SetBody(vBody: String); virtual;
		function GetBody(): String; virtual;

	public
		property Id: Integer read FId write FId;
		property Data: String read FData write FData;
		property Typ: String read FType write FType;
		property Body: String read GetBody write SetBody;
	end;

	TOutMessage = class(TMessage);

	TMessageType = class
	public
		MessageType: String;
		MessageModel: TModelClass;
		MessageCallbackType: String;
		MessageCallbackModel: TModelClass;

		constructor Create(const vType: String; const vModel: TModelClass);
		constructor Create(const vType: String; const vModel: TModelClass; const vCallbackModel: TModelClass);
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
	vParts: TStringArray;
begin
	// TODO: handle non-existing parts of this split
	vParts := vBody.Split([cSeparator], 3);

	if length(vParts[0]) > 0 then
		FId := StrToInt(vParts[0])
	else
		FId := -1;

	FType := vParts[1];
	FData := vParts[2];
end;

{}
function TMessage.GetBody(): String;
begin
	result := FId.ToString() + cSeparator + FType + cSeparator + FData;
end;

{}
constructor TMessageType.Create(const vType: String; const vModel: TModelClass);
begin
	MessageType := vType;
	MessageModel := vModel;
end;

{}
constructor TMessageType.Create(const vType: String; const vModel: TModelClass; const vCallbackModel: TModelClass);
begin
	MessageType := vType;
	MessageModel := vModel;
	MessageCallbackType := vCallbackModel.MessageType;
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

