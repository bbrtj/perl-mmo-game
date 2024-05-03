unit GameNetworkMessages;

interface

uses FGL, SysUtils,
	GameModels, GameModels.General, GameModels.Login, GameModels.Logout,
	GameModels.CharacterList, GameModels.EnterGame, GameModels.Location,
	GameModels.Move, GameModels.Discovery, GameModels.Ability,
	GameModels.Chat;

type
	TMessage = class
	const
		// TODO: make this configurable in an xml
		cSeparator = ';';

	protected

		FId: Integer;
		FData: String;
		FType: String;

		procedure SetBody(Body: String); virtual;
		function GetBody(): String; virtual;

	public
		property Id: Integer read FId write FId;
		property Data: String read FData write FData;
		property Typ: String read FType write FType;
		property Body: String read GetBody write SetBody;

		function HasId(): Boolean;
	end;

	TOutMessage = class(TMessage);

	TMessageType = class
	private
		FModel: TModelClass;
		FCallbackModel: TModelClass;

	public
		constructor Create(const Model: TModelClass);
		constructor Create(const Model: TModelClass; const CallbackModel: TModelClass);

		function HasCallback(): Boolean;
		function GetType(): String;

		property Model: TModelClass read FModel write FModel;
		property CallbackModel: TModelClass read FCallbackModel write FCallbackModel;
	end;

	TMessageTypes = specialize TFPGObjectList<TMessageType>;

var
	MessageTypesMap: TMessageTypes;
	FeedTypesMap: TMessageTypes;

function FindMessageType(const MessageType: TModelClass): TMessageType;
function FindFeedType(const MessageType: TModelClass): TMessageType;

implementation

function FindMessageType(const MessageType: TModelClass): TMessageType;
var
	LMessageType: TMessageType;
begin
	for LMessageType in MessageTypesMap do begin
		if LMessageType.Model = MessageType then
			exit(LMessageType);
	end;

	raise Exception.Create('No such network message type: ' + MessageType.MessageType);
end;

function FindFeedType(const MessageType: TModelClass): TMessageType;
var
	LFeedType: TMessageType;
begin
	for LFeedType in FeedTypesMap do begin
		if LFeedType.Model = MessageType then
			exit(LFeedType);
	end;

	raise Exception.Create('No such network feed type: ' + MessageType.MessageType);
end;

procedure TMessage.SetBody(Body: String);
var
	LParts: TStringArray;
begin
	// TODO: handle non-existing parts of this split
	LParts := Body.Split([cSeparator]);

	if length(LParts[0]) > 0 then
		FId := StrToInt(LParts[0])
	else
		FId := -1;

	FType := LParts[1];
	FData := String.Join(cSeparator, LParts, 2, length(LParts) - 2);
end;

function TMessage.GetBody(): String;
begin
	result := FId.ToString() + cSeparator + FType + cSeparator + FData;
end;

function TMessage.HasId(): Boolean;
begin
	result := FId >= 0;
end;

constructor TMessageType.Create(const Model: TModelClass);
begin
	FModel := Model;
	FCallbackModel := nil;
end;

constructor TMessageType.Create(const Model: TModelClass; const CallbackModel: TModelClass);
begin
	FModel := Model;
	FCallbackModel := CallbackModel;
end;

function TMessageType.GetType(): String;
begin
	result := FModel.MessageType;
end;

function TMessageType.HasCallback(): Boolean;
begin
	result := FCallbackModel <> nil;
end;

initialization
	MessageTypesMap := TMessageTypes.Create;
	FeedTypesMap := TMessageTypes.Create;

	MessageTypesMap.Add(TMessageType.Create(TMsgLogin, TMsgResSuccess));
	MessageTypesMap.Add(TMessageType.Create(TMsgLogout));
	MessageTypesMap.Add(TMessageType.Create(TMsgCharacterList, TMsgResCharacterList));
	MessageTypesMap.Add(TMessageType.Create(TMsgEnterGame, TMsgResSuccess));
	MessageTypesMap.Add(TMessageType.Create(TMsgMove));
	MessageTypesMap.Add(TMessageType.Create(TMsgStop));
	MessageTypesMap.Add(TMessageType.Create(TMsgUntargettedAbility));
	MessageTypesMap.Add(TMessageType.Create(TMsgChatSay));
	MessageTypesMap.Add(TMessageType.Create(TMsgActorsInfo, TMsgResActorsInfo));

	FeedTypesMap.Add(TMessageType.Create(TMsgFeedLocationData));
	FeedTypesMap.Add(TMessageType.Create(TMsgFeedActorMovement));
	FeedTypesMap.Add(TMessageType.Create(TMsgFeedActorPosition));
	FeedTypesMap.Add(TMessageType.Create(TMsgFeedDiscovery));
	FeedTypesMap.Add(TMessageType.Create(TMsgFeedChat));

finalization
	MessageTypesMap.Free;

end.

