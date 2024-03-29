unit GameChat;

interface

uses FGL,
	GameTypes,
	GameNetwork, GameActors,
	GameModels, GameModels.Chat;

type
	TChatMessage = class
	public
		Id: TUlid;
		Content: String;
		Resolved: Boolean;

		constructor Create(const AId: TUlid; const AContent: String);

		procedure Resolve(Sender: TObject);
	end;

	TChatMessageList = specialize TFPGObjectList<TChatMessage>;

	TGameChatHandler = procedure(Message: String) of object;

	TGameChat = class
	strict private
		FHandler: TGameChatHandler;
		FChatMessages: TChatMessageList;

		procedure SetHandler(const AHandler: TGameChatHandler);

	public
		constructor Create();
		destructor Destroy; override;

		procedure OnChatMessage(const Data: TModelBase);

		property Handler: TGameChatHandler read FHandler write SetHandler;
	end;

var
	GlobalChat: TGameChat;

implementation

constructor TGameChat.Create();
begin
	FHandler := nil;
	FChatMessages := TChatMessageList.Create;
end;

destructor TGameChat.Destroy;
begin
	FChatMessages.Free;
end;

procedure TGameChat.SetHandler(const AHandler: TGameChatHandler);
var
	LWasSet: Boolean;
begin
	LWasSet := FHandler <> nil;
	FHandler := AHandler;

	if (AHandler = nil) and LWasSet then
		GlobalClient.StopWaiting(TMsgFeedChat)
	else if (AHandler <> nil) and (not LWasSet) then
		GlobalClient.Await(TMsgFeedChat, @OnChatMessage);
end;

procedure TGameChat.OnChatMessage(const Data: TModelBase);
var
	LModel: TMsgFeedChat;
	LMessage: TChatMessage;
begin
	LModel := Data as TMsgFeedChat;

	// TODO: whisper
	LMessage := TChatMessage.Create(LModel.id, LModel.message);
	GlobalActorRepository.RequestActorInfo(LMessage.Id, @LMessage.Resolve);
	FChatMessages.Add(LMessage);
end;

constructor TChatMessage.Create(const AId: TUlid; const AContent: String);
begin
	self.Id := AId;
	self.Content := AContent;
	self.Resolved := False;
end;

procedure TChatMessage.Resolve(Sender: TObject);
var
	LActorInfo: TGameActorRepositoryRecord;
begin
	LActorInfo := GlobalActorRepository.GetActorInfo(self.Id);
	GlobalChat.Handler(LActorInfo.ActorName + ': ' + self.Content);
	self.Resolved := True;
end;


initialization
	GlobalChat := TGameChat.Create;

finalization
	GlobalChat.Free;

end.

