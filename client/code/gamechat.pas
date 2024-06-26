unit GameChat;

interface

uses FGL,
	GameTypes,
	GameNetwork, GameActors,
	GameModels, GameModels.Chat,
	GameTranslations;

type
	TChatMessage = class
	public
		Id: TUlid;
		Header: String;
		Color: String;
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

	LMessage := TChatMessage.Create(LModel.id, LModel.message);

	case LModel.&type of
		ctSay: LMessage.Color := 'fefefe';
		ctYell: LMessage.Color := 'fe0000';
		ctPrivate: begin
			LMessage.Color := 'fe00fe';
			if Length(LModel.sent_to) > 0 then
				LMessage.Header := _('to') + ' ' + LModel.sent_to;
		end;
		ctSystem: begin
			LMessage.Color := 'fefe00';
			LMessage.Content := _(LMessage.Content);
			LMessage.Header := _('System');
		end;
	end;

	GlobalActorRepository.RequestActorInfo(LMessage.Id, @LMessage.Resolve);
	FChatMessages.Add(LMessage);
end;

constructor TChatMessage.Create(const AId: TUlid; const AContent: String);
begin
	self.Id := AId;
	self.Header := '';
	self.Color := '';
	self.Content := AContent;
	self.Resolved := False;
end;

procedure TChatMessage.Resolve(Sender: TObject);
var
	LActorInfo: TGameActorRepositoryRecord;
	LFinalMessage: String;
begin
	LActorInfo := GlobalActorRepository.GetActorInfo(self.Id);

	// TODO: escape HTML
	if Length(self.Header) > 0 then
		LFinalMessage := self.Header
	else
		LFinalMessage := LActorInfo.ActorName;

	LFinalMessage := LFinalMessage + ': ' + self.Content;

	if Length(self.Color) > 0 then
		LFinalMessage := '<font color="#' + self.Color + '">' + LFinalMessage + '</font>';

	GlobalChat.Handler(LFinalMessage);
	self.Resolved := True;
end;


initialization
	GlobalChat := TGameChat.Create;

finalization
	GlobalChat.Free;

end.

