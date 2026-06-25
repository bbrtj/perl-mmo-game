unit GameChat;

interface

uses SysUtils, FGL, System.UIConsts,
	GameTypes, GameLog, GameMessageLog,
	GameNetwork, GameActors,
	GameModels, GameModels.Chat,
	GameTranslations;

type
	TChatMessage = class
	public
		Id: TUlid;
		Header: String;
		Message: TColoredMessage;
		Resolved: Boolean;
	public
		constructor Create(const AId: TUlid; const AHeader: String; const AMessage: TColoredMessage);
	public
		procedure Resolve(Sender: TObject);
	end;

	TChatMessageList = specialize TFPGObjectList<TChatMessage>;

	TGameChat = class
	strict private
		FMessageLog: TGameMessageLog;
		FChatMessages: TChatMessageList;
	private
		procedure SetMessageLog(AMessageLog: TGameMessageLog);
	public
		constructor Create();
		destructor Destroy; override;
	public
		procedure OnChatMessage(const Data: TModelBase);
	public
		property MessageLog: TGameMessageLog read FMessageLog write SetMessageLog;
	end;

var
	GlobalChat: TGameChat;

implementation

constructor TGameChat.Create();
begin
	FChatMessages := TChatMessageList.Create;
end;

destructor TGameChat.Destroy;
begin
	FChatMessages.Free;
end;

procedure TGameChat.SetMessageLog(AMessageLog: TGameMessageLog);
var
	LWasSet: Boolean;
begin
	LWasSet := FMessageLog <> nil;
	FMessageLog.Free;
	FMessageLog := AMessageLog;

	if (AMessageLog = nil) and LWasSet then
		GlobalClient.StopWaiting(TMsgFeedChat)
	else if (AMessageLog <> nil) and (not LWasSet) then
		GlobalClient.Await(TMsgFeedChat, @OnChatMessage);
end;

procedure TGameChat.OnChatMessage(const Data: TModelBase);
var
	I: Integer;
	LModel: TMsgFeedChat;
	LHeader: String;
	LColoredMsg: TColoredMessage;
	LMessage: TChatMessage;
begin
	LModel := Data as TMsgFeedChat;
	LColoredMsg.Content := LModel.message;
	LHeader := '';

	case LModel.&type of
		ctSay: LColoredMsg.Color := MakeColor($FE, $FE, $FE);
		ctYell: LColoredMsg.Color := MakeColor($7F, $00, $00);
		ctPrivate: begin
			LColoredMsg.Color := MakeColor($FE, $00, $FE);
			if Length(LModel.sent_to) > 0 then
				LHeader := _('to') + ' ' + LModel.sent_to;
		end;
		ctSystem: begin
			LColoredMsg.Color := MakeColor($FE, $FE, $00);
			LColoredMsg.Content := _(LColoredMsg.Content);
			LHeader := _('System');
		end;
	end;

	LMessage := TChatMessage.Create(LModel.id, LHeader, LColoredMsg);
	GlobalActorRepository.RequestActorInfo(LMessage.Id, @LMessage.Resolve);

	// TODO: maybe don't clear messages ASAP
	for I := FChatMessages.Count - 1 downto 0 do begin
		if FChatMessages[I].Resolved then
			FChatMessages.Delete(I);
	end;

	FChatMessages.Add(LMessage);
end;

constructor TChatMessage.Create(const AId: TUlid; const AHeader: String; const AMessage: TColoredMessage);
begin
	self.Id := AId;
	self.Header := AHeader;
	self.Message := AMessage;
	self.Resolved := False;
end;

procedure TChatMessage.Resolve(Sender: TObject);
var
	LActorInfo: TGameActorRepositoryRecord;
	LHeader: String;
begin
	if self.Resolved then exit;

	LActorInfo := GlobalActorRepository.GetActorInfo(self.Id);

	// TODO: escape HTML
	if Length(self.Header) > 0 then
		LHeader := self.Header
	else
		LHeader := LActorInfo.ActorName;

	self.Message.Content := LHeader + ': ' + self.Message.Content;
	GlobalChat.MessageLog.AddLine(self.Message.ToString);
	self.Resolved := True;
end;

initialization
	GlobalChat := TGameChat.Create;

finalization
	FreeAndNil(GlobalChat);

end.

