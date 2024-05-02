unit GameModels.Chat;

interface

uses GameModels, GameTypes;

type

	TMsgChatSay = class(TPlaintextModel)
	public
		class function MessageType(): String; override;
	end;

	TChatType = (ctSay, ctYell, ctPrivate, ctSystem);

	TMsgFeedChat = class(TModelBase)
	private
		FId: TLoreId;
		FMessage: String;
		FType: TChatType;
		FSentTo: String;

	public
		class function MessageType(): String; override;

	published
		property id: TLoreId read FId write FId;
		property message: String read FMessage write FMessage;
		property &type: TChatType read FType write FType;
		property sent_to: String read FSentTo write FSentTo;
	end;

implementation

class function TMsgChatSay.MessageType(): String;
begin
	result := 'say';
end;

class function TMsgFeedChat.MessageType(): String;
begin
	result := 'chat';
end;

end.

