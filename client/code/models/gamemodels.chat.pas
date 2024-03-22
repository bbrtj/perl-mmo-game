unit GameModels.Chat;

interface

uses GameModels, GameTypes;

type

	TMsgChatSay = class(TPlaintextModel)
	public
		class function MessageType(): String; override;
	end;

	TMsgChatYell = class(TPlaintextModel)
	public
		class function MessageType(): String; override;
	end;

	TMsgFeedChat = class(TModelBase)
	private
		FId: TLoreId;
		FMessage: String;
		FWhisper: Boolean;

	public
		class function MessageType(): String; override;

	published
		property id: TLoreId read FId write FId;
		property message: String read FMessage write FMessage;
		property whisper: Boolean read FWhisper write FWhisper;
	end;

implementation

class function TMsgChatSay.MessageType(): String;
begin
	result := 'say';
end;

class function TMsgChatYell.MessageType(): String;
begin
	result := 'yell';
end;

class function TMsgFeedChat.MessageType(): String;
begin
	result := 'chat';
end;

end.

