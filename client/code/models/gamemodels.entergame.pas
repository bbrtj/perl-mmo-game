unit GameModels.EnterGame;

interface

uses GameModels;

type
	TMsgEnterGame = class(TPlaintextModel)
	public
		class function MessageType(): String; override;

	end;

implementation

{}
class function TMsgEnterGame.MessageType(): String;
begin
	result := 'enter_game';
end;

{ implementation end }

end.

