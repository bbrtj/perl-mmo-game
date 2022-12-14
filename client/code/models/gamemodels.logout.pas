unit GameModels.Logout;

interface

uses GameModels;

type
	TMsgLogout = class(TEmptyModel)
	public
		class function MessageType(): String; override;

	end;

implementation

class function TMsgLogout.MessageType(): String;
begin
	result := 'logout';
end;

{ implementation end }

end.

