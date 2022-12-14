unit GameModels.Login;

interface

uses GameModels;

type
	TMsgLogin = class(TModelBase)
	private
		FEmail: String;
		FPassword: String;

	public
		class function MessageType(): String; override;

	published
		property email: String read FEmail write FEmail;
		property password: String read FPassword write FPassword;

	end;

implementation

{}
class function TMsgLogin.MessageType(): String;
begin
	result := 'login';
end;

{ implementation end }

end.

