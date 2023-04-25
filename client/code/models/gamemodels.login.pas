unit GameModels.Login;

interface

uses md5,
	GameModels;

type
	TMsgLogin = class(TModelBase)
	private
		FEmail: String;
		FPassword: String;

		procedure SetPassword(const vPassword: String);

	public
		class function MessageType(): String; override;

	published
		property email: String read FEmail write FEmail;
		property password: String read FPassword write SetPassword;

	end;

implementation

class function TMsgLogin.MessageType(): String;
begin
	result := 'login';
end;

procedure TMsgLogin.SetPassword(const vPassword: String);
begin
	FPassword := MD5Print(MD5String(vPassword));
end;

{ implementation end }

end.

