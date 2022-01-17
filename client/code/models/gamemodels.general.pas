unit GameModels.General;

interface

uses GameModels, GameTypes;

type

	TLoginResultMessage = class(TMessageBase)
	public
	type
		Model = class(TModelBase)
		private
			FSuccess: TPerlBoolean;

		published
			property success: TPerlBoolean read FSuccess write FSuccess;

		end;

		constructor Create(); override;

	end;

	TLoginMessage = class(TMessageBase)
	public
	type
		Model = class(TModelBase)
		private
			FEmail: String;
			FPassword: String;

		published
			property email: String read FEmail write FEmail;
			property password: String read FPassword write FPassword;

		end;

		constructor Create(); override;
		function ResultClass(): TMessageClass; override;

	end;

implementation

{}
constructor TLoginResultMessage.Create();
begin
	FModel := self.Model.Create;
end;

{}
constructor TLoginMessage.Create();
begin
	FType := 'login';
	FModel := self.Model.Create;
end;

{}
function TLoginMessage.ResultClass(): TMessageClass;
begin
	result := TLoginResultMessage;
end;

end.
