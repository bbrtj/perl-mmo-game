unit GameModels.General;

interface

uses GameNetworkMessages, GameModels, GameTypes;

type

	TLoginResultMessage = class(TModelBase)
	private
		FSuccess: TPerlBoolean;

	published
		property success: TPerlBoolean read FSuccess write FSuccess;

	end;


	TLoginMessage = class(TModelBase)
	private
		FEmail: String;
		FPassword: String;

	published
		property email: String read FEmail write FEmail;
		property password: String read FPassword write FPassword;

	end;

implementation

initialization
	MessageTypesMap.Add(TMessageType.Create('login', TLoginMessage, TLoginResultMessage));
end.

