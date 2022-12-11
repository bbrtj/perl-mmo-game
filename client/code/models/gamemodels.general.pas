unit GameModels.General;

interface

uses FGL,
	GameModels, GameTypes,
	Serialization;

type
	TSuccessResultMessage = class(TPlaintextModel);

	TLoginMessage = class(TModelBase)
	private
		FEmail: String;
		FPassword: String;

	published
		property email: String read FEmail write FEmail;
		property password: String read FPassword write FPassword;

	end;

	TCharacterResultMessage = class(TModelBase)
	private
		FId: TUlid;
		FName: String;
		FClass: TLoreId;
		FLastOnline: Variant;

	published
		property id: TUlid read FId write FId;
		property name: String read FName write FName;
		property &class: TLoreId read FClass write FClass;
		property last_online: Variant read FLastOnline write FLastOnline;

	end;

	TCharacterResultMessageList = specialize TFPGObjectList<TCharacterResultMessage>;

	TCharacterListResultMessage = class(TModelBase)
	private
		FCharacters: TCharacterResultMessageList;

	public
		constructor Create(); override;
		destructor Destroy; override;

	published
		property characters: TCharacterResultMessageList read FCharacters write FCharacters;
	end;

implementation

{}
constructor TCharacterListResultMessage.Create();
begin
	FCharacters := TCharacterResultMessageList.Create;
end;

{}
destructor TCharacterListResultMessage.Destroy;
begin
	FCharacters.Free;
end;

{ implementation end }

initialization
	ListSerializationMap.Add(TSerializedList.Create(TCharacterResultMessageList, TCharacterResultMessage));

end.

