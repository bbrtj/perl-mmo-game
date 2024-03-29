unit GameModels.CharacterList;

interface

uses FGL,
	GameModels, GameTypes,
	Serialization;

type
	TMsgCharacterList = class(TEmptyModel)
	public
		class function MessageType(): String; override;

	end;

	TMsgResCharacter = class(TSerialized)
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

	TMsgResCharacters = specialize TFPGObjectList<TMsgResCharacter>;

	TMsgResCharacterList = class(TModelBase)
	private
		FCharacters: TMsgResCharacters;

	public
		constructor Create(); override;
		destructor Destroy; override;

		class function MessageType(): String; override;

	published
		property list: TMsgResCharacters read FCharacters write FCharacters;
	end;

implementation

class function TMsgCharacterList.MessageType(): String;
begin
	result := 'list_characters';
end;

constructor TMsgResCharacterList.Create();
begin
	FCharacters := TMsgResCharacters.Create;
end;

destructor TMsgResCharacterList.Destroy;
begin
	FCharacters.Free;
end;

class function TMsgResCharacterList.MessageType(): String;
begin
	result := 'character_list';
end;

{ implementation end }

initialization
	ListSerializationMap.Add(TSerializedList.Create(TMsgResCharacters, TMsgResCharacter));

end.

