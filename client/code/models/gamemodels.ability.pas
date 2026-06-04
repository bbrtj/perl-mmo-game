unit GameModels.Ability;

interface

uses GameModels, GameTypes;

type
	TMsgUntargettedAbility = class(TModelBase)
	private
		FLoreId: TLoreId;

	public
		class function MessageType(): String; override;

	published
		property lore_id: TLoreId read FLoreId write FLoreId;

	end;

implementation

class function TMsgUntargettedAbility.MessageType(): String;
begin
	result := 'use_ability';
end;

end.

