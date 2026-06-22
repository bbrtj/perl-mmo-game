unit GameModels.Ability;

interface

uses GameModels, GameTypes;

type
	TMsgUseAbility = class(TPlaintextModel)
	public
		class function MessageType(): String; override;
	published
		property lore_id: TLoreId index 0 read GetValueIndexLoreId write SetValueIndexLoreId;
		property x: Single index 1 read GetValueIndexReal write SetValueIndexReal;
		property y: Single index 2 read GetValueIndexReal write SetValueIndexReal;
	end;

implementation

class function TMsgUseAbility.MessageType(): String;
begin
	result := 'use_ability';
end;

end.

