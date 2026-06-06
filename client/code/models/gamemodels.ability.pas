unit GameModels.Ability;

interface

uses GameModels, GameTypes;

type
	TMsgUseAbility = class(TModelBase)
	private
		FLoreId: TLoreId;
		FX: Single;
		FY: Single;

	public
		class function MessageType(): String; override;

	published
		property lore_id: TLoreId read FLoreId write FLoreId;
		property x: Single read FX write FX;
		property y: Single read FY write FY;

	end;

implementation

class function TMsgUseAbility.MessageType(): String;
begin
	result := 'use_ability';
end;

end.

