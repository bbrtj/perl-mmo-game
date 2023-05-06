unit GameModels.Ability;

interface

uses GameModels;

type
	TMsgUntargettedAbility = class(TModelBase)
	private
		FAbility: String;

	public
		class function MessageType(): String; override;

	published
		property ability: String read FAbility write FAbility;

	end;

implementation

class function TMsgUntargettedAbility.MessageType(): String;
begin
	result := 'use_ability';
end;

{ implementation end }

end.

