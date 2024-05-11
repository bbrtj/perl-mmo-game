unit GameModels.Actors;

interface

uses FGL, SysUtils, Classes,
	GameModels, GameModels.Move,
	GameTypes, GameConfig, Serialization;

type

	TMsgFeedActorEvent = class(TPlaintextModel)
	public
		class function MessageType(): String; override;

		function HasEvent(): Boolean;

		property Id: String index 0 read GetValueIndex;
		property Health: Single index 1 read GetValueIndexReal;
		property MaxHealth: Single index 2 read GetValueIndexReal;
		property Energy: Single index 3 read GetValueIndexReal;
		property MaxEnergy: Single index 4 read GetValueIndexReal;

		property EventSource: String index 5 read GetValueIndex;
		property HealthChange: Single index 6 read GetValueIndexReal;

	end;

implementation

class function TMsgFeedActorEvent.MessageType(): String;
begin
	result := 'actor_event';
end;

function TMsgFeedActorEvent.HasEvent(): Boolean;
begin
	result := Length(self.RawValue) >= 6;
end;

end.

