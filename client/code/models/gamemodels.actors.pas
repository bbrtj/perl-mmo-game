unit GameModels.Actors;

interface

uses FGL, SysUtils, Classes,
	GameModels, GameModels.Move,
	GameTypes, GameConfig, Serialization;

type

	TMsgFeedActorEvent = class(TPlaintextModel, IModelWithUlid)
	public
		class function MessageType(): String; override;
	public
		function GetId(): TUlid;
	public
		property Id: TUlid index 0 read GetValueIndexUlid;
		property Health: Single index 1 read GetValueIndexReal;
		property EventSource: TUlid index 2 read GetValueIndexUlid;
		property HealthChange: Single index 3 read GetValueIndexReal;
	end;

	TMsgFeedActorState = class(TPlaintextModel, IModelWithUlid)
	public
		class function MessageType(): String; override;
	public
		function GetId(): TUlid;
	public
		property Id: TUlid index 0 read GetValueIndexUlid;
		property Health: Single index 1 read GetValueIndexReal;
		property MaxHealth: Single index 2 read GetValueIndexReal;
		property HealthRegeneration: Single index 3 read GetValueIndexReal;
		property Energy: Single index 4 read GetValueIndexReal;
		property MaxEnergy: Single index 5 read GetValueIndexReal;
		property EnergyRegeneration: Single index 6 read GetValueIndexReal;
		property Size: Single index 7 read GetValueIndexReal;

	end;

	TMsgFeedActorAction = class(TPlaintextModel, IModelWithUlid)
	public
		class function MessageType(): String; override;
	public
		function GetId(): TUlid;
	public
		property Id: TUlid index 0 read GetValueIndexUlid;
		property LoreId: TLoreId index 1 read GetValueIndexLoreId;
		property Duration: Single index 2 read GetValueIndexReal;

	end;

implementation

class function TMsgFeedActorEvent.MessageType(): String;
begin
	result := 'actor_event';
end;

class function TMsgFeedActorState.MessageType(): String;
begin
	result := 'actor_state';
end;

class function TMsgFeedActorAction.MessageType(): String;
begin
	result := 'actor_action';
end;

function TMsgFeedActorEvent.GetId(): TUlid;
begin
	result := self.Id;
end;

function TMsgFeedActorState.GetId(): TUlid;
begin
	result := self.Id;
end;

function TMsgFeedActorAction.GetId(): TUlid;
begin
	result := self.Id;
end;

end.

