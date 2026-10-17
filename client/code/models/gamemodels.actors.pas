unit GameModels.Actors;

interface

uses FGL, SysUtils, Classes,
	GameModels, GameModels.Move,
	GameTypes, GameConfig, Serialization;

type

	TMsgFeedActorEventHealth = class(TPlaintextModel, IModelWithUlid)
	public
		class function MessageType(): String; override;
	public
		function GetId(): TUlid;
	public
		property Id: TUlid index 0 read GetValueIndexUlid;
		property Source: TUlid index 1 read GetValueIndexUlid;
		property LoreId: TLoreId index 2 read GetValueIndexLoreId;
		property Health: Single index 3 read GetValueIndexReal;
		property Change: Single index 4 read GetValueIndexReal;
	end;

	TMsgFeedActorPublicState = class(TPlaintextModel, IModelWithUlid)
	public
		class function MessageType(): String; override;
	public
		function GetId(): TUlid;
	public
		property Id: TUlid index 0 read GetValueIndexUlid;
		property Level: Int64 index 1 read GetValueIndexInt;
		property Health: Single index 2 read GetValueIndexReal;
		property MaxHealth: Single index 3 read GetValueIndexReal;
		property HealthRegeneration: Single index 4 read GetValueIndexReal;
		property Energy: Single index 5 read GetValueIndexReal;
		property MaxEnergy: Single index 6 read GetValueIndexReal;
		property EnergyRegeneration: Single index 7 read GetValueIndexReal;
		property Size: Single index 8 read GetValueIndexReal;

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

class function TMsgFeedActorEventHealth.MessageType(): String;
begin
	result := 'actor_event_health';
end;

class function TMsgFeedActorPublicState.MessageType(): String;
begin
	result := 'actor_public_state';
end;

class function TMsgFeedActorAction.MessageType(): String;
begin
	result := 'actor_action';
end;

function TMsgFeedActorEventHealth.GetId(): TUlid;
begin
	result := self.Id;
end;

function TMsgFeedActorPublicState.GetId(): TUlid;
begin
	result := self.Id;
end;

function TMsgFeedActorAction.GetId(): TUlid;
begin
	result := self.Id;
end;

end.

