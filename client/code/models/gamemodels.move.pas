unit GameModels.Move;

interface

uses SysUtils,
	GameModels, GameTypes, GameConfig;

type
	TMsgMove = class(TPlaintextModel)
	public
		class function MessageType(): String; override;
	public
		property X: Single index 0 read GetValueIndexReal write SetValueIndexReal;
		property Y: Single index 1 read GetValueIndexReal write SetValueIndexReal;
	end;

	TMsgStop = class(TPlaintextModel)
	public
		class function MessageType(): String; override;
	end;

	TMsgFeedActorMovement = class(TPlaintextModel, IModelWithUlid)
	public
		class function MessageType(): String; override;
	public
		function GetId(): TUlid;
	published
		property id: TUlid index 0 read GetValueIndexUlid write SetValueIndexUlid;
		property x: Single index 1 read GetValueIndexReal write SetValueIndexReal;
		property y: Single index 2 read GetValueIndexReal write SetValueIndexReal;
		property speed: Single index 3 read GetValueIndexReal write SetValueIndexReal;
		property to_x: Single index 4 read GetValueIndexReal write SetValueIndexReal;
		property to_y: Single index 5 read GetValueIndexReal write SetValueIndexReal;
	end;

	TMsgFeedActorPosition = class(TPlaintextModel, IModelWithUlid)
	public
		class function MessageType(): String; override;
	public
		function GetId(): TUlid;
	published
		property id: TUlid index 0 read GetValueIndexUlid write SetValueIndexUlid;
		property x: Single index 1 read GetValueIndexReal write SetValueIndexReal;
		property y: Single index 2 read GetValueIndexReal write SetValueIndexReal;
	end;

implementation

class function TMsgMove.MessageType(): String;
begin
	result := 'move';
end;

class function TMsgStop.MessageType(): String;
begin
	result := 'stop';
end;

class function TMsgFeedActorMovement.MessageType(): String;
begin
	result := 'actor_movement';
end;

class function TMsgFeedActorPosition.MessageType(): String;
begin
	result := 'actor_position';
end;

function TMsgFeedActorMovement.GetId(): TUlid;
begin
	result := self.id;
end;

function TMsgFeedActorPosition.GetId(): TUlid;
begin
	result := self.id;
end;

{ implementation end }

end.

