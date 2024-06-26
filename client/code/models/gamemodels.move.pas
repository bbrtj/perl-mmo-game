unit GameModels.Move;

interface

uses SysUtils,
	GameModels, GameTypes, GameConfig;

type
	TMsgMove = class(TPlaintextModel)
	public
		class function MessageType(): String; override;

		property X: Single index 0 read GetValueIndexReal write SetValueIndexReal;
		property Y: Single index 1 read GetValueIndexReal write SetValueIndexReal;

	end;

	TMsgStop = class(TPlaintextModel)
	public
		class function MessageType(): String; override;

	end;


	TMsgFeedActorMovement = class(TModelBase)
	private
		FId: TUlid;
		FPosX: Single;
		FPosY: Single;
		FSpeed: Single;
		FToX: Single;
		FToY: Single;

	public
		class function MessageType(): String; override;

	published
		property id: TUlid read FId write FId;
		property x: Single read FPosX write FPosX;
		property y: Single read FPosY write FPosY;
		property speed: Single read FSpeed write FSpeed;
		property to_x: Single read FToX write FToX;
		property to_y: Single read FToY write FToY;
	end;


	TMsgFeedActorPosition = class(TModelBase)
	private
		FId: TUlid;
		FPosX: Single;
		FPosY: Single;

	public
		class function MessageType(): String; override;

	published
		property id: TUlid read FId write FId;
		property x: Single read FPosX write FPosX;
		property y: Single read FPosY write FPosY;
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

{ implementation end }

end.

