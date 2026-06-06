unit GameModels.Projectiles;

interface

uses GameModels, GameTypes;

type
	TMsgFeedProjectile = class(TPlaintextModel)
	public
		class function MessageType(): String; override;

		property Id: TUlid index 0 read GetValueIndexUlid;
		property LoreId: TLoreId index 1 read GetValueIndexLoreId;
		property X: Single index 2 read GetValueIndexReal;
		property Y: Single index 3 read GetValueIndexReal;
		property Speed: Single index 4 read GetValueIndexReal;
		property Angle: Single index 5 read GetValueIndexReal;
		property MaxDistance: Single index 6 read GetValueIndexReal;
	end;

	TMsgFeedProjectileStop = class(TPlaintextModel)
	public
		class function MessageType(): String; override;

		property Id: TUlid index 0 read GetValueIndexUlid;
	end;

implementation

class function TMsgFeedProjectile.MessageType(): String;
begin
	result := 'projectile';
end;

class function TMsgFeedProjectileStop.MessageType(): String;
begin
	result := 'projectile_stop';
end;

end.

