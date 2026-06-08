unit GameProjectiles;

interface

uses SysUtils, Classes, Contnrs, Math,
	CastleUIControls, CastleControls, CastleRectangles, CastleBoxes,
	CastleTransform, CastleVectors, CastleViewport,
	GameTypes, GameLore, GameExceptions, GameConfig, GameMechanics;

type

	TGameProjectile = class(TGameModel)
	strict private
		FId: TUlid;
		FMovementVector: TVector3;
		FMovementTime: Single;
		FRadius: Single;
	public
		constructor Create(AOwner: TComponent);

		procedure SetPosition(X, Y: Single);
		function GetPosition(): TVector3;
		procedure Move(Angle, Speed, MaxDistance: Single);
		procedure SetSize(Radius: Single);

		procedure Update(const secondsPassed: Single; var removeMe: TRemoveType); override;
		function Finished(): Boolean;

		property Id: TUlid read FId write FId;
	end;

	TGameProjectileFactory = class
	strict private
		FUIViewport: TCastleViewport;
		FUIBoard: TCastleTransform;

	public
		constructor Create(Viewport: TCastleViewport; Board: TCastleTransform);

		function CreateProjectile(Id: TUlid; LoreId: TLoreId): TGameProjectile;
		procedure RemoveProjectile(Projectile: TGameProjectile);
	end;

implementation

procedure TGameProjectile.Update(const secondsPassed: Single; var removeMe: TRemoveType);
begin
	if FMovementTime > 0 then begin
		FMovementTime -= secondsPassed;
		self.Translation := self.Translation + FMovementVector * secondsPassed;
	end;

	self.Up := self.Up + FMovementVector;

	inherited;
end;

function TGameProjectile.Finished(): Boolean;
begin
	result := FMovementTime <= 0;
end;

constructor TGameProjectileFactory.Create(Viewport: TCastleViewport; Board: TCastleTransform);
begin
	FUIViewport := Viewport;
	FUIBoard := Board;
end;

function TGameProjectileFactory.CreateProjectile(Id: TUlid; LoreId: TLoreId): TGameProjectile;
begin
	result := TGameProjectile.Create(FUIBoard);
	result.Id := Id;
	result.Name := 'Projectile_' + Id;

	// TODO: use LoreId to get the appearance of the projectile
	result.URL := 'castle-data:/images/projectile.png';
	result.Translation := Vector3(0, 0, 99); // TODO: proper Z distance

	FUIBoard.Parent.Add(result);
end;

procedure TGameProjectileFactory.RemoveProjectile(Projectile: TGameProjectile);
begin
	FUIBoard.Parent.RemoveDelayed(Projectile, True);
end;

constructor TGameProjectile.Create(AOwner: TComponent);
begin
	inherited;
end;

procedure TGameProjectile.SetPosition(X, Y: Single);
begin
	self.Translation := Vector3(X, Y, self.Translation.Z);
end;

function TGameProjectile.GetPosition(): TVector3;
begin
	result := self.Translation;
end;

procedure TGameProjectile.Move(Angle, Speed, MaxDistance: Single);
begin
	FMovementVector := AngleToVector(Angle) * Speed;
	FMovementTime := MaxDistance / FMovementVector.Length;
end;

procedure TGameProjectile.SetSize(Radius: Single);
var
	LBox: TBox3D;
	LCurrentRadius: Single;
begin
	FRadius := Radius;
	LBox := self.BoundingBox;

	// NOTE: Box3D has width / height, while Size is a radius of a circle
	LCurrentRadius := Max(LBox.Size.X, LBox.Size.Y) / 2;
	self.Scale := self.Scale * Vector3(Radius / LCurrentRadius, Radius / LCurrentRadius, 1);
end;

end.

