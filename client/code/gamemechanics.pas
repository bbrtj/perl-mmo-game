unit GameMechanics;

interface

uses Math,
	CastleVectors,
	GameTypes;

function AngleToVector(Angle: Single): TVector3;

implementation

function AngleToVector(Angle: Single): TVector3;
begin
	result := Vector3(Cos(Angle), Sin(Angle), 0);
end;

end.

