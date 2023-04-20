unit GameActors;

interface

uses Classes,
	CastleTransform, CastleVectors,
	GameTypes;

type

	TGameActor = class(TCastleBehavior)
	strict private
	const
		cTurnSpeed = 0.25;

	var
		FModel: TGameModel;

		FMovementVector: TVector3;
		FMovementTime: Single;
	public
		constructor Create(const vModel: TGameModel);

		procedure SetPosition(const vX, vY: Single);
		function GetPosition(): TVector3;
		procedure Move(const vX, vY, vSpeed: Single);

		procedure Update(const secondsPassed: Single; var removeMe: TRemoveType); override;
	end;

	TGameActorFactory = class
	strict private
		FBoard: TCastleTransform;

	public
		constructor Create(const vBoard: TCastleTransform);

		function CreateActor(vId: TUlid): TGameActor;
	end;

implementation

constructor TGameActor.Create(const vModel: TGameModel);
begin
	inherited Create(vModel);
	FModel := vModel;
	FModel.AddBehavior(self);

	FModel.Translation := Vector3(0, 0, 0.05); // TODO
end;

procedure TGameActor.Update(const secondsPassed: Single; var removeMe: TRemoveType);
begin
	if FMovementTime > 0 then begin
		FMovementTime -= secondsPassed;
		FModel.Translation := FModel.Translation + FMovementVector * secondsPassed;
		if not (FMovementVector - FModel.Up).IsZero then
			FModel.Up := FModel.Up + FMovementVector * cTurnSpeed;
	end;
end;

constructor TGameActorFactory.Create(const vBoard: TCastleTransform);
begin
	FBoard := vBoard;
end;

function TGameActorFactory.CreateActor(vId: TUlid): TGameActor;
var
	vModel: TGameModel;
begin
	vModel := TGameModel.Create(FBoard);

	// TODO: use vId to get info about the appearance of the actor from some other component
	// (which will manage network in return, to get this data)
	vModel.URL := 'castle-data:/images/player.png';
	vModel.Scale := Vector3(0.01, 0.01, 0.01);

	result := TGameActor.Create(vModel);
	// TODO: set up some properties of the actor, like position, health etc. Or
	// maybe use more automated means of updating them according to network
	// data

	FBoard.Parent.Add(vModel);
end;

procedure TGameActor.SetPosition(const vX, vY: Single);
begin
	FModel.Translation := Vector3(vX, vY, FModel.Translation.Z);
end;

function TGameActor.GetPosition(): TVector3;
begin
	result := FModel.Translation;
end;

procedure TGameActor.Move(const vX, vY, vSpeed: Single);
begin
	FMovementVector := Vector3(vX - FModel.Translation.X, vY - FModel.Translation.Y, 0);
	FMovementTime := FMovementVector.Length / vSpeed;
	FMovementVector /= FMovementTime;
end;

{ implementation end }

end.

