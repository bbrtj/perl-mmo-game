unit GameActors;

interface

uses Classes,
	CastleTransform, CastleVectors,
	GameTypes;

type

	TGameActor = class(TGameModel)
	strict private
	const
		cTurnSpeed = 0.25;

	var
		FMovementVector: TVector3;
		FMovementTime: Single;

	public
		procedure SetPosition(const vX, vY: Single);
		function GetPosition(): TVector3;
		procedure Move(const vX, vY, vSpeed: Single);
		procedure Stop();

		procedure Update(const secondsPassed: Single; var removeMe: TRemoveType); override;
	end;

	TGameActorFactory = class
	strict private
		FUIBoard: TCastleTransform;

	public
		constructor Create(const vBoard: TCastleTransform);

		function CreateActor(vId: TUlid): TGameActor;
		procedure RemoveActor(const vActor: TGameActor);
	end;

implementation

procedure TGameActor.Update(const secondsPassed: Single; var removeMe: TRemoveType);
begin
	if FMovementTime > 0 then begin
		FMovementTime -= secondsPassed;
		self.Translation := self.Translation + FMovementVector * secondsPassed;
	end;

	if not (FMovementVector - self.Up).IsZero then
		self.Up := self.Up + FMovementVector * cTurnSpeed;
end;

constructor TGameActorFactory.Create(const vBoard: TCastleTransform);
begin
	FUIBoard := vBoard;
end;

function TGameActorFactory.CreateActor(vId: TUlid): TGameActor;
begin
	result := TGameActor.Create(FUIBoard);
	result.Name := 'Actor_' + vId;

	// TODO: use vId to get info about the appearance of the actor from some other component
	// (which will manage network in return, to get this data)
	result.URL := 'castle-data:/images/player.png';
	result.Scale := Vector3(0.0025, 0.0025, 0.0025); // TODO: scale properly
	result.Translation := Vector3(0, 0, 0.05); // TODO: proper Z distance

	// TODO: set up some properties of the actor, like position, health etc. Or
	// maybe use more automated means of updating them according to network
	// data

	FUIBoard.Parent.Add(result);
end;

procedure TGameActorFactory.RemoveActor(const vActor: TGameActor);
begin
	FUIBoard.Parent.RemoveDelayed(vActor, True);
end;

procedure TGameActor.SetPosition(const vX, vY: Single);
begin
	self.Translation := Vector3(vX, vY, self.Translation.Z);
end;

function TGameActor.GetPosition(): TVector3;
begin
	result := self.Translation;
end;

procedure TGameActor.Move(const vX, vY, vSpeed: Single);
begin
	FMovementVector := Vector3(vX - self.Translation.X, vY - self.Translation.Y, 0);
	FMovementTime := FMovementVector.Length / vSpeed;
	FMovementVector /= FMovementTime;
end;

procedure TGameActor.Stop();
begin
	FMovementTime := 0;
end;

{ implementation end }

end.

