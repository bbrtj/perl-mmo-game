unit GameActors;

interface

uses Classes,
	CastleTransform, CastleVectors,
	GameTypes;

type

	TGameActor = class(TCastleBehavior)
	strict private
		FModel: TGameModel;

	public
		constructor Create(const vModel: TGameModel);

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

	FModel.Translation := Vector3(0, 0, 0.01); // TODO
end;

procedure TGameActor.Update(const secondsPassed: Single; var removeMe: TRemoveType);
begin
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

{ implementation end }

end.

