unit GameActors;

interface

uses SysUtils, Classes, Contnrs,
	CastleTransform, CastleVectors,
	GameTypes, GameNetwork, GameModels, GameModels.Discovery;

type

	TGameActor = class(TGameModel)
	strict private
	const
		cTurnSpeed = 0.25;

	var
		FMovementVector: TVector3;
		FMovementTime: Single;

		FHealth: Single;
		FMaxHealth: Single;
		FEnergy: Single;
		FMaxEnergy: Single;

	public
		procedure SetPosition(X, Y: Single);
		function GetPosition(): TVector3;
		procedure Move(X, Y, Speed: Single);
		procedure Stop();

		procedure SetHealth(Current, Max: Single);
		procedure SetEnergy(Current, Max: Single);

		procedure Update(const secondsPassed: Single; var removeMe: TRemoveType); override;
	end;

	TGameActorFactory = class
	strict private
		FUIBoard: TCastleTransform;

	public
		constructor Create(const Board: TCastleTransform);

		function CreateActor(Id: TUlid): TGameActor;
		procedure RemoveActor(const Actor: TGameActor);
	end;

	TGameActorRepositoryRecord = class
	strict private
		FName: String;
		FClass: TLoreId;

	public
		property ActorName: String read FName write FName;
		property ActorClass: TLoreId read FClass write FClass;

	end;

	TGameActorRepository = class
	strict private
		FActorData: TFPHashObjectList;

		procedure OnActorsInfo(const ActorsInfo: TModelBase);

	public
		constructor Create();
		destructor Destroy; override;

		procedure RequestActorInfo(const Id: TUlid; const Notify: TNotifyEvent);
		function GetActorInfo(const Id: TUlid): TGameActorRepositoryRecord;
	end;

var
	GlobalActorRepository: TGameActorRepository;

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

constructor TGameActorFactory.Create(const Board: TCastleTransform);
begin
	FUIBoard := Board;
end;

function TGameActorFactory.CreateActor(Id: TUlid): TGameActor;
begin
	result := TGameActor.Create(FUIBoard);
	result.Name := 'Actor_' + Id;

	// TODO: use Id to get info about the appearance of the actor from some other component
	// (which will manage network in return, to get this data)
	result.URL := 'castle-data:/images/player.png';
	result.Scale := Vector3(0.0025, 0.0025, 1); // TODO: scale properly
	result.Translation := Vector3(0, 0, 100); // TODO: proper Z distance

	// TODO: set up some properties of the actor, like position, health etc. Or
	// maybe use more automated means of updating them according to network
	// data

	FUIBoard.Parent.Add(result);
end;

procedure TGameActorFactory.RemoveActor(const Actor: TGameActor);
begin
	FUIBoard.Parent.RemoveDelayed(Actor, True);
end;

procedure TGameActor.SetPosition(X, Y: Single);
begin
	self.Translation := Vector3(X, Y, self.Translation.Z);
end;

function TGameActor.GetPosition(): TVector3;
begin
	result := self.Translation;
end;

procedure TGameActor.Move(X, Y, Speed: Single);
begin
	FMovementVector := Vector3(X - self.Translation.X, Y - self.Translation.Y, 0);
	FMovementTime := FMovementVector.Length / Speed;
	FMovementVector /= FMovementTime;
end;

procedure TGameActor.Stop();
begin
	FMovementTime := 0;
end;

procedure TGameActor.SetHealth(Current, Max: Single);
begin
	FHealth := Current;
	FMaxHealth := Max;
end;

procedure TGameActor.SetEnergy(Current, Max: Single);
begin
	FEnergy := Current;
	FMaxEnergy := Max;
end;

constructor TGameActorRepository.Create();
begin
	FActorData := TFPHashObjectList.Create;
end;

destructor TGameActorRepository.Destroy;
begin
	FActorData.Free;
end;

procedure TGameActorRepository.OnActorsInfo(const ActorsInfo: TModelBase);
var
	LActorsInfo: TMsgResActorsInfo;
	LRecord: TGameActorRepositoryRecord;
	i: Integer;
begin
	LActorsInfo := ActorsInfo as TMsgResActorsInfo;

	for i := 0 to LActorsInfo.list.Count - 1 do begin
		LRecord := TGameActorRepositoryRecord.Create;
		LRecord.ActorName := LActorsInfo.list[i].name;
		LRecord.ActorClass := LActorsInfo.list[i].&class;

		FActorData.Add(LActorsInfo.list[i].id, LRecord);
	end;
end;

procedure TGameActorRepository.RequestActorInfo(const Id: TUlid; const Notify: TNotifyEvent);
var
	LObject: TObject;
	LModel: TMsgActorsInfo;
begin
	LObject := FActorData.Find(Id);
	if LObject = nil then begin
		LModel := TMsgActorsInfo.Create;
		LModel.AddActor(Id);
		GlobalClient.Send(TMsgActorsInfo, LModel, @OnActorsInfo, Notify);
	end
	else
		Notify(self);
end;

function TGameActorRepository.GetActorInfo(const Id: TUlid): TGameActorRepositoryRecord;
var
	LObject: TObject;
	LModel: TMsgActorsInfo;
begin
	LObject := FActorData.Find(Id);
	if LObject = nil then
		raise Exception.Create('Actor info needs to be requested before fetching');

	result := LObject as TGameActorRepositoryRecord;
end;

initialization
	GlobalActorRepository := TGameActorRepository.Create;

finalization
	GlobalActorRepository.Free;
end.

