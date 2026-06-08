unit GameActors;

interface

uses SysUtils, Classes, Contnrs, Math,
	CastleUIControls, CastleControls, CastleRectangles,
	CastleTransform, CastleVectors, CastleViewport, CastleBoxes,
	GameTypes, GameLore, GameExceptions, GameNetwork, GameConfig,
	GameModels, GameModels.Discovery;

type

	TPlayerBehavior = class(TCastleBehavior)
	strict private
		FUICamera: TCastleCamera;

	public
		procedure Update(const secondsPassed: Single; var removeMe: TRemoveType); override;

		property Camera: TCastleCamera read FUICamera write FUICamera;
	end;

	TGameActor = class(TGameModel)
	strict private
	const
		cTurnSpeed = 0.25;

	var
		FPlate: TCastleDesign;
		FPlateInitialHeight: Single;

		FId: TUlid;
		FName: String;

		FMovementVector: TVector3;
		FMovementTime: Single;

		FActionDuration: Single;
		FActionName: String;
		FActionTime: Single;

		FHealth: Single;
		FMaxHealth: Single;
		FHealthRegeneration: Single;
		FEnergy: Single;
		FMaxEnergy: Single;
		FEnergyRegeneration: Single;

		FSize: Single;

		procedure UpdatePlate(Sender: TObject);
		procedure UpdatePlateResources();
		procedure UpdatePlatePosition();
		procedure UpdatePlateAction();
	public
		constructor Create(AOwner: TComponent);

		procedure SetPosition(X, Y: Single);
		function GetPosition(): TVector3;
		procedure Move(X, Y, Speed: Single);
		procedure Stop();

		procedure SetHealth(Current, Max: Single);
		procedure SetEnergy(Current, Max: Single);
		procedure ModifyHealth(NewHealth: Single);
		procedure SetRegeneration(Health, Energy: Single);
		procedure SetAction(const LoreId: TLoreId; Duration: Single);
		procedure SetSize(Size: Single);

		procedure Update(const secondsPassed: Single; var removeMe: TRemoveType); override;

		property Id: TUlid read FId write FId;
		property Plate: TCastleDesign read FPlate write FPlate;
	end;

	TGameActorFactory = class
	strict private
		FUIViewport: TCastleViewport;
		FUIBoard: TCastleTransform;

	public
		constructor Create(Viewport: TCastleViewport; Board: TCastleTransform);

		function CreateActor(Id: TUlid): TGameActor;
		procedure RemoveActor(Actor: TGameActor);
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

procedure TPlayerBehavior.Update(const secondsPassed: Single; var removeMe: TRemoveType);
var
	LRect: TFloatRectangle;
begin
	LRect := FUICamera.Orthographic.EffectiveRect;
	FUICamera.Translation := Vector3(
		(Parent as TGameActor).GetPosition.X - LRect.Width / 2,
		(Parent as TGameActor).GetPosition.Y - LRect.Height / 2,
		GlobalConfig.CameraDistance
	);
end;

procedure TGameActor.Update(const secondsPassed: Single; var removeMe: TRemoveType);
begin
	FHealth := Min(FMaxHealth, FHealth + FHealthRegeneration * secondsPassed);
	FEnergy := Min(FMaxEnergy, FEnergy + FEnergyRegeneration * secondsPassed);
	self.UpdatePlateResources;

	if FMovementTime > 0 then begin
		FMovementTime -= secondsPassed;
		self.Translation := self.Translation + FMovementVector * secondsPassed;
	end;

	if not (FMovementVector - self.Up).IsZero then
		self.Up := self.Up + FMovementVector * cTurnSpeed;

	self.UpdatePlatePosition;

	if FActionTime > 0 then
		FActionTime -= secondsPassed;

	self.UpdatePlateAction;

	inherited;
end;

constructor TGameActorFactory.Create(Viewport: TCastleViewport; Board: TCastleTransform);
begin
	FUIViewport := Viewport;
	FUIBoard := Board;
end;

function TGameActorFactory.CreateActor(Id: TUlid): TGameActor;
begin
	result := TGameActor.Create(FUIBoard);
	result.Id := Id;
	result.Name := 'Actor_' + Id;

	// TODO: use Id to get info about the appearance of the actor from some other component
	// (which will manage network in return, to get this data)
	result.URL := 'castle-data:/images/player.png';
	result.Scale := Vector3(0.0025, 0.0025, 1); // TODO: scale properly
	result.Translation := Vector3(0, 0, 100); // TODO: proper Z distance

	result.Plate := TCastleDesign.Create(FUIViewport);
	FUIViewport.InsertFront(result.Plate);
	result.Plate.URL := 'castle-data:/actorplate.castle-user-interface';

	FUIBoard.Parent.Add(result);
end;

procedure TGameActorFactory.RemoveActor(Actor: TGameActor);
begin
	FUIViewport.RemoveControl(Actor.Plate);
	FUIBoard.Parent.RemoveDelayed(Actor, True);
end;

procedure TGameActor.UpdatePlate(Sender: TObject);
begin
	if Length(FName) = 0 then begin
		try
			FName := GlobalActorRepository.GetActorInfo(FId).ActorName;
			(FPlate.DesignedComponent('ActorName') as TCastleLabel)
				.Caption := FName;
		except
			on EActorNotFound do begin
				GlobalActorRepository.RequestActorInfo(FId, @UpdatePlate);
				exit;
			end;
		end;
	end;

	self.UpdatePlateResources;
end;

procedure TGameActor.UpdatePlateResources();
begin
	(FPlate.DesignedComponent('CurrentHealthBar') as TCastleRectangleControl)
		.WidthFraction := FHealth / FMaxHealth;

	(FPlate.DesignedComponent('CurrentEnergyBar') as TCastleRectangleControl)
		.WidthFraction := FEnergy / FMaxEnergy;
end;

procedure TGameActor.UpdatePlatePosition();
var
	LTranslation: TVector2;
	LUI: TCastleUserInterface;
begin
	LTranslation := (FPlate.Parent as TCastleViewport).PositionFromWorld(self.Translation);
	LUI := FPlate.DesignedComponent('ActorPlate') as TCastleUserInterface;
	if FPlateInitialHeight = 0 then
		FPlateInitialHeight := LUI.EffectiveHeight;

	LTranslation.X -= LUI.EffectiveWidth / 2;
	LTranslation.Y += FPlateInitialHeight * 0.2;
	FPlate.Translation := LTranslation;
end;

procedure TGameActor.UpdatePlateAction();
var
	LVisible: Boolean;
	LUI: TCastleRectangleControl;
begin
	LUI := FPlate.DesignedComponent('ActionBar') as TCastleRectangleControl;
	LVisible := FActionTime > 0;

	if (LVisible = LUI.Exists) and not LVisible then exit;

	LUI.Exists := LVisible;
	(FPlate.DesignedComponent('CurrentActionBar') as TCastleRectangleControl)
		.WidthFraction := (FActionDuration - FActionTime) / FActionDuration;

	(FPlate.DesignedComponent('ActionName') as TCastleLabel)
		.Caption := FActionName;

end;

constructor TGameActor.Create(AOwner: TComponent);
begin
	inherited;

	FName := '';
	FActionName := '';
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

	{ TODO: update all plate data since we need to update a name someday }
	self.UpdatePlate(self);
end;

procedure TGameActor.ModifyHealth(NewHealth: Single);
begin
	FHealth := NewHealth;

	self.UpdatePlateResources;
end;

procedure TGameActor.SetEnergy(Current, Max: Single);
begin
	FEnergy := Current;
	FMaxEnergy := Max;

	self.UpdatePlateResources;
end;

procedure TGameActor.SetRegeneration(Health, Energy: Single);
begin
	FHealthRegeneration := Health;
	FEnergyRegeneration := Energy;
end;

procedure TGameActor.SetAction(const LoreId: TLoreId; Duration: Single);
begin
	FActionName := LoreCollection.GetById(LoreId).LoreName;

	// NOTE: duration can be 0 when action is cancelled
	FActionDuration := Duration;
	FActionTime := Duration;
end;

procedure TGameActor.SetSize(Size: Single);
var
	LBox: TBox3D;
begin
	FSize := Size;
	LBox := self.BoundingBox;

	// NOTE: Box3D has width / height, while Size is a radius of a circle
	self.Scale := self.Scale * Vector3(LBox.SizeX / Size / 2, LBox.SizeY / Size / 2, 1);
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
begin
	LObject := FActorData.Find(Id);
	if LObject = nil then
		raise EActorNotFound.Create('Actor info needs to be requested before fetching');

	result := LObject as TGameActorRepositoryRecord;
end;

initialization
	GlobalActorRepository := TGameActorRepository.Create;

finalization
	GlobalActorRepository.Free;
end.

