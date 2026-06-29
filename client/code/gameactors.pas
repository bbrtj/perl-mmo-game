unit GameActors;

interface

uses SysUtils, Classes, Contnrs, Math,
	CastleUIControls, CastleControls, CastleRectangles, CastleRenderOptions,
	CastleScene, CastleTransform, CastleVectors, CastleViewport, CastleBoxes,
	GameTypes, GameLore, GameExceptions, GameNetwork, GameConfig,
	GameModels, GameModels.Discovery;

type
	TGameActorRepositoryRecord = class;

	TPlayerBehavior = class(TCastleBehavior)
	strict private
		FUICamera: TCastleCamera;
		FUILight: TCastleSpotLight;

	public
		procedure Update(const secondsPassed: Single; var removeMe: TRemoveType); override;

		property Camera: TCastleCamera read FUICamera write FUICamera;
		property Light: TCastleSpotLight read FUILight write FUILight;
	end;

	TGameActor = class(TGameModel)
	strict private
		FPlate: TCastleDesign;
		FPlateInitialHeight: Single;

		FId: TUlid;
		FActorRecord: TGameActorRepositoryRecord;

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

		procedure UpdatePlate();
		procedure UpdatePlateResources();
		procedure UpdatePlatePosition();
		procedure UpdatePlateAction();
	public
		constructor Create(AOwner: TComponent); override;

		procedure SetPosition(X, Y: Single);
		function GetPosition(): TVector3;
		procedure Move(X, Y, Speed: Single);
		procedure Stop();

		procedure SetHealth(Current, Max: Single);
		procedure SetEnergy(Current, Max: Single);
		procedure ModifyHealth(NewHealth: Single);
		procedure SetRegeneration(Health, Energy: Single);
		procedure SetAction(const LoreId: TLoreId; Duration: Single);
		procedure SetSize(ASize: Single);

		procedure Update(const secondsPassed: Single; var removeMe: TRemoveType); override;

		property Id: TUlid read FId write FId;
		property Plate: TCastleDesign read FPlate write FPlate;
		property ActorRecord: TGameActorRepositoryRecord read FActorRecord write FActorRecord;
	end;

	TGameActorRepositoryRecord = class
	strict private
		FId: TUlid;
		FName: String;
		FClass: TLoreId;
		FRace: TLoreId;
		FAlliance: TLoreId;
		FPlayer: Boolean;
	public
		property Id: TUlid read FId write FId;
		property ActorName: String read FName write FName;
		property ActorClass: TLoreId read FClass write FClass;
		property ActorRace: TLoreId read FRace write FRace;
		property ActorAlliance: TLoreId read FAlliance write FAlliance;
		property IsPlayer: Boolean read FPlayer write FPlayer;
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
		function HasActorInfo(const Id: TUlid): Boolean;
	end;

	TGameActorFactory = class
	strict private
		FUIViewport: TCastleViewport;
		FUIBoard: TCastleTransform;

	public
		constructor Create(Viewport: TCastleViewport; Board: TCastleTransform);

		function CreateActor(Info: TGameActorRepositoryRecord): TGameActor;
		procedure RemoveActor(Actor: TGameActor);
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

	FUILight.Translation := Vector3(
		(Parent as TGameActor).GetPosition.X,
		(Parent as TGameActor).GetPosition.Y,
		GlobalConfig.LightsDistance
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

		self.Up := FMovementVector;
	end;


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

function TGameActorFactory.CreateActor(Info: TGameActorRepositoryRecord): TGameActor;
var
	LLore: TLoreItem;
begin
	result := TGameActor.Create(FUIBoard);
	result.Id := Info.Id;
	result.Name := 'Actor_' + Info.Id;
	result.Material := pmPhong;
	result.ActorRecord := Info;

	LLore := LoreCollection.GetById(Info.ActorClass);

	result.URL := 'castle-data:' + LLore.GetVisuals.model;
	result.Translation := Vector3(0, 0, 0.1); // TODO: proper Z distance

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

procedure TGameActor.UpdatePlate();
begin
	(FPlate.DesignedComponent('ActorName') as TCastleLabel)
		.Caption := FActorRecord.ActorName;

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

	self.Pickable := false;
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

	{ TODO: this updates all plate data since we need to update a name at some point }
	self.UpdatePlate();
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
	FActionName := LoreCollection.GetById(LoreId).name;

	// NOTE: duration can be 0 when action is cancelled
	FActionDuration := Duration;
	FActionTime := Duration;
end;

procedure TGameActor.SetSize(ASize: Single);
var
	LBox: TBox3D;
	LCurrentRadius: Single;
begin
	FSize := ASize;
	LBox := self.BoundingBox;

	// NOTE: increase visual size by 10% to make up for non-perfect actor image
	// representations
	ASize *= 1.1;

	// NOTE: Box3D has width / height, while Size is a radius of a circle
	LCurrentRadius := Max(LBox.Size.X, LBox.Size.Y) / 2;
	self.Scale := self.Scale * Vector3(ASize / LCurrentRadius, ASize / LCurrentRadius, 1);
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
	LActorInfo: TMsgResActor;
	LRecord: TGameActorRepositoryRecord;
begin
	LActorsInfo := ActorsInfo as TMsgResActorsInfo;

	for LActorInfo in LActorsInfo.list do begin
		LRecord := TGameActorRepositoryRecord.Create;

		LRecord.Id := LActorInfo.id;
		LRecord.ActorName := LActorInfo.name;
		LRecord.ActorClass := LActorInfo.&class;
		LRecord.ActorRace := LActorInfo.race;
		LRecord.ActorAlliance := LActorInfo.alliance;
		LRecord.IsPlayer := LActorInfo.player;

		if not LRecord.IsPlayer then
			LRecord.ActorName := LoreCollection.GetById(LRecord.ActorName).name;

		FActorData.Add(LActorInfo.id, LRecord);
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

function TGameActorRepository.HasActorInfo(const Id: TUlid): Boolean;
var
	LObject: TObject;
begin
	LObject := FActorData.Find(Id);
	result := LObject <> nil;
end;

initialization
	GlobalActorRepository := TGameActorRepository.Create;

finalization
	FreeAndNil(GlobalActorRepository);
end.

