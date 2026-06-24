unit GameViewPlay;

interface

uses Classes, SysUtils, FGL,
	CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
	CastleTransform, CastleScene, CastleViewport, CastleTiledMap,
	GameTypes, GameLog, GameTranslations, GameState, GameChat,
	GameNetwork, GameActors,
	GameModels, GameModels.General, GameModels.Move, GameModels.Discovery,
	GameModels.Ability, GameModels.Chat, GameModels.Actors,
	GameModels.Projectiles,
	GamePipelines, GamePipelines.Actors;

type
	TActorActions = specialize TFPGObjectList<TModelBase>;
	TActorActionsMap = specialize TFPGMap<TUlid, TActorActions>;

	TViewPlay = class(TCastleView)
	published
		MainViewport: TCastleViewport;
		Board: TCastleTiledMap;
		PlayerCamera: TCastleCamera;
		AmbientLight: TCastleDirectionalLight;
	published
		PingDisplay: TCastleLabel;
		FpsDisplay: TCastleLabel;
		ChatEdit: TCastleEdit;
		ChatWindow: TCastleLabel;
	private
		FGameState: TGameState;
		FPlaying: Boolean;
		FUnknownActorActions: TActorActionsMap;
	private
		function FindMapPosition(MouseHit: TRayCollision; out Pos: TVector3): Boolean;
		procedure ActorReady(ActorInfo: TObject);
		procedure OnError(const Data: TModelBase);
	public
		constructor Create(AOwner: TComponent); override;
	public
		procedure Start; override;
		procedure Stop; override;
		procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
		function Press(const Event: TInputPressRelease): Boolean; override;
		procedure SendChatMessage();
		procedure SetMapPath(MapPath: String);
		procedure OnDiscovery(const Data: TModelBase);
		procedure OnActorFeed(const Data: TModelBase);
		procedure OnProjectile(const Data: TModelBase);
		procedure OnProjectileStop(const Data: TModelBase);
		procedure NewChatMessage(const Message: String);
	public
		property GameState: TGameState read FGameState write FGameState;
		property Playing: Boolean read FPlaying write FPlaying;
	end;

var
	ViewPlay: TViewPlay;

implementation

constructor TViewPlay.Create(AOwner: TComponent);
begin
	inherited;
	DesignUrl := 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TViewPlay.Start;
begin
	inherited;

	FPlaying := false;
	FGameState := TGameState.Create(MainViewport);
	FGameState.Board := Board;

	FUnknownActorActions := TActorActionsMap.Create;

	GlobalClient.OnError := @self.OnError;

	GlobalClient.Await(TMsgFeedDiscovery, @self.OnDiscovery);
	GlobalClient.Await(TMsgFeedActorMovement, @self.OnActorFeed);
	GlobalClient.Await(TMsgFeedActorPosition, @self.OnActorFeed);
	GlobalClient.Await(TMsgFeedActorEvent, @self.OnActorFeed);
	GlobalClient.Await(TMsgFeedActorState, @self.OnActorFeed);
	GlobalClient.Await(TMsgFeedActorAction, @self.OnActorFeed);
	GlobalClient.Await(TMsgFeedProjectile, @self.OnProjectile);
	GlobalClient.Await(TMsgFeedProjectileStop, @self.OnProjectileStop);

	GlobalChat.Handler := @self.NewChatMessage;
end;

procedure TViewPlay.Stop;
begin
	FGameState.Free;
	FUnknownActorActions.Free;

	GlobalChat.Handler := nil;
	GlobalClient.OnError := nil;
end;

function TViewPlay.FindMapPosition(MouseHit: TRayCollision; out Pos: TVector3): Boolean;
var
	LNode: TRayCollisionNode;
begin
	if MouseHit.Info(LNode) and (LNode.Item is TCastleTiledMap) then
	begin
		Pos := LNode.Item.LocalToWorld(LNode.Point);
		result := true;
	end else
		result := false;
end;

procedure TViewPlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
	inherited;

	if not FPlaying then exit;
	FGameState.Update(SecondsPassed);
	GlobalClient.Heartbeat(SecondsPassed);

	PingDisplay.Caption := 'Latency: ' + IntToStr(GlobalClient.Ping) + ' ms';
	FpsDisplay.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewPlay.SetMapPath(MapPath: String);
begin
	Board.URL := MapPath;
end;

function TViewPlay.Press(const Event: TInputPressRelease): Boolean;
var
	LPositionGrabbed: Boolean;
	LHasPosition: Boolean;
	LPosition: TVector3;
	LModel: TMsgMove;
	LAbility: TMsgUseAbility;

	function FindMousePosition(): Boolean;
	var
		LMouseHit: TRayCollision;
	begin
		LMouseHit := MainViewport.MouseRayHit;
		if LPositionGrabbed then exit(LHasPosition);

		LPositionGrabbed := true;
		LHasPosition := (LMouseHit <> nil) and FindMapPosition(LMouseHit, LPosition);

		if LHasPosition then begin
			LPosition.X := LPosition.X * 100;
			LPosition.Y := LPosition.Y * 100;
		end;

		result := LHasPosition;
	end;

begin
	result := inherited;
	if result then exit;

	// TODO: configurable keybinds

	if Event.IsKey(keyEnter) then begin
		if ChatEdit.Exists then begin
			SendChatMessage();
			ChatEdit.Exists := False;
			Container.ForceCaptureInput := nil;
		end
		else begin
			ChatEdit.Exists := True;
			Container.ForceCaptureInput := ChatEdit;
		end;

		exit(true);
	end;

	if Event.IsKey(keyS) then begin
		GlobalClient.Send(TMsgStop, TMsgStop.Create());
		exit(true);
	end;

	{ TODO: hardcoded ability lore_id }
	{ NOTE: untargetted - X/Y can be zero }
	if Event.IsKey(keyA) then begin
		FindMousePosition;
		LAbility := TMsgUseAbility.Create();
		LAbility.lore_id := 'abil.strike';
		LAbility.X := LPosition.X;
		LAbility.Y := LPosition.Y;

		GlobalClient.Send(TMsgUseAbility, LAbility);
		exit(true);
	end;

	if Event.IsKey(keyE) and FindMousePosition then begin
		LAbility := TMsgUseAbility.Create();
		LAbility.lore_id := 'abil.shoot';
		LAbility.X := LPosition.X;
		LAbility.Y := LPosition.Y;

		GlobalClient.Send(TMsgUseAbility, LAbility);
		exit(true);
	end;

	if Event.IsKey(keyU) and FindMousePosition then begin
		LAbility := TMsgUseAbility.Create();
		LAbility.lore_id := 'abil.fireb';
		LAbility.X := LPosition.X;
		LAbility.Y := LPosition.Y;

		GlobalClient.Send(TMsgUseAbility, LAbility);
		exit(true);
	end;

	if Event.IsMouseButton(buttonLeft) and FindMousePosition then begin
		LModel := TMsgMove.Create;
		LModel.X := LPosition.X;
		LModel.Y := LPosition.Y;

		GlobalClient.Send(TMsgMove, LModel);
		exit(true);
	end;
end;

procedure TViewPlay.SendChatMessage();
var
	LMessage: String;
	LMsgObject: TMsgChatSay;
begin
	LMessage := ChatEdit.Text;
	if length(LMessage) > 0 then begin
		LMsgObject := TMsgChatSay.Create();
		LMsgObject.value := LMessage;
		GlobalClient.Send(TMsgChatSay, LMsgObject);
		ChatEdit.Text := '';
	end;
end;

procedure TViewPlay.OnDiscovery(const Data: TModelBase);
var
	LModel: TMsgFeedDiscovery;
	LId: String;
	LPipeline: TRequestActorInfoPipeline;
begin
	LModel := Data as TMsgFeedDiscovery;

	for LId in LModel.new_actors do begin
		LPipeline := GlobalPipelineManager.New(TRequestActorInfoPipeline) as TRequestActorInfoPipeline;
		LPipeline.ActorId := LId;
		LPipeline.SetNext(@self.ActorReady);

		LPipeline.Start(self);
	end;

	// TODO: object may not be an actor
	for LId in LModel.old_objects do
		FGameState.RemoveActor(LId);
end;

procedure TViewPlay.ActorReady(ActorInfo: TObject);
var
	LActorInfo: TGameActorRepositoryRecord;
	LActions: TActorActions;
	I: Integer;
begin
	LActorInfo := ActorInfo as TGameActorRepositoryRecord;
	FGameState.CreateActor(LActorInfo);

	if FUnknownActorActions.TryGetData(LActorInfo.Id, LActions) then begin
		for I := 0 to LActions.Count - 1 do
			self.OnActorFeed(LActions[I]);

		LActions.Free;
		FUnknownActorActions.Remove(LActorInfo.Id);
	end;
end;

procedure TViewPlay.OnActorFeed(const Data: TModelBase);
var
	LId: TUlid;
	LActions: TActorActions;
begin
	try
		if Data is TMsgFeedActorMovement then
			FGameState.ProcessMovement(Data as TMsgFeedActorMovement)
		else if Data is TMsgFeedActorPosition then
			FGameState.ProcessPosition(Data as TMsgFeedActorPosition)
		else if Data is TMsgFeedActorEvent then
			FGameState.ProcessActorEvent(Data as TMsgFeedActorEvent)
		else if Data is TMsgFeedActorState then
			FGameState.ProcessActorState(Data as TMsgFeedActorState)
		else if Data is TMsgFeedActorAction then
			FGameState.ProcessActorAction(Data as TMsgFeedActorAction)
		else
			raise EUnknownMessage.Create('Unknown actor feed received');
	except
		on E: EActorNotFound do begin
			LId := (Data as IModelWithUlid).GetId;
			if not FUnknownActorActions.TryGetData(LId, LActions) then begin
				LActions := TActorActions.Create;
				FUnknownActorActions.Add(LId, LActions);
			end;

			LActions.Add(Data);
			Data.Adopted := true;
		end;
	end;
end;

procedure TViewPlay.OnProjectile(const Data: TModelBase);
begin
	FGameState.ProcessProjectile(Data as TMsgFeedProjectile);
end;

procedure TViewPlay.OnProjectileStop(const Data: TModelBase);
begin
	FGameState.ProcessProjectileStop(Data as TMsgFeedProjectileStop);
end;

procedure TViewPlay.NewChatMessage(const Message: String);
begin
	ViewPlay.ChatWindow.Text.Append(Message);
end;

procedure TViewPlay.OnError(const Data: TModelBase);
begin
	// TODO: notify user something's wrong
	LogDebug('Error: ' + _((Data as TMsgResError).Msg));
end;

end.

