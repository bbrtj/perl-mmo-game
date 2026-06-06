unit GameViewPlay;

interface

uses Classes, SysUtils, FGL,
	CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
	CastleTransform, CastleScene, CastleViewport, CastleTiledMap,
	GameState, GameChat,
	GameNetwork, GameActors,
	GameModels, GameModels.Move, GameModels.Discovery,
	GameModels.Ability, GameModels.Chat, GameModels.Actors,
	GameModels.Projectiles;

type

	TViewPlay = class(TCastleView)
	published
		MainViewport: TCastleViewport;
		Board: TCastleTiledMap;
		PlayerCamera: TCastleCamera;
		AmbientLight: TCastleDirectionalLight;

		PingDisplay: TCastleLabel;
		FpsDisplay: TCastleLabel;
		ChatEdit: TCastleEdit;
		ChatWindow: TCastleLabel;

	private
		FGameState: TGameState;
		FPlaying: Boolean;

		function FindMapPosition(MouseHit: TRayCollision; out Pos: TVector3): Boolean;

	public
		constructor Create(AOwner: TComponent); override;
		procedure Start; override;
		procedure Stop; override;

		procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
		function Press(const Event: TInputPressRelease): Boolean; override;

		procedure SendChatMessage();

		procedure SetMapPath(MapPath: String);

		procedure OnDiscovery(const Data: TModelBase);
		procedure OnActorMovement(const Data: TModelBase);
		procedure OnActorPosition(const Data: TModelBase);
		procedure OnActorEvent(const Data: TModelBase);
		procedure OnActorState(const Data: TModelBase);
		procedure OnActorAction(const Data: TModelBase);
		procedure OnProjectile(const Data: TModelBase);
		procedure OnProjectileStop(const Data: TModelBase);

		procedure NewChatMessage(Message: String);

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

	GlobalClient.Await(TMsgFeedDiscovery, @OnDiscovery);
	GlobalClient.Await(TMsgFeedActorMovement, @OnActorMovement);
	GlobalClient.Await(TMsgFeedActorPosition, @OnActorPosition);
	GlobalClient.Await(TMsgFeedActorEvent, @OnActorEvent);
	GlobalClient.Await(TMsgFeedActorState, @OnActorState);
	GlobalClient.Await(TMsgFeedActorAction, @OnActorAction);
	GlobalClient.Await(TMsgFeedProjectile, @OnProjectile);
	GlobalClient.Await(TMsgFeedProjectileStop, @OnProjectileStop);

	GlobalChat.Handler := @NewChatMessage;
end;

procedure TViewPlay.Stop;
begin
	FGameState.Free;

	GlobalChat.Handler := nil;
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
		LAbility.lore_id := 'ABIL.STRIKE';
		LAbility.X := LPosition.X;
		LAbility.Y := LPosition.Y;

		GlobalClient.Send(TMsgUseAbility, LAbility);
		exit(true);
	end;

	if Event.IsKey(keyE) and FindMousePosition then begin
		LAbility := TMsgUseAbility.Create();
		LAbility.lore_id := 'ABIL.SHOOT';
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
begin
	LModel := Data as TMsgFeedDiscovery;

	for LId in LModel.new_actors do
		FGameState.AddActor(LId);

	for LId in LModel.old_actors do
		FGameState.RemoveActor(LId);
end;

procedure TViewPlay.OnActorMovement(const Data: TModelBase);
var
	LModel: TMsgFeedActorMovement;
begin
	LModel := Data as TMsgFeedActorMovement;

	FGameState.ProcessMovement(LModel);
end;

procedure TViewPlay.OnActorPosition(const Data: TModelBase);
var
	LModel: TMsgFeedActorPosition;
begin
	LModel := Data as TMsgFeedActorPosition;

	// TODO: movement stopped should be detected on clientside as well for smooth stop animation
	// (for example, when hitting walls)
	FGameState.ProcessPosition(LModel);
end;

procedure TViewPlay.OnActorEvent(const Data: TModelBase);
begin
	FGameState.ProcessActorEvent(Data as TMsgFeedActorEvent);
end;

procedure TViewPlay.OnActorState(const Data: TModelBase);
begin
	FGameState.ProcessActorState(Data as TMsgFeedActorState);
end;

procedure TViewPlay.OnActorAction(const Data: TModelBase);
begin
	FGameState.ProcessActorAction(Data as TMsgFeedActorAction);
end;

procedure TViewPlay.OnProjectile(const Data: TModelBase);
begin
	FGameState.ProcessProjectile(Data as TMsgFeedProjectile);
end;

procedure TViewPlay.OnProjectileStop(const Data: TModelBase);
begin
	FGameState.ProcessProjectileStop(Data as TMsgFeedProjectileStop);
end;

procedure TViewPlay.NewChatMessage(Message: String);
begin
	ViewPlay.ChatWindow.Text.Append(Message);
end;

end.

