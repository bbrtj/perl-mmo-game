unit GameViewPlay;

interface

uses Classes, SysUtils, FGL,
	CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
	CastleTransform, CastleScene, CastleViewport, CastleTiledMap,
	GameState, GameChat,
	GameNetwork, GameActors,
	GameModels, GameModels.Move, GameModels.Discovery,
	GameModels.Ability, GameModels.Chat, GameModels.Actors;

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
	MouseHit: TRayCollision;
	LPosition: TVector3;
	LModel: TMsgMove;

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

	if Event.IsKey(keyA) then begin
		GlobalClient.Send(TMsgUntargettedAbility, TMsgUntargettedAbility.Create());
		exit(true);
	end;

	if Event.IsMouseButton(buttonLeft) then begin
		MouseHit := MainViewport.MouseRayHit;
		if MouseHit <> nil then begin
			if FindMapPosition(MouseHit, LPosition) then
			begin
				LModel := TMsgMove.Create;
				LModel.X := LPosition.X * 100;
				LModel.Y := LPosition.Y * 100;

				GlobalClient.Send(TMsgMove, LModel);
				exit(true);
			end;
		end;
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
	FGameState.ProcessEvent(Data as TMsgFeedActorEvent);
end;

procedure TViewPlay.NewChatMessage(Message: String);
begin
	ViewPlay.ChatWindow.Text.Append(Message);
end;

end.

