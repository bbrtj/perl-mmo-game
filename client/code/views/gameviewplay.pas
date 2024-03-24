unit GameViewPlay;

interface

uses Classes, SysUtils, FGL,
	CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
	CastleTransform, CastleScene, CastleViewport, CastleTiledMap,
	GameTypes, GameState,
	GameNetwork, GameActors,
	GameModels, GameModels.Move, GameModels.Discovery, GameModels.Ability, GameModels.Chat;

type
	TUnresolvedChatMessage = class
	public
		Id: TUlid;
		Content: String;
		Resolved: Boolean;

		constructor Create(const AId: TUlid; const AContent: String);

		procedure Resolve(Sender: TObject);
	end;

	TUnresolvedChatMessageList = specialize TFPGObjectList<TUnresolvedChatMessage>;

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
		FChatMessages: TUnresolvedChatMessageList;

		function FindMapPosition(MouseHit: TRayCollision; out Pos: TVector3): Boolean;

	public
		constructor Create(aOwner: TComponent); override;
		procedure Start; override;
		procedure Stop; override;

		procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
		function Press(const Event: TInputPressRelease): Boolean; override;

		procedure SendChatMessage();

		procedure SetMapPath(MapPath: String);

		procedure OnDiscovery(const Data: TModelBase);
		procedure OnActorMovement(const Data: TModelBase);
		procedure OnActorPosition(const Data: TModelBase);
		procedure OnChatMessage(const Data: TModelBase);

		property GameState: TGameState read FGameState write FGameState;
		property Playing: Boolean read FPlaying write FPlaying;

	end;

var
	ViewPlay: TViewPlay;

implementation

constructor TViewPlay.Create(aOwner: TComponent);
begin
	inherited;
	DesignUrl := 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TViewPlay.Start;
begin
	inherited;

	FPlaying := false;
	FGameState := TGameState.Create(Board, PlayerCamera);
	FChatMessages := TUnresolvedChatMessageList.Create;

	GlobalClient.Await(TMsgFeedDiscovery, @OnDiscovery);
	GlobalClient.Await(TMsgFeedActorMovement, @OnActorMovement);
	GlobalClient.Await(TMsgFeedActorPosition, @OnActorPosition);
	GlobalClient.Await(TMsgFeedActorPosition, @OnActorPosition);
	GlobalClient.Await(TMsgFeedChat, @OnChatMessage);
end;

procedure TViewPlay.Stop;
begin
	FGameState.Free;
	FChatMessages.Free;
end;

function TViewPlay.FindMapPosition(MouseHit: TRayCollision; out Pos: TVector3): Boolean;
var
	LNode: TRayCollisionNode;
begin
	if MouseHit.Info(LNode) and (LNode.Item is TCastleTiledMap) then
	begin
		writeln('hit map: ', LNode.Item.Name, ' ', LNode.Item.ClassName, ' at ',
		  LNode.Point.X:1:2, ':',
		  LNode.Point.Y:1:2);
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
		SendChatMessage();
	end
	else if Event.IsKey(keyS) then begin
		GlobalClient.Send(TMsgStop, TMsgStop.Create());
		exit(true);
	end
	else if Event.IsKey(keyA) then begin
		GlobalClient.Send(TMsgUntargettedAbility, TMsgUntargettedAbility.Create());
		exit(true);
	end;
	if Event.IsMouseButton(buttonLeft) then begin
		MouseHit := MainViewport.MouseRayHit;
		if MouseHit <> nil then begin
			if FindMapPosition(MouseHit, LPosition) then
			begin
				LModel := TMsgMove.Create;
				LModel.SetValue(LPosition.X * 100, LPosition.Y * 100);

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
	LObject: TMsgFeedNewObject;
	LId: String;
begin
	LModel := Data as TMsgFeedDiscovery;

	for LObject in LModel.new_actors do
		FGameState.AddActor(LObject);

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

procedure TViewPlay.OnChatMessage(const Data: TModelBase);
var
	LModel: TMsgFeedChat;
	LUnresolved: TUnresolvedChatMessage;
begin
	LModel := Data as TMsgFeedChat;

	// TODO: whisper
	LUnresolved := TUnresolvedChatMessage.Create(LModel.id, LModel.message);
	GlobalActorRepository.RequestActorInfo(LUnresolved.Id, @LUnresolved.Resolve);
	FChatMessages.Add(LUnresolved);
end;

constructor TUnresolvedChatMessage.Create(const AId: TUlid; const AContent: String);
begin
	self.Id := AId;
	self.Content := AContent;
	self.Resolved := False;
end;

procedure TUnresolvedChatMessage.Resolve(Sender: TObject);
var
	LActorInfo: TGameActorRepositoryRecord;
begin
	LActorInfo := GlobalActorRepository.GetActorInfo(self.Id);
	ViewPlay.ChatWindow.Text.Append(LActorInfo.ActorName + ': ' + self.Content);
	self.Resolved := True;
end;

end.

