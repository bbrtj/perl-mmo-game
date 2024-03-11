unit GameViewPlay;

interface

uses Classes, SysUtils,
	CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
	CastleTransform, CastleScene, CastleViewport, CastleTiledMap,
	GameState,
	GameNetwork,
	GameModels, GameModels.Move, GameModels.Discovery, GameModels.Ability;

type
	TViewPlay = class(TCastleView)
	published
		MainViewport: TCastleViewport;
		Board: TCastleTiledMap;
		PlayerCamera: TCastleCamera;
		AmbientLight: TCastleDirectionalLight;

		PingDisplay: TCastleLabel;
		FpsDisplay: TCastleLabel;

	private
		FGameState: TGameState;
		FPlaying: Boolean;

		function FindMapPosition(vMouseHit: TRayCollision; out Pos: TVector3): Boolean;

	public
		constructor Create(vOwner: TComponent); override;
		procedure Start; override;
		procedure Stop; override;

		procedure Update(const vSecondsPassed: Single; var vHandleInput: Boolean); override;
		function Press(const vEvent: TInputPressRelease): Boolean; override;

		procedure SetMapPath(vMapPath: String);

		procedure OnDiscovery(const vData: TModelBase);
		procedure OnActorMovement(const vData: TModelBase);
		procedure OnActorPosition(const vData: TModelBase);

		property GameState: TGameState read FGameState write FGameState;
		property Playing: Boolean read FPlaying write FPlaying;

	end;

var
	ViewPlay: TViewPlay;

implementation

constructor TViewPlay.Create(vOwner: TComponent);
begin
	inherited;
	DesignUrl := 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TViewPlay.Start;
begin
	inherited;

	FPlaying := false;
	FGameState := TGameState.Create(Board, PlayerCamera);

	GlobalClient.Await(TMsgFeedDiscovery, @OnDiscovery);
	GlobalClient.Await(TMsgFeedActorMovement, @OnActorMovement);
	GlobalClient.Await(TMsgFeedActorPosition, @OnActorPosition);
end;

procedure TViewPlay.Stop;
begin
	FGameState.Free;
end;

function TViewPlay.FindMapPosition(vMouseHit: TRayCollision; out Pos: TVector3): Boolean;
var
	vNode: TRayCollisionNode;
begin
	if vMouseHit.Info(vNode) and (vNode.Item is TCastleTiledMap) then
	begin
		writeln('hit map: ', vNode.Item.Name, ' ', vNode.Item.ClassName, ' at ',
		  vNode.Point.X:1:2, ':',
		  vNode.Point.Y:1:2);
		Pos := vNode.Item.LocalToWorld(vNode.Point);
		result := true;
	end else
		result := false;
end;

procedure TViewPlay.Update(const vSecondsPassed: Single; var vHandleInput: Boolean);
begin
	inherited;

	if not FPlaying then exit;
	FGameState.Update(vSecondsPassed);
	GlobalClient.Heartbeat(vSecondsPassed);

	PingDisplay.Caption := 'Latency: ' + IntToStr(GlobalClient.Ping) + ' ms';
	FpsDisplay.Caption := 'FPS: ' + Container.Fps.ToString;

end;

procedure TViewPlay.SetMapPath(vMapPath: String);
begin
	Board.URL := vMapPath;
end;

function TViewPlay.Press(const vEvent: TInputPressRelease): Boolean;
var
	vMouseHit: TRayCollision;
	vPosition: TVector3;
	vModel: TMsgMove;

begin
	result := inherited;
	if result then exit;

	// TODO: configurable keybinds
	if vEvent.IsKey(keyS) then begin
		GlobalClient.Send(TMsgStop, TMsgStop.Create());
		exit(true);
	end
	else if vEvent.IsKey(keyA) then begin
		GlobalClient.Send(TMsgUntargettedAbility, TMsgUntargettedAbility.Create());
		exit(true);
	end;
	if vEvent.IsMouseButton(buttonLeft) then begin
		vMouseHit := MainViewport.MouseRayHit;
		if vMouseHit <> nil then begin
			if FindMapPosition(vMouseHit, vPosition) then
			begin
				vModel := TMsgMove.Create;
				vModel.SetValue(vPosition.X * 100, vPosition.Y * 100);

				GlobalClient.Send(TMsgMove, vModel);
				exit(true);
			end;
		end;
	end;
end;

procedure TViewPlay.OnDiscovery(const vData: TModelBase);
var
	vModel: TMsgFeedDiscovery;
	vObject: TMsgFeedNewObject;
	vId: String;
begin
	vModel := vData as TMsgFeedDiscovery;

	for vObject in vModel.new_actors do
		FGameState.AddActor(vObject);

	for vId in vModel.old_actors do
		FGameState.RemoveActor(vId);
end;

procedure TViewPlay.OnActorMovement(const vData: TModelBase);
var
	vModel: TMsgFeedActorMovement;
begin
	vModel := vData as TMsgFeedActorMovement;

	FGameState.ProcessMovement(vModel);
end;

procedure TViewPlay.OnActorPosition(const vData: TModelBase);
var
	vModel: TMsgFeedActorPosition;
begin
	vModel := vData as TMsgFeedActorPosition;

	// TODO: movement stopped should be detected on clientside as well for smooth stop animation
	// (for example, when hitting walls)
	FGameState.ProcessPosition(vModel);
end;

{ implementation end }

end.

