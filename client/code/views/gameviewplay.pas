unit GameViewPlay;

interface

uses Classes, SysUtils,
	CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
	CastleTransform, CastleScene, CastleViewport,
	GameState,
	GameNetwork,
	GameModels, GameModels.Move, GameModels.Discovery;

type
	TViewPlay = class(TCastleView)
	private
		MainViewport: TCastleViewport;
		Board: TCastlePlane;
		PlayerCamera: TCastleCamera;
		AmbientLight: TCastleDirectionalLight;
		PingDisplay: TCastleLabel;

		FGameState: TGameState;
		FMapImagePath: String;
		FPlaying: Boolean;

		function FindMapPosition(vMouseHit: TRayCollision): TVector3;

	public
		constructor Create(vOwner: TComponent); override;
		procedure Start; override;
		procedure Stop; override;

		procedure Update(const vSecondsPassed: Single; var vHandleInput: Boolean); override;
		function Press(const vEvent: TInputPressRelease): Boolean; override;

		procedure SetMapImagePath(vMapImagePath: String);

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

	MainViewport := DesignedComponent('MainViewport') as TCastleViewport;
	Board := DesignedComponent('Board') as TCastlePlane;
	PlayerCamera := DesignedComponent('PlayerCamera') as TCastleCamera;
	AmbientLight := DesignedComponent('AmbientLight') as TCastleDirectionalLight;
	PingDisplay := DesignedComponent('PingDisplay') as TCastleLabel;
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

function TViewPlay.FindMapPosition(vMouseHit: TRayCollision): TVector3;
var
	vNode: TRayCollisionNode;
begin
	vNode := vMouseHit[0];
	result := vNode.Item.LocalToWorld(vNode.Point);
end;

procedure TViewPlay.Update(const vSecondsPassed: Single; var vHandleInput: Boolean);
begin
	inherited;

	if not FPlaying then exit;
	FGameState.Update(vSecondsPassed);
	GlobalClient.Heartbeat(vSecondsPassed);

	PingDisplay.Caption := FloatToStr(round(GlobalClient.Ping * 100000) / 100) + ' ms';

end;

procedure TViewPlay.SetMapImagePath(vMapImagePath: String);
begin
	FMapImagePath := vMapImagePath;

	Board.Texture := FMapImagePath;
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
	end;
	if vEvent.IsMouseButton(buttonLeft) then begin
		vMouseHit := MainViewport.MouseRayHit;
		if vMouseHit <> nil then begin
			vPosition := FindMapPosition(vMouseHit);

			vModel := TMsgMove.Create;
			vModel.SetValue(vPosition.X, vPosition.Y);

			GlobalClient.Send(TMsgMove, vModel);
			exit(true);
		end;
	end;
end;

procedure TViewPlay.OnDiscovery(const vData: TModelBase);
var
	vModel: TMsgFeedDiscovery;
	vId: String;
begin
	vModel := vData as TMsgFeedDiscovery;

	for vId in vModel.new_actors do
		FGameState.AddActor(vId);

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
	FGameState.ProcessPosition(vModel);
end;

{ implementation end }

end.

