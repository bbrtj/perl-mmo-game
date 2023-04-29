unit GameViewLoading;

interface

uses Classes,
	CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
	GameTypes, GameNetwork, GameLore, GameMaps,
	GameModels, GameModels.Location;

type
	TViewLoading = class(TCastleView)
	private
		{ Components designed using CGE editor, loaded from the castle-user-interface file. }
		FUIHintText1: TCastleLabel;
		FUIHintText2: TCastleLabel;
		Loader: TCastleImageControl;

		FFading: Boolean;
		FLoaded: Boolean;

		FPlayerId: TUlid;

		FMapId: TLoreId;
		FPlayerX: Single;
		FPlayerY: Single;

		procedure RefreshLocationHints();
		procedure DoLoad(vSender: TObject);
		procedure OnLoaded();

	public
		constructor Create(vOwner: TComponent); override;
		procedure Start; override;
		procedure Update(const vSecondsPassed: Single; var vHandleInput: Boolean); override;

		procedure OnLocationData(const vData: TModelBase);

		property PlayerId: TUlid write FPlayerId;

	end;

procedure StartLoading(const vContainer: TCastleContainer; const vPlayerId: TUlid);

var
	ViewLoading: TViewLoading;

implementation

uses GameViewPlay;

constructor TViewLoading.Create(vOwner: TComponent);
begin
	inherited;
	DesignUrl := 'castle-data:/gameviewloading.castle-user-interface';
end;

procedure TViewLoading.RefreshLocationHints();
var
	vLore: TLoreItem;
begin
	vLore := LoreCollection.GetById(FMapId);
	FUIHintText1.Caption := vLore.LoreName;
	FUIHintText2.Caption := vLore.LoreDescription;
end;

procedure TViewLoading.DoLoad(vSender: TObject);
begin
	ViewPlay.SetMapPath(MapIndex.GetMapPath(FMapId));
	ViewPlay.GameState.SetMapData(MapIndex.GetMapData(FMapId));
	ViewPlay.GameState.CreatePlayer(FPlayerId, FPlayerX, FPlayerY);

	FLoaded := true;
	GlobalClient.Pooling := False;
end;

procedure TViewLoading.OnLoaded();
begin
	Container.PopView(ViewLoading);
	ViewPlay.Playing := true;
end;

procedure TViewLoading.Start;
begin
	inherited;

	FUIHintText1 := DesignedComponent('HintText1') as TCastleLabel;
	FUIHintText2 := DesignedComponent('HintText2') as TCastleLabel;
	Loader := DesignedComponent('Loader') as TCastleImageControl;

	GlobalClient.Await(TMsgFeedLocationData, @OnLocationData);

	FFading := true;
	FLoaded := false;
end;

procedure TViewLoading.Update(const vSecondsPassed: Single; var vHandleInput: Boolean);
const
	cRotationSpeed = 0.05;
	cFadeSpeed = 0.02;
begin
	inherited;

	Loader.Rotation := Loader.Rotation - cRotationSpeed;

	if FLoaded then begin
		if FFading and (Loader.Color.W > 0) then begin
			Loader.Color := Loader.Color - Vector4(0, 0, 0, cFadeSpeed);
		end
		else if FFading then begin
			FFading := false;
			OnLoaded;
		end;
	end;
end;

procedure TViewLoading.OnLocationData(const vData: TModelBase);
var
	vModel: TMsgFeedLocationData;
begin
	GlobalClient.StopWaiting(TMsgFeedLocationData);
	GlobalClient.Pooling := True;

	vModel := vData as TMsgFeedLocationData;

	FMapId := vModel.id;
	FPlayerX := vModel.player_x;
	FPlayerY := vModel.player_y;

	RefreshLocationHints;
	WaitForRenderAndCall(@self.DoLoad);
end;

procedure StartLoading(const vContainer: TCastleContainer; const vPlayerId: TUlid);
begin
	GlobalClient.ContextChange;

	vContainer.PopView();
	vContainer.PushView(ViewPlay);
	vContainer.PushView(ViewLoading);

	ViewLoading.PlayerId := vPlayerId;
end;

{ implementation end }

end.

