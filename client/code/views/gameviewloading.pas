unit GameViewLoading;

interface

uses Classes,
	CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
	GameTypes, GameNetwork, GameLore, GameMaps,
	GameModels, GameModels.Location;

type
	TViewLoading = class(TCastleView)
	published
		HintText1: TCastleLabel;
		HintText2: TCastleLabel;
		Loader: TCastleImageControl;

	private
		FFading: Boolean;
		FLoaded: Boolean;

		FPlayerId: TUlid;

		FMapId: TLoreId;
		FPlayerX: Single;
		FPlayerY: Single;

		procedure RefreshLocationHints();
		procedure DoLoad(Sender: TObject);
		procedure OnLoaded();

	public
		constructor Create(AOwner: TComponent); override;
		procedure Start; override;
		procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;

		procedure OnLocationData(const Data: TModelBase);

		property PlayerId: TUlid write FPlayerId;

	end;

procedure StartLoading(const Container: TCastleContainer; const PlayerId: TUlid);

var
	ViewLoading: TViewLoading;

implementation

uses GameViewPlay;

constructor TViewLoading.Create(AOwner: TComponent);
begin
	inherited;
	DesignUrl := 'castle-data:/gameviewloading.castle-user-interface';
end;

procedure TViewLoading.RefreshLocationHints();
var
	LLore: TLoreItem;
begin
	LLore := LoreCollection.GetById(FMapId);
	HintText1.Caption := LLore.LoreName;
	HintText2.Caption := LLore.LoreDescription;
end;

procedure TViewLoading.DoLoad(Sender: TObject);
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

	GlobalClient.Await(TMsgFeedLocationData, @OnLocationData);

	FFading := true;
	FLoaded := false;
end;

procedure TViewLoading.Update(const SecondsPassed: Single; var HandleInput: Boolean);
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

procedure TViewLoading.OnLocationData(const Data: TModelBase);
var
	LModel: TMsgFeedLocationData;
begin
	GlobalClient.StopWaiting(TMsgFeedLocationData);
	GlobalClient.Pooling := True;

	LModel := Data as TMsgFeedLocationData;

	FMapId := LModel.id;
	FPlayerX := LModel.player_x;
	FPlayerY := LModel.player_y;

	RefreshLocationHints;
	WaitForRenderAndCall(@self.DoLoad);
end;

procedure StartLoading(const Container: TCastleContainer; const PlayerId: TUlid);
begin
	GlobalClient.ContextChange;

	Container.PopView();
	Container.PushView(ViewPlay);
	Container.PushView(ViewLoading);

	ViewLoading.PlayerId := PlayerId;
end;

end.

