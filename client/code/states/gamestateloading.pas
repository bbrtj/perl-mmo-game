unit GameStateLoading;

interface

uses Classes,
	CastleVectors, CastleUIState, CastleUIControls, CastleControls, CastleKeysMouse,
	GameTypes, GameNetwork, GameLore, GameMaps,
	GameModels, GameModels.Location;

type
	TStateLoading = class(TUIState)
	private
		{ Components designed using CGE editor, loaded from the castle-user-interface file. }
		HintText1: TCastleLabel;
		HintText2: TCastleLabel;
		Loader: TCastleImageControl;

		FFading: Boolean;
		FLoaded: Boolean;
		FMapId: TLoreId;

		procedure RefreshLocationHints();
		procedure DoLoad();
		procedure OnLoaded();

	public
		constructor Create(vOwner: TComponent); override;
		procedure Start; override;
		procedure Update(const vSecondsPassed: Single; var vHandleInput: Boolean); override;

		procedure OnLocationData(const vData: TModelBase);

	end;

var
	StateLoading: TStateLoading;

implementation

uses GameStatePlay;

constructor TStateLoading.Create(vOwner: TComponent);
begin
	inherited;
	DesignUrl := 'castle-data:/gamestateloading.castle-user-interface';
end;

procedure TStateLoading.RefreshLocationHints();
var
	vLore: TLoreItem;
begin
	vLore := LoreCollection.GetById(FMapId);
	HintText1.Caption := vLore.LoreName;
	HintText2.Caption := vLore.LoreDescription;
end;

procedure TStateLoading.DoLoad();
begin
	StatePlay.MapData := MapIndex.GetMapData(FMapId);
	StatePlay.MapImagePath := MapIndex.GetMapImagePath(FMapId);
	FLoaded := true;
end;

procedure TStateLoading.OnLoaded();
begin
	TUIState.Current := StatePlay;
end;

procedure TStateLoading.Start;
begin
	inherited;
	GlobalClient.ContextChange;

	HintText1 := DesignedComponent('HintText1') as TCastleLabel;
	HintText2 := DesignedComponent('HintText2') as TCastleLabel;
	Loader := DesignedComponent('Loader') as TCastleImageControl;

	GlobalClient.Await(TMsgFeedLocationData, @OnLocationData);

	FLoaded := false;
	FFading := true;
end;

procedure TStateLoading.Update(const vSecondsPassed: Single; var vHandleInput: Boolean);
const
	cRotationSpeed = 0.05;
	cFadeSpeed = 0.02;
begin
	inherited;

	if not FLoaded then
		Loader.Rotation := Loader.Rotation - cRotationSpeed
	else if FFading and (Loader.Color.W > 0) then begin
		Loader.Rotation := Loader.Rotation - cRotationSpeed * 2;
		Loader.Color := Loader.Color - Vector4(0, 0, 0, cFadeSpeed);
	end
	else if FFading then begin
		FFading := false;
		OnLoaded;
	end;
end;

procedure TStateLoading.OnLocationData(const vData: TModelBase);
var
	vModel: TMsgFeedLocationData;
begin
	vModel := vData as TMsgFeedLocationData;
	FMapId := vModel.id;

	RefreshLocationHints;
	DoLoad;
end;

{ implementation end }

end.

