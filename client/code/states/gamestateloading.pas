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

		FLoaded: Boolean;
		FMapData: TMapData;

		procedure RefreshLocationHints(const vLocationId: TLoreId);

	public
		constructor Create(vOwner: TComponent); override;
		procedure Start; override;
		procedure Update(const vSecondsPassed: Single; var vHandleInput: Boolean); override;

		procedure OnLocationData(const vData: TModelBase);

	end;

var
	StateLoading: TStateLoading;

implementation

constructor TStateLoading.Create(vOwner: TComponent);
begin
	inherited;
	DesignUrl := 'castle-data:/gamestateloading.castle-user-interface';
end;

procedure TStateLoading.RefreshLocationHints(const vLocationId: TLoreId);
var
	vLore: TLoreItem;
begin
	vLore := LoreCollection.GetById(vLocationId);
	HintText1.Caption := vLore.LoreName;
	HintText2.Caption := vLore.LoreDescription;
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
end;

procedure TStateLoading.Update(const vSecondsPassed: Single; var vHandleInput: Boolean);
const
	cRotationSpeed = 0.05;
	cFadeSpeed = 0.02;
begin
	inherited;

	if not FLoaded then
		Loader.Rotation := Loader.Rotation - cRotationSpeed
	else if Loader.Color.W > 0 then begin
		Loader.Rotation := Loader.Rotation - cRotationSpeed * 2;
		Loader.Color := Loader.Color - Vector4(0, 0, 0, cFadeSpeed);
	end;
end;

procedure TStateLoading.OnLocationData(const vData: TModelBase);
var
	vModel: TMsgFeedLocationData;
begin
	vModel := vData as TMsgFeedLocationData;
	RefreshLocationHints(vModel.id);
	FMapData := MapIndex.GetMapData(vModel.id);
	FLoaded := true;
end;

{ implementation end }

end.

