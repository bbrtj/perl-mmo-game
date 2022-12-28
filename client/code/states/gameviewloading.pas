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
		HintText1: TCastleLabel;
		HintText2: TCastleLabel;
		Loader: TCastleImageControl;

		FFading: Boolean;
		FLoadingDelay: Integer;
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

procedure StartLoading(const vContainer: TCastleContainer);

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
	HintText1.Caption := vLore.LoreName;
	HintText2.Caption := vLore.LoreDescription;
end;

procedure TViewLoading.DoLoad();
begin
	ViewPlay.SetMapData(MapIndex.GetMapData(FMapId));
	ViewPlay.SetMapImagePath(MapIndex.GetMapImagePath(FMapId));
	FLoaded := true;
end;

procedure TViewLoading.OnLoaded();
begin
	Container.PopView(ViewLoading);
end;

procedure TViewLoading.Start;
begin
	inherited;
	GlobalClient.ContextChange;

	HintText1 := DesignedComponent('HintText1') as TCastleLabel;
	HintText2 := DesignedComponent('HintText2') as TCastleLabel;
	Loader := DesignedComponent('Loader') as TCastleImageControl;

	GlobalClient.Await(TMsgFeedLocationData, @OnLocationData);

	FFading := true;
	FLoadingDelay := 0;
	FLoaded := false;
end;

procedure TViewLoading.Update(const vSecondsPassed: Single; var vHandleInput: Boolean);
const
	cRotationSpeed = 0.05;
	cFadeSpeed = 0.02;
	cLoadingDelay = 10;
begin
	inherited;

	if not FLoaded then begin
		if FLoadingDelay > cLoadingDelay then
			DoLoad
		else if FLoadingDelay > 0 then
			FLoadingDelay += 1;

		Loader.Rotation := Loader.Rotation - cRotationSpeed
	end
	else if FFading and (Loader.Color.W > 0) then begin
		Loader.Rotation := Loader.Rotation - cRotationSpeed * 2;
		Loader.Color := Loader.Color - Vector4(0, 0, 0, cFadeSpeed);
	end
	else if FFading then begin
		FFading := false;
		OnLoaded;
	end;
end;

procedure TViewLoading.OnLocationData(const vData: TModelBase);
var
	vModel: TMsgFeedLocationData;
begin
	vModel := vData as TMsgFeedLocationData;
	FMapId := vModel.id;

	RefreshLocationHints;
	FLoadingDelay := 1;
end;

procedure StartLoading(const vContainer: TCastleContainer);
begin
	vContainer.PushView(ViewPlay);
	vContainer.PushView(ViewLoading);
end;

{ implementation end }

end.
