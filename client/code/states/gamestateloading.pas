unit GameStateLoading;

interface

uses Classes,
	CastleVectors, CastleUIState, CastleUIControls, CastleControls, CastleKeysMouse,
	GameTypes, GameNetwork, GameLore,
	GameModels, GameModels.Location;

type
	TStateLoading = class(TUIState)
	private
		{ Components designed using CGE editor, loaded from the castle-user-interface file. }
		HintText1: TCastleLabel;
		HintText2: TCastleLabel;

		FLocationId: TLoreId;

		procedure RefreshLocationHints();
	public
		constructor Create(AOwner: TComponent); override;
		procedure Start; override;

		procedure OnLocationData(const vData: TModelBase);
	end;

var
	StateLoading: TStateLoading;

implementation

constructor TStateLoading.Create(AOwner: TComponent);
begin
	inherited;
	DesignUrl := 'castle-data:/gamestateloading.castle-user-interface';
end;

procedure TStateLoading.RefreshLocationHints();
var
	vLore: TLoreItem;
begin
	vLore := LoreCollection.GetById(FLocationId);
	HintText1.Caption := vLore.LoreName;
	HintText2.Caption := vLore.LoreDescription;
end;

procedure TStateLoading.Start;
begin
	inherited;
	GlobalClient.ContextChange;

	HintText1 := DesignedComponent('HintText1') as TCastleLabel;
	HintText2 := DesignedComponent('HintText2') as TCastleLabel;

	GlobalClient.Await(TMsgFeedLocationData, @OnLocationData);
end;

procedure TStateLoading.OnLocationData(const vData: TModelBase);
var
	vModel: TMsgFeedLocationData;
begin
	vModel := vData as TMsgFeedLocationData;
	FLocationId := vModel.id;
	RefreshLocationHints;
end;

{ implementation end }

end.

