unit GameLore;

interface

uses FGL, Classes, SysUtils,
	CastleDownload,
	GameTypes, Serialization;

type
	TLoreItem = class (TSerialized)
	private

		FLoreId: TLoreId;
		FLoreName: String;
		FLoreDescription: Variant;

	published

		property LoreId: TLoreId read FLoreId write FLoreId;
		property LoreName: String read FLoreName write FLoreName;
		property LoreDescription: Variant read FLoreDescription write FLoreDescription;

	end;

	TLoreItems = specialize TFPGObjectList<TLoreItem>;

	TLoreStore = class
	private

		FItems: TLoreItems;

	public
		constructor Create();
		destructor Destroy(); override;

		procedure Initialize();
		function GetByName(const vName: TLoreId): TLoreItem;

	published

		property Items: TLoreItems read FItems write FItems;
	end;

	var
		LoreCollection: TLoreStore;

implementation

{}
constructor TLoreStore.Create();
begin
	FItems := TLoreItems.Create;
end;

{}
destructor TLoreStore.Destroy();
begin
	FItems.Free;
	inherited;
end;

{}
procedure TLoreStore.Initialize();
var
	vStreamer: TGameStreamer;
	vLines: TStringList;
begin
	vStreamer := TGameStreamer.Create;
	vLines := TStringList.Create;

	vLines.LoadFromStream(Download('castle-data:/lore.pl.json'));
	vStreamer.DeStreamer.JSONToObject(vLines.Text, self);

	vStreamer.Free;
end;

{}
function TLoreStore.GetByName(const vName: TLoreId): TLoreItem;
var
	vItem: TLoreItem;
begin
	result := nil;
	for vItem in FItems do begin
		if vItem.LoreId = vName then
			result := vItem;
	end;

	if result = nil then
		raise Exception.Create('Lore item with id ' + vName + ' does not exist');
end;

{ implementation end }

initialization
	ListSerializationMap.Add(TSerializedList.Create(TLoreItems, TLoreItem));
	LoreCollection := TLoreStore.Create;

finalization
	LoreCollection.Free;

end.

