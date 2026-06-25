unit GameLore;

interface

uses FGL, Classes, SysUtils,
	CastleDownload,
	GameTypes, Serialization, FPJSON;

type
	ELore = class(Exception);

	TLoreVisuals = class (TSerialized)
	private
		FModel: String;
		FModelSize: Single;
	published
		property model: String read FModel write FModel;
		property model_size: Single read FModelSize write FModelSize;
	end;

	TLoreItem = class (TSerialized)
	private
		FId: TLoreId;
		FName: String;
		FDescription: String;
		FVisuals: TLoreVisuals;

	public
		constructor Create(); override;
		destructor Destroy; override;

		function GetVisuals(): TLoreVisuals;

	published
		property id: TLoreId read FId write FId;
		property name: String read FName write FName;
		property description: String read FDescription write FDescription;
		property visuals: TLoreVisuals read FVisuals write FVisuals;
	end;

	TLoreItems = specialize TFPGObjectList<TLoreItem>;

	TLoreStore = class
	private
		FItems: TLoreItems;

	public
		constructor Create();
		destructor Destroy(); override;

		procedure Initialize();
		function GetById(const Id: TLoreId): TLoreItem;

	published
		property Items: TLoreItems read FItems write FItems;
	end;

var
	LoreCollection: TLoreStore;

implementation

constructor TLoreItem.Create();
begin
	inherited;
	FVisuals := TLoreVisuals.Create;
end;

destructor TLoreItem.Destroy();
begin
	FVisuals.Free;
end;

function TLoreItem.GetVisuals(): TLoreVisuals;
begin
	if FVisuals = nil then
		raise ELore.Create('missing visuals for lore ' + FId);

	result := FVisuals;
end;

constructor TLoreStore.Create();
begin
	FItems := TLoreItems.Create;
end;

destructor TLoreStore.Destroy();
begin
	FItems.Free;
	inherited;
end;

procedure TLoreStore.Initialize();
var
	LStreamer: TGameStreamer;
	LLines: TStringList;
	LStream: TStream;
begin
	LStreamer := TGameStreamer.Create;
	LLines := TStringList.Create;

	LStream := Download('castle-data:/lore.json');
	LLines.LoadFromStream(LStream);
	LStreamer.DeStreamer.JSONToObject(LLines.Text, self);

	LStreamer.Free;
	LLines.Free;
	LStream.Free;
end;

// TODO: this should be a map to avoid linear search
function TLoreStore.GetById(const Id: TLoreId): TLoreItem;
var
	LItem: TLoreItem;
begin
	result := nil;
	for LItem in FItems do begin
		if LItem.id = Id then
			result := LItem;
	end;

	if result = nil then
		raise ELore.Create('Lore item with id ' + Id + ' does not exist');
end;

initialization
	ListSerializationMap.Add(TSerializedList.Create(TLoreItems, TLoreItem));
	LoreCollection := TLoreStore.Create;

finalization
	FreeAndNil(LoreCollection);

end.

