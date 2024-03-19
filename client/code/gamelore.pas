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
		FLoreDescription: String;

	published
		property LoreId: TLoreId read FLoreId write FLoreId;
		property LoreName: String read FLoreName write FLoreName;
		property LoreDescription: String read FLoreDescription write FLoreDescription;

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

function TLoreStore.GetById(const Id: TLoreId): TLoreItem;
var
	LItem: TLoreItem;
begin
	result := nil;
	for LItem in FItems do begin
		if LItem.LoreId = Id then
			result := LItem;
	end;

	if result = nil then
		raise Exception.Create('Lore item with id ' + Id + ' does not exist');
end;

{ implementation end }

initialization
	ListSerializationMap.Add(TSerializedList.Create(TLoreItems, TLoreItem));
	LoreCollection := TLoreStore.Create;

finalization
	LoreCollection.Free;

end.

