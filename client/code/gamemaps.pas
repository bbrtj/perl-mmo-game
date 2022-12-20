unit GameMaps;

interface

uses FGL, Classes,
	CastleDownload,
	Serialization,
	GameLore,
	GameTypes;

type

	TMapCoordinate = class(TSerialized)
	private
		FType: String;
		FContents: String;
		FTerrain: String;

		function GetContents(): String;
		function GetTerrain(): String;

	public
		constructor Create(); override;

	published
		property &type: String read FType write FType;
		property contents: String read GetContents write FContents;
		property terrain: String read GetTerrain write FTerrain;

	end;

	TMapCoordinates = specialize TFPGObjectList<TMapCoordinate>;

	TMap = class(TSerialized)
	private
		FCoordinates: TMapCoordinates;
		FSizeX: Cardinal;
		FSizeY: Cardinal;

	public
		constructor Create(); override;
		destructor Destroy; override;

	published
		property coordinates: TMapCoordinates read FCoordinates write FCoordinates;
		property size_x: Cardinal read FSizeX write FSizeX;
		property size_y: Cardinal read FSizeY write FSizeY;

	end;

	TMapData = class(TSerialized)
	private
		FId: TLoreId;
		FArea: TLoreId;
		FPosX: Single;
		FPosY: Single;
		FMap: TMap;
		FConnectedTo: TLoreIds;

		function GetLore(): TLoreItem;
	public
		constructor Create(); override;
		destructor Destroy; override;

		property Lore: TLoreItem read GetLore;

	published
		property id: TLoreId read FId write FId;
		property area: TLoreId read FArea write FArea;
		property pos_x: Single read FPosX write FPosX;
		property pos_y: Single read FPosY write FPosY;
		property map: TMap read FMap write FMap;
		property connected_to: TLoreIds read FConnectedTo write FConnectedTo;

	end;

	TMapIndexEntry = class(TSerialized)
	private
		FId: String;
		FFile: String;

	published
		property id: String read FId write FId;
		property &file: String read FFile write FFile;

	end;

	TMapIndexEntries = specialize TFPGObjectList<TMapIndexEntry>;

	TMapIndex = class
	private
		FIndex: TMapIndexEntries;

	public
		constructor Create();
		destructor Destroy; override;

		procedure Initialize();
		function GetMapData(const vId: String): TMapData;

	published
		property &index: TMapIndexEntries read FIndex write FIndex;

	end;

var
	MapIndex: TMapIndex;

implementation

constructor TMapCoordinate.Create();
begin
	inherited;
	FContents := '';
	FTerrain := '';
end;

function TMapCoordinate.GetContents(): String;
begin
	result := FContents;
	if result = '' then
		result := FType;
end;

function TMapCoordinate.GetTerrain(): String;
begin
	result := FTerrain;
	if result = '' then
		result := FType;
end;

constructor TMap.Create();
begin
	inherited;
	FCoordinates := TMapCoordinates.Create;
end;

destructor TMap.Destroy;
begin
	inherited;
	FCoordinates.Free;
end;

constructor TMapData.Create();
begin
	inherited;
	FConnectedTo := TLoreIds.Create;
end;

destructor TMapData.Destroy;
begin
	inherited;
	FConnectedTo.Free;
end;

function TMapData.GetLore(): TLoreItem;
begin
	result := LoreCollection.GetById(FId);
end;

constructor TMapIndex.Create();
begin
	FIndex := TMapIndexEntries.Create;
end;

destructor TMapIndex.Destroy;
begin
	FIndex.Free;
end;

procedure TMapIndex.Initialize();
var
	vStreamer: TGameStreamer;
	vLines: TStringList;
begin
	vStreamer := TGameStreamer.Create;
	vLines := TStringList.Create;

	vLines.LoadFromStream(Download('castle-data:/mapindex.json'));
	vStreamer.DeStreamer.JSONToObject(vLines.Text, self);

	vStreamer.Free;
	vLines.Free;
end;

function TMapIndex.GetMapData(const vId: String): TMapData;
var
	vEntry: TMapIndexEntry;
	vStreamer: TGameStreamer;
	vLines: TStringList;
begin
	vStreamer := TGameStreamer.Create;
	vLines := TStringList.Create;
	result := nil;

	for vEntry in FIndex do begin
		if vEntry.id = vId then begin
			result := TMapData.Create;
			vLines.LoadFromStream(Download('castle-data:/maps/' + vEntry.&file + '.json'));
			vStreamer.DeStreamer.JSONToObject(vLines.Text, result);
			break;
		end;
	end;

	vStreamer.Free;
	vLines.Free;
end;

{ implementation end }

initialization
	ListSerializationMap.Add(TSerializedList.Create(TMapIndexEntries, TMapIndexEntry));
	MapIndex := TMapIndex.Create();

finalization
	MapIndex.Free;

end.

