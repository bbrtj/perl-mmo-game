unit GameMaps;

interface

uses FGL, Classes,
	CastleDownload,
	Serialization,
	GameLore,
	GameTypes;

type

	TMap = class(TSerialized)
	private
		FSizeX: Cardinal;
		FSizeY: Cardinal;

	public
		constructor Create(); override;
		destructor Destroy; override;

	published
		property SizeX: Cardinal read FSizeX write FSizeX;
		property SizeY: Cardinal read FSizeY write FSizeY;

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
		property Id: TLoreId read FId write FId;
		property Area: TLoreId read FArea write FArea;
		property PosX: Single read FPosX write FPosX;
		property PosY: Single read FPosY write FPosY;
		property Map: TMap read FMap write FMap;
		property ConnectedTo: TLoreIds read FConnectedTo write FConnectedTo;

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
		function GetMapData(const Id: String): TMapData;
		function GetMapPath(const Id: String): String;

	published
		property &index: TMapIndexEntries read FIndex write FIndex;

	end;

var
	MapIndex: TMapIndex;

implementation

constructor TMap.Create();
begin
	inherited;
end;

destructor TMap.Destroy;
begin
	inherited;
end;

constructor TMapData.Create();
begin
	inherited;
	FMap := TMap.Create;
	FConnectedTo := TLoreIds.Create;
end;

destructor TMapData.Destroy;
begin
	inherited;
	FMap.Free;
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
	LStreamer: TGameStreamer;
	LLines: TStringList;
	LStream: TStream;
begin
	LStreamer := TGameStreamer.Create;
	LLines := TStringList.Create;

	LStream := Download('castle-data:/maps/index.json');
	LLines.LoadFromStream(LStream);
	LStreamer.DeStreamer.JSONToObject(LLines.Text, self);

	LStreamer.Free;
	LLines.Free;
	LStream.Free;
end;

function TMapIndex.GetMapData(const Id: String): TMapData;
var
	LEntry: TMapIndexEntry;
	LStreamer: TGameStreamer;
	LLines: TStringList;
	LStream: TStream;
begin
	LStreamer := TGameStreamer.Create;
	LLines := TStringList.Create;
	result := nil;

	for LEntry in FIndex do begin
		if LEntry.id = Id then begin
			result := TMapData.Create;

			LStream := Download('castle-data:/maps/meta/' + LEntry.&file + '.json');
			LLines.LoadFromStream(LStream);
			LStreamer.DeStreamer.JSONToObject(LLines.Text, result);

			LStream.Free;
			break;
		end;
	end;

	LStreamer.Free;
	LLines.Free;
end;

function TMapIndex.GetMapPath(const Id: String): String;
var
	LEntry: TMapIndexEntry;
begin
	for LEntry in FIndex do begin
		if LEntry.id = Id then begin
			result := 'castle-data:/maps/' + LEntry.&file + '.tmx';
			break;
		end;
	end;
end;

{ implementation end }

initialization
	ListSerializationMap.Add(TSerializedList.Create(TMapIndexEntries, TMapIndexEntry));
	MapIndex := TMapIndex.Create();

finalization
	MapIndex.Free;

end.

