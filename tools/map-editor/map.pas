unit map;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, ExtCtrls, Graphics, FGL, FPJSON, JSONParser,
	markerdata, editortypes;

type

	TMap = class;

	TMarker = TShape;
	TLoreId = String[20];
	TLoreName = String[255];

	TMarkerData = class
	private
		FLoreId: TLoreId;
		FLoreName: TLoreName;
		FLoreDescription: String;
		FPosX: Integer;
		FPosY: Integer;
		FMarker: TMarker;
		FMap: TMap;

	public
		constructor Create(map: TMap);
		destructor Destroy; override;

		procedure Initialize();
		procedure Initialize(const X, Y: Integer; const LoreId, LoreName, LoreDescription: String);

		procedure SetPosX(const position: Integer);
		procedure SetPosY(const position: Integer);

		procedure MarkerClicked(Sender: TObject);

		function Export(): TJSONObject;
		procedure Import(const json: TJSONObject);

		procedure Disable();

		property LoreId: TLoreId read FLoreId write FLoreId;
		property LoreName: TLoreName read FLoreName write FLoreName;
		property LoreDescription: String read FLoreDescription write FLoreDescription;
		property PosX: Integer read FPosX write SetPosX;
		property PosY: Integer read FPosY write SetPosY;
	end;

	TMarkers = specialize TFPGObjectList<TMarkerData>;
	TMarkerConnection = specialize TFPGList<TMarkerData>;
	TMarkerConnections = specialize TFPGObjectList<TMarkerConnection>;

	TMap = class
	private
		imageFilename: String;
		FMetaFilename: String;
		image: TPicture;
		FMarkerBlueprint: TShape;
		markers: TMarkers;
		deletedMarkers: TMarkers;
		connections: TMarkerConnections;

		FOffsetX: Integer;
		FOffsetY: Integer;

		FEdited: TMarkerData;
		FConnecting: Boolean;
		FConnected: TMarkerData;
		FDeleting: Boolean;

		FLogger: TLoggerProcedure;
		FOnChange: TMapChangedProcedure;

		function FindConnection(const marker1, marker2: TMarkerData): Integer;
		procedure DeleteConnections(const marker: TMarkerData);
		procedure AddConnection(const marker1, marker2: TMarkerData);
		procedure AddConnection(const marker1, marker2: String);
		procedure DeleteConnection(const marker1, marker2: TMarkerData);

		function CreateMarker(): TMarkerData;
		procedure SetEdited(value: TMarkerData);
		procedure SetConnected(value: TMarkerData);
	public
		constructor Create(const filename: String);
		destructor Destroy; override;
		procedure Initialize(const canvas: TImage);

		procedure AddMarker(const X, Y: Integer);
		procedure DeleteMarker(const marker: TMarkerData);

		procedure Import(const filename: String);
		function Export(canvas: TImage): TJSONObject;

		procedure Draw(canvas: TCanvas);

		property MetaFileName: String read FMetaFilename;
		property MarkerBlueprint: TShape read FMarkerBlueprint write FMarkerBlueprint;

		property Edited: TMarkerData read FEdited write SetEdited;
		property Connecting: Boolean read FConnecting write FConnecting;
		property Connected: TMarkerData read FConnected write SetConnected;
		property Deleting: Boolean read FDeleting write FDeleting;

		property Logger: TLoggerProcedure read FLogger write FLogger;
		property OnChange: TMapChangedProcedure read FOnChange write FOnChange;

		property OffsetX: Integer read FOffsetX;
		property OffsetY: Integer read FOffsetY;
	end;
implementation

{ TMarkerData }

{}
constructor TMarkerData.Create(map: TMap);
begin
	FMap := map;
	FMarker := TMarker.Create(map.MarkerBlueprint.Owner);
	FMarker.Parent := map.MarkerBlueprint.Parent;

	FMarker.Width := map.MarkerBlueprint.Width;
	FMarker.Height := map.MarkerBlueprint.Height;
	FMarker.Shape := map.MarkerBlueprint.Shape;
	FMarker.Brush := map.MarkerBlueprint.Brush;
	FMarker.ShowHint := map.MarkerBlueprint.ShowHint;
end;

{}
destructor TMarkerData.Destroy;
begin
	if FMarker <> nil then
		FMarker.Free;
	inherited;
end;

{}
procedure TMarkerData.Initialize(const X, Y: Integer; const LoreId, LoreName, LoreDescription: String);
begin
	self.LoreId := LoreId;
	self.LoreName := LoreName;
	self.LoreDescription := LoreDescription;
	self.PosX := X;
	self.PosY := Y;

	Initialize();
end;

{}
procedure TMarkerData.Initialize();
begin
	FMarker.OnClick := @self.MarkerClicked;
	FMarker.Hint := self.LoreName;
	SetPosX(FPosX);
	SetPosY(FPosY);
end;

{}
procedure TMarkerData.SetPosX(const position: Integer);
begin
	FPosX := position;
	if FMarker <> Nil then
		FMarker.Left := position + FMap.OffsetX - FMarker.Width div 2;
end;

{}
procedure TMarkerData.SetPosY(const position: Integer);
begin
	FPosY := position;
	if FMarker <> Nil then
		FMarker.Top := position + FMap.OffsetY - FMarker.Height div 2;
end;

{}
procedure TMarkerData.MarkerClicked(Sender: TObject);
begin
	if FMap.Connecting then
		FMap.Connected := self
	else begin
		if FMap.Edited = self then begin
			if FMap.Deleting then
				FMap.DeleteMarker(self)
			else
				FMap.AddMarker(self.PosX, self.PosY);
		end
		else
			FMap.Edited := self;
	end;
end;

{}
function TMarkerData.Export(): TJSONObject;
begin
	result := TJSONObject.Create([
		'lore_id', LoreId,
		'lore_name', LoreName,
		'lore_description', LoreDescription,
		'x', PosX,
		'y', PosY
	]);
end;

{}
procedure TMarkerData.Import(const json: TJSONObject);
begin
	Initialize(
		json.Get('x', 0),
		json.Get('y', 0),
		json.Get('lore_id', ''),
		json.Get('lore_name', ''),
		json.Get('lore_description', '')
	);
end;

{}
procedure TMarkerData.Disable();
begin
	FMarker.Parent := nil;
end;

{ TMap }

{}
constructor TMap.Create(const filename: String);
begin
	markers := TMarkers.Create;
	deletedMarkers := TMarkers.Create;
	connections := TMarkerConnections.Create;
	image := TPicture.Create;
	FConnecting := false;

	imageFilename := '';
	FMetaFilename := '';

	if filename.EndsWith('.map.json') then
		FMetaFilename := filename
	else
		imageFilename := filename;
end;

{}
destructor TMap.Destroy;
begin
	{ Do not free marker blueprint! }
	image.Free;
	markers.Free;
	deletedMarkers.Free;
	inherited;
end;

{}
procedure TMap.Initialize(const canvas: TImage);
begin
	FOffsetX := canvas.Left;
	FOffsetY := canvas.Top;

	if length(FMetaFilename) > 0 then
		Import(FMetaFilename)
	else
		FMetaFilename := imageFilename + '.map.json';

	image.LoadFromFile(imageFilename);
	canvas.Picture := image;
	Logger('Map initialized');
end;

{}
function TMap.CreateMarker(): TMarkerData;
begin
	result := TMarkerData.Create(self);
	markers.Add(result);
end;

{}
procedure TMap.DeleteMarker(const marker: TMarkerData);
begin
	DeleteConnections(marker);
	deletedMarkers.Add(markers.Extract(marker));
	marker.Disable();
	OnChange;
end;

{}
procedure TMap.AddMarker(const X, Y: Integer);
var
	dialog: TMarkerForm;
begin
	if FDeleting then begin
		Logger('You are in a wrong mode!');
		exit;
	end;

	dialog := TMarkerForm.Create(nil);

	if FEdited <> nil then begin
		dialog.LoreIdValue := FEdited.LoreId;
		dialog.LoreNameValue := FEdited.LoreName;
		dialog.LoreDescriptionValue := FEdited.LoreDescription;
	end;

	dialog.ShowModal();

	if dialog.MarkerAdded then begin
		if FEdited = nil then begin
			FEdited := CreateMarker();
			Logger('Marker created: ' + dialog.LoreIdValue);
		end
		else
			Logger('Marker updated ' + dialog.LoreIdValue);

		FEdited.Initialize(X, Y, dialog.LoreIdValue, dialog.LoreNameValue, dialog.LoreDescriptionValue);
		OnChange();
	end
	else
		Logger('Aborted');

	Edited := nil;
	dialog.Free;
end;

{}
procedure TMap.SetEdited(value: TMarkerData);
begin
	FEdited := value;
	if value <> nil then
		Logger('Updating a marker ' + value.LoreId);
end;

{}
procedure TMap.SetConnected(value: TMarkerData);
begin
	if FConnected = nil then begin
		FConnected := value;
		Logger('Select a second marker to connect');
	end
	else begin
		if not FDeleting then begin
			AddConnection(FConnected, value);
			Logger('Connected markers: ' + FConnected.LoreId + ' -> ' + value.LoreId);
		end
		else begin
			DeleteConnection(FConnected, value);
			Logger('Deleted connection');
		end;

		FConnected := nil;
		OnChange();
	end;
end;

{}
function TMap.FindConnection(const marker1, marker2: TMarkerData): Integer;
var
	index: Integer;
begin
	result := -1;
	for index := 0 to connections.Count - 1 do begin
		if ((connections[index][0] = marker1) and (connections[index][1] = marker2)
		   or (connections[index][0] = marker2) and (connections[index][1] = marker1))
		   then
			exit(index);
	end;
end;

{}
procedure TMap.AddConnection(const marker1, marker2: TMarkerData);
var
	conn: TMarkerConnection;
begin
	if FindConnection(marker1, marker2) < 0 then begin
		conn := TMarkerConnection.Create;
		conn.Add(marker1);
		conn.Add(marker2);
		connections.Add(conn);
	end;
end;

{}
procedure TMap.AddConnection(const marker1, marker2: String);
var
	marker: TMarkerData;
	foundMarker: TMarkerData;
begin
	for marker in markers do begin
		if (marker.LoreId = marker1) or (marker.LoreId = marker2) then begin
			if foundMarker <> nil then
				AddConnection(foundMarker, marker)
			else
				foundMarker := marker;
		end;
	end;
end;

{}
procedure TMap.DeleteConnection(const marker1, marker2: TMarkerData);
begin
	connections.Delete(FindConnection(marker1, marker2));
end;

{}
procedure TMap.DeleteConnections(const marker: TMarkerData);
var
	index: Integer;
	indexList: TIndexList;
begin
	indexList := TIndexList.Create;

	for index := 0 to connections.Count - 1 do begin
		if (connections[index][0] = marker) or (connections[index][1] = marker) then
			indexList.Add(index);
	end;

	for index := indexList.Count - 1 downto 0 do
		connections.Delete(indexList[index]);

	indexList.Free;
end;

{}
function TMap.Export(canvas: TImage): TJSONObject;
var
	marker: TMarkerData;
	jsonArray: TJSONArray;
	conn: TMarkerConnection;
begin
	result := TJSONObject.Create([
		'name', imageFilename,
		'canvas_width', canvas.Width,
		'canvas_height', canvas.Height
	]);

	jsonArray := TJSONArray.Create;
	for marker in markers do begin
		jsonArray.Add(marker.Export());
	end;

	result.Add('markers', jsonArray);

	jsonArray := TJSONArray.Create;
	for conn in connections do begin
		jsonArray.Add(TJSONArray.Create([conn[0].LoreId, conn[1].LoreId]));
	end;

	result.Add('connections', jsonArray);
end;

{}
procedure TMap.Import(const filename: String);
const
	readSize = 4096;
var
	contents: String;

	inFile: TFileStream;
	readBuffer: Array[0 .. 4095] of Char;
	bytesRead: Integer;

	jsonObject: TJSONObject;

	jsonMarkers: TJSONArray;
	jsonConnections: TJSONArray;
	jsonLoopVar: TJSONEnum;
	jsonConnection: TJSONArray;

begin
	inFile := TFileStream.Create(filename, fmOpenRead);
	contents := '';
	repeat
		bytesRead := inFile.Read(readBuffer, readSize);
		contents := contents + readBuffer;
	until bytesRead < readSize;
	inFile.Free;

	jsonObject := GetJSON(contents) as TJSONObject;
	imageFilename := jsonObject.Get('name', '');
	jsonMarkers := jsonObject.get('markers', TJSONArray.Create);

	for jsonLoopVar in jsonMarkers do
		CreateMarker().Import(jsonLoopVar.Value as TJSONObject);

	jsonConnections := jsonObject.get('connections', TJSONArray.Create);
	for jsonLoopVar in jsonConnections do begin
		jsonConnection := jsonLoopVar.Value as TJSONArray;
		AddConnection(jsonConnection.Strings[0], jsonConnection.Strings[1]);
	end;

end;

procedure TMap.Draw(canvas: TCanvas);
var
	points: Array[0 .. 1] of TPoint;

	conn: TMarkerConnection;
begin
	canvas.Pen.Color := clBlue;
	canvas.Pen.Width := 2;
	canvas.Pen.Style := psSolid;

	for conn in connections do begin
		points[0].X := conn[0].PosX;
		points[0].Y := conn[0].PosY;
		points[1].X := conn[1].PosX;
		points[1].Y := conn[1].PosY;
		canvas.Polyline(points, 0, 2);
	end;
end;

end.
