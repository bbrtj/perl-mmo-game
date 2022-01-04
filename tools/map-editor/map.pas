unit map;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, ExtCtrls, Graphics, FGL, FPJSON, JSONParser,
	markerdata, editortypes;

type

	TMap = class;

	TMarker = TShape;

	TMapMarker = class(TMarkerData)
	private
		FPosX: Integer;
		FPosY: Integer;
		FMarker: TMarker;
		FMap: TMap;

	public
		constructor Create(map: TMap);
		destructor Destroy; override;

		procedure Initialize();
		procedure Initialize(const X, Y: Integer);
		procedure Initialize(const X, Y: Integer; const vLoreId, vLoreName, vLoreDescription: String);

		procedure SetPosX(const position: Integer);
		procedure SetPosY(const position: Integer);

		procedure MarkerClicked(Sender: TObject);

		function Export(): TJSONObject; override;
		procedure Import(const json: TJSONObject); override;

		procedure Disable();

		property PosX: Integer read FPosX write SetPosX;
		property PosY: Integer read FPosY write SetPosY;
	end;

	TMarkers = specialize TFPGObjectList<TMapMarker>;
	TMarkerConnection = specialize TFPGList<TMapMarker>;
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

		FMapData: TMarkerData;

		FOffsetX: Integer;
		FOffsetY: Integer;

		FEdited: TMapMarker;
		FConnecting: Boolean;
		FDeleting: Boolean;

		FLogger: TLoggerProcedure;
		FOnChange: TMapChangedProcedure;

		function FindConnection(const marker1, marker2: TMapMarker): Integer;
		procedure AddConnection(const marker1, marker2: TMapMarker);
		procedure AddConnection(const marker1, marker2: String);
		procedure DeleteConnection(const marker1, marker2: TMapMarker);
		procedure DeleteConnections(const marker: TMapMarker);

		function CreateMarker(): TMapMarker;
	public
		constructor Create(const filename: String);
		destructor Destroy; override;
		procedure Initialize(const canvas: TImage);

		procedure AddMarker(const X, Y: Integer);
		procedure DeleteMarker(const marker: TMapMarker);

		procedure SetConnected(const value: TMapMarker);
		procedure SetEdited(const value: TMapMarker);

		procedure Import(const filename: String);
		function Export(canvas: TImage): TJSONObject;

		procedure Draw(canvas: TCanvas);

		property MetaFileName: String read FMetaFilename;
		property MarkerBlueprint: TShape read FMarkerBlueprint write FMarkerBlueprint;

		property Connecting: Boolean read FConnecting write FConnecting;
		property Deleting: Boolean read FDeleting write FDeleting;

		property Logger: TLoggerProcedure read FLogger write FLogger;
		property OnChange: TMapChangedProcedure read FOnChange write FOnChange;

		property OffsetX: Integer read FOffsetX;
		property OffsetY: Integer read FOffsetY;

		property MapData: TMarkerData read FMapData;
	end;
implementation

{ TMapMarker }

{}
constructor TMapMarker.Create(map: TMap);
begin
	FMap := map;
	FMarker := TMarker.Create(map.MarkerBlueprint.Owner);
	FMarker.Parent := map.MarkerBlueprint.Parent;

	FMarker.Width := map.MarkerBlueprint.Width;
	FMarker.Height := map.MarkerBlueprint.Height;
	FMarker.Shape := map.MarkerBlueprint.Shape;
	FMarker.Brush := map.MarkerBlueprint.Brush;
	FMarker.ShowHint := map.MarkerBlueprint.ShowHint;
	FMarker.Cursor := map.MarkerBlueprint.Cursor;
end;

{}
destructor TMapMarker.Destroy;
begin
	if FMarker <> nil then
		FMarker.Free;
	inherited;
end;

{}
procedure TMapMarker.Initialize();
begin
	FMarker.OnClick := @self.MarkerClicked;
	FMarker.Hint := LoreName;
	SetPosX(FPosX);
	SetPosY(FPosY);
end;

{}
procedure TMapMarker.Initialize(const X, Y: Integer);
begin
	PosX := X;
	PosY := Y;
end;

{}
procedure TMapMarker.Initialize(const X, Y: Integer; const vLoreId, vLoreName, vLoreDescription: String);
begin
	Initialize(X, Y);
	inherited Initialize(vLoreId, vLoreName, vLoreDescription);
	Initialize();
end;

{}
procedure TMapMarker.SetPosX(const position: Integer);
begin
	FPosX := position;
	if FMarker <> Nil then
		FMarker.Left := position + FMap.OffsetX - FMarker.Width div 2;
end;

{}
procedure TMapMarker.SetPosY(const position: Integer);
begin
	FPosY := position;
	if FMarker <> Nil then
		FMarker.Top := position + FMap.OffsetY - FMarker.Height div 2;
end;

{}
procedure TMapMarker.MarkerClicked(Sender: TObject);
begin
	if FMap.Connecting then
		FMap.SetConnected(self)
	else
		FMap.SetEdited(self);
end;

{}
function TMapMarker.Export(): TJSONObject;
begin
	result := inherited;
	result.Add('x', PosX);
	result.Add('y', PosY);
end;

{}
procedure TMapMarker.Import(const json: TJSONObject);
begin
	inherited;
	Initialize(
		json.Get('x', 0),
		json.Get('y', 0)
	);
	Initialize();
end;

{}
procedure TMapMarker.Disable();
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
	FMapData := TMarkerData.Create;

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
	FMapData.Free;
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
function TMap.CreateMarker(): TMapMarker;
begin
	result := TMapMarker.Create(self);
	markers.Add(result);
end;

{}
procedure TMap.DeleteMarker(const marker: TMapMarker);
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
	marker: TMapMarker;
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
		marker := FEdited;
		if marker = nil then begin
			marker := CreateMarker();
			Logger('Marker created: ' + dialog.LoreIdValue);
		end
		else
			Logger('Marker updated ' + dialog.LoreIdValue);

		marker.Initialize(X, Y, dialog.LoreIdValue, dialog.LoreNameValue, dialog.LoreDescriptionValue);
		OnChange();
	end
	else
		Logger('Aborted');

	FEdited := nil;

	dialog.Free;
end;

{}
procedure TMap.SetEdited(const value: TMapMarker);
begin
	if value = nil then begin
		FEdited := nil;
		exit;
	end;

	if FEdited = value then begin
		if FDeleting then
			DeleteMarker(value)
		else
			AddMarker(value.PosX, value.PosY);
		FEdited := nil;
	end
	else begin
		FEdited := value;
		Logger('Updating a marker ' + value.LoreId);
	end;

end;

{}
procedure TMap.SetConnected(const value: TMapMarker);
begin
	if FEdited = nil then begin
		FEdited := value;
		Logger('Select a second marker to connect');
	end
	else begin
		if value <> FEdited then begin
			if not FDeleting then begin
				AddConnection(FEdited, value);
				Logger('Connected markers: ' + FEdited.LoreId + ' -> ' + value.LoreId);
			end
			else begin
				DeleteConnection(FEdited, value);
				Logger('Deleted connection');
			end;

			OnChange();
		end
		else
			Logger('Will not connect to self!');

		FEdited := nil;
	end;
end;

{}
function TMap.FindConnection(const marker1, marker2: TMapMarker): Integer;
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
procedure TMap.AddConnection(const marker1, marker2: TMapMarker);
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
	marker: TMapMarker;
	foundMarker: TMapMarker;
begin
	foundMarker := nil;

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
procedure TMap.DeleteConnection(const marker1, marker2: TMapMarker);
var
	index: Integer;
begin
	index := FindConnection(marker1, marker2);
	if index >= 0 then
		connections.Delete(index);
end;

{}
procedure TMap.DeleteConnections(const marker: TMapMarker);
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
	marker: TMapMarker;
	jsonArray: TJSONArray;
	conn: TMarkerConnection;
begin
	result := FMapData.Export();
	result.Add('name', imageFilename);
	result.Add('canvas_width', canvas.Width);
	result.Add('canvas_height', canvas.Height);

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
	FMapData.Import(jsonObject);

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
