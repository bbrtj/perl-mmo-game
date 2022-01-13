unit map;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, ExtCtrls, Graphics, FGL, fpjsonrtti, FPJSON, JSONParser,
	translationdialog, loreiddialog, editortypes, serialization;

type

	TMap = class;

	TMapMarker = class(TSerialized)
	private
		FLoreId: TLoreId;
		FPosX: Integer;
		FPosY: Integer;
		FMarker: TShape;
		FMap: TMap;
		FTranslations: TTranslations;

		procedure SetPosX(const position: Integer);
		procedure SetPosY(const position: Integer);
	public
		constructor Create(); override;
		destructor Destroy; override;

		procedure SetMap(map: TMap);

		procedure MarkerClicked(Sender: TObject);
		procedure UpdateTranslations();

		procedure Disable();

	published
		property LoreId: TLoreId read FLoreId write FLoreId;
		property PosX: Integer read FPosX write SetPosX;
		property PosY: Integer read FPosY write SetPosY;
		property Translations: TTranslations read FTranslations write FTranslations;
	end;

	TMarkers = specialize TFPGObjectList<TMapMarker>;
	TMarkerConnection = specialize TFPGObjectList<TMapMarker>;
	TMarkerConnections = specialize TFPGObjectList<TMarkerConnection>;

	TMap = class(TSerialized)
	private
		FImageFilename: String;
		FMetaFilename: String;
		FImage: TPicture;
		FMarkerBlueprint: TShape;
		FMarkers: TMarkers;
		FDeletedMarkers: TMarkers;
		FConnections: TMarkerConnections;

		FMapData: TTranslations;

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

		procedure UpdateTranslations();

		procedure AddMarker(const X, Y: Integer);
		procedure DeleteMarker(const marker: TMapMarker);

		procedure SetConnected(const value: TMapMarker);
		procedure SetEdited(const value: TMapMarker);

		procedure Import(const filename: String);
		function Export(): String;

		procedure Draw(canvas: TCanvas);

		property MetaFileName: String read FMetaFilename;
		property MarkerBlueprint: TShape read FMarkerBlueprint write FMarkerBlueprint;

		property Connecting: Boolean read FConnecting write FConnecting;
		property Deleting: Boolean read FDeleting write FDeleting;

		property Logger: TLoggerProcedure read FLogger write FLogger;
		property OnChange: TMapChangedProcedure read FOnChange write FOnChange;

		property OffsetX: Integer read FOffsetX;
		property OffsetY: Integer read FOffsetY;

	published
		property ImageName: String read FImageFilename write FImageFilename;
		property Translations: TTranslations read FMapData write FMapData;
		property Connections: TMarkerConnections read FConnections write FConnections;
		property Markers: TMarkers read FMarkers write FMarkers;

	end;
implementation

{ TMapMarker }

{}
constructor TMapMarker.Create();
begin
	FTranslations := TTranslations.Create;
end;

{}
destructor TMapMarker.Destroy;
begin
	if FMarker <> nil then
		FMarker.Free;
	FTranslations.Free;
	inherited;
end;

procedure TMapMarker.SetMap(map: TMap);
begin
	FMap := map;
	FMarker := TShape.Create(map.MarkerBlueprint.Owner);
	FMarker.Parent := map.MarkerBlueprint.Parent;

	FMarker.Width := map.MarkerBlueprint.Width;
	FMarker.Height := map.MarkerBlueprint.Height;
	FMarker.Shape := map.MarkerBlueprint.Shape;
	FMarker.Brush := map.MarkerBlueprint.Brush;
	FMarker.ShowHint := map.MarkerBlueprint.ShowHint;
	FMarker.Cursor := map.MarkerBlueprint.Cursor;

	FMarker.OnClick := @self.MarkerClicked;

	SetPosX(FPosX);
	SetPosY(FPosY);
end;

{}
procedure TMapMarker.SetPosX(const position: Integer);
begin
	FPosX := position;
	if FMarker <> nil then
		FMarker.Left := position + FMap.OffsetX - FMarker.Width div 2;
end;

{}
procedure TMapMarker.SetPosY(const position: Integer);
begin
	FPosY := position;
	if FMarker <> nil then
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
procedure TMapMarker.UpdateTranslations();
var
	dialog: TTranslationDialog;
begin
	dialog := TTranslationDialog.Create(nil);

	dialog.Translations := FTranslations;
	dialog.ShowModal();
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
	FMarkers := TMarkers.Create;
	FDeletedMarkers := TMarkers.Create;
	FConnections := TMarkerConnections.Create;
	FImage := TPicture.Create;
	FConnecting := false;
	FMapData := TTranslations.Create;

	FImageFilename := '';
	FMetaFilename := '';

	if filename.EndsWith('.map.json') then
		FMetaFilename := filename
	else
		FImageFilename := filename;
end;

{}
destructor TMap.Destroy;
begin
	{ Do not free marker blueprint! }
	FImage.Free;
	FMarkers.Free;
	FDeletedMarkers.Free;
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
		FMetaFilename := FImageFilename + '.map.json';

	FImage.LoadFromFile(FImageFilename);
	canvas.Picture := FImage;
	Logger('Map initialized');
end;

{}
procedure TMap.UpdateTranslations();
var
	dialog: TTranslationDialog;
begin
	dialog := TTranslationDialog.Create(nil);

	dialog.Translations := FMapData;
	dialog.ShowModal();
end;

{}
function TMap.CreateMarker(): TMapMarker;
begin
	result := TMapMarker.Create();
	result.SetMap(self);
	FMarkers.Add(result);
end;

{}
procedure TMap.DeleteMarker(const marker: TMapMarker);
begin
	DeleteConnections(marker);
	FDeletedMarkers.Add(markers.Extract(marker));
	marker.Disable();
	OnChange;
end;

{}
procedure TMap.AddMarker(const X, Y: Integer);
var
	dialog: TLoreIdDialog;
	marker: TMapMarker;
begin
	if FDeleting then begin
		Logger('You are in a wrong mode!');
		exit;
	end;

	dialog := TLoreIdDialog.Create(nil);

	marker := nil;
	if FEdited <> nil then begin
		marker := FEdited;
		dialog.LoreIdValue := marker.LoreId;
	end;

	dialog.ShowModal();

	if dialog.Saved then begin
		if marker = nil then
			marker := CreateMarker();

		marker.LoreId := dialog.LoreIdValue;
		marker.PosX := X;
		marker.PosY := Y;
		OnChange();
		Logger('Marker saved: ' + dialog.LoreIdValue);
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
			FEdited.UpdateTranslations();
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
	for index := 0 to FConnections.Count - 1 do begin
		if ((FConnections[index][0] = marker1) and (FConnections[index][1] = marker2)
		   or (FConnections[index][0] = marker2) and (FConnections[index][1] = marker1))
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
		conn := TMarkerConnection.Create(false);
		conn.Add(marker1);
		conn.Add(marker2);
		FConnections.Add(conn);
	end;
end;

{}
procedure TMap.AddConnection(const marker1, marker2: String);
var
	marker: TMapMarker;
	foundMarker: TMapMarker;
begin
	foundMarker := nil;

	for marker in FMarkers do begin
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
		FConnections.Delete(index);
end;

{}
procedure TMap.DeleteConnections(const marker: TMapMarker);
var
	index: Integer;
	indexList: TIndexList;
begin
	indexList := TIndexList.Create;

	for index := 0 to FConnections.Count - 1 do begin
		if (FConnections[index][0] = marker) or (FConnections[index][1] = marker) then
			indexList.Add(index);
	end;

	for index := indexList.Count - 1 downto 0 do
		FConnections.Delete(indexList[index]);

	indexList.Free;
end;

{}
function TMap.Export(): String;
var
	streamer: TGameStreamer;
begin
	streamer := TGameStreamer.Create();
	result := streamer.Streamer.ObjectToJSONString(self);

	streamer.Free;
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

	streamer: TGameStreamer;
	marker: TMapMarker;
begin
	inFile := TFileStream.Create(filename, fmOpenRead);
	contents := '';
	repeat
		bytesRead := inFile.Read(readBuffer, readSize);
		contents := contents + readBuffer;
	until bytesRead < readSize;
	inFile.Free;

	streamer := TGameStreamer.Create();
	streamer.DeStreamer.JSONToObject(contents, self);
	streamer.Free;

	for marker in FMarkers do
		marker.SetMap(self);

end;

{}
procedure TMap.Draw(canvas: TCanvas);
var
	points: Array[0 .. 1] of TPoint;

	conn: TMarkerConnection;
begin
	canvas.Pen.Color := clBlue;
	canvas.Pen.Width := 2;
	canvas.Pen.Style := psSolid;

	for conn in FConnections do begin
		points[0].X := conn[0].PosX;
		points[0].Y := conn[0].PosY;
		points[1].X := conn[1].PosX;
		points[1].Y := conn[1].PosY;
		canvas.Polyline(points, 0, 2);
	end;
end;

initialization
	ListSerializationMap.Add(TSerializedList.Create(TMarkers, TMapMarker));

end.
