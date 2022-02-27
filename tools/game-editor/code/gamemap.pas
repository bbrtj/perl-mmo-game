unit gamemap;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, ExtCtrls, Graphics, FGL, fpjsonrtti, FPJSON,
	translationdialog, loreiddialog, editorcommon, serialization;

type

	TMap = class;

	TMapMarker = class(TSerialized)
	private
		FLoreId: TLoreId;
		FPosX: Real;
		FPosY: Real;
		FMarker: TShape;
		FMap: TMap;
		FTranslations: TTranslations;
		FConnectedTo: TStringList;

		procedure SetPosX(position: Real);
		procedure SetPosY(position: Real);
		procedure SetLoreId(const id: TLoreId);
	public
		constructor Create(); override;
		destructor Destroy; override;

		procedure SetMap(map: TMap);

		procedure MarkerClicked(Sender: TObject);
		procedure UpdateTranslations();

		procedure Disable();

		procedure ConnectWith(markerId: TLoreId);
		procedure DisconnectWith(markerId: TLoreId);

	published
		property LoreId: TLoreId read FLoreId write SetLoreId;
		property PosX: Real read FPosX write SetPosX;
		property PosY: Real read FPosY write SetPosY;
		property Translations: TTranslations read FTranslations write FTranslations;
		property ConnectedTo: TStringlist read FConnectedTo write FConnectedTo;
	end;

	TMarkers = specialize TFPGObjectList<TMapMarker>;

	TMap = class(TSerialized)
	private
		FImageFilename: String;
		FMetaFilename: String;
		FImage: TPicture;
		FMarkerBlueprint: TShape;
		FMarkers: TMarkers;
		FDeletedMarkers: TMarkers;

		FMapData: TTranslations;

		FEdited: TMapMarker;
		FConnecting: Boolean;
		FDeleting: Boolean;

		FLogger: TLoggerProcedure;
		FOnChange: TMapChangedProcedure;

		FCanvasWidth: Integer;
		FCanvasHeight: Integer;

		procedure AddConnection(const marker1, marker2: TMapMarker);
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

		property CanvasWidth: Integer read FCanvasWidth;
		property CanvasHeight: Integer read FCanvasHeight;

	published
		property ImageName: String read FImageFilename write FImageFilename;
		property Translations: TTranslations read FMapData write FMapData;
		property Markers: TMarkers read FMarkers write FMarkers;

	end;
implementation

{ TMapMarker }

{}
constructor TMapMarker.Create();
begin
	FTranslations := TTranslations.Create;
	FConnectedTo := TStringList.Create;
	FConnectedTo.Sorted := true;
	FConnectedTo.Duplicates := dupIgnore;
	FConnectedTo.LineBreak := ';';
	FConnectedTo.TrailingLineBreak := false;
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
	SetLoreId(FLoreId);
end;

{}
procedure TMapMarker.SetPosX(position: Real);
begin
	if (position > 1) and (FMap <> nil) then
		position := position / FMap.CanvasWidth;

	FPosX := position;
	if FMarker <> nil then
		FMarker.Left := round(position * FMap.CanvasWidth) - FMarker.Width div 2;
end;

{}
procedure TMapMarker.SetPosY(position: Real);
begin
	if (position > 1) and (FMap <> nil) then
		position := position / FMap.CanvasHeight;

	FPosY := position;
	if FMarker <> nil then
		FMarker.Top := round(position * FMap.CanvasHeight) - FMarker.Height div 2;
end;

{}
procedure TMapMarker.SetLoreId(const id: TLoreId);
begin
	FLoreId := id;
	if FMarker <> nil then
		FMarker.Hint := id;
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
procedure TMapMarker.ConnectWith(markerId: TLoreId);
begin
	FConnectedTo.Add(markerId);
end;

{}
procedure TMapMarker.DisconnectWith(markerId: TLoreId);
var
	ind: Integer;
begin
	if FConnectedTo.Find(markerId, ind) then
		FConnectedTo.Delete(ind);
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
	FImage := TPicture.Create;
	FConnecting := false;
	FMapData := TTranslations.Create;

	FImageFilename := '';
	FMetaFilename := '';

	if filename.EndsWith('.json') then
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
	{ Do this before importing! }
	FCanvasWidth := canvas.Width;
	FCanvasHeight := canvas.Height;

	if length(FMetaFilename) > 0 then
		Import(FMetaFilename)
	else
		FMetaFilename := GetDataDirectory(ddtMap, ChangeFileExt(ExtractFileName(FImageFilename), '.json'));

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
procedure TMap.AddConnection(const marker1, marker2: TMapMarker);
begin
	marker1.ConnectWith(marker2.LoreId);
	marker2.ConnectWith(marker1.LoreId);
end;

{}
procedure TMap.DeleteConnection(const marker1, marker2: TMapMarker);
begin
	marker1.DisconnectWith(marker2.LoreId);
	marker2.DisconnectWith(marker1.LoreId);
end;

{}
procedure TMap.DeleteConnections(const marker: TMapMarker);
var
	loopMarker: TMapMarker;
begin
	for loopMarker in FMarkers do
		loopMarker.DisconnectWith(marker.LoreId);

	marker.ConnectedTo.Clear;
end;

{}
function TMap.Export(): String;
var
	jsonResult: TJSONObject;
	streamer: TGameStreamer;
begin
	streamer := TGameStreamer.Create();
	jsonResult := streamer.Streamer.ObjectToJSON(self);
	jsonResult.Strings['ImageName'] := ExtractFileName(FImageFilename);
	result := jsonResult.FormatJSON([foUseTabchar], 1);

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

	FImageFilename := 'assets/maps/' + FImageFilename;
end;

{}
procedure TMap.Draw(canvas: TCanvas);
var
	points: Array[0 .. 1] of TPoint;

	ind1, ind2: Integer;
	pass: Integer;
begin
	canvas.Pen.Color := clBlue;
	canvas.Pen.Width := 2;
	canvas.Pen.Style := psSolid;

	for ind1 := 0 to FMarkers.Count - 1 do begin
		for ind2 := ind1 + 1 to FMarkers.Count - 1 do begin
			if FMarkers[ind1].ConnectedTo.Find(FMarkers[ind2].LoreId, pass) then begin
				points[0].X := round(FMarkers[ind1].PosX * CanvasWidth);
				points[0].Y := round(FMarkers[ind1].PosY * CanvasHeight);
				points[1].X := round(FMarkers[ind2].PosX * CanvasWidth);
				points[1].Y := round(FMarkers[ind2].PosY * CanvasHeight);
				canvas.Polyline(points, 0, 2);
			end;
		end;
	end;
end;

initialization
	ListSerializationMap.Add(TSerializedList.Create(TMarkers, TMapMarker));

end.

