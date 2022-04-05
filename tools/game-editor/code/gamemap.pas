unit gamemap;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, ExtCtrls, Graphics, FGL, fpjsonrtti, FPJSON,
	loreiddialog, editorcommon, serialization;

type

	TMap = class;

	TMapMarker = class(TSerialized)
	private
		FLoreId: TLoreId;
		FPosX: Real;
		FPosY: Real;
		FMarker: TShape;
		FMap: TMap;
		FConnectedTo: TStringList;

		procedure SetPosX(position: Real);
		procedure SetPosY(position: Real);
		procedure SetLoreId(const id: TLoreId);
	public
		constructor Create(); override;
		destructor Destroy; override;

		procedure SetMap(map: TMap);

		procedure MarkerClicked(Sender: TObject);

		procedure OnObjectStreamed(vJson: TJSONObject); override;

	published
		property LoreId: TLoreId read FLoreId write SetLoreId;
		property PosX: Real read FPosX write SetPosX;
		property PosY: Real read FPosY write SetPosY;
		property ConnectedTo: TStringlist read FConnectedTo write FConnectedTo;
	end;

	TMarkers = specialize TFPGObjectList<TMapMarker>;

	TMap = class(TSerialized)
	private
		FLoreFileName: String;
		FImage: TPicture;
		FMarkerBlueprint: TShape;
		FMarkers: TMarkers;

		FLoreId: TLoreId;

		FEdited: TMapMarker;

		FLogger: TLoggerProcedure;
		FOnChange: TMapChangedProcedure;

		FCanvasWidth: Integer;
		FCanvasHeight: Integer;

	public
		constructor Create;
		destructor Destroy; override;
		procedure Initialize(const canvas: TImage; const vMarker: TShape; const vContent: String = '');

		procedure AddMarker(const X, Y: Integer);

		procedure SetEdited(const value: TMapMarker);

		procedure Import(const vContent: String);
		function Export(): String;

		function MetaFileName(): String;
		function ImageName(): String;

		procedure Draw(canvas: TCanvas);

		property MarkerBlueprint: TShape read FMarkerBlueprint write FMarkerBlueprint;

		property Logger: TLoggerProcedure read FLogger write FLogger;
		property OnChange: TMapChangedProcedure read FOnChange write FOnChange;

		property CanvasWidth: Integer read FCanvasWidth;
		property CanvasHeight: Integer read FCanvasHeight;

		property LoreFileName: String read FLoreFileName write FLoreFileName;

	published
		property Markers: TMarkers read FMarkers write FMarkers;

	end;
implementation

{ TMapMarker }

{}
constructor TMapMarker.Create();
begin
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
	FMap.SetEdited(self);
end;

{}
procedure TMapMarker.OnObjectStreamed(vJson: TJSONObject);
begin
	vJson.Delete('ConnectedTo');
end;

{ TMap }

{}
constructor TMap.Create();
begin
	FMarkers := TMarkers.Create;
	FImage := TPicture.Create;
end;

{}
destructor TMap.Destroy;
begin
	{ Do not free marker blueprint! }
	FImage.Free;
	FMarkers.Free;
	inherited;
end;

{}
procedure TMap.Initialize(const canvas: TImage; const vMarker: TShape; const vContent: String = '');
begin
	{ Do this before importing! }
	FMarkerBlueprint := vMarker;
	FCanvasWidth := canvas.Width;
	FCanvasHeight := canvas.Height;

	if length(vContent) > 0 then
		Import(vContent);

	FImage.LoadFromFile(ImageName);
	canvas.Picture := FImage;
	Logger('Map initialized');
end;

{}
procedure TMap.AddMarker(const X, Y: Integer);
begin
	if FEdited <> nil then begin
		FEdited.PosX := X;
		FEdited.PosY := Y;
		Logger('Marker saved: ' + FEdited.LoreId);
		FEdited := nil;
		OnChange();
	end;
end;

{}
procedure TMap.SetEdited(const value: TMapMarker);
begin
	if value = nil then begin
		FEdited := nil;
		exit;
	end;

	if FEdited = value then begin
		FEdited := nil;
	end
	else begin
		FEdited := value;
		Logger('Updating a marker ' + value.LoreId);
	end;

end;

{}
function TMap.Export(): String;
var
	jsonResult: TJSONObject;
	streamer: TGameStreamer;
begin
	streamer := TGameStreamer.Create();
	jsonResult := streamer.Streamer.ObjectToJSON(self);
	result := jsonResult.FormatJSON([foUseTabchar], 1);

	streamer.Free;
end;

{}
procedure TMap.Import(const vContent: String);
var
	streamer: TGameStreamer;
	marker: TMapMarker;
begin
	streamer := TGameStreamer.Create();
	streamer.DeStreamer.JSONToObject(vContent, self);
	streamer.Free;

	for marker in FMarkers do
		marker.SetMap(self);
end;

{}
function TMap.MetaFileName(): String;
begin
	result := GetDataDirectory(ddtMap, ChangeFileExt(ExtractFileName(FLoreFileName), '.json'));
end;

{}
function TMap.ImageName(): String;
begin
	result := GetAssetDirectory(ddtMap, ChangeFileExt(ExtractFileName(FLoreFileName), '.png'));
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

{ implementation end }

initialization
	ListSerializationMap.Add(TSerializedList.Create(TMarkers, TMapMarker));

end.

