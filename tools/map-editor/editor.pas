unit editor;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
	Menus, ActnList, ComCtrls, FPJSON,
	map, markerdata;

type

	{ TEditorForm }

	TEditorForm = class(TForm)
		EditorMenu: TMainMenu;
		MenuItem1: TMenuItem;
		MenuItem2: TMenuItem;
		MenuItem3: TMenuItem;
		MenuItem4: TMenuItem;
		MenuItem5: TMenuItem;
		MenuItem6: TMenuItem;
		MenuItem7: TMenuItem;
		MenuItem8: TMenuItem;
		MenuItem9: TMenuItem;

		StateInfo: TLabel;
		ActionInfo: TLabel;

		MapView: TImage;
		Marker: TShape;

		procedure FormCreate(Sender: TObject);
  procedure MenuLoadClick(Sender: TObject);
		procedure MenuSaveClick(Sender: TObject);
		procedure MenuPropertiesClick(Sender: TObject);
		procedure MenuQuitClick(Sender: TObject);
		procedure MenuEditPathsClick(Sender: TObject);
		procedure MenuDeleteClick(Sender: TObject);

		procedure MapViewClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure MapViewPaint(Sender: TObject);

		procedure ClearMap();
	private
		map: TMap;

		procedure UpdateStateInfo();

	public
		procedure UpdateInfo(const actionText: String);
		procedure MapChanged();

	end;

var
	EditorForm: TEditorForm;

implementation

{$R *.lfm}

{ TEditorForm }

{}
procedure TEditorForm.MapViewClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if ssDouble in Shift then begin
		if map <> Nil then
			map.AddMarker(X, Y)
		else begin
			ShowMessage('Please load a map first');
		end;
	end;
end;

{}
procedure TEditorForm.MapViewPaint(Sender: TObject);
begin
	if map <> Nil then begin
		map.Draw(MapView.Canvas);
	end;
end;

{}
procedure TEditorForm.MenuLoadClick(Sender: TObject);
var
	dialog: TOpenDialog;
	image: TPicture;
begin
	dialog := TOpenDialog.Create(self);

	dialog.Options := [ofFileMustExist];
	dialog.Filter := 'Metadata files|*.map.json|Map images|*.png';

	if dialog.Execute then begin
		ClearMap;
		map := TMap.Create(dialog.FileName);
		map.MarkerBlueprint := Marker;
		map.Logger := @self.UpdateInfo;
		map.OnChange := @self.MapChanged;
		map.Initialize(MapView);
	end;

	dialog.Free;
end;

procedure TEditorForm.FormCreate(Sender: TObject);
begin

end;

procedure TEditorForm.MenuQuitClick(Sender: TObject);
begin
	Close;
end;

{}
procedure TEditorForm.MenuSaveClick(Sender: TObject);
var
	exported: TJSONObject;
	exportedString: String;
	outFile: TFileStream;
begin
	if map <> Nil then begin
		exported := map.Export(MapView);
		exportedString := exported.FormatJSON([foUseTabchar], 1);

		outFile := TFileStream.Create(map.MetaFileName, fmCreate);
		outFile.WriteBuffer(exportedString[1], length(exportedString));

		outFile.Free;
		exported.Free;

		UpdateInfo('Save successful (' + map.MetaFileName + ')');
	end
	else begin
		ShowMessage('Please load a map first');
	end;
end;

{}
procedure TEditorForm.MenuPropertiesClick(Sender: TObject);
var
	dialog: TMarkerForm;
begin
	if map <> Nil then begin
		dialog := TMarkerForm.Create(nil);

		dialog.LoreIdValue := map.MapData.LoreId;
		dialog.LoreNameValue := map.MapData.LoreName;
		dialog.LoreDescriptionValue := map.MapData.LoreDescription;

		dialog.ShowModal();

		if dialog.MarkerAdded then
			map.MapData.Initialize(dialog.LoreIdValue, dialog.LoreNameValue, dialog.LoreDescriptionValue)
		else
			UpdateInfo('Aborted');

		dialog.Free;
	end
	else begin
		ShowMessage('Please load a map first');
	end;
end;

{}
procedure TEditorForm.MenuEditPathsClick(Sender: TObject);
begin
	if map <> nil then begin
		map.SetEdited(nil);
		map.Connecting := not map.Connecting;
		UpdateStateInfo;
	end
	else begin
		ShowMessage('Please load a map first');
	end;
end;

{}
procedure TEditorForm.MenuDeleteClick(Sender: TObject);
begin
	if map <> nil then begin
		map.SetEdited(nil);
		map.Deleting := not map.Deleting;
		UpdateStateInfo;
	end
	else begin
		ShowMessage('Please load a map first');
	end;
end;

{}
procedure TEditorForm.UpdateInfo(const actionText: String);
begin
	ActionInfo.Caption := actionText;
end;

{}
procedure TEditorForm.UpdateStateInfo();
var
	info: String;
begin
	if (map.Connecting) then
		info := 'Connecting'
	else
		info := 'Normal';

	if (map.Deleting) then
		info += ' (Deleting)';

	StateInfo.Caption := info;
end;

{}
procedure TEditorForm.MapChanged();
begin
	MapView.Invalidate;
end;

{}
procedure TEditorForm.ClearMap();
begin
	if map <> Nil then begin
		FreeAndNil(map);
		MapView.Picture := Nil;
	end;
end;

end.

