unit mapeditor;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
	Menus, ActnList, ComCtrls, Buttons, FPJSON,
	GameMap;

type

	{ TMapEditorForm }

	TMapEditorForm = class(TForm)
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
		Panel1: TPanel;

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

		procedure LoadMap(const vMapFile: String);

	end;

implementation

{$R *.lfm}

{ TMapEditorForm }

{}
procedure TMapEditorForm.MapViewClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if ssDouble in Shift then begin
		if map <> Nil then
			map.AddMarker(X + MapView.Left, Y + MapView.Top)
		else begin
			ShowMessage('Please load a map first');
		end;
	end;
end;

{}
procedure TMapEditorForm.MapViewPaint(Sender: TObject);
begin
	if map <> Nil then begin
		map.Draw(MapView.Canvas);
	end;
end;

{}
procedure TMapEditorForm.MenuLoadClick(Sender: TObject);
var
	dialog: TOpenDialog;
	image: TPicture;
begin
	dialog := TOpenDialog.Create(self);

	dialog.Options := [ofFileMustExist];
	dialog.Filter := 'Metadata files|*.json|Map images|*.png';

	if dialog.Execute then
		LoadMap(dialog.FileName);

	dialog.Free;
end;

procedure TMapEditorForm.FormCreate(Sender: TObject);
begin

end;

procedure TMapEditorForm.MenuQuitClick(Sender: TObject);
begin
	Close;
end;

{}
procedure TMapEditorForm.MenuSaveClick(Sender: TObject);
var
	// exported: TJSONObject;
	exportedString: String;
	outFile: TFileStream;
begin
	if map <> Nil then begin
		exportedString := map.Export();
		// exportedString := exported.FormatJSON([foUseTabchar], 1);

		outFile := TFileStream.Create(map.MetaFileName, fmCreate);
		outFile.WriteBuffer(exportedString[1], length(exportedString));

		outFile.Free;
		// exported.Free;

		UpdateInfo('Save successful (' + map.MetaFileName + ')');
	end
	else begin
		ShowMessage('Please load a map first');
	end;
end;

{}
procedure TMapEditorForm.MenuPropertiesClick(Sender: TObject);
begin
	if map <> Nil then begin
		map.UpdateTranslations();
	end
	else begin
		ShowMessage('Please load a map first');
	end;
end;

{}
procedure TMapEditorForm.MenuEditPathsClick(Sender: TObject);
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
procedure TMapEditorForm.MenuDeleteClick(Sender: TObject);
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
procedure TMapEditorForm.UpdateInfo(const actionText: String);
begin
	ActionInfo.Caption := actionText;
end;

{}
procedure TMapEditorForm.UpdateStateInfo();
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
procedure TMapEditorForm.MapChanged();
begin
	MapView.Invalidate;
end;

{}
procedure TMapEditorForm.ClearMap();
begin
	if map <> Nil then begin
		FreeAndNil(map);
		MapView.Picture := Nil;
	end;
end;

{}
procedure TMapEditorForm.LoadMap(const vMapFile: String);
begin
	ClearMap;
	map := TMap.Create(vMapFile);
	map.MarkerBlueprint := Marker;
	map.Logger := @self.UpdateInfo;
	map.OnChange := @self.MapChanged;
	map.Initialize(MapView);
end;

{ implementation end }

end.

