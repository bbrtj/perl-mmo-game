unit mapeditor;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
	Menus, ActnList, ComCtrls, Buttons, FPJSON, Process,
	GameMap, editorcommon;

type

	{ TMapEditorForm }

	TMapEditorForm = class(TForm)
		EditorMenu: TMainMenu;
		MenuItem1: TMenuItem;
		SaveMenuItem: TMenuItem;
		MenuItem5: TMenuItem;
		QuitMenuItem: TMenuItem;
		Panel1: TPanel;

		ActionInfo: TLabel;

		MapView: TImage;
		Marker: TShape;

		procedure FormCreate(Sender: TObject);
		procedure MenuSaveClick(Sender: TObject);
		procedure MenuQuitClick(Sender: TObject);

		procedure MapViewClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure MapViewPaint(Sender: TObject);

		procedure ClearMap();
	private
		map: TMap;

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
	vExported: TStrings;
begin
	if map <> Nil then begin
		vExported := TStringList.Create;
		vExported.Text := map.Export();
		vExported.SaveToFile(map.MetaFileName);

		vExported.Free;

		UpdateInfo('Save successful (' + map.MetaFileName + ')');
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
	function FetchFileContent(const vFileName: String): String;
	begin
		RunCommand('tools/map-exporter ' + vFileName, result);
	end;

begin
	ClearMap;
	map := TMap.Create;
	map.Logger := @self.UpdateInfo;
	map.OnChange := @self.MapChanged;

	map.LoreFileName := vMapFile;
	map.Initialize(MapView, Marker, FetchFileContent(vMapFile))
end;

{ implementation end }

end.

