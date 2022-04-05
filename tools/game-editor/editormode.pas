unit editormode;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
	StdCtrls, ComCtrls, Menus, ActnList, FPJSON,
	mapeditor, editorcommon;

type

	{ TEditorModeForm }
	TEditorModeForm = class(TForm)
		ExitAction: TAction;
		ActionList1: TActionList;
		EditorMenu: TMainMenu;
		MapsList: TListBox;
		MenuItem1: TMenuItem;
		ExitButton: TMenuItem;
		PageControl1: TPageControl;
		MapsTab: TTabSheet;
		procedure ExitActionExecute(Sender: TObject);
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure FormCreate(Sender: TObject);
		procedure MapsListDblClick(Sender: TObject);

		procedure UpdateMapList();
	private

	public

	end;

var
	EditorModeForm: TEditorModeForm;

implementation

{$R *.lfm}

{ TEditorModeForm }

{}
procedure TEditorModeForm.FormCreate(Sender: TObject);
begin
	UpdateMapList();
end;

procedure TEditorModeForm.MapsListDblClick(Sender: TObject);
var
	vItem: String;
	mapEd: TMapEditorForm;
begin
	if MapsList.ItemIndex >= 0 then begin
		vItem := MapsList.Items[MapsList.ItemIndex];

		Visible := False;
		mapEd := TMapEditorForm.Create(self);
		mapEd.OnClose := @FormClose;
		mapEd.Show;

		mapEd.LoadMap(vItem);
	end;
end;

{}
procedure TEditorModeForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
	CloseAction := caFree;

	if Sender is TMapEditorForm then
		UpdateMapList();

	Visible := True;
end;

{}
procedure TEditorModeForm.ExitActionExecute(Sender: TObject);
begin
	 Close;
end;

{}
procedure TEditorModeForm.UpdateMapList();
var
	vFile: String;
	vSearchDir: String;

	vResult: Integer;
	vMapInfo: TSearchRec;

begin
	MapsList.Clear;
	vSearchDir := GetDataDirectory(ddtMap, '*.gd');

	vResult := findFirst(vSearchDir, faAnyFile, vMapInfo);
	while vResult = 0 do begin
		vFile := GetDataDirectory(ddtMap, vMapInfo.Name);
		MapsList.Items.Add(vFile);

		vResult := findNext(vMapInfo);
	end;

	findClose(vMapInfo);
end;

{ implementation end }

end.

