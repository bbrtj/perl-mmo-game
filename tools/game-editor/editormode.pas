unit editormode;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
	StdCtrls, ComCtrls, Menus, ActnList, FPJSON,
	mapeditor, classeditor, editorcommon;

type

	{ TEditorModeForm }
	TEditorModeForm = class(TForm)
		ExitAction: TAction;
		ActionList1: TActionList;
		EditorMenu: TMainMenu;
		MapsList: TListBox;
		ClassesList: TListBox;
		MenuItem1: TMenuItem;
		ExitButton: TMenuItem;
		PageControl1: TPageControl;
		MapsTab: TTabSheet;
		ClassesTab: TTabSheet;
		procedure ClassesListDblClick(Sender: TObject);
		procedure ExitActionExecute(Sender: TObject);
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure FormCreate(Sender: TObject);
		procedure MapsListDblClick(Sender: TObject);

		procedure UpdateMapList();
		procedure UpdateClassList();
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
	UpdateClassList();
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

	if Sender is TClassEditor then
		UpdateClassList();

	Visible := True;
end;

{}
procedure TEditorModeForm.ExitActionExecute(Sender: TObject);
begin
	 Close;
end;

procedure TEditorModeForm.ClassesListDblClick(Sender: TObject);
var
	vItem: String;
	classEd: TClassEditor;
begin
	if ClassesList.ItemIndex >= 0 then begin
		vItem := ClassesList.Items[ClassesList.ItemIndex];

		Visible := False;
		classEd := TClassEditor.Create(self);
		classEd.OnClose := @FormClose;
		classEd.Show;

		classEd.LoadClass(vItem);
	end;
end;

{}
procedure TEditorModeForm.UpdateMapList();
var
	vMapImages: TStrings;
	vFile: String;
	vSearchDir: String;

	vResult: Integer;
	vMapInfo: TSearchRec;

	function GetMapImageFromJson(const vFile: String): String;
	var
		vJsonContents: TStrings;
		vJsonObject: TJSONObject;

	begin
		vJsonContents := TStringList.Create;
		vJsonContents.LoadFromFile(vFile);

		vJsonObject := GetJSON(vJsonContents.Text) as TJSONObject;
		result := vJsonObject.Get('ImageName', '');

		vJsonObject.Free;
		vJsonContents.Free;
	end;

	procedure FillMapImages();
	begin
		vSearchDir := GetAssetDirectory(ddtMap, '*.png');

		vResult := findFirst(vSearchDir, faAnyFile, vMapInfo);
		while vResult = 0 do begin
			vMapImages.Add(vMapInfo.Name);

			vResult := findNext(vMapInfo);
		end;

		findClose(vMapInfo);
	end;

	procedure HideMapImage(const vName: String);
	var
		vIndex: Integer;
	begin
		vIndex := vMapImages.IndexOf(vName);
		if vIndex >= 0 then
			vMapImages.Delete(vIndex);
	end;

begin
	vMapImages := TStringList.Create;
	FillMapImages();

	MapsList.Clear;
	vSearchDir := GetDataDirectory(ddtMap, '*.json');

	vResult := findFirst(vSearchDir, faAnyFile, vMapInfo);
	while vResult = 0 do begin
		vFile := GetDataDirectory(ddtMap, vMapInfo.Name);
		HideMapImage(GetMapImageFromJson(vFile));
		MapsList.Items.Add(vFile);

		vResult := findNext(vMapInfo);
	end;

	findClose(vMapInfo);

	for vFile in vMapImages do begin
		MapsList.Items.Add(GetAssetDirectory(ddtMap, vFile));
	end;
	vMapImages.Free;
end;

{}
procedure TEditorModeForm.UpdateClassList();
var
	vFile: String;
	vSearchDir: String;

	vResult: Integer;
	vClassInfo: TSearchRec;
begin
	ClassesList.Clear;
	vSearchDir := GetDataDirectory(ddtClass, '*.json');

	vResult := findFirst(vSearchDir, faAnyFile, vClassInfo);
	while vResult = 0 do begin
		vFile := GetDataDirectory(ddtMap, vClassInfo.Name);
		ClassesList.Items.Add(vFile);

		vResult := findNext(vClassInfo);
	end;

	findClose(vClassInfo);

	ClassesList.Items.Add('+new');
end;

{ implementation end }

end.

