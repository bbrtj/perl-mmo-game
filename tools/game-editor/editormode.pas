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

{ implementation end }

end.

