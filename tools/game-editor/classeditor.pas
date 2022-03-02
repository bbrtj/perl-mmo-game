unit classeditor;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
	GameClass, translationdialog, loreiddialog, editorcommon;

type

	{ TClassEditor }

	TClassEditor = class(TForm)
		IDAction: TAction;
		TranslationsAction: TAction;
		MenuItem2: TMenuItem;
		IdMenuItem: TMenuItem;
		TranslationsMenuItem: TMenuItem;
		SaveAction: TAction;
		QuitAction: TAction;
		ActionList1: TActionList;
		ClassEditorMenu: TMainMenu;
		MenuItem1: TMenuItem;
		SaveMenuItem: TMenuItem;
		QuitMenuItem: TMenuItem;
		procedure IDActionExecute(Sender: TObject);
		procedure QuitActionExecute(Sender: TObject);
		procedure SaveActionExecute(Sender: TObject);
		procedure TranslationsActionExecute(Sender: TObject);
	private

		FClass: TClass;

	public
		constructor Create(vOwner: TComponent); override;
		destructor Destroy(); override;
		procedure LoadClass(vClass: String);

	end;

implementation

{$R *.lfm}

{ TClassEditor }

{}
constructor TClassEditor.Create(vOwner: TComponent);
begin
	inherited;
	FClass := TClass.Create;
end;

{}
destructor TClassEditor.Destroy();
begin
	FClass.Free;
	inherited;
end;

{}
procedure TClassEditor.SaveActionExecute(Sender: TObject);
var
	vExported: TStrings;
begin
	vExported := TStringList.Create;
	vExported.Text := FClass.Export();
	vExported.SaveToFile(GetDataDirectory(ddtClass, FClass.LoreId + '.json'));

	vExported.Free;
end;

{}
procedure TClassEditor.TranslationsActionExecute(Sender: TObject);
var
	dialog: TTranslationDialog;
begin
	dialog := TTranslationDialog.Create(nil);

	dialog.Translations := FClass.Translations;
	dialog.ShowModal();
	dialog.Free;
end;

{}
procedure TClassEditor.QuitActionExecute(Sender: TObject);
begin
	Close;
end;

{}
procedure TClassEditor.IDActionExecute(Sender: TObject);
var
	dialog: TLoreIdDialog;
begin
	dialog := TLoreIdDialog.Create(nil);
	dialog.LoreIdValue := FClass.LoreId;
	dialog.ShowModal();

	if dialog.Saved then
		FClass.LoreId := dialog.LoreIdValue;
	dialog.Free;
end;

{}
procedure TClassEditor.LoadClass(vClass: String);
var
	vContent: TStrings;
begin
	vContent := TStringList.Create;
	vContent.LoadFromFile(vClass);

	FClass.Import(vContent.Text);
	vContent.Free;
end;

{ implementation end }

end.

