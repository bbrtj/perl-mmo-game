unit classeditor;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
	GameClass;

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

	public
		procedure LoadClass(vClass: String);

	end;

implementation

{$R *.lfm}

{ TClassEditor }


procedure TClassEditor.SaveActionExecute(Sender: TObject);
begin

end;

procedure TClassEditor.TranslationsActionExecute(Sender: TObject);
begin

end;

procedure TClassEditor.QuitActionExecute(Sender: TObject);
begin
	Close;
end;

procedure TClassEditor.IDActionExecute(Sender: TObject);
begin

end;

{}
procedure TClassEditor.LoadClass(vClass: String);
begin
end;

{ implementation end }

end.

