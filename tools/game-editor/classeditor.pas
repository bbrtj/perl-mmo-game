unit classeditor;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
	GameClass;

type

	{ TClassEditor }

	TClassEditor = class(TForm)
		SaveAction: TAction;
		QuitAction: TAction;
		ActionList1: TActionList;
		ClassEditorMenu: TMainMenu;
		MenuItem1: TMenuItem;
		SaveMenuItem: TMenuItem;
		QuitMenuItem: TMenuItem;
		procedure QuitActionExecute(Sender: TObject);
		procedure SaveActionExecute(Sender: TObject);
	private

	public

	end;

implementation

{$R *.lfm}

{ TClassEditor }


procedure TClassEditor.SaveActionExecute(Sender: TObject);
begin

end;

procedure TClassEditor.QuitActionExecute(Sender: TObject);
begin
	Close;
end;

end.

