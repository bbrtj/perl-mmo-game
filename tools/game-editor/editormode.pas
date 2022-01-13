unit editormode;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
	StdCtrls,
	mapeditor;

type

	{ TEditorModeForm }

 TEditorModeForm = class(TForm)
		ClassEditorButton: TButton;
		MapEditorButton: TButton;
		ExitButton: TButton;
		procedure FormCreate(Sender: TObject);

		procedure ClassEditorButtonClick(Sender: TObject);
		procedure MapEditorButtonClick(Sender: TObject);
		procedure ExitButtonClick(Sender: TObject);
	private

	public

	end;

var
	EditorModeForm: TEditorModeForm;

implementation

{$R *.lfm}

{ TEditorModeForm }

procedure TEditorModeForm.FormCreate(Sender: TObject);
begin

end;

procedure TEditorModeForm.MapEditorButtonClick(Sender: TObject);
var
	mapEd: TMapEditorForm;
begin
	Visible := False;
	mapEd := TMapEditorForm.Create(self);
	mapEd.ShowModal;
	mapEd.Free;
	Visible := True;
end;

procedure TEditorModeForm.ClassEditorButtonClick(Sender: TObject);
begin

end;

procedure TEditorModeForm.ExitButtonClick(Sender: TObject);
begin
	Close;
end;

end.

