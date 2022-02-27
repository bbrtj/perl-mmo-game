program editor;

{$mode objfpc}{$H+}

uses
	{$IFDEF UNIX}{$IFDEF UseCThreads}
	cthreads,
	{$ENDIF}{$ENDIF}
	Interfaces, // this includes the LCL widgetset
	Forms, mapeditor, editormode, classeditor
	{ you can add units after this };

{$R *.res}

begin
	RequireDerivedFormResource:=True;
	Application.Title:='Editor';
	Application.Scaled:=True;
	Application.Initialize;
	Application.CreateForm(TEditorModeForm, EditorModeForm);
	Application.Run;
end.

