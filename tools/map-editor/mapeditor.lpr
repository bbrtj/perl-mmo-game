program mapeditor;

{$mode objfpc}{$H+}

uses
	{$IFDEF UNIX}{$IFDEF UseCThreads}
	cthreads,
	{$ENDIF}{$ENDIF}
	Interfaces, // this includes the LCL widgetset
	Forms, editor, markerdata
	{ you can add units after this };

{$R *.res}

begin
	RequireDerivedFormResource:=True;
	Application.Scaled:=True;
	Application.Initialize;
	Application.CreateForm(TEditorForm, EditorForm);
	Application.Run;
end.

