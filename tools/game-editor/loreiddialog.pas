unit loreiddialog;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
	ActnList, editorcommon;

type

	{ TLoreIdDialog }

	TLoreIdDialog = class(TForm)
		CancelAction: TAction;
		AddAction: TAction;
		ActionList1: TActionList;
		AddButton: TButton;
		CancelButton: TButton;
		LoreId: TLabeledEdit;
		procedure AddActionExecute(Sender: TObject);
		procedure CancelActionExecute(Sender: TObject);
	private
		function GetLoreId(): TLoreId;
		procedure SetLoreId(value: TLoreId);

	public
		Saved: Boolean;
		property LoreIdValue: TLoreId read GetLoreId write SetLoreId;

	end;

implementation

{$R *.lfm}

{ TLoreIdDialog }

procedure TLoreIdDialog.AddActionExecute(Sender: TObject);
begin
	Saved := true;

	Close;
end;

procedure TLoreIdDialog.CancelActionExecute(Sender: TObject);
begin
	Saved := false;

	Close;
end;

{}
function TLoreIdDialog.GetLoreId(): TLoreId;
begin
	result := LoreId.Text;
end;

{}
procedure TLoreIdDialog.SetLoreId(value: TLoreId);
begin
	LoreId.Text := value;
end;

end.

