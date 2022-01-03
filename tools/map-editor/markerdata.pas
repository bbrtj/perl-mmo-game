unit markerdata;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, FPJSON;

type
	{ TMarkerForm }

	TMarkerForm = class(TForm)
		AddButton: TButton;
		CancelButton: TButton;
		Label3: TLabel;
		LoreName: TEdit;
		Label2: TLabel;
		LoreId: TEdit;
		Label1: TLabel;
		LoreDescription: TMemo;
		procedure AddButtonClick(Sender: TObject);
		procedure CancelButtonClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
	private

	public
		MarkerAdded: Boolean;

		function GetId(): String;
		procedure SetId(value: String);

		function GetName(): String;
		procedure SetName(value: String);

		function GetDescription(): String;
		procedure SetDescription(value: String);

		property LoreIdValue: String read GetId write SetId;
		property LoreNameValue: String read GetName write SetName;
		property LoreDescriptionValue: String read GetDescription write SetDescription;
	end;

implementation

{$R *.lfm}

{ TMarkerForm }

{}
procedure TMarkerForm.FormCreate(Sender: TObject);
begin
end;

{}
procedure TMarkerForm.AddButtonClick(Sender: TObject);
begin
	MarkerAdded := true;
	Close;
end;

{}
procedure TMarkerForm.CancelButtonClick(Sender: TObject);
begin
	MarkerAdded := false;
	Close;
end;

{}
function TMarkerForm.GetId(): String;
begin
	result := LoreId.Text;
end;

{}
procedure TMarkerForm.SetId(value: String);
begin
	LoreId.Text := value;
end;

{}
function TMarkerForm.GetName(): String;
begin
	result := LoreName.Text;
end;

{}
procedure TMarkerForm.SetName(value: String);
begin
	LoreName.Text := value;
end;

{}
function TMarkerForm.GetDescription(): String;
begin
	result := LoreDescription.Text;
end;

{}
procedure TMarkerForm.SetDescription(value: String);
begin
	LoreDescription.Text := value;
end;

end.

