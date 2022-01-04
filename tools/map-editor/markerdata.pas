unit markerdata;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, FPJSON,
	editortypes;

type

	TMarkerData = class

	private
		FLoreId: TLoreId;
		FLoreName: TLoreName;
		FLoreDescription: String;

	public
		procedure Initialize(const vLoreId, vLoreName, vLoreDescription: String);

		function Export(): TJSONObject; virtual;
		procedure Import(const json: TJSONObject); virtual;

		property LoreId: TLoreId read FLoreId write FLoreId;
		property LoreName: TLoreName read FLoreName write FLoreName;
		property LoreDescription: String read FLoreDescription write FLoreDescription;

	end;

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

{ TMarkerData }

{}
function TMarkerData.Export(): TJSONObject;
begin
	result := TJSONObject.Create([
		'lore_id', LoreId,
		'lore_name', LoreName,
		'lore_description', LoreDescription
	]);
end;

{}
procedure TMarkerData.Import(const json: TJSONObject);
begin
	Initialize(
		json.Get('lore_id', ''),
		json.Get('lore_name', ''),
		json.Get('lore_description', '')
	);
end;

{}
procedure TMarkerData.Initialize(const vLoreId, vLoreName, vLoreDescription: String);
begin
	LoreId := vLoreId;
	LoreName := vLoreName;
	LoreDescription := vLoreDescription;
end;

{ TMarkerForm }

{}
procedure TMarkerForm.FormCreate(Sender: TObject);
begin
end;

{}
procedure TMarkerForm.AddButtonClick(Sender: TObject);
begin
	if length(LoreIdValue) = 0 then
		Label1.Caption := 'Fill!'
	else begin
		MarkerAdded := true;
		Close;
	end;
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

