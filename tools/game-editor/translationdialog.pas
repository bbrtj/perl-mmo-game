unit translationdialog;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
	ComCtrls, FGL,
	editorcommon, serialization;

type

	TTranslationData = class(TSerialized)

	private
		FLanguageId: TLanguageId;
		FLoreName: TLoreName;
		FLoreDescription: String;

	published
		property LanguageId: TLanguageId read FLanguageId write FLanguageId;
		property LoreName: TLoreName read FLoreName write FLoreName;
		property LoreDescription: String read FLoreDescription write FLoreDescription;

	end;

	TTranslations = specialize TFPGObjectList<TTranslationData>;

	{ TTranslationDialog }

	TTranslationDialog = class(TForm)
		AddButton: TButton;
		CancelButton: TButton;
		Label2: TLabel;
		LanguagesList: TListBox;
		LoreDescription: TMemo;
		LoreName: TEdit;

		procedure AddButtonClick(Sender: TObject);
		procedure CancelButtonClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure LanguageChanged(Sender: TObject; User: boolean);

	const
		FLanguages: Array of TLanguageId = ('pl', 'en');

	private
		FTranslations: TTranslations;
		FRealTranslations: TTranslations;
		FCurrentLanguage: TLanguageId;

		procedure UpdateTranslation();
		procedure UpdateUI();
		procedure FinalizeTranslations();

	public
		constructor Create(aOwner: TComponent); override;
		destructor Destroy(); override;

		property Translations: TTranslations read FRealTranslations write FRealTranslations;

	end;

implementation

{$R *.lfm}

{ TTranslationDialog }

{}
constructor TTranslationDialog.Create(aOwner: TComponent);
begin
	FTranslations := TTranslations.Create;
	inherited;
end;

{}
destructor TTranslationDialog.Destroy();
begin
	FTranslations.Free;
	inherited;
end;

{}
procedure TTranslationDialog.FormCreate(Sender: TObject);
var
	language: TLanguageId;
begin
	FCurrentLanguage := FLanguages[0];

	for language in FLanguages do
		LanguagesList.AddItem(language, nil);
end;

{}
procedure TTranslationDialog.FormShow(Sender: TObject);
begin
	UpdateUI();
end;

{}
procedure TTranslationDialog.LanguageChanged(Sender: TObject; User: boolean);
begin
	if User then begin
		UpdateTranslation();
		FCurrentLanguage := LanguagesList.Items[LanguagesList.ItemIndex];
		UpdateUI();
	end;
end;

{}
procedure TTranslationDialog.AddButtonClick(Sender: TObject);
begin
	UpdateTranslation();
	FinalizeTranslations();
	Close;
end;

{}
procedure TTranslationDialog.CancelButtonClick(Sender: TObject);
begin
	Close;
end;

{}
procedure TTranslationDialog.UpdateUI();
var
	found: TTranslationData;
	obj: TTranslationData;
	list: Array [0 .. 1] of TTranslations;
	listIndex: Integer;
begin
	list[0] := FTranslations;
	list[1] := FRealTranslations;

	found := nil;
	for listIndex := 0 to 1 do begin
		for obj in list[listIndex] do begin
			if obj.LanguageId = FCurrentLanguage then begin
				found := obj;
				break;
			end;
		end;

		if found <> nil then break;
	end;

	if found = nil then
		found := TTranslationData.Create;

	LoreName.Text := found.LoreName;
	LoreDescription.Text := found.LoreDescription;

	for listIndex := low(FLanguages) to high(FLanguages) do begin
		if FLanguages[listIndex] = FCurrentLanguage then begin
			LanguagesList.ItemIndex := listIndex;
			break;
		end;
	end;
end;

{}
procedure TTranslationDialog.UpdateTranslation();
var
	found: TTranslationData;
	obj: TTranslationData;
begin
	found := nil;
	for obj in FTranslations do begin
		if obj.LanguageId = FCurrentLanguage then begin
			found := obj;
			break;
		end;
	end;

	if found = nil then begin
		found := TTranslationData.Create;
		FTranslations.Add(found);
	end;

	found.LanguageId := FCurrentLanguage;
	found.LoreName := LoreName.Text;
	found.LoreDescription := LoreDescription.Text;
end;

{}
procedure TTranslationDialog.FinalizeTranslations();
begin
	FRealTranslations.Clear;
	while FTranslations.Count > 0 do
		FRealTranslations.Add(FTranslations.Extract(FTranslations.First));
end;

initialization
	ListSerializationMap.Add(TSerializedList.Create(TTranslations, TTranslationData));

end.

