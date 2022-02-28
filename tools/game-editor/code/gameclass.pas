unit gameclass;

{$mode objfpc}{$H+}

interface

uses translationdialog, loreiddialog, editorcommon, serialization;

type
	TClass = class
	private
		FLoreId: TLoreId;
		FTranslations: TTranslations;

	published

		property LoreId: TLoreId read FLoreId write FLoreId;
		property Translations: TTranslations read FTranslations write FTranslations;
	end;

implementation

initialization
	// ListSerializationMap.Add(TSerializedList.Create(TMarkers, TMapMarker));

end.

