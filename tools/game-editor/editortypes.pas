unit editortypes;

interface

uses
	FGL;

type

	TLoggerProcedure = procedure (const aText: String) of object;
	TMapChangedProcedure = procedure () of object;

	TLanguageId = String[2];
	TLoreId = String[20];
	TLoreName = String[255];

implementation

end.
