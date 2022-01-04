unit editortypes;

interface

uses
	FGL;

type

	TLoggerProcedure = procedure (const aText: String) of object;
	TMapChangedProcedure = procedure () of object;
	TIndexList = specialize TFPGList<Integer>;

	TLoreId = String[20];
	TLoreName = String[255];


implementation

end.
