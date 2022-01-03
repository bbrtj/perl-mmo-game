unit editortypes;

interface

uses
	FGL;

type

	TLoggerProcedure = procedure (const aText: String) of object;
	TMapChangedProcedure = procedure () of object;
	TIndexList = specialize TFPGList<Integer>;


implementation

end.
