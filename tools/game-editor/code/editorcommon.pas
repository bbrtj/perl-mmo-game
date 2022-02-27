unit editorcommon;

interface

uses
	SysUtils, FGL;

type

	TLoggerProcedure = procedure (const aText: String) of object;
	TMapChangedProcedure = procedure () of object;

	TLanguageId = String[2];
	TLoreId = String[20];
	TLoreName = String[255];

	TDataDirectoryType = (ddtNone, ddtMap);

function GetDataDirectory(const vType: TDataDirectoryType; const vPath: String): String;
function GetAssetDirectory(const vType: TDataDirectoryType; const vPath: String): String;

implementation

{}
function GetDataDirectory(const vType: TDataDirectoryType; const vPath: String): String;
begin
	result := 'game-data/';

	case vType of
		ddtMap: result += 'maps/';
	end;

	result += vPath;
end;

{}
function GetAssetDirectory(const vType: TDataDirectoryType; const vPath: String): String;
begin
	result := 'assets/';

	case vType of
		ddtMap: result += 'maps/';
	end;

	result += vPath;
end;

end.

