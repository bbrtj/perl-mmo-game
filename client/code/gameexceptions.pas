unit GameExceptions;

interface

uses SysUtils;

type
	EGameObjectNotFound = class(Exception);
	EActorNotFound = class(EGameObjectNotFound);

implementation

end.

