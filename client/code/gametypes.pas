unit GameTypes;

interface

uses FGL, SysUtils,
	CastleScene;

type
	TLoreId = String[32];
	TUlid = String[26];

	TLoreIds = specialize TFPGList<TLoreId>;

	// TGameModel = class(TCastleScene);
	TGameModel = class(TCastleImageTransform);

	EGameException = class(Exception)
	public
		constructor Create(const Txt: String = '<no exception message>');
	end;

	EUnknownMessage = class(EGameException);

implementation

constructor EGameException.Create(const Txt: String);
begin
	inherited Create(Txt);
end;

end.

