unit GameTypes;

interface

uses FGL, SysUtils,
	CastleScene;

type
	TLoreId = String[32];
	TUlid = String[26];

	TLoreIds = specialize TFPGList<TLoreId>;

	TGameModel = class(TCastleScene);

	ELore = class(Exception);

implementation

end.

