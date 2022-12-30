unit GameTypes;

interface

uses FGL,
	CastleScene;

type
	TLoreId = String[32];
	TUlid = String[26];

	TLoreIds = specialize TFPGList<TLoreId>;

	TGameModel = class(TCastleScene);

implementation

end.

