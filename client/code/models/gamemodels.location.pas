unit GameModels.Location;

interface

uses GameModels, GameTypes;

type

	TMsgFeedLocationData = class(TModelBase)
	private
		FId: TLoreId;
		FPlayerX: Single;
		FPlayerY: Single;

	public
		class function MessageType(): String; override;

	published
		property id: TLoreId read FId write FId;
		property player_x: Single read FPlayerX write FPlayerX;
		property player_y: Single read FPlayerY write FPlayerY;
	end;

implementation

class function TMsgFeedLocationData.MessageType(): String;
begin
	result := 'location_data';
end;

{ implementation end }

end.

