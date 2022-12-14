unit GameModels.Location;

interface

uses GameModels, GameTypes;

type

	TMsgFeedLocationData = class(TModelBase)
	private
		FId: TLoreId;
		FName: String;

	public
		class function MessageType(): String; override;

	published
		property id: TLoreId read FId write FId;
		property name: String read FName write FName;
	end;

implementation

{}
class function TMsgFeedLocationData.MessageType(): String;
begin
	result := 'location_data';
end;

{ implementation end }

end.

