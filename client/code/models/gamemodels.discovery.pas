unit GameModels.Discovery;

interface

uses SysUtils, Classes,
	GameModels, GameTypes;

type

	TMsgFeedDiscovery = class(TModelBase)
	private
		FNewActors: TStringList;
		FOldActors: TStringList;

	public
		class function MessageType(): String; override;

		constructor Create(); override;
		destructor Destroy; override;

	published
		property new_actors: TStringList read FNewActors write FNewActors;
		property old_actors: TStringList read FOldActors write FOldActors;
	end;

implementation

class function TMsgFeedDiscovery.MessageType(): String;
begin
	result := 'discovery';
end;

constructor TMsgFeedDiscovery.Create();
begin
	FNewActors := TStringList.Create;
	FOldActors := TStringList.Create;
end;

destructor TMsgFeedDiscovery.Destroy;
begin
	FNewActors.Free;
	FOldActors.Free;
end;

{ implementation end }

end.

