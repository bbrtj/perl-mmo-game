unit GameModels.Discovery;

interface

uses FGL, SysUtils, Classes,
	GameModels, GameModels.Move,
	GameTypes, Serialization;

type

	TMsgFeedNewObject = class(TMsgFeedActorPosition);

	TMsgFeedNewObjects = specialize TFPGObjectList<TMsgFeedNewObject>;

	TMsgFeedDiscovery = class(TModelBase)
	private
		FNewActors: TMsgFeedNewObjects;
		FOldActors: TStringList;

	public
		class function MessageType(): String; override;

		constructor Create(); override;
		destructor Destroy; override;

	published
		property new_actors: TMsgFeedNewObjects read FNewActors write FNewActors;
		property old_actors: TStringList read FOldActors write FOldActors;
	end;

implementation

class function TMsgFeedDiscovery.MessageType(): String;
begin
	result := 'discovery';
end;

constructor TMsgFeedDiscovery.Create();
begin
	FNewActors := TMsgFeedNewObjects.Create;
	FOldActors := TStringList.Create;
end;

destructor TMsgFeedDiscovery.Destroy;
begin
	FNewActors.Free;
	FOldActors.Free;
end;

{ implementation end }

initialization
	ListSerializationMap.Add(TSerializedList.Create(TMsgFeedNewObjects, TMsgFeedNewObject));

end.

