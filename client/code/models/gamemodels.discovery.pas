unit GameModels.Discovery;

interface

uses FGL, SysUtils, Classes,
	GameModels, GameModels.Move,
	GameTypes, GameConfig, Serialization;

type

	TMsgActorsInfo = class(TPlaintextModel)
	public
		class function MessageType(): String; override;

		procedure AddActor(const Id: TUlid);

	end;

	TMsgResActor = class(TSerialized)
	private
		FId: TUlid;
		FName: String;
		FClass: TLoreId;

	published
		property id: TUlid read FId write FId;
		property name: String read FName write FName;
		property &class: TLoreId read FClass write FClass;

	end;

	TMsgResActors = specialize TFPGObjectList<TMsgResActor>;

	TMsgResActorsInfo = class(TModelBase)
	private
		FActors: TMsgResActors;

	public
		constructor Create(); override;
		destructor Destroy; override;

		class function MessageType(): String; override;

	published
		property list: TMsgResActors read FActors write FActors;
	end;

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

class function TMsgActorsInfo.MessageType(): String;
begin
	result := 'get_actors_info';
end;

procedure TMsgActorsInfo.AddActor(const Id: TUlid);
begin
	if self.value = '' then
		self.value := Id
	else
		self.value := self.value +  GlobalConfig.NetworkSeparatorCharacter + Id;
end;

constructor TMsgResActorsInfo.Create();
begin
	FActors := TMsgResActors.Create;
end;

destructor TMsgResActorsInfo.Destroy;
begin
	FActors.Free;
end;

class function TMsgResActorsInfo.MessageType(): String;
begin
	result := 'actors_info';
end;

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

initialization
	ListSerializationMap.Add(TSerializedList.Create(TMsgFeedNewObjects, TMsgFeedNewObject));
	ListSerializationMap.Add(TSerializedList.Create(TMsgResActors, TMsgResActor));

end.

