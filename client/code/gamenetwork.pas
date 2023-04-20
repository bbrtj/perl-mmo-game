unit GameNetwork;

interface

uses
	FGL, SysUtils,
	CastleClientServer,
	GameNetworkMessages,
	GameModels;

type
	TNetworkCallback = procedure() of object;
	TNetworkMessageCallback = procedure(const vModel: TModelBase) of Object;

	TCallbackItem = class
	public
		Id: Integer;
		Callback: TNetworkMessageCallback;
		CallbackModel: TModelClass;

		constructor Create(const vId: Integer; const vCallback: TNetworkMessageCallback; const vModel: TModelClass);
	end;

	TFeedItem = class
	public
		Callback: TNetworkMessageCallback;
		CallbackModel: TModelClass;

		constructor Create(const vCallback: TNetworkMessageCallback; const vModel: TModelClass);
	end;

	TCallbackItems = specialize TFPGObjectList<TCallbackItem>;
	TFeedItems = specialize TFPGObjectList<TFeedItem>;

	TNetwork = class sealed
	private
		FClient: TCastleTCPClient;
		FCallbacks: TCallbackItems;
		FFeeds: TFeedItems;
		FModelSerializer: TModelSerializationBase;
		FSecondsPassed: Single;
		FPingStart: Double;
		FPing: Single;

		procedure OnDisconnected;
		procedure OnMessageReceived(const vReceived: String);

		function DoSend(const vType: TMessageType; const vData: TModelBase): Integer;
		function AssignId(): Integer;

	public
	const
		// TODO: configure at compile stage
		cDefaultHost = 'localhost';
		cDefaultPort = 14832;

		constructor Create;
		destructor Destroy; override;

		procedure Connect(const vHost: String; const vPort: Word; const vCallback: TNetworkCallback);
		procedure Disconnect();

		procedure Send(const vModel: TModelClass; const vData: TModelBase);
		procedure Send(const vModel: TModelClass; const vData: TModelBase; const vCallback: TNetworkMessageCallback);

		procedure Await(const vModel: TModelClass; const vCallback: TNetworkMessageCallback);
		procedure ContextChange();

		procedure Heartbeat(const vPassed: Single);

		property Ping: Single read FPing;
	end;

var
	GlobalClient: TNetwork;

implementation

constructor TCallbackItem.Create(const vId: Integer; const vCallback: TNetworkMessageCallback; const vModel: TModelClass);
begin
	Id := vId;
	Callback := vCallback;
	CallbackModel := vModel;
end;

constructor TFeedItem.Create(const vCallback: TNetworkMessageCallback; const vModel: TModelClass);
begin
	Callback := vCallback;
	CallbackModel := vModel;
end;

constructor TNetwork.Create();
begin
	FClient := TCastleTCPClient.Create;
	FCallbacks := TCallbackItems.Create;
	FFeeds := TFeedItems.Create;
	FModelSerializer := TJSONModelSerialization.Create;
end;

destructor TNetwork.Destroy;
begin
	FClient.Free;
	FCallbacks.Free;
	FFeeds.Free;
	FModelSerializer.Free;
	inherited;
end;

procedure TNetwork.Connect(const vHost: String; const vPort: Word; const vCallback: TNetworkCallback);
begin
	if FClient.IsConnected then begin
		vCallback();
		exit;
	end;

	FClient.Hostname := vHost;
	FClient.Port := vPort;

	FClient.OnConnected := vCallback;
	FClient.OnDisconnected := @OnDisconnected;
	FClient.OnMessageReceived := @OnMessageReceived;

	FClient.Connect;
end;

procedure TNetwork.Disconnect();
begin
	if FClient.IsConnected then begin
		FClient.OnDisconnected := nil;
		FClient.Disconnect;
		self.ContextChange;
	end;
end;

procedure TNetwork.OnDisconnected;
begin
	writeln('disconnected');
	// TODO: we should try to reconnect with a timeout
end;

procedure TNetwork.OnMessageReceived (const vReceived: String);
var
	vMessage: TMessage;
	vHandled: Boolean;

	{ nested procedure }
	procedure HandleCallbacks;
	var
		vCallback: TCallbackItem;
		vModel: TModelBase;
	begin
		for vCallback in FCallbacks do begin
			if not (vCallback.Id = vMessage.Id) then continue;
			if not (vCallback.CallbackModel.MessageType = vMessage.Typ) then continue;

			vModel := FModelSerializer.DeSerialize(vMessage.Data, vCallback.CallbackModel);
			vCallback.Callback(vModel);

			FCallbacks.Remove(vCallback);
			vModel.Free;

			vHandled := true;
			break;
		end;
	end;

	procedure HandleFeeds;
	var
		vFeed: TFeedItem;
		vModel: TModelBase;
	begin
		for vFeed in FFeeds do begin
			if not (vFeed.CallbackModel.MessageType = vMessage.Typ) then continue;

			vModel := FModelSerializer.DeSerialize(vMessage.Data, vFeed.CallbackModel);
			vFeed.Callback(vModel);

			vModel.Free;
			vHandled := true;
		end;
	end;

begin
	writeln('got: ' + vReceived);

	if vReceived = 'ping' then begin
		FPing := Time - FPingStart;
		exit;
	end;

	vMessage := TMessage.Create;
	vMessage.Body := vReceived;
	vHandled := false;

	if vMessage.HasId() then
		HandleCallbacks()
	else
		HandleFeeds();

	vMessage.Free;

	if not vHandled then
		writeln('message was not handled');
end;

function TNetwork.AssignId(): Integer;
var
	vInd: Integer;
	vId: Integer;
begin
	result := 0;
	for vInd := 0 to FCallbacks.Count - 1 do begin
		vId := FCallbacks.Items[vInd].id;
		if vId > result then
			result := vId;
	end;

	result += 1;
end;

function TNetwork.DoSend(const vType: TMessageType; const vData: TModelBase): Integer;
var
	vToSend: TOutMessage;
begin
	// TODO: make sure we are connected?
	vToSend := TOutMessage.Create;
	vToSend.Id := AssignId();
	vToSend.Typ := vType.GetType;
	vToSend.Data := FModelSerializer.Serialize(vData);

	result := vToSend.Id;

	writeln('Sending ' + vToSend.Body);
	FClient.Send(vToSend.Body);
	vToSend.Free;
end;

procedure TNetwork.Send(const vModel: TModelClass; const vData: TModelBase; const vCallback: TNetworkMessageCallback);
var
	vType: TMessageType;
begin
	vType := FindMessageType(vModel);

	if not vType.HasCallback then
		raise Exception.Create('Type ' + vType.GetType + ' does not have a callback');

	FCallbacks.Add(TCallbackItem.Create(DoSend(vType, vData), vCallback, vType.CallbackModel));
end;

procedure TNetwork.Send(const vModel: TModelClass; const vData: TModelBase);
var
	vType: TMessageType;
begin
	vType := FindMessageType(vModel);

	if vType.HasCallback then
		raise Exception.Create('Type ' + vType.GetType + ' has a callback, but no callback passed');

	DoSend(vType, vData);
end;

procedure TNetwork.Await(const vModel: TModelClass; const vCallback: TNetworkMessageCallback);
var
	vType: TMessageType;
begin
	vType := FindFeedType(vModel);

	FFeeds.Add(TFeedItem.Create(vCallback, vType.Model));
end;

procedure TNetwork.ContextChange();
begin
	// during a context change, we no longer wait for unresolved callbacks / feeds
	FCallbacks.Clear;
	FFeeds.Clear;
end;

procedure TNetwork.Heartbeat(const vPassed: Single);
begin
	FSecondsPassed += vPassed;

	if (FSecondsPassed > 15) or (FPing = 0) then begin
		FPingStart := Time;
		FClient.Send('ping');
		FSecondsPassed := 0;
	end;
end;

{ implementation end }

initialization
	GlobalClient := TNetwork.Create;

finalization
	// TODO: this hangs and throws access violation
	// GlobalClient.Free;

end.

