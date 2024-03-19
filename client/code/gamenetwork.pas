unit GameNetwork;

interface

uses
	Classes, FGL, SysUtils, DateUtils,
	CastleClientServer, CastleConfig,
	GameLog,
	GameNetworkMessages,
	GameModels;

type
	TNetworkCallback = procedure() of object;
	TNetworkMessageCallback = procedure(const GModel: TModelBase) of Object;

	TCallbackItem = class
	public
		Id: Integer;
		Callback: TNetworkMessageCallback;
		CallbackModel: TModelClass;

		constructor Create(const aId: Integer; const aCallback: TNetworkMessageCallback; const Model: TModelClass);
	end;

	TFeedItem = class
	public
		Callback: TNetworkMessageCallback;
		CallbackModel: TModelClass;

		constructor Create(const aCallback: TNetworkMessageCallback; const Model: TModelClass);
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
		FPingStart: TDateTime;
		FPing: Int64;

		FPooling: Boolean;
		FPool: TStringList;

		FOnDisconnected: TNetworkCallback;

		procedure OnDisconnectedInternal;
		procedure OnMessageReceived(const Received: String);

		function DoSend(const MessageType: TMessageType; const Data: TModelBase): Integer;
		function AssignId(): Integer;

		procedure SetPooling(const Value: Boolean);

	public
		constructor Create;
		destructor Destroy; override;

		procedure Connect(const Callback: TNetworkCallback);
		procedure Disconnect(const RaiseEvent: Boolean = True);

		procedure Send(const Model: TModelClass; const Data: TModelBase);
		procedure Send(const Model: TModelClass; const Data: TModelBase; const Callback: TNetworkMessageCallback);

		procedure Await(const Model: TModelClass; const Callback: TNetworkMessageCallback);
		procedure StopWaiting(const Model: TModelClass);
		procedure ContextChange();

		procedure Heartbeat(const Passed: Single);

		property Ping: Int64 read FPing;
		property Pooling: Boolean read FPooling write SetPooling;
		property OnDisconnected: TNetworkCallback write FOnDisconnected;
	end;

var
	GlobalClient: TNetwork;

implementation

constructor TCallbackItem.Create(const aId: Integer; const aCallback: TNetworkMessageCallback; const Model: TModelClass);
begin
	Id := aId;
	Callback := aCallback;
	CallbackModel := Model;
end;

constructor TFeedItem.Create(const aCallback: TNetworkMessageCallback; const Model: TModelClass);
begin
	Callback := aCallback;
	CallbackModel := Model;
end;

constructor TNetwork.Create();
begin
	FClient := TCastleTCPClient.Create;
	FCallbacks := TCallbackItems.Create;
	FFeeds := TFeedItems.Create;
	FModelSerializer := TJSONModelSerialization.Create;

	FPooling := False;
	FPool := TStringList.Create;

	FOnDisconnected := nil;

	FClient.OnDisconnected := @OnDisconnectedInternal;
	FClient.OnMessageReceived := @OnMessageReceived;
end;

destructor TNetwork.Destroy;
begin
	FClient.Free;
	FCallbacks.Free;
	FFeeds.Free;
	FModelSerializer.Free;
	FPool.Free;
	inherited;
end;

procedure TNetwork.Connect(const Callback: TNetworkCallback);
const
	cDefaultHost = 'localhost';
	cDefaultPort = 14832;

begin
	if FClient.IsConnected then begin
		Callback();
		exit;
	end;

	FClient.Hostname := UserConfig.GetValue('server_host', cDefaultHost);
	FClient.Port := UserConfig.GetValue('server_port', cDefaultPort);

	FClient.OnConnected := Callback;
	FClient.Connect;
end;

procedure TNetwork.Disconnect(const RaiseEvent: Boolean = True);
begin
	if FClient.IsConnected then begin
		FClient.Disconnect;
		self.ContextChange;

		if RaiseEvent then
			self.OnDisconnectedInternal;
	end;
end;

procedure TNetwork.OnDisconnectedInternal;
begin
	LogDebug('Network: disconnected');

	if FOnDisconnected <> nil then
		FOnDisconnected();
end;

procedure TNetwork.OnMessageReceived(const Received: String);
var
	LMessage: TMessage;
	LHandled: Boolean;

	{ nested procedure }
	procedure HandleCallbacks;
	var
		LCallback: TCallbackItem;
		GModel: TModelBase;
	begin
		for LCallback in FCallbacks do begin
			if not (LCallback.Id = LMessage.Id) then continue;
			if not (LCallback.CallbackModel.MessageType = LMessage.Typ) then continue;

			GModel := FModelSerializer.DeSerialize(LMessage.Data, LCallback.CallbackModel);
			LCallback.Callback(GModel);

			FCallbacks.Remove(LCallback);
			GModel.Free;

			LHandled := true;
			break;
		end;
	end;

	procedure HandleFeeds;
	var
		LFeed: TFeedItem;
		GModel: TModelBase;
	begin
		for LFeed in FFeeds do begin
			if not (LFeed.CallbackModel.MessageType = LMessage.Typ) then continue;

			GModel := FModelSerializer.DeSerialize(LMessage.Data, LFeed.CallbackModel);
			LFeed.Callback(GModel);

			GModel.Free;
			LHandled := true;
		end;
	end;

begin
	if Received = 'ping' then begin
		FPing := MilliSecondsBetween(Now, FPingStart);
		exit;
	end;

	if FPooling then begin
		FPool.Add(Received);
		LogDebug('Network: pooled ' + Received);
		exit;
	end;

	LogDebug('Network: got ' + Received);

	LMessage := TMessage.Create;
	LMessage.Body := Received;
	LHandled := false;

	if LMessage.HasId() then
		HandleCallbacks()
	else
		HandleFeeds();

	LMessage.Free;

	if not LHandled then
		LogDebug('Network: message was not handled');
end;

function TNetwork.AssignId(): Integer;
var
	LInd: Integer;
	LId: Integer;
begin
	result := 0;
	for LInd := 0 to FCallbacks.Count - 1 do begin
		LId := FCallbacks.Items[LInd].id;
		if LId > result then
			result := LId;
	end;

	result += 1;
end;

function TNetwork.DoSend(const MessageType: TMessageType; const Data: TModelBase): Integer;
var
	LToSend: TOutMessage;
begin
	// TODO: make sure we are connected?
	LToSend := TOutMessage.Create;
	LToSend.Id := AssignId();
	LToSend.Typ := MessageType.GetType;
	LToSend.Data := FModelSerializer.Serialize(Data);

	result := LToSend.Id;

	LogDebug('Network: sending ' + LToSend.Body);
	FClient.Send(LToSend.Body);
	LToSend.Free;
end;

procedure TNetwork.Send(const Model: TModelClass; const Data: TModelBase; const Callback: TNetworkMessageCallback);
var
	LType: TMessageType;
begin
	LType := FindMessageType(Model);

	if not LType.HasCallback then
		raise Exception.Create('Type ' + LType.GetType + ' does not have a callback');

	FCallbacks.Add(TCallbackItem.Create(DoSend(LType, Data), Callback, LType.CallbackModel));
end;

procedure TNetwork.Send(const Model: TModelClass; const Data: TModelBase);
var
	LType: TMessageType;
begin
	LType := FindMessageType(Model);

	if LType.HasCallback then
		raise Exception.Create('Type ' + LType.GetType + ' has a callback, but no callback passed');

	DoSend(LType, Data);
end;

procedure TNetwork.Await(const Model: TModelClass; const Callback: TNetworkMessageCallback);
var
	LType: TMessageType;
begin
	LType := FindFeedType(Model);

	FFeeds.Add(TFeedItem.Create(Callback, LType.Model));
end;

procedure TNetwork.StopWaiting(const Model: TModelClass);
var
	LFeed: TFeedItem;
begin
	for LFeed in FFeeds do begin
		if LFeed.CallbackModel = Model then begin
			FFeeds.Remove(LFeed);
			break;
		end;
	end;
end;

procedure TNetwork.ContextChange();
begin
	// during a context change, we no longer wait for unresolved callbacks / feeds
	FCallbacks.Clear;
	FFeeds.Clear;
end;

procedure TNetwork.Heartbeat(const Passed: Single);
begin
	FSecondsPassed += Passed;

	if (FSecondsPassed > 15) or (FPing = 0) then begin
		FPingStart := Now;
		FClient.Send('ping');
		FSecondsPassed := 0;
	end;
end;

procedure TNetwork.SetPooling(const Value: Boolean);
var
	LMessage: String;
begin
	FPooling := Value;

	if Value = False then begin
		for LMessage in FPool do
			OnMessageReceived(LMessage);
		FPool.Clear;
	end;
end;

{ implementation end }

initialization
	GlobalClient := TNetwork.Create;

finalization
	// FIXME: this hangs and throws access violation
	// GlobalClient.Free;

end.

