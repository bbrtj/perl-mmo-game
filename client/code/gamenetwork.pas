unit GameNetwork;

interface

uses
	Classes, FGL, SysUtils, DateUtils,
	CastleClientServer, CastleConfig,
	GameLog, GameConfig,
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
		Notify: TNotifyEvent;

		constructor Create(const AId: Integer; const ACallback: TNetworkMessageCallback; const Model: TModelClass; const ANotify: TNotifyEvent);
	end;

	TFeedItem = class
	public
		Callback: TNetworkMessageCallback;
		CallbackModel: TModelClass;

		constructor Create(const ACallback: TNetworkMessageCallback; const Model: TModelClass);
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
		procedure Send(const Model: TModelClass; const Data: TModelBase; const Callback: TNetworkMessageCallback; const Notify: TNotifyEvent = nil);

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

constructor TCallbackItem.Create(const AId: Integer; const ACallback: TNetworkMessageCallback; const Model: TModelClass; const ANotify: TNotifyEvent);
begin
	Id := AId;
	Callback := ACallback;
	CallbackModel := Model;
	Notify := ANotify;
end;

constructor TFeedItem.Create(const ACallback: TNetworkMessageCallback; const Model: TModelClass);
begin
	Callback := ACallback;
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
begin
	if FClient.IsConnected then begin
		Callback();
		exit;
	end;

	FClient.Hostname := UserConfig.GetValue('server_host', 'localhost');
	FClient.Port := UserConfig.GetValue('server_port', GlobalConfig.NetworkPort);

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

	function HandleCallbacks(const Msg: TMessage): Boolean;
	var
		LModel: TModelBase;
		LCallback: TNetworkMessageCallback;
		LNotify: TNotifyEvent;
		I: Integer;
	begin
		result := false;

		for I := 0 to FCallbacks.Count - 1 do begin
			if not (FCallbacks[I].Id = Msg.Id) then continue;
			if not (FCallbacks[I].CallbackModel.MessageType = Msg.Typ) then continue;

			LCallback := FCallbacks[I].Callback;
			LNotify := FCallbacks[I].Notify;
			LModel := FModelSerializer.DeSerialize(Msg.Data, FCallbacks[I].CallbackModel);
			FCallbacks.Delete(I);

			LCallback(LModel);
			LModel.Free;

			if LNotify <> nil then LNotify(self);
			exit(true);
		end;
	end;

	function HandleFeeds(const Msg: TMessage): Boolean;
	var
		I: Integer;
		LModel: TModelBase;
		LCallback: TNetworkMessageCallback;
	begin
		result := false;

		for I := 0 to FFeeds.Count - 1 do begin
			if not (FFeeds[I].CallbackModel.MessageType = Msg.Typ) then continue;

			LCallback := FFeeds[I].Callback;
			LModel := FModelSerializer.DeSerialize(Msg.Data, FFeeds[I].CallbackModel);

			LCallback(LModel);
			LModel.Free;

			result := true;
		end;
	end;

var
	LMessage: TMessage;
	LHandled: Boolean;
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

	if LMessage.HasId() then
		LHandled := HandleCallbacks(LMessage)
	else
		LHandled := HandleFeeds(LMessage);

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

procedure TNetwork.Send(const Model: TModelClass; const Data: TModelBase; const Callback: TNetworkMessageCallback; const Notify: TNotifyEvent = nil);
var
	LType: TMessageType;
	LCallback: TCallbackItem;
begin
	LType := FindMessageType(Model);

	if not LType.HasCallback then
		raise Exception.Create('Type ' + LType.GetType + ' does not have a callback');

	LCallback := TCallbackItem.Create(DoSend(LType, Data), Callback, LType.CallbackModel, Notify);
	FCallbacks.Add(LCallback);
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

