unit GameNetwork;

interface

uses
	FGL,
	CastleClientServer,
	GameNetworkMessages,
	GameModels, GameModels.General;

type
	TNetworkCallback = procedure() of object;
	TNetworkMessageCallback = procedure(const vModel: TModelBase) of Object;

	TCallbackItem = class
	public
		Id: Integer;
		Callback: TNetworkMessageCallback;
		MessageType: TMessageType;

		constructor Create(const vId: Integer; const vCallback: TNetworkMessageCallback; const vType: TMessageType);
	end;

	TCallbackItems = specialize TFPGObjectList<TCallbackItem>;

	TNetwork = class sealed
	private
		FClient: TCastleTCPClient;
		FCallbacks: TCallbackItems;
		FModelSerializer: TModelSerializationBase;

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

		procedure Send(const vType: TMessageType; const vData: TModelBase);
		procedure Send(const vType: TMessageType; const vData: TModelBase; const vCallback: TNetworkMessageCallback);
	end;

var
	GlobalClient: TNetwork;

implementation

{}
constructor TCallbackItem.Create(const vId: Integer; const vCallback: TNetworkMessageCallback; const vType: TMessageType);
begin
	Id := vId;
	Callback := vCallback;
	MessageType := vType;
end;

{}
constructor TNetwork.Create();
begin
	FClient := TCastleTCPClient.Create;
	FCallbacks := TCallbackItems.Create;
	FModelSerializer := TJSONModelSerialization.Create;
end;

{}
destructor TNetwork.Destroy;
begin
	FClient.Free;
	FCallbacks.Free;
	FModelSerializer.Free;
	inherited;
end;

{}
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

{}
procedure TNetwork.Disconnect();
begin
	if FClient.IsConnected then begin
		FClient.OnDisconnected := nil;
		FClient.Disconnect;
		FCallbacks.Clear;
	end;
end;

{}
procedure TNetwork.OnDisconnected;
begin
	writeln('disconnected');
	// TODO: we should try to reconnect with a timeout
end;

{}
procedure TNetwork.OnMessageReceived (const vReceived: String);
var
	vMessage: TMessage;
	vModel: TModelBase;
	vCallback: TCallbackItem;
begin
	writeln('got: ' + vReceived);
	vMessage := TMessage.Create;
	vMessage.Body := vReceived;

	for vCallback in FCallbacks do begin
		if (vCallback.Id = vMessage.Id) and (vCallback.MessageType.MessageCallbackType = vMessage.Typ) then begin
			vModel := FModelSerializer.DeSerialize(vMessage.Data, vCallback.MessageType.MessageCallbackModel);

			vCallback.callback(vModel);

			FCallbacks.Remove(vCallback);
			vMessage.Free;
			vModel.Free;
			exit;
		end;
	end;

	// TODO: handle something that we were not waiting for
	writeln('not handled');
	vMessage.Free;
end;

{}
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

{}
function TNetwork.DoSend(const vType: TMessageType; const vData: TModelBase): Integer;
var
	vToSend: TOutMessage;
begin
	// TODO: make sure we are connected?
	vToSend := TOutMessage.Create;
	vToSend.Id := AssignId();
	vToSend.Typ := vType.MessageType;
	vToSend.Data := FModelSerializer.Serialize(vData);

	result := vToSend.Id;

	writeln('Sending ' + vToSend.Body);
	FClient.Send(vToSend.Body);
	vToSend.Free;
end;

{}
procedure TNetwork.Send(const vType: TMessageType; const vData: TModelBase; const vCallback: TNetworkMessageCallback);
begin
	FCallbacks.Add(TCallbackItem.Create(DoSend(vType, vData), vCallback, vType));
end;

{}
procedure TNetwork.Send(const vType: TMessageType; const vData: TModelBase);
begin
	DoSend(vType, vData);
end;

initialization
	GlobalClient := TNetwork.Create;

finalization
	// TODO: this hangs and throws access violation
	// GlobalClient.Free;

{ implementation end }

end.

