unit GameNetwork;

interface

uses
	FGL,
	CastleClientServer,
	GameModels, GameModels.General,
	Serialization;

type
	TNetworkCallback = procedure() of object;
	TNetworkMessageCallback = procedure(const vModel: TModelBase) of Object;

	TCallbackItem = class
	public
		id: Integer;
		callback: TNetworkMessageCallback;
		messageClass: TMessageClass;

		constructor Create(const vId: Integer; const vCallback: TNetworkMessageCallback; const vClass: TMessageClass);
	end;

	TCallbackItems = specialize TFPGObjectList<TCallbackItem>;

	TNetwork = class sealed
	private
		FClient: TCastleTCPClient;
		FCallbacks: TCallbackItems;
		FStreamer: TGameStreamer;

		procedure OnDisconnected;
		procedure OnMessageRecieved(const vReceived: String);

		function AssignId(): Integer;

	public
	const
		cDefaultHost = 'localhost';
		cDefaultPort = 14832;

		constructor Create;
		destructor Destroy; override;

		procedure Connect(const vHost: String; const vPort: Word; const vCallback: TNetworkCallback);
		procedure Disconnect();

		procedure Send(const vMessage: TMessageBase);
		procedure Send(const vMessage: TMessageBase; const vCallback: TNetworkMessageCallback);
	end;

var
	GlobalClient: TNetwork;

implementation

{}
constructor TCallbackItem.Create(const vId: Integer; const vCallback: TNetworkMessageCallback; const vClass: TMessageClass);
begin
	id := vId;
	callback := vCallback;
	messageClass := vClass;
end;

{}
constructor TNetwork.Create();
begin
	FClient := TCastleTCPClient.Create;
	FCallbacks := TCallbackItems.Create;
	FStreamer := TGameStreamer.Create;
end;

{}
destructor TNetwork.Destroy;
begin
	FClient.Free;
	FCallbacks.Free;
	FStreamer.Free;
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
	FClient.OnMessageRecieved := @OnMessageRecieved;

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
procedure TNetwork.OnMessageRecieved (const vReceived: String);
var
	vMeta: TMessageMeta;
	vMessage: TMessageBase;
	vCallback: TCallbackItem;
begin
	writeln('got: ' + vReceived);
	vMeta := TMessageMeta.Create;
	FStreamer.DeStreamer.JSONToObject(vReceived, vMeta);

	for vCallback in FCallbacks do begin
		if vCallback.id = vMeta.n then begin
			vMessage := vCallback.messageClass.Create;
			FStreamer.DeStreamer.JSONToObject(vReceived, vMessage);

			vCallback.callback(vMessage.d);

			FCallbacks.Remove(vCallback);
			vMeta.Free;
			vMessage.Free;
			exit;
		end;
	end;

	// TODO: handle something that we were not waiting for
	vMeta.Free;
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
procedure TNetwork.Send(const vMessage: TMessageBase);
var
	vToSend: String;
begin
	vToSend := FStreamer.Streamer.ObjectToJSONString(vMessage);
	FClient.Send(vToSend);
end;

{}
procedure TNetwork.Send(const vMessage: TMessageBase; const vCallback: TNetworkMessageCallback);
begin
	vMessage.n := AssignId();
	Send(vMessage);
	FCallbacks.Add(TCallbackItem.Create(vMessage.n, vCallback, vMessage.ResultClass()));
end;

initialization
	GlobalClient := TNetwork.Create;

finalization
	// TODO: this hangs and throws access violation
	// GlobalClient.Free;

end.
