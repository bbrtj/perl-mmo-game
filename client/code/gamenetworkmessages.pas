unit GameNetworkMessages;

interface

uses FGL, SysUtils,
	GameModels, GameModels.General, GameModels.Login, GameModels.Logout,
	GameModels.CharacterList, GameModels.EnterGame, GameModels.Location;

type
	TMessage = class
	const
		// TODO: make this configurable in an xml
		cSeparator = ';';

	protected

		FId: Integer;
		FData: String;
		FType: String;

		procedure SetBody(vBody: String); virtual;
		function GetBody(): String; virtual;

	public
		property Id: Integer read FId write FId;
		property Data: String read FData write FData;
		property Typ: String read FType write FType;
		property Body: String read GetBody write SetBody;

		function HasId(): Boolean;
	end;

	TOutMessage = class(TMessage);

	TMessageType = class
	private
		FModel: TModelClass;
		FCallbackModel: TModelClass;

	public
		constructor Create(const vModel: TModelClass);
		constructor Create(const vModel: TModelClass; const vCallbackModel: TModelClass);

		function HasCallback(): Boolean;
		function GetType(): String;

		property Model: TModelClass read FModel write FModel;
		property CallbackModel: TModelClass read FCallbackModel write FCallbackModel;
	end;

	TMessageTypes = specialize TFPGObjectList<TMessageType>;

var
	MessageTypesMap: TMessageTypes;
	FeedTypesMap: TMessageTypes;

function FindMessageType(const vType: TModelClass): TMessageType;
function FindFeedType(const vType: TModelClass): TMessageType;

implementation

function FindMessageType(const vType: TModelClass): TMessageType;
var
	vMessageType: TMessageType;
begin
	for vMessageType in MessageTypesMap do begin
		if vMessageType.Model = vType then
			exit(vMessageType);
	end;

	raise Exception.Create('No such network message type: ' + vType.MessageType);
end;

function FindFeedType(const vType: TModelClass): TMessageType;
var
	vFeedType: TMessageType;
begin
	for vFeedType in FeedTypesMap do begin
		if vFeedType.Model = vType then
			exit(vFeedType);
	end;

	raise Exception.Create('No such network feed type: ' + vType.MessageType);
end;

procedure TMessage.SetBody(vBody: String);
var
	vParts: TStringArray;
begin
	// TODO: handle non-existing parts of this split
	vParts := vBody.Split([cSeparator], 3);

	if length(vParts[0]) > 0 then
		FId := StrToInt(vParts[0])
	else
		FId := -1;

	FType := vParts[1];
	FData := vParts[2];
end;

function TMessage.GetBody(): String;
begin
	result := FId.ToString() + cSeparator + FType + cSeparator + FData;
end;

function TMessage.HasId(): Boolean;
begin
	result := FId >= 0;
end;

constructor TMessageType.Create(const vModel: TModelClass);
begin
	FModel := vModel;
	FCallbackModel := nil;
end;

constructor TMessageType.Create(const vModel: TModelClass; const vCallbackModel: TModelClass);
begin
	FModel := vModel;
	FCallbackModel := vCallbackModel;
end;

function TMessageType.GetType(): String;
begin
	result := FModel.MessageType;
end;

function TMessageType.HasCallback(): Boolean;
begin
	result := FCallbackModel <> nil;
end;

{ implementation end }

initialization
	MessageTypesMap := TMessageTypes.Create;
	FeedTypesMap := TMessageTypes.Create;

	MessageTypesMap.Add(TMessageType.Create(TMsgLogin, TMsgResSuccess));
	MessageTypesMap.Add(TMessageType.Create(TMsgLogout));
	MessageTypesMap.Add(TMessageType.Create(TMsgCharacterList, TMsgResCharacterList));
	MessageTypesMap.Add(TMessageType.Create(TMsgEnterGame, TMsgResSuccess));

	FeedTypesMap.Add(TMessageType.Create(TMsgFeedLocationData));

finalization
	MessageTypesMap.Free;

end.

