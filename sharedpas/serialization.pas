unit serialization;

interface

uses
	Classes, SysUtils, TypInfo, FPJSONRTTI, FPJSON, FGL;

type

	TSerialized = class(TPersistent)
	public
		constructor Create(); virtual;
		procedure OnObjectStreamed(Json: TJSONObject); virtual;
	end;

	TSerializedListType = class of TFPSList;
	TSerializedListItemType = class of TSerialized;

	TSerializedList = class
	public
		ListType: TSerializedListType;
		ListItemType: TSerializedListItemType;

		constructor Create(_ListType: TSerializedListType; Itemtype: TSerializedListItemType);
	end;

	TSerializedLists = specialize TFPGObjectList<TSerializedList>;

	TGameStreamer = class
	private
		FStreamer: TJSONStreamer;
		FDeStreamer: TJSONDeStreamer;

		procedure OnStreamProperty(Sender: TObject; ThisObject: TObject; Info: PPropInfo; var res: TJSONData);
		function StreamGenericList(const List: TFPSList): TJSONArray;

		procedure OnRestoreProperty(Sender: TObject; ThisObject: TObject; Info: PPropInfo; Value: TJSONData; var Handled: Boolean);
		procedure DeStreamGenericList(const StreamedArray: TJSONArray; Value: TObject; ItemInfo: TSerializedList);
		procedure DeStreamPerlBoolean(const Json: TJSONData; ThisObject: TObject; Info: PPropInfo);

		procedure OnObjectStreamed(Sender: TObject; ThisObject: TObject; Json: TJSONObject);
	public
		constructor Create();
		destructor Destroy(); override;

		property Streamer: TJSONStreamer read FStreamer;
		property DeStreamer: TJSONDeStreamer read FDeStreamer;
	end;

var
	ListSerializationMap: TSerializedLists;

function IsListHandled(Value: TObject; var ListInfo: TSerializedList): Boolean;

implementation

constructor TSerialized.Create();
begin
end;

procedure TSerialized.OnObjectStreamed(Json: TJSONObject);
begin
	// can be reimplemented
end;


constructor TSerializedList.Create(_ListType: TSerializedListType; Itemtype: TSerializedListItemType);
begin
	ListType := _ListType;
	ListItemType := Itemtype;
end;

{ class TGameStreamer }

constructor TGameStreamer.Create();
begin
	FStreamer := TJSONStreamer.Create(nil);
	FDeStreamer := TJSONDeStreamer.Create(nil);

	FStreamer.OnStreamProperty := @OnStreamProperty;
	FStreamer.AfterStreamObject := @OnObjectStreamed;
	FDeStreamer.OnRestoreProperty := @OnRestoreProperty;
end;

destructor TGameStreamer.Destroy();
begin
	FStreamer.Free;
	FDeStreamer.Free;
end;

procedure TGameStreamer.OnStreamProperty(Sender: TObject; ThisObject: TObject; Info: PPropInfo; var res: TJSONData);
var
	LValue: TObject;
begin
	if Info^.PropType^.Kind = tkClass then begin
		LValue := GetObjectProp(ThisObject, Info);
		if LValue is TFPSList then begin
			res.Free;
			res := StreamGenericList(LValue as TFPSList);
		end;
	end;
end;

procedure TGameStreamer.OnRestoreProperty(Sender: TObject; ThisObject: TObject; Info: PPropInfo; Value: TJSONData; var Handled: Boolean);
var
	LPropValue: TObject;
	LListInfo: TSerializedList;
begin
	Handled := false;

	if Info^.PropType^.Kind = tkClass then begin
		LPropValue := GetObjectProp(ThisObject, Info);
		if IsListHandled(LPropValue, LListInfo) then begin
			Handled := true;
			DeStreamGenericList(Value as TJSONArray, LPropValue, LListInfo);
		end;
	end
	else if (Info^.PropType^.Kind in [tkSString, tkLString, tkAString, tkWString, tkUString])
		and (Value.JSONType = jtNull)
		then begin
		Handled := true;
		SetStrProp(ThisObject, Info, '');
	end
	else if (Info^.PropType^.Kind = tkBool)
		and (Value.JSONType <> jtBoolean)
		then begin
		Handled := true;
		DeStreamPerlBoolean(Value, ThisObject, Info);
	end;
end;

function TGameStreamer.StreamGenericList(const List: TFPSList): TJSONArray;
var
	ind: Integer;
begin
	if List = nil then
		exit(nil);

	result := TJSONArray.Create;
	try
		for ind := 0 to List.Count - 1 do
			result.Add(FStreamer.ObjectToJSON(TObject(List.Items[ind]^)));
	except
		freeAndNil(result);
		raise;
	end;
end;

procedure TGameStreamer.DeStreamGenericList(const StreamedArray: TJSONArray; Value: TObject; ItemInfo: TSerializedList);
var
	ind: Integer;
	resultObject: TSerialized;
begin
	for ind := 0 to StreamedArray.Count - 1 do begin
		resultObject := ItemInfo.ListItemType.Create();
		FDeStreamer.JSONToObject(StreamedArray.Objects[ind], resultObject);
		// This uses TFPSList .Add anyway, so take reference
		(Value as ItemInfo.ListType).Add(@resultObject);
	end;
end;

procedure TGameStreamer.DeStreamPerlBoolean(const Json: TJSONData; ThisObject: TObject; Info: PPropInfo);
	procedure SetBooleanProp(Boolean: Boolean);
	var
		LEnumValue: String;
	begin
		if Boolean then LEnumValue := 'true'
		else LEnumValue := 'false';

		SetEnumProp(ThisObject, Info, LEnumValue);
	end;

begin
	case (Json.JsonType) of
		jtNumber:
			SetBooleanProp(Json.AsInteger <> 0);
		jtString:
			SetBooleanProp((Json.AsString <> '') and (Json.AsString <> '0'));
		jtNull:
			SetBooleanProp(false)
		else
			SetBooleanProp(true);
	end;
end;

procedure TGameStreamer.OnObjectStreamed(Sender: TObject; ThisObject: TObject; Json: TJSONObject);
begin
	if ThisObject is TSerialized then
		(ThisObject as TSerialized).OnObjectStreamed(Json);
end;

{ implementation end }

{ Free functions }

function IsListHandled(Value: TObject; var ListInfo: TSerializedList): Boolean;
var
	LLoopItem: TSerializedList;
begin
	result := false;
	if not(Value is TFPSList) then
		exit;

	for LLoopItem in ListSerializationMap do begin
		if Value is LLoopItem.ListType then begin
			result := true;
			ListInfo := LLoopItem;
			break;
		end;
	end;
end;

initialization
	ListSerializationMap := TSerializedLists.Create;

finalization
	ListSerializationMap.Free;

end.

