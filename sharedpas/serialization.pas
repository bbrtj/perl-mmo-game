unit serialization;

interface

uses
	Classes, SysUtils, TypInfo, FPJSONRTTI, FPJSON, FGL;

type

	TSerialized = class(TPersistent)
	public
		constructor Create(); virtual;
		procedure OnObjectStreamed(vJson: TJSONObject); virtual;
	end;

	TSerializedListType = class of TFPSList;
	TSerializedListItemType = class of TSerialized;

	TSerializedList = class
	public
		ListType: TSerializedListType;
		ListItemType: TSerializedListItemType;

		constructor Create(vType: TSerializedListType; vItemtype: TSerializedListItemType);
	end;

	TSerializedLists = specialize TFPGObjectList<TSerializedList>;

	TGameStreamer = class
	private
		FStreamer: TJSONStreamer;
		FDeStreamer: TJSONDeStreamer;

		procedure OnStreamProperty(vSender: TObject; vObject: TObject; vInfo: PPropInfo; var res: TJSONData);
		function StreamGenericList(const vList: TFPSList): TJSONArray;

		procedure OnRestoreProperty(vSender: TObject; vObject: TObject; vInfo: PPropInfo; vValue: TJSONData; var vHandled: Boolean);
		procedure DeStreamGenericList(const vArray: TJSONArray; vValue: TObject; vItemInfo: TSerializedList);
		procedure DeStreamPerlBoolean(const vJson: TJSONData; vObject: TObject; vInfo: PPropInfo);

		procedure OnObjectStreamed(vSender: TObject; vObject: TObject; vJson: TJSONObject);
	public
		constructor Create();
		destructor Destroy(); override;

		property Streamer: TJSONStreamer read FStreamer;
		property DeStreamer: TJSONDeStreamer read FDeStreamer;
	end;

var
	ListSerializationMap: TSerializedLists;

function IsListHandled(vValue: TObject; var vListInfo: TSerializedList): Boolean;

implementation

constructor TSerialized.Create();
begin
end;

procedure TSerialized.OnObjectStreamed(vJson: TJSONObject);
begin
	// can be reimplemented
end;


constructor TSerializedList.Create(vType: TSerializedListType; vItemtype: TSerializedListItemType);
begin
	ListType := vType;
	ListItemType := vItemtype;
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

procedure TGameStreamer.OnStreamProperty(vSender: TObject; vObject: TObject; vInfo: PPropInfo; var res: TJSONData);
var
	vValue: TObject;
begin
	if vInfo^.PropType^.Kind = tkClass then begin
		vValue := GetObjectProp(vObject, vInfo);
		if vValue is TFPSList then begin
			res.Free;
			res := StreamGenericList(vValue as TFPSList);
		end;
	end;
end;

procedure TGameStreamer.OnRestoreProperty(vSender: TObject; vObject: TObject; vInfo: PPropInfo; vValue: TJSONData; var vHandled: Boolean);
var
	vPropValue: TObject;
	vListInfo: TSerializedList;
begin
	vHandled := false;

	if vInfo^.PropType^.Kind = tkClass then begin
		vPropValue := GetObjectProp(vObject, vInfo);
		if IsListHandled(vPropValue, vListInfo) then begin
			vHandled := true;
			DeStreamGenericList(vValue as TJSONArray, vPropValue, vListInfo);
		end;
	end
	else if (vInfo^.PropType^.Kind in [tkSString, tkLString, tkAString, tkWString, tkUString])
		and (vValue.JSONType = jtNull)
		then begin
		vHandled := true;
		SetStrProp(vObject, vInfo, '');
	end
	else if (vInfo^.PropType^.Kind = tkBool)
		and (vValue.JSONType <> jtBoolean)
		then begin
		vHandled := true;
		DeStreamPerlBoolean(vValue, vObject, vInfo);
	end;
end;

function TGameStreamer.StreamGenericList(const vList: TFPSList): TJSONArray;
var
	ind: Integer;
begin
	if vList = nil then
		exit(nil);

	result := TJSONArray.Create;
	try
		for ind := 0 to vList.Count - 1 do
			result.Add(FStreamer.ObjectToJSON(TObject(vList.Items[ind]^)));
	except
		freeAndNil(result);
		raise;
	end;
end;

procedure TGameStreamer.DeStreamGenericList(const vArray: TJSONArray; vValue: TObject; vItemInfo: TSerializedList);
var
	ind: Integer;
	resultObject: TSerialized;
begin
	for ind := 0 to vArray.Count - 1 do begin
		resultObject := vItemInfo.ListItemType.Create();
		FDeStreamer.JSONToObject(vArray.Objects[ind], resultObject);
		// This uses TFPSList .Add anyway, so take reference
		(vValue as vItemInfo.ListType).Add(@resultObject);
	end;
end;

procedure TGameStreamer.DeStreamPerlBoolean(const vJson: TJSONData; vObject: TObject; vInfo: PPropInfo);
	procedure SetBooleanProp(vBoolean: Boolean);
	var
		vEnumValue: String;
	begin
		if vBoolean then vEnumValue := 'true'
		else vEnumValue := 'false';

		SetEnumProp(vObject, vInfo, vEnumValue);
	end;

begin
	case (vJson.JsonType) of
		jtNumber:
			SetBooleanProp(vJson.AsInteger <> 0);
		jtString:
			SetBooleanProp((vJson.AsString <> '') and (vJson.AsString <> '0'));
		jtNull:
			SetBooleanProp(false)
		else
			SetBooleanProp(true);
	end;
end;

procedure TGameStreamer.OnObjectStreamed(vSender: TObject; vObject: TObject; vJson: TJSONObject);
begin
	if vObject is TSerialized then
		(vObject as TSerialized).OnObjectStreamed(vJson);
end;

{ implementation end }

{ Free functions }

function IsListHandled(vValue: TObject; var vListInfo: TSerializedList): Boolean;
var
	vLoopItem: TSerializedList;
begin
	result := false;
	if not(vValue is TFPSList) then
		exit;

	for vLoopItem in ListSerializationMap do begin
		if vValue is vLoopItem.ListType then begin
			result := true;
			vListInfo := vLoopItem;
			break;
		end;
	end;
end;

initialization
	ListSerializationMap := TSerializedLists.Create;

finalization
	ListSerializationMap.Free;

end.

