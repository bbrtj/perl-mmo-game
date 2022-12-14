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

		constructor Create(ltype: TSerializedListType; litemtype: TSerializedListItemType);
	end;

	TSerializedLists = specialize TFPGObjectList<TSerializedList>;

	TGameStreamer = class
	private
		FStreamer: TJSONStreamer;
		FDeStreamer: TJSONDeStreamer;

		procedure OnStreamProperty(sender: TObject; aObject: TObject; info: PPropInfo; var res: TJSONData);
		function StreamGenericList(const aList: TFPSList): TJSONArray;

		procedure OnRestoreProperty(sender: TObject; aObject: TObject; info: PPropInfo; aValue: TJSONData; var handled: Boolean);
		procedure DeStreamGenericList(const aArray: TJSONArray; value: TObject; itemInfo: TSerializedList);

		procedure OnObjectStreamed(sender: TObject; aObject: TObject; vJson: TJSONObject);
	public
		constructor Create();
		destructor Destroy(); override;

		property Streamer: TJSONStreamer read FStreamer;
		property DeStreamer: TJSONDeStreamer read FDeStreamer;
	end;

var
	ListSerializationMap: TSerializedLists;

function IsListHandled(value: TObject; var listInfo: TSerializedList): Boolean;

implementation

constructor TSerialized.Create();
begin
end;

procedure TSerialized.OnObjectStreamed(vJson: TJSONObject);
begin
	// can be reimplemented
end;


constructor TSerializedList.Create(ltype: TSerializedListType; litemtype: TSerializedListItemType);
begin
	ListType := ltype;
	ListItemType := litemtype;
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

procedure TGameStreamer.OnStreamProperty(sender: TObject; aObject: TObject; info: PPropInfo; var res: TJSONData);
var
	value: TObject;
begin
	if info^.PropType^.Kind = tkClass then begin
		value := GetObjectProp(aObject, info);
		if value is TFPSList then begin
			res.Free;
			res := StreamGenericList(value as TFPSList);
		end;
	end;
end;

procedure TGameStreamer.OnRestoreProperty(sender: TObject; aObject: TObject; info: PPropInfo; aValue: TJSONData; var handled: Boolean);
var
	value: TObject;
	listInfo: TSerializedList;
begin
	handled := false;

	if info^.PropType^.Kind = tkClass then begin
		value := GetObjectProp(aObject, info);
		if IsListHandled(value, listInfo) then begin
			handled := true;
			DeStreamGenericList(aValue as TJSONArray, value, listInfo);
		end;
	end;
end;

function TGameStreamer.StreamGenericList(const aList: TFPSList): TJSONArray;
var
	ind: Integer;
begin
	if aList = nil then
		exit(nil);

	result := TJSONArray.Create;
	try
		for ind := 0 to aList.Count - 1 do
			result.Add(FStreamer.ObjectToJSON(TObject(aList.Items[ind]^)));
	except
		freeAndNil(result);
		raise;
	end;
end;

procedure TGameStreamer.DeStreamGenericList(const aArray: TJSONArray; value: TObject; itemInfo: TSerializedList);
var
	ind: Integer;
	resultObject: TSerialized;
begin
	for ind := 0 to aArray.Count - 1 do begin
		resultObject := itemInfo.ListItemType.Create();
		FDeStreamer.JSONToObject(aArray.Objects[ind], resultObject);
		// This uses TFPSList .Add anyway, so take reference
		(value as itemInfo.ListType).Add(@resultObject);
	end;
end;

procedure TGameStreamer.OnObjectStreamed(sender: TObject; aObject: TObject; vJson: TJSONObject);
begin
	if aObject is TSerialized then
		(aObject as TSerialized).OnObjectStreamed(vJson);
end;

{ implementation end }

{ Free functions }

function IsListHandled(value: TObject; var listInfo: TSerializedList): Boolean;
var
	loopItem: TSerializedList;
begin
	result := false;
	if not(value is TFPSList) then
		exit;

	for loopItem in ListSerializationMap do begin
		if value is loopItem.ListType then begin
			result := true;
			listInfo := loopItem;
			break;
		end;
	end;
end;

initialization
	ListSerializationMap := TSerializedLists.Create;

finalization
	ListSerializationMap.Free;

end.

