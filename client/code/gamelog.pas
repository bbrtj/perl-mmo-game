unit GameLog;

{$modeswitch advancedrecords}

interface

uses System.UITypes, System.UIConsts,
	GameMessageLog;

type
	TColoredMessage = record
		Content: String;
		Color: TAlphaColor;

		class function New(AColor: TAlphaColor; const AContent: String): TColoredMessage; static;
		function ToString(): String;
	end;

	TCombatLogType = (cltError, cltCombat);

function MakeGameColor(R, G, B: Byte; A: Byte = $FF): TAlphaColor;

procedure LogDebug(const Message: String);
procedure LogError(const Message: String);
procedure LogToServer(const Message: String);
procedure LogCombat(LogType: TCombatLogType; const Message: String);

var
	GlobalCombatLog: TGameMessageLog;

implementation

class function TColoredMessage.New(AColor: TAlphaColor; const AContent: String): TColoredMessage;
begin
	result.Color := AColor;
	result.Content := AContent;
end;

function TColoredMessage.ToString(): String;
begin
	result := '<font color="'
		+ AlphaColorToString(self.Color) + '">'
		+ self.Content
		+ '</font>'
		;
end;

function MakeGameColor(R, G, B: Byte; A: Byte): TAlphaColor;
begin
	// NOTE: weird stringification of alphacolor, puts alpha at the front.
	// Fight it by switching the order.
	result := MakeColor(G, B, A, R);
end;

procedure LogDebug(const Message: String);
begin
	{$IFDEF DEBUG}
	writeln(Message);
	{$ENDIF}
end;

procedure LogError(const Message: String);
begin
	// TODO: write to a log file?
	writeln('ERROR ' + Message);
end;

procedure LogToServer(const Message: String);
begin
	// TODO: send a log report to the server
	LogError(Message);
end;

procedure LogCombat(LogType: TCombatLogType; const Message: String);
var
	LColor: TAlphaColor;
begin
	case LogType of
		cltError: LColor := MakeGameColor($CC, $00, $00, $CC);
		cltCombat: LColor := MakeGameColor($CC, $CC, $00, $CC);
	end;

	if GlobalCombatLog <> nil then
		GlobalCombatLog.AddLine(TColoredMessage.New(LColor, Message).ToString);
end;

initialization
	LogDebug('Debugging executable - printing symbols to the console...');
	GlobalCombatLog := nil;

end.

