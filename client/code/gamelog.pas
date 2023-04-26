unit GameLog;

interface

procedure LogDebug(const vMessage: String);
procedure LogError(const vMessage: String);
procedure LogToServer(const vMessage: String);

implementation

procedure LogDebug(const vMessage: String);
begin
	{$IFDEF DEBUG}
	writeln(vMessage);
	{$ENDIF}
end;

procedure LogError(const vMessage: String);
begin
	// TODO: write to a log file?
	writeln('ERROR ' + vMessage);
end;

procedure LogToServer(const vMessage: String);
begin
	// TODO: send a log report to the server
	LogError(vMessage);
end;

initialization
	LogDebug('Debugging executable - printing symbols to the console...');

end.

