unit GameLog;

interface

procedure LogDebug(const Message: String);
procedure LogError(const Message: String);
procedure LogToServer(const Message: String);

implementation

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

initialization
	LogDebug('Debugging executable - printing symbols to the console...');

end.

