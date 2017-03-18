unit consoleCheckerThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

type TConsoleCheckerThread = class(TThread)
  private
    FStates: array[0..3] of integer;
    FShouldStop: boolean;
    procedure checkConsole(num: integer);
    function getConsoleState(index:integer): integer;
  protected
    procedure Execute; override;
  public
    property states[index:integer]: integer read getConsoleState;
    property shouldStop: boolean read FShouldStop write FShouldStop default false;
  const
    CONSOLE_RUNNING = 0;
    CONSOLE_UNKNOWN = 1;
    CONSOLE_UP = 2;
    CONSOLE_DOWN = 3;
  end;

implementation

function TConsoleCheckerThread.getConsoleState(Index:integer):integer;
begin
  result := FStates[index];
end;

procedure TConsoleCheckerThread.checkConsole(num: integer);
var pingProc: TProcess;
begin
  // todo check last message from console, to see if we know the state without
  // doing a ping.
  pingProc := TProcess.create(nil);
  pingProc.Executable:='/bin/ping';
  pingProc.Parameters.Add('192.168.1.3' + inttostr(num+1));
  pingProc.Parameters.Add('-w1');
  pingProc.Parameters.Add('-c1');
  pingProc.Options := pingProc.Options + [poWaitOnExit];
  pingProc.Execute;
  write('Console ' + inttostr(num) + ':');
  writeln(pingProc.ExitStatus);
  if (pingProc.ExitStatus <> 0) then
    FStates[num] := CONSOLE_DOWN
  else
    FStates[num] := CONSOLE_UP;
  pingProc.free();
end;

procedure TConsoleCheckerThread.Execute;
var i: integer;
begin
  for i:= 0 to 3 do begin
    FStates[i] := CONSOLE_UNKNOWN;
  end;
  while not FShouldStop do begin
    for i:= 0 to 3 do
      checkConsole(i);
    Sleep(3000);
  end;
end;

end.

