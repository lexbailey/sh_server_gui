unit progrunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process;

type TProgRunner = class(TObject)
  public
    class function getBashCommandOutput(bashCommand: string; chomp: boolean = true):string;
    class procedure startExternalProgram(bashCommand: string);
  end;

implementation


class function TProgRunner.getBashCommandOutput(bashCommand: string; chomp: boolean = true): string;
const
  BUF_SIZE = 999;
var bashProc: TProcess;
  OutputStream : TStringStream;
  BytesRead : longint;
  Buffer : array[1..BUF_SIZE] of byte;
begin
  bashProc := TProcess.create(nil);
  bashProc.Executable:='/bin/bash';
  bashProc.Parameters.Add('-c');
  bashProc.Parameters.Add(bashCommand);
  bashProc.Options := bashProc.Options + [poWaitOnExit, poUsePipes];
  bashProc.Execute;
  OutputStream := TStringStream.Create('');
  repeat
    BytesRead := bashProc.Output.Read(Buffer, BUF_SIZE);
    OutputStream.Write(Buffer, BytesRead);
  until BytesRead = 0;
  bashProc.Free;
  result := OutputStream.DataString;
  if chomp then
    setlength(result, length(result)-1);
end;

class procedure TProgRunner.startExternalProgram(bashCommand: string);
var bashProc: TProcess;
begin
  bashProc := TProcess.create(nil);
  bashProc.Executable:='/bin/bash';
  bashProc.Parameters.Add('-c');
  bashProc.Parameters.Add(bashCommand);
  bashProc.Execute;
end;


end.

