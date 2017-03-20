unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, MQTTComponent, Process, consoleCheckerThread,
  mqttmanagerthread, regexpr, confirmation;

type

  { TfrmMonitor }

  TfrmMonitor = class(TForm)
    btnReset: TButton;
    btnSkipIntro: TButton;
    GroupBox1: TGroupBox;
    lblC4State: TLabel;
    lblC3State: TLabel;
    lblC2State: TLabel;
    lblLives: TLabel;
    lblRound: TLabel;
    lblMQTTState: TLabel;
    lblC1State: TLabel;
    MQTTClient1: TMQTTClient;
    Panel1: TPanel;
    pnlSystemCommands: TPanel;
    pnlMQTTCommands: TPanel;
    pbVolume: TProgressBar;
    tmrStatusLabels: TTimer;
    volUpDown: TUpDown;
    procedure btnResetClick(Sender: TObject);
    procedure btnSkipIntroClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MQTTClient1ConnAck(Sender: TObject; ReturnCode: integer);
    procedure MQTTClient1Publish(Sender: TObject; topic, payload: ansistring);
    procedure tmrStatusLabelsTimer(Sender: TObject);
    procedure setMonitorLabelState(lbl: tlabel; level: integer; newcaption: string);
    procedure volUpDownClick(Sender: TObject; Button: TUDBtnType);
    function getBashCommandOutput(bashCommand: string; chomp: boolean = true):string;
    procedure checkVolume;
    procedure doCommand(commandTopic, commandText: string);
  private
    { private declarations }
  public
    { public declarations }
  var
    consoleChecker: TConsoleCheckerThread;
    MQTTManager: TMQTTManagerThread;
    soundDevice : string;
  const
    LEVEL_GOOD = 0;
    LEVEL_MINOR_PROBLEM = 1;
    LEVEL_BAD = 2;

  end;

var
  frmMonitor: TfrmMonitor;

implementation

{$R *.lfm}

{ TfrmMonitor }

procedure TfrmMonitor.doCommand(commandTopic, commandText: string);
begin
  if TfrmConfirm.shouldTakeAction(commandText) then
    if MQTTManager.connected then
      MQTTClient1.Publish('command/' + commandTopic, '');
end;

function TFrmMonitor.getBashCommandOutput(bashCommand: string; chomp: boolean = true): string;
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

procedure tfrmMonitor.checkVolume;
var
  curVolString : string;
  curVol: integer;
begin
  pbVolume.Min:=0;
  pbVolume.Max:=100;
  curVolString := getBashCommandOutput('amixer get ' + soundDevice + ' | grep Mono: | sed -e "s/[^[]*\[\([0-9]\+\)%.*/\1/"');
  curVol := strtoint(curVolString);
  pbVolume.Position:=curVol;
end;

procedure TfrmMonitor.btnSkipIntroClick(Sender: TObject);
begin
  doCommand('skip_intro', 'Skip the introduction sequence.');
end;

procedure TfrmMonitor.btnResetClick(Sender: TObject);
begin
  doCommand('stop_game', 'Stop the current game now, so a new game can be started.');
end;

procedure TfrmMonitor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  consoleChecker.shouldStop:=true;
end;

procedure TfrmMonitor.FormCreate(Sender: TObject);
begin
  soundDevice := getBashCommandOutput('amixer | head -n1 | sed -e "s/[^'']*''\([^'']*\)''.*/\1/"');
  checkVolume;
  consoleChecker := TConsoleCheckerThread.Create(false);
  MQTTManager := TMQTTManagerThread.Create(false);
  MQTTManager.client := MQTTClient1;
  //consoleChecker.shouldStop:=true; // Uncomment to prevent ping output getting in the way of debugging
  MQTTClient1.Connect;

  tmrStatusLabels.enabled := true;
end;

procedure TfrmMonitor.setMonitorLabelState(lbl: tlabel; level: integer; newcaption: string);
begin
  lbl.Caption:=lbl.Hint + #10 + newcaption;
  if level = LEVEL_GOOD then begin
    lbl.color := $00a0ffb1;
  end;
  if level = LEVEL_MINOR_PROBLEM then begin
    lbl.color := $0084FAFF;
  end;
  if level = LEVEL_BAD then begin
    lbl.color := $00a0a0ff;
  end;
end;

procedure TfrmMonitor.volUpDownClick(Sender: TObject; Button: TUDBtnType);
var curVol: integer;
begin
  curVol := strtoint(getBashCommandOutput('amixer get ' + soundDevice + ' | grep Mono: | sed -e "s/[^[]*\[\([0-9]\+\)%.*/\1/"'));
  if button = btNext then
    getBashCommandOutput('amixer set ' + soundDevice + ' -- ' + inttostr(curVol+2) + '%' );
  if button = btPrev then
    getBashCommandOutput('amixer set ' + soundDevice + ' -- ' + inttostr(curVol-2) + '%' );
  checkVolume;
end;

procedure TfrmMonitor.MQTTClient1ConnAck(Sender: TObject; ReturnCode: integer);
begin
  MQTTClient1.Subscribe('status/lives');
  MQTTClient1.Subscribe('status/round');
  MQTTClient1.Subscribe('clients/#');
end;

procedure TfrmMonitor.MQTTClient1Publish(Sender: TObject; topic,
  payload: ansistring);
var
    RegexObj: TRegExpr;
    id: integer;
begin
  writeln('got message on topic: ' + topic);
  if topic = 'status/lives' then
    lblLives.Caption:='Lives' + #10 + payload;
  if topic = 'status/round' then
    lblRound.Caption:='Round' + #10 + payload;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := 'clients/192\.168\.1\.3\d/heartbeat';
  if RegexObj.Exec(topic) then begin
    writeln('Seen a console');
    writeln('Seen a console');
        writeln('Seen a console');
            writeln('Seen a console');
    id := strtoint(topic[20])-1;
    consoleChecker.seenConsole(id);
  end;
  RegexObj.Free;
end;

procedure TfrmMonitor.tmrStatusLabelsTimer(Sender: TObject);
var i: integer;
  consoleLabelName:string;
  consoleLabel:TLabel;
begin
  checkvolume;
  if MQTTManager.connected then begin
    setMonitorLabelState(lblMQTTState, LEVEL_GOOD, 'Connected');
    pnlMQTTCommands.Enabled:=true;
  end else
  begin
    pnlMQTTCommands.Enabled:=false;
    setMonitorLabelState(lblMQTTState, LEVEL_BAD, 'Disconnected');
  end;

  for i:= 0 to 3 do begin
    consoleLabelName := 'lblC' + inttostr(i+1) + 'State';
    consoleLabel := TLabel(FindComponent(consoleLabelName));
    if (consoleChecker.states[i] = TConsoleCheckerThread.CONSOLE_UNKNOWN) or (consoleChecker.states[i] = TConsoleCheckerThread.CONSOLE_DOWN) then begin
      setMonitorLabelState(consoleLabel, LEVEL_BAD, 'Unreachable');
    end else
    if consoleChecker.states[i] = TConsoleCheckerThread.CONSOLE_UP then begin
      setMonitorLabelState(consoleLabel, LEVEL_MINOR_PROBLEM, 'Idle');
    end else
    if consoleChecker.states[i] = TConsoleCheckerThread.CONSOLE_RUNNING then begin
      setMonitorLabelState(consoleLabel, LEVEL_GOOD, 'Connected');
    end;
  end;
end;

end.

