unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, MQTTComponent, Process, consoleCheckerThread, MQTTComponentReadThread,
  regexpr;

type

  { TfrmMonitor }

  TfrmMonitor = class(TForm)
    btnSkipIntro: TButton;
    btnReset: TButton;
    lblC4State: TLabel;
    lblC3State: TLabel;
    lblC2State: TLabel;
    lblLives: TLabel;
    lblRound: TLabel;
    lblMQTTState: TLabel;
    lblC1State: TLabel;
    MQTTClient1: TMQTTClient;
    Panel1: TPanel;
    tmrStatusLabels: TTimer;
    procedure btnSkipIntroClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MQTTClient1ConnAck(Sender: TObject; ReturnCode: integer);
    procedure MQTTClient1Publish(Sender: TObject; topic, payload: ansistring);
    procedure tmrStatusLabelsTimer(Sender: TObject);
    procedure setMonitorLabelState(lbl: tlabel; level: integer; newcaption: string);
  private
    { private declarations }
  public
    { public declarations }
  var
    consoleChecker: TConsoleCheckerThread;
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

procedure TfrmMonitor.btnSkipIntroClick(Sender: TObject);
begin
  if MQTTClient1.isConnected then
    MQTTClient1.Publish('command/skip_intro', '')
end;

procedure TfrmMonitor.FormCreate(Sender: TObject);
var i: integer;
begin
  consoleChecker := TConsoleCheckerThread.Create(false);
  //consoleChecker.shouldStop:=true; // Uncomment to prevent ping output getting in the way of debugging
  MQTTClient1.Connect;
end;

procedure TfrmMonitor.setMonitorLabelState(lbl: tlabel; level: integer; newcaption: string);
begin
  lbl.Caption:=lbl.Hint + #10 + newcaption;
  if level = LEVEL_GOOD then begin
    lbl.color := clGreen;
  end;
  if level = LEVEL_MINOR_PROBLEM then begin
    lbl.color := clYellow;
  end;
  if level = LEVEL_BAD then begin
    lbl.color := clRed;
  end;
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


//'ping 192.168.1.32 -w1 -c1'

procedure TfrmMonitor.tmrStatusLabelsTimer(Sender: TObject);
var i: integer;
  consoleLabelName:string;
  consoleLabel:TLabel;
begin
  if not MQTTClient1.isConnected then
    begin
      setMonitorLabelState(lblMQTTState, LEVEL_BAD, 'Disconnected');
      MQTTClient1.Connect;
    end else
    begin
      if MQTTClient1.publish('status/monitorconnected', '1') then
        setMonitorLabelState(lblMQTTState, LEVEL_GOOD, 'Connected')
      else
        setMonitorLabelState(lblMQTTState, LEVEL_GOOD, 'Disconnected');
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

