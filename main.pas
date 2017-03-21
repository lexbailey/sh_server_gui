unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, MQTTComponent, Process, consoleCheckerThread,
  mqttmanagerthread, regexpr, confirmation, consoleform, progrunner;

type

  { TfrmMonitor }

  TfrmMonitor = class(TForm)
    btnRebootPi: TButton;
    btnRestartGame: TButton;
    btnTerminal: TButton;
    btnReset: TButton;
    btnSkipIntro: TButton;
    btnPoweroffAll: TButton;
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
    Panel2: TPanel;
    pnlSystemCommands: TPanel;
    pnlMQTTCommands: TPanel;
    pbVolume: TProgressBar;
    tmrStatusLabels: TTimer;
    volUpDown: TUpDown;
    procedure btnPoweroffAllClick(Sender: TObject);
    procedure btnRebootPiClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnRestartGameClick(Sender: TObject);
    procedure btnSkipIntroClick(Sender: TObject);
    procedure btnTerminalClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure lblConsoleStateClick(Sender: TObject);
    procedure MQTTClient1ConnAck(Sender: TObject; ReturnCode: integer);
    procedure MQTTClient1Publish(Sender: TObject; topic, payload: ansistring);
    procedure tmrStatusLabelsTimer(Sender: TObject);
    class procedure setMonitorLabelState(lbl: tlabel; level: integer; newcaption: string; hintPrefix: boolean = true);
    procedure volUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure checkVolume;
    procedure doCommand(commandTopic, commandText: string);
  private
    { private declarations }
    frmConsole: TfrmConsole;
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
    timeoutCommand = 'timeout 3 ';
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



procedure tfrmMonitor.checkVolume;
var
  curVolString : string;
  curVol: integer;
begin
  pbVolume.Min:=0;
  pbVolume.Max:=100;
  curVolString := TProgRunner.getBashCommandOutput('amixer get ' + soundDevice + ' | grep Mono: | sed -e "s/[^[]*\[\([0-9]\+\)%.*/\1/"');
  curVol := strtoint(curVolString);
  pbVolume.Position:=curVol;
end;

procedure TfrmMonitor.btnSkipIntroClick(Sender: TObject);
begin
  doCommand('skip_intro', 'Skip the introduction sequence.');
end;

procedure TfrmMonitor.btnTerminalClick(Sender: TObject);
begin
  TProgRunner.startExternalProgram('xterm');
end;

procedure TfrmMonitor.btnResetClick(Sender: TObject);
begin
  doCommand('stop_game', 'Stop the current game now, so a new game can be started.');
end;

procedure TfrmMonitor.btnRestartGameClick(Sender: TObject);
begin
  if TfrmConfirm.shouldTakeAction('Restart Spacehack game server service') then
    TProgRunner.getBashCommandOutput(timeoutCommand + 'ssh -oPasswordAuthentication=no pi@192.168.1.30 sudo systemctl restart SpacehackServer.service');

end;

procedure TfrmMonitor.btnPoweroffAllClick(Sender: TObject);
begin
  if TfrmConfirm.shouldTakeAction('Shutdown all consoles and the server.') then
  begin
    TProgRunner.getBashCommandOutput(timeoutCommand + 'ssh -oPasswordAuthentication=no root@192.168.1.31 /sbin/poweroff');
    TProgRunner.getBashCommandOutput(timeoutCommand + 'ssh -oPasswordAuthentication=no root@192.168.1.32 /sbin/poweroff');
    TProgRunner.getBashCommandOutput(timeoutCommand + 'ssh -oPasswordAuthentication=no root@192.168.1.33 /sbin/poweroff');
    TProgRunner.getBashCommandOutput(timeoutCommand + 'ssh -oPasswordAuthentication=no root@192.168.1.34 /sbin/poweroff');
    TProgRunner.getBashCommandOutput(timeoutCommand + 'ssh -oPasswordAuthentication=no pi@192.168.1.30 sudo /sbin/poweroff');
  end;
end;

procedure TfrmMonitor.btnRebootPiClick(Sender: TObject);
begin
  if TfrmConfirm.shouldTakeAction('Reboot the spacehack server box') then
    TProgRunner.getBashCommandOutput(timeoutCommand + 'ssh -oPasswordAuthentication=no pi@192.168.1.30 sudo /sbin/reboot');
end;

procedure TfrmMonitor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  consoleChecker.shouldStop:=true;
end;

procedure TfrmMonitor.FormCreate(Sender: TObject);
begin
  frmConsole := nil;
  soundDevice := TProgRunner.getBashCommandOutput('amixer | head -n1 | sed -e "s/[^'']*''\([^'']*\)''.*/\1/"');
  checkVolume;
  consoleChecker := TConsoleCheckerThread.Create(false);
  MQTTManager := TMQTTManagerThread.Create(false);
  MQTTManager.client := MQTTClient1;
  //consoleChecker.shouldStop:=true; // Uncomment to prevent ping output getting in the way of debugging
  MQTTClient1.Connect;

  tmrStatusLabels.enabled := true;
end;

procedure TfrmMonitor.lblConsoleStateClick(Sender: TObject);
begin
  if frmConsole <> nil then
    frmConsole.close;
    frmConsole.free;
  frmConsole := TfrmConsole.Create(self);
  frmConsole.curConsole:=tlabel(sender).tag;
  frmConsole.show;
end;

class procedure TfrmMonitor.setMonitorLabelState(lbl: tlabel; level: integer; newcaption: string; hintPrefix: boolean = true);
begin
  if hintPrefix then
    lbl.Caption:=lbl.Hint + #10 + newcaption
  else
    lbl.Caption:=newcaption;
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
  curVol := strtoint(TProgRunner.getBashCommandOutput('amixer get ' + soundDevice + ' | grep Mono: | sed -e "s/[^[]*\[\([0-9]\+\)%.*/\1/"'));
  if button = btNext then
    TProgRunner.getBashCommandOutput('amixer set ' + soundDevice + ' -- ' + inttostr(curVol+2) + '%' );
  if button = btPrev then
    TProgRunner.getBashCommandOutput('amixer set ' + soundDevice + ' -- ' + inttostr(curVol-2) + '%' );
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
  level: integer;
  description: string;
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
      level := LEVEL_BAD;
      description:='Unreachable';
    end else
    if consoleChecker.states[i] = TConsoleCheckerThread.CONSOLE_UP then begin
      level := LEVEL_MINOR_PROBLEM;
      description:='Idle';
    end else
    if consoleChecker.states[i] = TConsoleCheckerThread.CONSOLE_RUNNING then begin
      level := LEVEL_GOOD;
      description:='Connected';
    end;
    setMonitorLabelState(consoleLabel, level, description);
    if (frmConsole <> nil) and (frmConsole.curConsole = consoleLabel.Tag) then
    begin
      setMonitorLabelState(frmConsole.lblStatus, level, description, false);
    end;
  end;
end;

end.

