unit consoleform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  progrunner, confirmation;

type

  { TfrmConsole }

  TfrmConsole = class(TForm)
    btnRemoveFromGame: TButton;
    btnBack: TButton;
    btnSSH: TButton;
    btnRestartOS: TButton;
    btnShutdown: TButton;
    btnRestartclient: TButton;
    btnAddToGame: TButton;
    lblStatus: TLabel;
    lblConsoleName: TLabel;
    procedure btnBackClick(Sender: TObject);
    procedure btnRestartclientClick(Sender: TObject);
    procedure btnRestartOSClick(Sender: TObject);
    procedure btnShutdownClick(Sender: TObject);
    procedure btnSSHClick(Sender: TObject);
  private
    { private declarations }
    FCurConsole: integer;
    procedure setCurConsole(console: integer);
    procedure sendBashCommand(command: string; confirm: boolean = false; actionString: string = '');
  public
    { public declarations }
    property curConsole: integer read FCurConsole write setCurConsole;
  const
    timeoutCommand = 'timeout 3 ';
  end;

implementation

{$R *.lfm}

{ TfrmConsole }

procedure TfrmConsole.sendBashCommand(command: string; confirm: boolean = false; actionString: string = '');
begin
  if (confirm) then
    if not TfrmConfirm.shouldTakeAction(actionString) then
      exit;
  TProgRunner.getBashCommandOutput(timeoutCommand + 'ssh -oPasswordAuthentication=no root@192.168.1.3' + inttostr(FCurConsole) + ' ' + command);
end;

procedure TfrmConsole.setCurConsole(console: integer);
var colour: TColor;
begin
  FCurConsole := console;
  lblConsoleName.caption := 'Console ' + inttostr(FCurConsole);
  case FCurConsole of
    1: colour := $008eff92;
    2: colour := $00ffb98e;
    3: colour := $008efdff;
    4: colour := $008e8eff;
  end;
  lblConsoleName.Color:=colour;
end;

procedure TfrmConsole.btnBackClick(Sender: TObject);
begin
  close;
end;

procedure TfrmConsole.btnRestartclientClick(Sender: TObject);
begin
  sendBashCommand('systemctl restart consolegame.service', true, 'Restart the Spacehack game cient on the selected console');
end;

procedure TfrmConsole.btnRestartOSClick(Sender: TObject);
begin
  sendBashCommand('/sbin/reboot', true, 'Reboot the selected console');
end;

procedure TfrmConsole.btnShutdownClick(Sender: TObject);
begin
  sendBashCommand('/sbin/poweroff', true, 'Turn off the selected console');
end;

procedure TfrmConsole.btnSSHClick(Sender: TObject);
begin
  TProgRunner.startExternalProgram('xterm -e "ssh -oPasswordAuthentication=no root@192.168.1.3' + inttostr(FCurConsole)+'"');
end;

end.

