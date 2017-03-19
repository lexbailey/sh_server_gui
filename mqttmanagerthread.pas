unit mqttmanagerthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MQTTComponent;

type TMQTTManagerThread = class(TThread)
  private
    FClient: TMQTTClient;
    FShouldStop: boolean;
    FConnected:boolean;
  protected
    procedure Execute; override;
  public
    property client: TMQTTClient read FClient write FClient default nil;
    property shouldStop: boolean read FShouldStop write FShouldStop default false;
    property connected: boolean read FConnected default false;
  end;

implementation

procedure TMQTTManagerThread.Execute;
begin
  while not FShouldStop do begin
    if Fclient <> nil then begin
      writeln('check connecion');
      if not FClient.isConnected then
      begin
        FConnected:=false;
        FClient.Connect;
      end else
      begin
        if FClient.publish('status/monitorconnected', '1') then begin
          FConnected:=true;
        end else
        begin
          FConnected:=false;
          FClient.FullDisconnect;
          FClient.Connect;
        end;
      end;
    end;
    sleep(3000);
  end;
end;

end.

