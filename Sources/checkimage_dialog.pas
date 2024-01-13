unit CheckImage_Dialog;

{$mode ObjFPC}
{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ButtonPanel,
    StdCtrls;

type
    TCheckMessageCallBack = procedure(AMessage: string) of object;
    TCheckFunctionCallBack = procedure(ADoRepair: boolean; AMessage: TCheckMessageCallBack) of object;

    { TCheckImageDialog }

    TCheckImageDialog = class(TForm)
        buttonCheckImage: TButton;
        ButtonPanel1: TButtonPanel;
        checkRepairFilesystemErrors: TCheckBox;
        memoCheckMessages: TMemo;
        Panel1: TPanel;
        Panel2: TPanel;
        procedure buttonCheckImageClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
    private
        FCheckFunctionCallBack: TCheckFunctionCallBack;

    public
        procedure SetImageFile(AImageFile: string);
        procedure PrintCheckMessageLine(AValue: string);
        procedure SetCheckFunctionCallBack(ACheckFunctionCallBack: TCheckFunctionCallBack);
        //procedure SetCheckFunction;
        //procedure SetMessageFunction;

    end;

var
    CheckImageDialog: TCheckImageDialog;

implementation

{$R *.lfm}

uses XMLSettings, CifeGlobals, LCLIntf, LCLType;

    { TCheckImageDialog }

// --------------------------------------------------------------------------------
procedure TCheckImageDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    with TXMLSettings.Create(SettingsFile) do begin

        try
            SaveFormState(TForm(self));
        finally
            Free;
        end;

    end;

    CloseAction := caFree;
end;

// --------------------------------------------------------------------------------
procedure TCheckImageDialog.buttonCheckImageClick(Sender: TObject);
begin
    memoCheckMessages.Clear;
    if Assigned(FCheckFunctionCallBack) then begin
        FCheckFunctionCallBack(checkRepairFilesystemErrors.Checked, @PrintCheckMessageLine);
    end;
end;

// --------------------------------------------------------------------------------
procedure TCheckImageDialog.FormShow(Sender: TObject);
var
    CharWidth, CharHeight: integer;
    ScrollWidth: integer;
begin

    with TXMLSettings.Create(SettingsFile) do begin

        try
            RestoreFormState(TForm(self));
        finally
            Free;
        end;

    end;

    {$ifdef Windows}
    memoCheckMessages.Font.Name := 'Consolas';
    {$else}
    memoCheckMessages.Font.Name := 'Liberation Mono';
    {$endif}

    Canvas.Font := memoCheckMessages.Font;
    CharWidth := Canvas.Font.GetTextWidth('#');
    CharHeight := Canvas.Font.GetTextHeight('#');
    ScrollWidth := GetSystemMetrics(SM_CXVSCROLL);

    {$ifdef Windows}
    memoCheckMessages.Constraints.MinWidth := (40 * (CharWidth + 2)) + ScrollWidth;
    memoCheckMessages.Constraints.MinHeight := (16 * CharHeight) + 7;
    {$else}
    memoCheckMessages.Constraints.MinWidth := (40 * (CharWidth + 2)) + ScrollWidth;
    memoCheckMessages.Constraints.MinHeight := (16 * CharHeight) - 4;
    {$endif}
    memoCheckMessages.Clear;

    SetAutoSize(True);
    Constraints.MinWidth := Width;
    Constraints.MaxWidth := Width;
    Constraints.MinHeight := Height;
    Constraints.MaxHeight := Height;
    SetAutoSize(False);
end;

// --------------------------------------------------------------------------------
procedure TCheckImageDialog.SetImageFile(AImageFile: string);
begin
    self.Caption := 'Check CP/M-Image ''' + ExtractFileName(AImageFile) + '''';
end;

// --------------------------------------------------------------------------------
procedure TCheckImageDialog.PrintCheckMessageLine(AValue: string);
begin
    memoCheckMessages.Append(AValue);
end;

// --------------------------------------------------------------------------------
procedure TCheckImageDialog.SetCheckFunctionCallBack(ACheckFunctionCallBack: TCheckFunctionCallBack);
begin
    FCheckFunctionCallBack := ACheckFunctionCallBack;
end;

end.
