{***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************}
unit Characteristics_Dialog;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
    ButtonPanel, ComCtrls, CifeGlobals;

type

    { TCharacteristicsDialog }

    TCharacteristicsDialog = class(TForm)
        ButtonPanel1: TButtonPanel;
        checkgroupAttributes: TCheckGroup;
        checkgroupProtections: TCheckGroup;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label7: TLabel;
        Label6: TLabel;
        Label8: TLabel;
        labelUserNumber: TLabel;
        labelCreated: TLabel;
        labelFileName: TLabel;
        labelFileSize: TLabel;
        labelLastAccess: TLabel;
        labelUpdated: TLabel;
        labelUsedRecords: TLabel;
        PageControl: TPageControl;
        panelFileData: TPanel;
        sheetGeneral: TTabSheet;
        sheetPermissions: TTabSheet;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
    private
        FOldAttributes: integer;
        procedure SetAttributes(AAttributes: integer);
        function GetAttributes: integer;

    public
        function GetNewAttributes: integer;
        procedure SetFileInfo(AFileInfo: TFileInfo);

    end;

var
    CharacteristicsDialog: TCharacteristicsDialog;

implementation

{$R *.lfm}

uses XMLSettings, CpmDefs;

{ TCharacteristicsDialog }

// --------------------------------------------------------------------------------
function TCharacteristicsDialog.GetNewAttributes: integer;
begin
    Result := GetAttributes;
end;

// --------------------------------------------------------------------------------
procedure TCharacteristicsDialog.SetFileInfo(AFileInfo: TFileInfo);
begin
    labelUserNumber.Caption := Format('%d', [AFileInfo.UserNumber]);
    labelFileName.Caption := AFileInfo.Name;
    labelFileSize.Caption := Format('%d bytes', [AFileInfo.UsedBytes]);
    labelUsedRecords.Caption := Format('%d', [AFileInfo.UsedRecords]);

    if (AFileInfo.MTime <> 0) then begin
        Label6.Visible := True;
        labelUpdated.Visible := True;
        labelUpdated.Caption := FormatDateTime('DD-MMM-YYYY HH:MM', AFileInfo.MTime);
    end;

    if (AFileInfo.CTime <> 0) then begin
        Label7.Visible := True;
        labelCreated.Visible := True;
        labelCreated.Caption := FormatDateTime('DD-MMM-YYYY HH:MM', AFileInfo.CTime);
    end;

    if (AFileInfo.ATime <> 0) then begin
        Label8.Visible := True;
        labelLastAccess.Visible := True;
        labelLastAccess.Caption := FormatDateTime('DD-MMM-YYYY HH:MM', AFileInfo.ATime);
    end;

    FOldAttributes := AFileInfo.Attributes;
    SetAttributes(AFileInfo.Attributes);
end;

// --------------------------------------------------------------------------------
procedure TCharacteristicsDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    with TXMLSettings.Create(SettingsFile) do begin

        try
            SaveFormState(TForm(self));
        finally
            Free;
        end;

    end;

    if ((FOldAttributes = GetAttributes) and (ModalResult = mrOk)) then begin
        ModalResult := mrCancel;
    end;

    CloseAction := caFree;
end;

// --------------------------------------------------------------------------------
procedure TCharacteristicsDialog.FormShow(Sender: TObject);
begin

    with TXMLSettings.Create(SettingsFile) do begin
        try
            RestoreFormState(TForm(self));
        finally
            Free;
        end;
    end;

    SetAutoSize(True);
    Constraints.MinWidth := Width;
    Constraints.MaxWidth := Width;
    Constraints.MinHeight := Height;
    Constraints.MaxHeight := Height;
    SetAutoSize(False);

    PageControl.ActivePage := sheetGeneral;

end;

// --------------------------------------------------------------------------------
procedure TCharacteristicsDialog.SetAttributes(AAttributes: integer);
begin
    checkgroupAttributes.Checked[0] := ((AAttributes and CPM_ATTR_F1) <> 0);
    checkgroupAttributes.Checked[1] := ((AAttributes and CPM_ATTR_RO) <> 0);
    checkgroupAttributes.Checked[2] := ((AAttributes and CPM_ATTR_F2) <> 0);
    checkgroupAttributes.Checked[3] := ((AAttributes and CPM_ATTR_SYS) <> 0);
    checkgroupAttributes.Checked[4] := ((AAttributes and CPM_ATTR_F3) <> 0);
    checkgroupAttributes.Checked[5] := ((AAttributes and CPM_ATTR_ARCV) <> 0);
    checkgroupAttributes.Checked[6] := ((AAttributes and CPM_ATTR_F4) <> 0);
    checkgroupProtections.Checked[0] := ((AAttributes and CPM_ATTR_PWREAD) <> 0);
    checkgroupProtections.Checked[1] := ((AAttributes and CPM_ATTR_PWWRITE) <> 0);
    checkgroupProtections.Checked[2] := ((AAttributes and CPM_ATTR_PWDEL) <> 0);
end;

// --------------------------------------------------------------------------------
function TCharacteristicsDialog.GetAttributes: integer;
begin
    Result := FOldAttributes;

    if (checkgroupAttributes.Checked[0]) then begin
        Result := Result or CPM_ATTR_F1;
    end
    else begin
        Result := Result and not CPM_ATTR_F1;
    end;

    if (checkgroupAttributes.Checked[1]) then begin
        Result := Result or CPM_ATTR_RO;
    end
    else begin
        Result := Result and not CPM_ATTR_RO;
    end;

    if (checkgroupAttributes.Checked[2]) then begin
        Result := Result or CPM_ATTR_F2;
    end
    else begin
        Result := Result and not CPM_ATTR_F2;
    end;

    if (checkgroupAttributes.Checked[3]) then begin
        Result := Result or CPM_ATTR_SYS;
    end
    else begin
        Result := Result and not CPM_ATTR_SYS;
    end;

    if (checkgroupAttributes.Checked[4]) then begin
        Result := Result or CPM_ATTR_F3;
    end
    else begin
        Result := Result and not CPM_ATTR_F3;
    end;

    if (checkgroupAttributes.Checked[5]) then begin
        Result := Result or CPM_ATTR_ARCV;
    end
    else begin
        Result := Result and not CPM_ATTR_ARCV;
    end;

    if (checkgroupAttributes.Checked[6]) then begin
        Result := Result or CPM_ATTR_F4;
    end
    else begin
        Result := Result and not CPM_ATTR_F4;
    end;

    if (checkgroupProtections.Checked[0]) then begin
        Result := Result or CPM_ATTR_PWREAD;
    end
    else begin
        Result := Result and not CPM_ATTR_PWREAD;
    end;

    if (checkgroupProtections.Checked[1]) then begin
        Result := Result or CPM_ATTR_PWWRITE;
    end
    else begin
        Result := Result and not CPM_ATTR_PWWRITE;
    end;

    if (checkgroupProtections.Checked[2]) then begin
        Result := Result or CPM_ATTR_PWDEL;
    end
    else begin
        Result := Result and not CPM_ATTR_PWDEL;
    end;

end;

end.
