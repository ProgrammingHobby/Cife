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
unit Settings_Dialog;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, ComCtrls,
    ButtonPanel, ExtCtrls, StdCtrls, Spin;

type

    { TSettingsDialog }

    TSettingsDialog = class(TForm)
        buttonBrowseDiskdefsFile: TButton;
        ButtonPanel: TButtonPanel;
        checkboxKeepTimeStamps: TCheckBox;
        checkboxOpenLastImage: TCheckBox;
        checkboxUppercaseCpmCharacters: TCheckBox;
        editDiskdefsPath: TEdit;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        memoTextfileEndings: TMemo;
        Notebook: TNotebook;
        Page1: TPage;
        Page2: TPage;
        Panel1: TPanel;
        Panel4: TPanel;
        Panel2: TPanel;
        Panel3: TPanel;
        Panel5: TPanel;
        Panel6: TPanel;
        Panel7: TPanel;
        Panel8: TPanel;
        spineditUserNumber: TSpinEdit;
        Splitter: TSplitter;
        treeviewSettingPages: TTreeView;
        procedure buttonBrowseDiskdefsFileClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
        procedure PanelPaint(Sender: TObject);
        procedure treeviewSettingPagesSelectionChanged(Sender: TObject);
    private

    public

    end;

var
    SettingsDialog: TSettingsDialog;

implementation

{$R *.lfm}

uses XMLSettings, CifeGlobals, Dialogs;

{ TSettingsDialog }
// --------------------------------------------------------------------------------
procedure TSettingsDialog.PanelPaint(Sender: TObject);
const
    R = 4;  // Rundungsradius
var
    panel: TPanel;
begin
    if not (Sender is TPanel) then begin
        exit;
    end;
    panel := TPanel(Sender);
    panel.Canvas.Brush.Style := bsClear;
    panel.Canvas.Pen.Color := clSilver;
    panel.Canvas.Pen.Width := 1;
    panel.Canvas.RoundRect(0, 0, panel.ClientWidth, panel.ClientHeight, R, R);
end;

// --------------------------------------------------------------------------------
procedure TSettingsDialog.treeviewSettingPagesSelectionChanged(Sender: TObject);
var
    Index: integer;
begin
    Index := (Sender as TTreeView).Selected.AbsoluteIndex;

    if (Index = 0) then begin
        Inc(Index);
    end;
    Notebook.PageIndex := Index - 1;
end;

// --------------------------------------------------------------------------------
procedure TSettingsDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

    with TXMLSettings.Create(SettingsFile) do begin

        try

            if (ModalResult = mrOk) then begin
                OpenKey('Settings');
                SetValue('OpenLastImages', checkboxOpenLastImage.Checked);
                SetValue('UseUppercaseCharacters', checkboxUppercaseCpmCharacters.Checked);
                SetValue('KeepTimestamps', checkboxKeepTimeStamps.Checked);
                SetValue('DefaultUserNumber', spineditUserNumber.Value);
                SetValue('TextFileEndings', memoTextfileEndings.Text);
                SetValue('DiskdefsFile', editDiskdefsPath.Text);
                CloseKey;
            end;

            SaveFormState(TForm(self));
        finally
            Free;
        end;

    end;

    CloseAction := caFree;
end;

// --------------------------------------------------------------------------------
procedure TSettingsDialog.buttonBrowseDiskdefsFileClick(Sender: TObject);
var
    Dialog: TOpenDialog;
begin
    try
        Dialog := TOpenDialog.Create(self);
        Dialog.Title := 'Select CP/M Diskdefs File';
        Dialog.InitialDir := ExtractFilePath(editDiskdefsPath.Text);
        if (Dialog.Execute) then begin
            editDiskdefsPath.Text := Dialog.FileName;
            editDiskdefsPath.SelStart := editDiskdefsPath.GetTextLen;
        end;
    finally
        FreeAndNil(Dialog);
    end;
end;

// --------------------------------------------------------------------------------
procedure TSettingsDialog.FormShow(Sender: TObject);
var
    MinWidth, MinHeight: integer;
begin

    with TXMLSettings.Create(SettingsFile) do begin

        try
            OpenKey('Settings');
            checkboxOpenLastImage.Checked := GetValue('OpenLastImages', False);
            checkboxUppercaseCpmCharacters.Checked := GetValue('UseUppercaseCharacters', False);
            checkboxKeepTimeStamps.Checked := GetValue('KeepTimestamps', True);
            spineditUserNumber.Value := GetValue('DefaultUserNumber', 0);
            memoTextfileEndings.Text := GetValue('TextFileEndings', 'txt pip pas');
            editDiskdefsPath.Text := GetValue('DiskdefsFile', '');
            editDiskdefsPath.SelStart := editDiskdefsPath.GetTextLen;
            CloseKey;
            RestoreFormState(TForm(self));
        finally
            Free;
        end;

    end;

    CalculatePreferredSize(MinWidth, MinHeight, True);
    Constraints.MinWidth := MinWidth;
    Constraints.MinHeight := MinHeight;
    treeviewSettingPages.Items[1].Selected := True;
end;

end.
