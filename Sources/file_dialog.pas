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
unit File_Dialog;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel,
    ComCtrls, ExtCtrls, Buttons, ShellCtrls, StdCtrls;

type

    { TFileDialog }

    TFileDialog = class(TForm)
        ButtonPanel: TButtonPanel;
        comboboxImageTypes: TComboBox;
        comboboxFileTypes: TComboBox;
        editFileName: TEdit;
        ImageList: TImageList;
        Label1: TLabel;
        labelFilePath: TLabel;
        Panel1: TPanel;
        buttonListView: TSpeedButton;
        buttonDetailView: TSpeedButton;
        buttonFolderUp: TSpeedButton;
        buttonHomeFolder: TSpeedButton;
        buttonNewFolder: TSpeedButton;
        Panel10: TPanel;
        Panel11: TPanel;
        Panel2: TPanel;
        Panel3: TPanel;
        Panel4: TPanel;
        Panel5: TPanel;
        Panel6: TPanel;
        Panel7: TPanel;
        Panel8: TPanel;
        Panel9: TPanel;
        ShellListView: TShellListView;
        ShellTreeView: TShellTreeView;
        Splitter: TSplitter;
        procedure buttonDetailViewClick(Sender: TObject);
        procedure buttonHomeFolderClick(Sender: TObject);
        procedure buttonListViewClick(Sender: TObject);
        procedure comboboxFileTypesChange(Sender: TObject);
        procedure comboboxImageTypesChange(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
        procedure Panel4Paint(Sender: TObject);
        procedure ShellListViewSelectItem(Sender: TObject; Item: TListItem; Selected: boolean);
        procedure ShellTreeViewClick(Sender: TObject);
    private
        FWildcardsList: TStringArray;
        procedure CheckOkButtonState;

    public
        procedure SetDialogTitle(title: string);
        procedure SetRootPath(path: string);
        procedure SetDefaultPath(path: string);
        procedure SetDefaultFile(filename: string);
        procedure SetFileWildcards(wildcards: string);
        function GetFullFileName: string;
        function GetImageType: string;

    end;

var
    FileDialog: TFileDialog;

implementation

{$R *.lfm}

{ TFileDialog }

uses XMLSettings, CifeGlobals;

// --------------------------------------------------------------------------------
procedure TFileDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    with TXMLSettings.Create(SettingsFile) do begin
        try
            SaveFormState(TForm(self));
            SetAttribute('Forms/' + self.Name + '/Splitterpos', Splitter.GetSplitterPosition);
        finally
            Free;
        end;
    end;
    CloseAction := caFree;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.comboboxFileTypesChange(Sender: TObject);
var
    cmb: TComboBox;
begin
    if (Sender is TComboBox) then begin
        cmb := TComboBox(Sender);
        ShellListView.Mask := FWildcardsList[(cmb.ItemIndex * 2) + 1];
        ShellListView.Refresh;
        ShellListView.Sort;
    end;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.comboboxImageTypesChange(Sender: TObject);
begin
    CheckOkButtonState;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.buttonListViewClick(Sender: TObject);
begin
    ShellListView.ViewStyle := vsIcon;
    ShellListView.ShowColumnHeaders := False;
    ShellListView.Repaint;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.buttonDetailViewClick(Sender: TObject);
begin
    ShellListView.ViewStyle := vsReport;
    ShellListView.ShowColumnHeaders := True;
    ShellListView.Repaint;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.buttonHomeFolderClick(Sender: TObject);
begin
    ShellTreeView.Path := ShellTreeView.GetRootPath;
    labelFilePath.Caption := ShellTreeView.Path;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.FormShow(Sender: TObject);
begin
    GetDiskDefsList(comboboxImageTypes.Items);
    SetAutoSize(False);
    Constraints.MinWidth := Width;
    Constraints.MinHeight := Height;
    with TXMLSettings.Create(SettingsFile) do begin
        try
            RestoreFormState(TForm(self));
            Splitter.SetSplitterPosition(GetAttribute('Forms/' + self.Name + '/Splitterpos', 196));
        finally
            Free;
        end;
    end;
    ButtonPanel.OKButton.Enabled := False;
    buttonNewFolder.Enabled := False;
    editFileName.ReadOnly := True;
    ShellListView.Sort;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.Panel4Paint(Sender: TObject);
const
    Radius = 4;
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
    panel.Canvas.RoundRect(0, 0, panel.ClientWidth, panel.ClientHeight, Radius, Radius);
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.ShellListViewSelectItem(Sender: TObject; Item: TListItem; Selected: boolean);
begin
    if (Sender is TShellListView) then begin
        if (Selected) then begin
            editFileName.Caption := Item.Caption;
        end;
        CheckOkButtonState;
    end;
end;

procedure TFileDialog.ShellTreeViewClick(Sender: TObject);
var
    Stv: TShellTreeView;
begin
    if (Sender is TShellTreeView) then begin
        Stv := TShellTreeView(Sender);
        labelFilePath.Caption := Stv.Path;
        ShellListView.Sort;
    end;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.CheckOkButtonState;
begin
    if (editFileName.Caption = '') or (comboboxImageTypes.ItemIndex = -1) then begin
        ButtonPanel.OKButton.Enabled := False;
    end
    else begin
        ButtonPanel.OKButton.Enabled := True;
    end;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.SetDialogTitle(title: string);
begin
    Caption := title;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.SetRootPath(path: string);
begin
    ShellTreeView.Root := path;
    labelFilePath.Caption := path;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.SetDefaultPath(path: string);
begin
    ShellTreeView.Path := path;
    labelFilePath.Caption := path;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.SetDefaultFile(filename: string);
begin
    editFileName.Text := filename;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.SetFileWildcards(wildcards: string);
var
    idx: integer;
begin
    comboboxFileTypes.Clear;
    FWildcardsList := wildcards.Split('|');
    idx := 0;
    while (idx < Length(FWildcardsList)) do begin
        comboboxFileTypes.Items.Add(FWildcardsList[idx]);
        Inc(idx, 2);
    end;
    comboboxFileTypes.ItemIndex := 0;
    comboboxFileTypesChange(comboboxFileTypes);
end;

// --------------------------------------------------------------------------------
function TFileDialog.GetFullFileName: string;
begin
    Result := '';
    if Assigned(ShellListView.Selected) then begin
        Result := IncludeTrailingPathDelimiter(ShellListView.Root) + ShellListView.Selected.Caption;
    end;
end;

// --------------------------------------------------------------------------------
function TFileDialog.GetImageType: string;
begin
    Result := '';
    if (comboboxImageTypes.ItemIndex > -1) then begin
        Result := comboboxImageTypes.Items[comboboxImageTypes.ItemIndex];
    end;
end;

// --------------------------------------------------------------------------------
end.
