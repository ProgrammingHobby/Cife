{*
 *  Copyright (C) 2023  Uwe Merker
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *}
unit File_Dialog;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, ButtonPanel, ExtCtrls, StdCtrls;

type
    TCfdType = (cfdOpenImage, cfdCreateNewImage, cfdFormatCurrentImage);

    { TFileDialog }

    TFileDialog = class(TForm)
        buttonOpenImageFile: TButton;
        buttonOpenBootTrackFile: TButton;
        ButtonPanel1: TButtonPanel;
        checkboxUseTimestamps: TCheckBox;
        editImageFile: TEdit;
        editBootTrackFile: TEdit;
        editFileSystemLabel: TEdit;
        comboboxImageType: TComboBox;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        labelDialogNotice: TLabel;
        panelSystemData: TPanel;
        panelImageFile: TPanel;
        panelDialogNotice: TPanel;
        procedure buttonOpenImageFileClick(Sender: TObject);
        procedure comboboxImageTypeChange(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure PanelPaint(Sender: TObject);
    private
        FDialogType: TCfdType;
        FRootPath: string;
        FDefaultPath: string;
        FWildcards: string;
        procedure PrepareImageTypesComboBox;
        procedure CheckImageFileData;

    public
        procedure SetDialogType(ADialogType: TCfdType);
        procedure SetDialogTitle(ATitle: string);
        procedure SetRootPath(ARootPath: string);
        procedure SetDefaultPath(ADefaultPath: string);
        procedure SetWildcards(AWildcards: string);
        function GetFullFileName: string;
        function GetImageType: string;

    end;

var
    FileDialog: TFileDialog;

implementation

{$R *.lfm}

{ TFileDialog }

uses XMLSettings, CifeGlobals, Dialogs, Math, LCLType;

// --------------------------------------------------------------------------------
procedure TFileDialog.PanelPaint(Sender: TObject);
const
    radius = 4;
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
    panel.Canvas.RoundRect(0, 0, panel.ClientWidth, panel.ClientHeight, radius, radius);
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.PrepareImageTypesComboBox;
var
    IndexI, NewWidth: integer;
begin
    with TXMLSettings.Create(SettingsFile) do begin

        try
            OpenKey('Settings');
            GetDiskDefsList(GetValue('DiskdefsFile', ''), comboboxImageType.Items);
            CloseKey;
        finally
            Free;
        end;

    end;

    comboboxImageType.Sorted := True;
    comboboxImageType.Canvas.Font.Assign(comboboxImageType.Font);
    NewWidth := 0;

    for IndexI := 0 to comboboxImageType.Items.Count - 1 do begin
        NewWidth := Max(comboboxImageType.Canvas.TextWidth(' ' + comboboxImageType.Items[IndexI]), NewWidth);
    end;

    Inc(NewWidth, MulDiv(41, ScreenInfo.PixelsPerInchX, 96));
    comboboxImageType.ClientWidth := NewWidth;

    if (comboboxImageType.Items.Count = 2) then begin
        IndexI := comboboxImageType.Items.IndexOf('Amstrad (PCW16)');
        comboboxImageType.TextHint := '';

        if (IndexI > 0) then begin
            Dec(IndexI);
        end
        else begin
            Inc(IndexI);
        end;

        comboboxImageType.ItemIndex := IndexI;
    end;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.CheckImageFileData;
begin
    if ((comboboxImageType.ItemIndex > -1) and (editImageFile.Hint <> '')) then begin
        ButtonPanel1.OKButton.Enabled := True;
    end;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.SetDialogType(ADialogType: TCfdType);
begin
    FDialogType := ADialogType;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.SetDialogTitle(ATitle: string);
begin
    self.Caption := ATitle;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.SetRootPath(ARootPath: string);
begin
    FRootPath := ARootPath;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.SetDefaultPath(ADefaultPath: string);
begin
    FDefaultPath := ADefaultPath;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.SetWildcards(AWildcards: string);
begin
    FWildcards := AWildcards;
end;

// --------------------------------------------------------------------------------
function TFileDialog.GetFullFileName: string;
begin
    Result := editImageFile.Hint;
end;

// --------------------------------------------------------------------------------
function TFileDialog.GetImageType: string;
begin
    Result := comboboxImageType.Items[comboboxImageType.ItemIndex];
end;


// --------------------------------------------------------------------------------
procedure TFileDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
procedure TFileDialog.buttonOpenImageFileClick(Sender: TObject);
var
    Dialog: TOpenDialog;
begin
    case (FDialogType) of

        cfdOpenImage: begin

            try
                Dialog := TOpenDialog.Create(self);
                Dialog.Title := 'Select CP/M Disk Image File';
                Dialog.Filter := FWildcards;

                if (FDefaultPath.IsEmpty) then begin

                    with TXMLSettings.Create(SettingsFile) do begin

                        try
                            OpenKey('Settings');
                            Dialog.InitialDir := ExtractFilePath(GetValue('DiskdefsFile', ''));
                            CloseKey;
                        finally
                            Free;
                        end;

                    end;

                end
                else begin
                    Dialog.InitialDir := FDefaultPath;
                end;

                if (Dialog.Execute) then begin
                    editImageFile.Hint := Dialog.FileName;
                    editImageFile.Text := ExtractFileName(Dialog.FileName);
                    editImageFile.SelStart := editImageFile.GetTextLen;
                end;
                CheckImageFileData;
            finally
                FreeAndNil(Dialog);
            end;

        end;

    end;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.comboboxImageTypeChange(Sender: TObject);
begin
    CheckImageFileData;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.FormCreate(Sender: TObject);
begin
    FDialogType := cfdOpenImage;
    ButtonPanel1.OKButton.Enabled := False;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.FormShow(Sender: TObject);
begin

    case (FDialogType) of

        cfdOpenImage: begin
            panelImageFile.Visible := True;
            panelImageFile.Align := alClient;
            panelSystemData.Visible := False;
            panelDialogNotice.Visible := False;
            PrepareImageTypesComboBox;
        end;

        cfdCreateNewImage: begin
            panelImageFile.Visible := True;
            panelImageFile.Align := alTop;
            panelSystemData.Visible := True;
            panelSystemData.Align := alTop;
            panelDialogNotice.Visible := True;
            panelDialogNotice.Align := alClient;
        end;

        cfdFormatCurrentImage: begin
            panelImageFile.Visible := False;
            panelSystemData.Visible := True;
            panelSystemData.Align := alTop;
            panelDialogNotice.Visible := True;
            panelDialogNotice.Align := alClient;
        end;

    end;

    with TXMLSettings.Create(SettingsFile) do begin
        try
            RestoreFormState(TForm(self));
        finally
            Free;
        end;
    end;

    SetAutoSize(True);
    Constraints.MinWidth := Width;
    Constraints.MinHeight := Height;
    Constraints.MaxHeight := Height;
    SetAutoSize(False);
end;

// --------------------------------------------------------------------------------
end.
