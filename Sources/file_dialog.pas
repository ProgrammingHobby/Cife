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

{$mode ObjFPC}
{$H+}

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
        procedure buttonOpenBootTrackFileClick(Sender: TObject);
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
        FDefaultType: string;
        FBootTracks: boolean;
        procedure PrepareImageTypesComboBox;
        procedure CheckImageFileData;
    public
        procedure SetDialogType(ADialogType: TCfdType);
        procedure SetRootPath(ARootPath: string);
        procedure SetDefaultPath(ADefaultPath: string);
        procedure SetDefaultType(ADefaultType: string);
        procedure SetBoottracksUsed(AEnable: boolean);
        function GetFullFileName: string;
        function GetImageType: string;
        function GetBootFileimage: string;
        function GetFilesystemLabel: string;
        function GetTimestampsUsed: boolean;

    end;

var
    FileDialog: TFileDialog;

implementation

{$R *.lfm}

{ TFileDialog }

uses XMLSettings, CifeGlobals, Dialogs, Math, LCLType, ImageTypeInfo;

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
            GetDiskDefsList(GetValue('DiskdefsFile', ''), comboboxImageType);
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
procedure TFileDialog.SetDefaultType(ADefaultType: string);
begin
    FDefaultType := ADefaultType;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.SetBoottracksUsed(AEnable: boolean);
begin
    FBootTracks := AEnable;
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
function TFileDialog.GetBootFileimage: string;
begin
    Result := editBootTrackFile.Hint;
end;

// --------------------------------------------------------------------------------
function TFileDialog.GetFilesystemLabel: string;
begin
    Result := editFileSystemLabel.Text;
end;

// --------------------------------------------------------------------------------
function TFileDialog.GetTimestampsUsed: boolean;
begin
    Result := checkboxUseTimestamps.Checked;
end;


// --------------------------------------------------------------------------------
procedure TFileDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
    IndexI: integer;
begin
    with TXMLSettings.Create(SettingsFile) do begin

        try
            SaveFormState(TForm(self));
        finally
            Free;
        end;

    end;

    comboboxImageType.Items.BeginUpdate;

    for IndexI := 0 to comboboxImageType.Items.Count - 1 do begin
        comboboxImageType.Items.Objects[IndexI].Free;
    end;

    comboboxImageType.Items.EndUpdate;
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
                Dialog.Filter :=
                    'Image Files (*.img,*.fdd,*.hdd,*.dsk,*.bin,*.raw)|*.img;*.IMG;*.fdd;*.FDD;*.hdd;*.HDD;*.dsk;*.DSK;*.bin;*.BIN;*.raw;*.RAW|all Files (*.*)|*';

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

        cfdCreateNewImage: begin
            try
                Dialog := TSaveDialog.Create(self);
                Dialog.Title := 'Select new CP/M Disk Image File';
                Dialog.Filter :=
                    'Image Files (*.img,*.fdd,*.hdd, *.dsk)|*.img;*.IMG;*.fdd;*.FDD;*.hdd;*.HDD;*.dsk;*.DSK|all Files (*.*)|*';

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

        cfdFormatCurrentImage: begin

        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.buttonOpenBootTrackFileClick(Sender: TObject);
var
    Dialog: TOpenDialog;
begin
    try
        Dialog := TOpenDialog.Create(self);
        Dialog.Title := 'Select CP/M Boot-Image File';
        Dialog.Filter := 'Boot-Images (*.bin,*.rel)|*.bin;*.BIN;*.rel;*.REL|all Files (*.*)|*';

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
            editBootTrackFile.Hint := Dialog.FileName;
            editBootTrackFile.Text := ExtractFileName(Dialog.FileName);
            editBootTrackFile.SelStart := editImageFile.GetTextLen;
        end;

    finally
        FreeAndNil(Dialog);
    end;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.comboboxImageTypeChange(Sender: TObject);
var
    ImageTypeInfo: TImageTypeInfo;
begin
    CheckImageFileData;
    ImageTypeInfo := TImageTypeInfo(comboboxImageType.Items.Objects[comboboxImageType.ItemIndex]);
    editBootTrackFile.Enabled := ImageTypeInfo.BootTrackUsed;
    buttonOpenBootTrackFile.Enabled := ImageTypeInfo.BootTrackUsed;
    editFileSystemLabel.Enabled := ImageTypeInfo.LabelUsed;
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
            self.Caption := 'Open CP/M Disk Image File';
            panelImageFile.Visible := True;
            panelImageFile.Align := alClient;
            panelSystemData.Visible := False;
            panelDialogNotice.Visible := False;
            PrepareImageTypesComboBox;
        end;

        cfdCreateNewImage: begin
            self.Caption := 'Create new CP/M Disk Image File';
            panelImageFile.Visible := True;
            panelImageFile.Align := alTop;
            panelSystemData.Visible := True;
            panelSystemData.Align := alClient;
            panelDialogNotice.Visible := False;
            PrepareImageTypesComboBox;
        end;

        cfdFormatCurrentImage: begin
            self.Caption := 'Format current CP/M Disk Image File';
            panelImageFile.Visible := True;
            editImageFile.Enabled := False;
            editImageFile.Hint := FDefaultPath;
            editImageFile.Text := ExtractFileName(FDefaultPath);
            buttonOpenImageFile.Enabled := False;
            PrepareImageTypesComboBox;
            comboboxImageType.Enabled := False;
            comboboxImageType.Text := FDefaultType;
            comboboxImageType.ItemIndex := comboboxImageType.Items.IndexOf(FDefaultType);
            comboboxImageTypeChange(nil);
            panelSystemData.Visible := True;
            panelSystemData.Align := alTop;
            panelDialogNotice.Visible := True;
            panelDialogNotice.Align := alClient;
            labelDialogNotice.Caption := 'Are you sure you want recreate existing Image-File ? All existing Data will be lost !';
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
    Constraints.MaxWidth := Width;
    Constraints.MinHeight := Height;
    Constraints.MaxHeight := Height;
    SetAutoSize(False);
end;

// --------------------------------------------------------------------------------
end.
