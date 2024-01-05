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
unit Main_Window;

{$mode objfpc}
{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
    ActnList, ExtCtrls, StdCtrls, ImageFileHistory, ImagePage, CifeGlobals;

type

    { TMainWindow }

    TMainWindow = class(TForm)
        actionClearHistory: TAction;
        actionNew: TAction;
        actionDelete: TAction;
        actionCharacteristics: TAction;
        actionFormatCurrent: TAction;
        actionRefresh: TAction;
        actionCheckImage: TAction;
        actionSettings: TAction;
        actionAbout: TAction;
        actionOpen: TAction;
        actionClose: TAction;
        actionQuit: TAction;
        actionCut: TAction;
        actionCopy: TAction;
        actionPaste: TAction;
        actionSelectAll: TAction;
        actionRename: TAction;
        ActionList: TActionList;
        groupImageFile: TGroupBox;
        groupDirStatistics: TGroupBox;
        groupDriveData: TGroupBox;
        ImageList: TImageList;
        Label1: TLabel;
        Label10: TLabel;
        Label11: TLabel;
        Label12: TLabel;
        Label13: TLabel;
        Label14: TLabel;
        Label15: TLabel;
        Label16: TLabel;
        Label17: TLabel;
        labelType: TLabel;
        labelUsedDirEntries: TLabel;
        labelMaxDirEntries: TLabel;
        labelFilesFound: TLabel;
        labelTotalRecords: TLabel;
        labelTotal1kBlocks: TLabel;
        labelTotalBytes: TLabel;
        labelFile: TLabel;
        labelOs: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        Label9: TLabel;
        labelSkew: TLabel;
        labelOffset: TLabel;
        labelBootsectors: TLabel;
        labelMaxdir: TLabel;
        labelBlocksize: TLabel;
        labelSectorBytes: TLabel;
        labelSectors: TLabel;
        labelTracks: TLabel;
        MainMenu: TMainMenu;
        menuFile: TMenuItem;
        menuitemClearHistory: TMenuItem;
        menuitemRecentFiles: TMenuItem;
        PageControl: TPageControl;
        Panel1: TPanel;
        Panel2: TPanel;
        Panel3: TPanel;
        Panel4: TPanel;
        Panel5: TPanel;
        Panel6: TPanel;
        popupItemRefresh: TMenuItem;
        Separator10: TMenuItem;
        Separator8: TMenuItem;
        popupItemAttributes: TMenuItem;
        Separator9: TMenuItem;
        popupItemCheckImage: TMenuItem;
        Separator6: TMenuItem;
        popupItemCut: TMenuItem;
        popupItemCopy: TMenuItem;
        popupItemPaste: TMenuItem;
        popupItemSelectAll: TMenuItem;
        Separator7: TMenuItem;
        popupItemRename: TMenuItem;
        popupItemDelete: TMenuItem;
        menuItemCopy: TMenuItem;
        menuItemPaste: TMenuItem;
        menuItemSelectAll: TMenuItem;
        PopupMenu1: TPopupMenu;
        Separator3: TMenuItem;
        menuItemRename: TMenuItem;
        menuItemDelete: TMenuItem;
        Separator4: TMenuItem;
        menuItemCharacteristics: TMenuItem;
        Separator5: TMenuItem;
        menuItemNew: TMenuItem;
        menuItemFormatCurrent: TMenuItem;
        menuView: TMenuItem;
        menuItemRefresh: TMenuItem;
        menuTools: TMenuItem;
        menuItemCheckImage: TMenuItem;
        menuOptions: TMenuItem;
        menuSettings: TMenuItem;
        menuHelp: TMenuItem;
        menuItemAbout: TMenuItem;
        menuItemOpen: TMenuItem;
        Separator1: TMenuItem;
        menuItemClose: TMenuItem;
        Separator2: TMenuItem;
        menuItemQuit: TMenuItem;
        menuEdit: TMenuItem;
        menuItemCut: TMenuItem;
        StatusBar1: TStatusBar;
        procedure actionAboutExecute(Sender: TObject);
        procedure actionCharacteristicsExecute(Sender: TObject);
        procedure actionClearHistoryExecute(Sender: TObject);
        procedure actionCloseExecute(Sender: TObject);
        procedure actionDeleteExecute(Sender: TObject);
        procedure actionNewExecute(Sender: TObject);
        procedure actionOpenExecute(Sender: TObject);
        procedure actionQuitExecute(Sender: TObject);
        procedure actionRefreshExecute(Sender: TObject);
        procedure actionRenameExecute(Sender: TObject);
        procedure actionSettingsExecute(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
        procedure PageControlCloseTabClicked(Sender: TObject);
    private
        FImageFileHistory: TImageFileHistory;
        procedure AddImagePage(AImageFile: string; AImageType: string; ANewImage: boolean;
            ABootFile: string = ''; AFileSystemLabel: string = ''; ATimeStampsUsed: boolean = False);
        procedure HistoryMenuItemClick(Sender: TObject);
        procedure ShowImageInfo(AInfo: TFileSystemInfo);
        procedure ShowDirectoryStatistic(AStatistic: TDirStatistic);
        procedure MenuActionsControl(AMenuAction: TEnableAction);
    public

    end;

var
    MainWindow: TMainWindow;

implementation

{$R *.lfm}

uses File_Dialog, Settings_Dialog, About_Dialog, XMLSettings;

    { TMainWindow }

// --------------------------------------------------------------------------------
procedure TMainWindow.actionNewExecute(Sender: TObject);
var
    Dialog: TFileDialog;
    ImageFile, ImageType, BootFile, FileSystemLabel: string;
    TimeStampsUsed: boolean;
    HistoryEntry: THistoryEntry;
begin

    try
        Dialog := TFileDialog.Create(self);
        Dialog.SetDialogType(cfdCreateNewImage);
        Dialog.SetDialogTitle('Create new CP/M Disk Image File');
        Dialog.SetRootPath(GetUserDir);
        HistoryEntry := FImageFileHistory.GetHistoryEntry(0);
        Dialog.SetDefaultPath(ExtractFileDir(HistoryEntry.FileName));
        Dialog.SetWildcards('Image Files (*.img,*.bin,*.rel)|*.img;*.IMG;*.bin;*.BIN;*.rel;*.REL|all Files (*.*)|*');

        if (Dialog.ShowModal = mrOk) then begin
            ImageFile := Dialog.GetFullFileName;
            ImageType := Dialog.GetImageType;
            BootFile := Dialog.GetBootFileimage;
            FileSystemLabel := Dialog.GetFilesystemLabel;
            TimeStampsUsed := Dialog.GetTimestampsUsed;
            AddImagePage(ImageFile, ImageType, True, BootFile, FileSystemLabel, TimeStampsUsed);
        end;

    finally
        FreeAndNil(Dialog);
    end;

end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionCharacteristicsExecute(Sender: TObject);
var
    Page: TImagePage;
begin
    Page := PageControl.ActivePage as TImagePage;

    if (Assigned(Page)) then begin
        Page.ShowFileCharacteristics;
    end;

end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionClearHistoryExecute(Sender: TObject);
begin
    FImageFileHistory.Clear;
    actionClearHistory.Enabled := False;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionCloseExecute(Sender: TObject);
var
    Page: TImagePage;
begin
    MenuActionsControl([]);
    Page := PageControl.ActivePage as TImagePage;
    Page.Close;
    Page.Free;

    if (PageControl.PageCount <= 0) then begin
        actionClose.Enabled := False;
    end;

end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionDeleteExecute(Sender: TObject);
var
    Page: TImagePage;
begin
    Page := PageControl.ActivePage as TImagePage;

    if (Assigned(Page)) then begin
        Page.DeleteFile;
    end;

end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionAboutExecute(Sender: TObject);
var
    Dialog: TAboutDialog;
begin

    try
        Dialog := TAboutDialog.Create(self);
        Dialog.ShowModal;
    finally
        FreeAndNil(Dialog);
    end;

end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionOpenExecute(Sender: TObject);
var
    Dialog: TFileDialog;
    ImageFile, ImageType: string;
    HistoryEntry: THistoryEntry;
begin

    try
        Dialog := TFileDialog.Create(self);
        Dialog.SetDialogType(cfdOpenImage);
        Dialog.SetDialogTitle('Open CP/M Disk Image File');
        Dialog.SetRootPath(GetUserDir);
        HistoryEntry := FImageFileHistory.GetHistoryEntry(0);
        Dialog.SetDefaultPath(ExtractFileDir(HistoryEntry.FileName));
        Dialog.SetWildcards('Image Files (*.img,*.fdd,*.dsk)|*.img;*.IMG;*.fdd;*.FDD;*.dsk;*.DSK|all Files (*.*)|*');

        if (Dialog.ShowModal = mrOk) then begin
            ImageFile := Dialog.GetFullFileName;
            ImageType := Dialog.GetImageType;
            AddImagePage(ImageFile, ImageType, False);
        end;

    finally
        FreeAndNil(Dialog);
    end;

end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionQuitExecute(Sender: TObject);
begin
    Close;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionRefreshExecute(Sender: TObject);
var
    Page: TImagePage;
begin
    Page := PageControl.ActivePage as TImagePage;

    if (Assigned(Page)) then begin
        Page.RefreshDirectory;
    end;

end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionRenameExecute(Sender: TObject);
var
    Page: TImagePage;
begin
    Page := PageControl.ActivePage as TImagePage;

    if (Assigned(Page)) then begin
        Page.RenameFile;
    end;

end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionSettingsExecute(Sender: TObject);
var
    Dialog: TSettingsDialog;
    OldUseUpperCase: boolean;
    OldDiskdefsFile: string;
begin
    with TXMLSettings.Create(SettingsFile) do begin

        try
            OpenKey('Settings');
            OldUseUpperCase := GetValue('UseUppercaseCharacters', False);
            OldDiskdefsFile := GetValue('DiskdefsFile', '');
            CloseKey;
        finally
            Free;
        end;

    end;

    try
        Dialog := TSettingsDialog.Create(self);
        Dialog.ShowModal;
    finally
        FreeAndNil(Dialog);
    end;

    with TXMLSettings.Create(SettingsFile) do begin

        try
            OpenKey('Settings');

            if (OldUseUpperCase <> GetValue('UseUppercaseCharacters', False)) then begin
                (PageControl.ActivePage as TImagePage).RefreshDirectory;
            end;

            if (OldDiskdefsFile <> GetValue('DiskdefsFile', '')) and (FileExists(GetValue('DiskdefsFile', ''))) then begin
                actionNew.Enabled := True;
                actionOpen.Enabled := True;
                menuitemRecentFiles.Enabled := True;
                StatusBar1.SimpleText := GetValue('DiskdefsFile', '');
            end
            else begin
                actionNew.Enabled := False;
                actionOpen.Enabled := False;
                menuitemRecentFiles.Enabled := False;
                StatusBar1.SimpleText := '';
            end;

            CloseKey;
        finally
            Free;
        end;

    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    FImageFileHistory.Save;
    FreeAndNil(FImageFileHistory);

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
procedure TMainWindow.FormShow(Sender: TObject);
var
    LabelWidth: integer;
begin

    LabelWidth := labelTotalBytes.Canvas.GetTextWidth('#######');
    labelTotalBytes.Constraints.MinWidth := LabelWidth;
    labelTotal1kBlocks.Constraints.MinWidth := LabelWidth;
    Panel3.AutoSize := False;
    labelTotalRecords.Constraints.MinWidth := LabelWidth;
    labelFilesFound.Constraints.MinWidth := LabelWidth;
    Panel4.AutoSize := False;
    labelMaxDirEntries.Constraints.MinWidth := LabelWidth;
    labelUsedDirEntries.Constraints.MinWidth := LabelWidth;
    Panel5.AutoSize := False;

    LabelWidth := labelTracks.Canvas.GetTextWidth('#######');
    labelTracks.Constraints.MinWidth := LabelWidth;
    labelSectors.Constraints.MinWidth := LabelWidth;
    labelSectorBytes.Constraints.MinWidth := LabelWidth;
    labelBlocksize.Constraints.MinWidth := LabelWidth;
    labelMaxdir.Constraints.MinWidth := LabelWidth;
    labelBootsectors.Constraints.MinWidth := LabelWidth;
    labelOffset.Constraints.MinWidth := LabelWidth;
    labelSkew.Constraints.MinWidth := LabelWidth;
    labelOs.Constraints.MinWidth := LabelWidth;
    groupDriveData.AutoSize := False;

    SetAutoSize(True);
    Constraints.MinWidth := Width;
    Constraints.MinHeight := Height;
    SetAutoSize(False);

    with TXMLSettings.Create(SettingsFile) do begin

        try
            OpenKey('Settings');

            if (FileExists(GetValue('DiskdefsFile', ''))) then begin
                actionNew.Enabled := True;
                actionOpen.Enabled := True;
                menuitemRecentFiles.Enabled := True;
                StatusBar1.SimpleText := GetValue('DiskdefsFile', '');
            end
            else begin
                MessageDlg('Diskdefinitions file not found. Please select valid ''diskdefs'' file' +
                    ' on the Options -> Settings -> General-Settings page.'
                    , mtError, [mbOK], 0);
                actionNew.Enabled := False;
                actionOpen.Enabled := False;
                menuitemRecentFiles.Enabled := False;
                StatusBar1.SimpleText := '';
            end;

            CloseKey;
            RestoreFormState(TForm(self));
        finally
            Free;
        end;

    end;

    actionClose.Enabled := False;
    FImageFileHistory := TImageFileHistory.Create(menuitemRecentFiles);
    FImageFileHistory.SetHistoryMenuItemsEvent(@HistoryMenuItemClick);
    actionClearHistory.Enabled := FImageFileHistory.Load;
    MenuActionsControl([]);
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.PageControlCloseTabClicked(Sender: TObject);
begin

    if (Sender is TTabSheet) then begin
        TTabSheet(Sender).Free;
    end;

end;

// --------------------------------------------------------------------------------
procedure TMainWindow.AddImagePage(AImageFile: string; AImageType: string; ANewImage: boolean;
    ABootFile: string; AFileSystemLabel: string; ATimeStampsUsed: boolean);
var
    ImagePage: TImagePage;
begin
    ImagePage := TImagePage.Create(PageControl);
    ImagePage.SetFileSystemInfoCallBack(@ShowImageInfo);
    ImagePage.SetDirectoryStatisticCallBack(@ShowDirectoryStatistic);
    ImagePage.SetMenuActionCallBack(@MenuActionsControl);
    ImagePage.SetPopupMenu(PopupMenu1);

    if (ANewImage) then begin

        if not (ImagePage.New(AImageFile, AImageType, ABootFile, AFileSystemLabel, ATimeStampsUsed)) then begin
            FreeAndNil(ImagePage);
            exit;
        end;

    end;

    if (ImagePage.Open(AImageFile, AImageType)) then begin
        ImagePage.Caption := ExtractFileName(AImageFile);
        ImagePage.PageControl := PageControl;
        PageControl.ActivePage := ImagePage;

        if (PageControl.PageCount > 0) then begin
            actionClose.Enabled := True;
            actionClearHistory.Enabled := True;
        end;

        FImageFileHistory.AddItem(AImageFile, AImageType);
    end
    else begin
        FreeAndNil(ImagePage);
    end;

end;

// --------------------------------------------------------------------------------
procedure TMainWindow.HistoryMenuItemClick(Sender: TObject);
var
    HistoryMenuItem: TMenuItem;
    HistoryEntry: THistoryEntry;
    Index: integer;
    TabExisting: boolean;
    ImagePage: TImagePage;
begin

    if (Sender is TMenuItem) then begin
        HistoryMenuItem := TMenuItem(Sender);
        HistoryEntry := FImageFileHistory.GetHistoryEntry(HistoryMenuItem.Tag);
        TabExisting := False;

        for Index := 0 to (PageControl.PageCount - 1) do begin
            ImagePage := TImagePage(PageControl.Pages[Index]);

            if (ImagePage.GetFileName = HistoryEntry.FileName) then begin
                TabExisting := True;
                break;
            end;

        end;

        if TabExisting then begin
            PageControl.ActivePage := PageControl.Pages[Index];
        end
        else begin
            AddImagePage(HistoryEntry.FileName, HistoryEntry.FileType, False);
        end;

    end;

end;

// --------------------------------------------------------------------------------
procedure TMainWindow.ShowImageInfo(AInfo: TFileSystemInfo);
begin
    labelFile.Caption := AInfo.FileName;
    labelType.Caption := AInfo.FileType;
    labelTracks.Caption := AInfo.Tracks;
    labelSectors.Caption := AInfo.Sectors;
    labelSectorBytes.Caption := AInfo.SecBytes;
    labelBlocksize.Caption := AInfo.BlockSize;
    labelMaxdir.Caption := AInfo.MaxDir;
    labelBootsectors.Caption := AInfo.BootSectors;
    labelOffset.Caption := AInfo.Offset;
    labelSkew.Caption := AInfo.skew;
    labelOs.Caption := AInfo.System;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.ShowDirectoryStatistic(AStatistic: TDirStatistic);
begin
    labelTotalBytes.Caption := IntToStr(AStatistic.TotalBytes) + 'K';
    labelTotal1kBlocks.Caption := IntToStr(AStatistic.Total1KBlocks);
    labelTotalRecords.Caption := IntToStr(AStatistic.TotalRecords);
    labelFilesFound.Caption := IntToStr(AStatistic.FilesFound);
    labelMaxDirEntries.Caption := IntToStr(AStatistic.MaxDirEntries);
    labelUsedDirEntries.Caption := IntToStr(AStatistic.UsedDirEntries);
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.MenuActionsControl(AMenuAction: TEnableAction);
begin
    actionCut.Enabled := (MAcut in AMenuAction);
    actionCopy.Enabled := (MAcopy in AMenuAction);
    actionPaste.Enabled := (MApaste in AMenuAction);
    actionSelectAll.Enabled := (MAselectall in AMenuAction);
    actionRename.Enabled := (MArename in AMenuAction);
    actionDelete.Enabled := (MAdelete in AMenuAction);
    actionFormatCurrent.Enabled := (MAformat in AMenuAction);
    actionCharacteristics.Enabled := (MAcharacteristic in AMenuAction);
    actionRefresh.Enabled := (MArefresh in AMenuAction);
    actionCheckImage.Enabled := (MAcheck in AMenuAction);
end;

// --------------------------------------------------------------------------------
end.
