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
        Label18: TLabel;
        Label19: TLabel;
        labelTotalFreeBytes: TLabel;
        labelTotalDiskBytes: TLabel;
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
        Panel7: TPanel;
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
        procedure actionCheckImageExecute(Sender: TObject);
        procedure actionClearHistoryExecute(Sender: TObject);
        procedure actionCloseExecute(Sender: TObject);
        procedure actionCopyExecute(Sender: TObject);
        procedure actionCutExecute(Sender: TObject);
        procedure actionDeleteExecute(Sender: TObject);
        procedure actionFormatCurrentExecute(Sender: TObject);
        procedure actionNewExecute(Sender: TObject);
        procedure actionOpenExecute(Sender: TObject);
        procedure actionPasteExecute(Sender: TObject);
        procedure actionQuitExecute(Sender: TObject);
        procedure actionRefreshExecute(Sender: TObject);
        procedure actionRenameExecute(Sender: TObject);
        procedure actionSettingsExecute(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
        procedure FormShow(Sender: TObject);
        procedure menuEditClick(Sender: TObject);
        procedure PageControlCloseTabClicked(Sender: TObject);
        procedure PopupMenu1Popup(Sender: TObject);
    private
        FImageFileHistory: TImageFileHistory;
        FCuttedFilesPage: integer;
        procedure AddImagePage(AImageFile: string; AImageType: string; ANewImage: boolean;
            ABootFile: string = ''; AFileSystemLabel: string = ''; ATimeStampsUsed: boolean = False);
        procedure HistoryMenuItemClick(Sender: TObject);
        procedure ShowImageInfo(AInfo: TFileSystemInfo);
        procedure ShowDirectoryStatistic(AStatistic: TDirStatistic);
        procedure MenuActionsControl(AMenuAction: TEnableAction);
        function IsTabExisting(AImageFile, AImageType: string): boolean;
        procedure CheckValidClipboardData;
        {$ifdef UNIX}
        function IsRegular(const AStrPath: string): boolean;
        {$endif}
    public

    end;

var
    MainWindow: TMainWindow;

implementation

{$R *.lfm}

uses File_Dialog, Settings_Dialog, About_Dialog, XMLSettings, Clipbrd, Types
    {$ifdef WINDOWS}
    , Windows
    {$else}
    , URIParser, BaseUnix
    {$endif}
    ;

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
        Dialog.SetRootPath(GetUserDir);
        HistoryEntry := FImageFileHistory.GetHistoryEntry(0);
        Dialog.SetDefaultPath(ExtractFileDir(HistoryEntry.FileName));

        if (Dialog.ShowModal = mrOk) then begin
            ImageFile := Dialog.GetFullFileName;
            ImageType := Dialog.GetImageType;
            BootFile := Dialog.GetBootFileimage;
            FileSystemLabel := Dialog.GetFilesystemLabel;
            TimeStampsUsed := Dialog.GetTimestampsUsed;

            if not IsTabExisting(ImageFile, ImageType) then begin
                AddImagePage(ImageFile, ImageType, True, BootFile, FileSystemLabel, TimeStampsUsed);
            end;

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
procedure TMainWindow.actionCheckImageExecute(Sender: TObject);
var
    Page: TImagePage;
begin
    Page := PageControl.ActivePage as TImagePage;

    if (Assigned(Page)) then begin
        Page.CheckImage;
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
    Page.CloseImage;
    Page.Free;

    if (PageControl.PageCount <= 0) then begin
        actionClose.Enabled := False;
        AllowDropFiles := False;
    end;

end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionCopyExecute(Sender: TObject);
var
    Page: TImagePage;
begin
    Page := PageControl.ActivePage as TImagePage;

    if (Assigned(Page)) then begin
        Page.CopyFiles(False);
    end;

end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionCutExecute(Sender: TObject);
var
    Page: TImagePage;
begin
    Page := PageControl.ActivePage as TImagePage;

    if (Assigned(Page)) then begin
        Page.CopyFiles(True);
        FCuttedFilesPage := Page.TabIndex;
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
procedure TMainWindow.actionFormatCurrentExecute(Sender: TObject);
var
    Page: TImagePage;
begin
    Page := PageControl.ActivePage as TImagePage;

    if (Assigned(Page)) then begin
        Page.FormatImage;
        Page.RefreshDirectory;
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
        Dialog.SetRootPath(GetUserDir);
        HistoryEntry := FImageFileHistory.GetHistoryEntry(0);
        Dialog.SetDefaultPath(ExtractFileDir(HistoryEntry.FileName));

        if (Dialog.ShowModal = mrOk) then begin
            ImageFile := Dialog.GetFullFileName;
            ImageType := Dialog.GetImageType;

            if not IsTabExisting(ImageFile, ImageType) then begin
                AddImagePage(ImageFile, ImageType, False);
            end;

        end;

    finally
        FreeAndNil(Dialog);
    end;

end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionPasteExecute(Sender: TObject);
var
    Page: TImagePage;
    {$ifdef WINDOWS}
    ClipboardFileList: HDROP;
    FileBuffer: PChar;
    BufferSize: integer;
    {$else}
    ClipboardFileList: TStringArray;
    FileBuffer: string;
    {$endif}
    IndexI: integer;
    FilesToPaste: TStringArray;
begin

    {$ifdef WINDOWS}
    if (Clipboard.HasFormat(CF_HDROP) and OpenClipboard(0)) then begin

        try
            ClipboardFileList := GetClipboardData(CF_HDROP);

            if ClipboardFileList <> 0 then begin

                for IndexI := 0 to (DragQueryFile(ClipboardFileList, $FFFFFFFF, nil, 0) - 1) do begin
                    BufferSize := DragQueryFile(ClipboardFileList, IndexI, nil, 0);
                    FileBuffer := StrAlloc(BufferSize + 1);

                    try

                        if (DragQueryFile(ClipboardFileList, IndexI, FileBuffer, BufferSize + 1) > 0) then begin

                            if (FileExists(FileBuffer)) then begin
                                SetLength(FilesToPaste, IndexI + 1);
                                FilesToPaste[IndexI] := FileBuffer;
                            end;

                        end;

                    finally
                        StrDispose(FileBuffer);
                    end;

                end;

            end;

        finally
            CloseClipboard;
        end;

    end;
    {$else}
    if Clipboard.HasFormat(CF_Text) then begin
        ClipboardFileList := Clipboard.AsText.Trim.Split(Chr($0A));

        for IndexI := 0 to (Length(ClipboardFileList) - 1) do begin
            FileBuffer := ParseURI(ClipboardFileList[IndexI]).Path + ParseURI(ClipboardFileList[IndexI]).Document;

            if (IsRegular(FileBuffer)) then begin
                SetLength(FilesToPaste, IndexI + 1);
                FilesToPaste[IndexI] := FileBuffer;
            end;

        end;

    end;
    {$endif}

    if (Length(FilesToPaste) > 0) then begin
        Page := PageControl.ActivePage as TImagePage;

        if (Assigned(Page)) then begin
            Page.PasteFiles(FilesToPaste);

            if (FCuttedFilesPage <> -1) then begin
                Page := PageControl.Pages[FCuttedFilesPage] as TImagePage;
                Page.DeleteCopiedFiles;
                FCuttedFilesPage := -1;
            end;

        end;

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
begin
    with TXMLSettings.Create(SettingsFile) do begin

        try
            OpenKey('Settings');
            OldUseUpperCase := GetValue('UseUppercaseCharacters', False);
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

            if (FileExists(GetValue('DiskdefsFile', ''))) then begin
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
procedure TMainWindow.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
    MousePoint: TPoint;
    Page: TImagePage;
    IndexI: integer;
    FilesToPaste: TStringArray;
    FileBuffer: string;
begin
    Page := PageControl.ActivePage as TImagePage;

    if (Assigned(Page)) then begin
        MousePoint := Page.ScreenToControl(Mouse.CursorPos);

        if Page.ClientRect.Contains(MousePoint) then begin

            {$ifdef WINDOWS}
            for IndexI := Low(FileNames) to High(FileNames) do begin
                FileBuffer := FileNames[IndexI];

                if not DirectoryExists(FileBuffer) then begin
                    SetLength(FilesToPaste, IndexI + 1);
                    FilesToPaste[IndexI] := FileBuffer;
                end;

            end;
            {$else}
            for IndexI := Low(FileNames) to High(FileNames) do begin
                FileBuffer := FileNames[IndexI];

                if (IsRegular(FileBuffer)) then begin
                    SetLength(FilesToPaste, IndexI + 1);
                    FilesToPaste[IndexI] := FileBuffer;
                end;

            end;
            {$endif}
            Page.PasteFiles(FilesToPaste);
        end;

    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.FormShow(Sender: TObject);
var
    LabelWidth: integer;
begin

    LabelWidth := labelTotalBytes.Canvas.GetTextWidth('######');
    labelTotalBytes.Constraints.MinWidth := LabelWidth;
    labelTotal1kBlocks.Constraints.MinWidth := LabelWidth;
    Panel3.AutoSize := False;
    labelTotalRecords.Constraints.MinWidth := LabelWidth;
    labelFilesFound.Constraints.MinWidth := LabelWidth;
    Panel4.AutoSize := False;
    labelMaxDirEntries.Constraints.MinWidth := LabelWidth;
    labelUsedDirEntries.Constraints.MinWidth := LabelWidth;
    Panel5.AutoSize := False;
    labelTotalFreeBytes.Constraints.MinWidth := LabelWidth;
    labelTotalDiskBytes.Constraints.MinWidth := LabelWidth;
    Panel7.AutoSize := False;

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
    FCuttedFilesPage := -1;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.menuEditClick(Sender: TObject);
begin
    CheckValidClipboardData;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.PageControlCloseTabClicked(Sender: TObject);
begin

    if (Sender is TTabSheet) then begin
        TTabSheet(Sender).Free;
    end;

end;

// --------------------------------------------------------------------------------
procedure TMainWindow.PopupMenu1Popup(Sender: TObject);
begin
    CheckValidClipboardData;
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

        if not (ImagePage.NewImage(AImageFile, AImageType, ABootFile, AFileSystemLabel, ATimeStampsUsed)) then begin
            FreeAndNil(ImagePage);
            exit;
        end;

    end;

    if (ImagePage.OpenImage(AImageFile, AImageType)) then begin
        ImagePage.Caption := ExtractFileName(AImageFile);
        ImagePage.PageControl := PageControl;
        PageControl.ActivePage := ImagePage;

        if (PageControl.PageCount > 0) then begin
            actionClose.Enabled := True;
            actionClearHistory.Enabled := True;
            AllowDropFiles := True;
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
    TestBox: TComboBox;
    IndexI: integer;
begin

    if (Sender is TMenuItem) then begin
        HistoryMenuItem := TMenuItem(Sender);
        HistoryEntry := FImageFileHistory.GetHistoryEntry(HistoryMenuItem.Tag);
        try
            TestBox := TComboBox.Create(nil);
            with TXMLSettings.Create(SettingsFile) do begin

                try
                    OpenKey('Settings');
                    GetDiskDefsList(GetValue('DiskdefsFile', ''), TestBox);
                    CloseKey;
                finally
                    Free;
                end;

            end;

            if (TestBox.Items.IndexOf(HistoryEntry.FileType) = -1) then begin
                if (MessageDlg('Not a valid Image-Type !' + LineEnding + 'Do you want to delete the History Entry ?',
                    mtConfirmation, [mbYes, mbNo], 0) = mrYes) then begin
                    FImageFileHistory.DeleteItem(HistoryMenuItem.Tag);
                end;
                exit;
            end;
        finally
            TestBox.Items.BeginUpdate;

            for IndexI := 0 to TestBox.Items.Count - 1 do begin
                TestBox.Items.Objects[IndexI].Free;
            end;

            TestBox.Items.EndUpdate;
            FreeAndNil(TestBox);
        end;

        if not IsTabExisting(HistoryEntry.FileName, HistoryEntry.FileType) then begin
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
    labelTotalBytes.Caption := AStatistic.TotalBytes;
    labelTotal1kBlocks.Caption := AStatistic.Total1KBlocks;
    labelTotalRecords.Caption := AStatistic.TotalRecords;
    labelFilesFound.Caption := AStatistic.FilesFound;
    labelMaxDirEntries.Caption := AStatistic.MaxDirEntries;
    labelUsedDirEntries.Caption := AStatistic.UsedDirEntries;
    labelTotalFreeBytes.Caption := AStatistic.TotalFreeBytes;
    labelTotalDiskBytes.Caption := AStatistic.TotalDiskBytes;
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
function TMainWindow.IsTabExisting(AImageFile, AImageType: string): boolean;
var
    Index: integer;
    ImagePage: TImagePage;
begin
    { #todo : Evtl. unterscheiden ob nur der Name oder auch der Typ geprüft werden soll. }
    Result := False;

    for Index := 0 to (PageControl.PageCount - 1) do begin
        ImagePage := TImagePage(PageControl.Pages[Index]);

        if (ImagePage.GetFileName = AImageFile) then begin
            PageControl.ActivePage := PageControl.Pages[Index];
            Result := True;
            break;
        end;

    end;

end;

// --------------------------------------------------------------------------------
procedure TMainWindow.CheckValidClipboardData;
var
    {$ifdef WINDOWS}
    ClipboardFileList: HDROP;
    FileBuffer: PChar;
    BufferSize: integer;
    {$else}
    ClipboardFileList: TStringArray;
    FileBuffer: string;
    {$endif}
    IndexI: integer;
begin
    actionPaste.Enabled := False;

    if (PageControl.PageCount >= 1) then begin

        {$ifdef WINDOWS}
        if (Clipboard.HasFormat(CF_HDROP) and OpenClipboard(0)) then begin

            try
                ClipboardFileList := GetClipboardData(CF_HDROP);

                if ClipboardFileList <> 0 then begin

                    for IndexI := 0 to (DragQueryFile(ClipboardFileList, $FFFFFFFF, nil, 0) - 1) do begin
                        BufferSize := DragQueryFile(ClipboardFileList, IndexI, nil, 0);
                        FileBuffer := StrAlloc(BufferSize + 1);

                        try

                            if (DragQueryFile(ClipboardFileList, IndexI, FileBuffer, BufferSize + 1) > 0) then begin

                                if (FileExists(FileBuffer)) then begin
                                    actionPaste.Enabled := True;
                                    break;
                                end;

                            end;

                        finally
                            StrDispose(FileBuffer);
                        end;

                    end;

                end;

            finally
                CloseClipboard;
            end;

        end;
        {$else}
        if Clipboard.HasFormat(CF_Text) then begin
            ClipboardFileList := Clipboard.AsText.Trim.Split(Chr($0A));

            for IndexI := 0 to (Length(ClipboardFileList) - 1) do begin
                FileBuffer := ParseURI(ClipboardFileList[IndexI]).Path + ParseURI(ClipboardFileList[IndexI]).Document;

                if (IsRegular(FileBuffer)) then begin
                    actionPaste.Enabled := True;
                    break;
                end;

            end;

        end;
        {$endif}

    end;
end;

{$ifdef UNIX}
// --------------------------------------------------------------------------------
function TMainWindow.IsRegular(const AStrPath: string): boolean;
var
    info: stat;
begin
    fplstat(AStrPath, @info);
    Result := (fpS_ISREG(info.st_mode));
end;
{$endif}

// --------------------------------------------------------------------------------
end.
