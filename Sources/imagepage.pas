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
unit ImagePage;

{$mode ObjFPC}
{$H+}

interface

uses
    Classes, SysUtils, Menus, Controls, ComCtrls, CpmTools, CifeGlobals;

type
    TMenuAction = (MAcut, MAcopy, MApaste, MAselectall, MArename, MAdelete, MAformat, MAcharacteristic, MArefresh, MAcheck);
    TEnableAction = set of TMenuAction;
    TFileSystemInfoCB = procedure(AInfo: TFileSystemInfo) of object;
    TDirectoryStatisticCB = procedure(AStatistic: TDirStatistic) of object;
    TMenuActionEnableCB = procedure(AEnableAction: TEnableAction) of object;

    { TImagePage }

    TImagePage = class(TTabSheet)
    public    // Attribute

    public    // Methoden
        procedure DoShow; override;
        procedure SetFileSystemInfoCallBack(AFileSystemInfoCB: TFileSystemInfoCB);
        procedure SetDirectoryStatisticCallBack(ADirectoryStatisticCB: TDirectoryStatisticCB);
        procedure SetMenuActionCallBack(AMenuActionEnableCB: TMenuActionEnableCB);
        procedure SetPopupMenu(APopupMenu: TPopupMenu);
        function OpenImage(const AFileName: string; const AFileType: string): boolean;
        function NewImage(AImageFile: string; AImageType: string; ABootFile: string; AFileSystemLabel: string;
            ATimeStampsUsed: boolean): boolean;
        procedure FormatImage;
        function CloseImage: boolean;
        function GetFileName: string;
        procedure RefreshDirectory;
        procedure RenameFile;
        procedure DeleteFile;
        procedure ShowFileCharacteristics;
        procedure CheckImage;
        procedure PasteFiles(const AFiles: TStringArray);
        procedure CopyFiles(ADoCut: boolean);
        procedure DeleteCopiedFiles;
    public  // Konstruktor/Destruktor
        constructor Create(ATheOwner: TComponent); override;
        destructor Destroy; override;

    protected // Attribute

    protected // Methoden 
        procedure DirectoryListResize(ASender: TObject);
        procedure DirectorySelectItem(Sender: TObject; Item: TListItem; Selected: boolean);
        procedure DirectoryListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
        procedure DirectoryItemDblClick(ASender: TObject);

    private   // Attribute
        FDirectoryList: TListView;
        FDirPopupMenu: TPopupMenu;
        FCpmTools: TCpmTools;
        FFileSystemInfoCallBack: TFileSystemInfoCB;
        FDirStatisticCallBack: TDirectoryStatisticCB;
        FMenuActionEnableCallBack: TMenuActionEnableCB;
        FEnableAction: TEnableAction;
        FTempFolder: string;
        FCuttedFiles: TStringList;

    private   // Methoden
        procedure CreateDirectoryListView;
        procedure ClearFileSystemInfo;
        procedure ClearDirectoryStatistics;
        procedure PrintDirectoryEntry(AColumn: integer; ARow: integer; AData: string);
        function CreateTempFolderName(ALength: integer): string;
        procedure ReadCpmFile(ACpmFileName: string; ATmpFileName: string; AIsTextFile: boolean; APreserveTimeStamps: boolean);
        procedure CopyFilesToClipboard(AFileList: TStringList);

    end;

implementation

{ TImagePage }

uses StdCtrls, RenameFile_Dialog, StrUtils, Graphics, XMLSettings, Dialogs, Characteristics_Dialog,
    File_Dialog, CheckImage_Dialog, Math, FileUtil, ClipBrd, FileHexEdit_Dialog

    {$ifdef UNIX}
    , BaseUnix, URIParser, DateUtils
    {$endif}

    {$ifdef WINDOWS}
    , Windows, ShlObj
    {$endif}
    ;

// --------------------------------------------------------------------------------
procedure TImagePage.DoShow;
begin
    inherited DoShow;
    RefreshDirectory;

    if Assigned(FFileSystemInfoCallBack) then begin
        FFileSystemInfoCallBack(FCpmTools.GetFileSystemInfo);
    end;

    FEnableAction := [MArefresh, MAformat, MAcheck];

    if (FDirectoryList.Items.Count > 0) then begin
        FEnableAction := FEnableAction + [MAselectall];
    end;

    if Assigned(FMenuActionEnableCallBack) then begin
        FMenuActionEnableCallBack(FEnableAction);
    end;

end;

// --------------------------------------------------------------------------------
procedure TImagePage.SetFileSystemInfoCallBack(AFileSystemInfoCB: TFileSystemInfoCB);
begin
    FFileSystemInfoCallBack := AFileSystemInfoCB;
end;

// --------------------------------------------------------------------------------
procedure TImagePage.SetDirectoryStatisticCallBack(ADirectoryStatisticCB: TDirectoryStatisticCB);
begin
    FDirStatisticCallBack := ADirectoryStatisticCB;
end;

// --------------------------------------------------------------------------------
procedure TImagePage.SetMenuActionCallBack(AMenuActionEnableCB: TMenuActionEnableCB);
begin
    FMenuActionEnableCallBack := AMenuActionEnableCB;
end;

procedure TImagePage.SetPopupMenu(APopupMenu: TPopupMenu);
begin
    FDirPopupMenu := APopupMenu;
end;

// --------------------------------------------------------------------------------
function TImagePage.OpenImage(const AFileName: string; const AFileType: string): boolean;
var
    UpperCase: boolean;
begin
    UpperCase := False;

    with TXMLSettings.Create(SettingsFile) do begin
        try
            OpenKey('Settings');
            UpperCase := GetValue('UseUppercaseCharacters', False);
            CloseKey;
        finally
            Free;
        end;
    end;

    Result := FCpmTools.OpenImage(AFileName, AFileType, UpperCase);
end;

// --------------------------------------------------------------------------------
function TImagePage.NewImage(AImageFile: string; AImageType: string; ABootFile: string; AFileSystemLabel: string;
    ATimeStampsUsed: boolean): boolean;
var
    UseUpperCase: boolean;
begin
    with TXMLSettings.Create(SettingsFile) do begin

        try
            OpenKey('Settings');
            UseUpperCase := GetValue('UseUppercaseCharacters', False);
            CloseKey;
        finally
            Free;
        end;

    end;

    Result := FCpmTools.CreateNewImage(AImageFile, AImageType, ABootFile, AFileSystemLabel, ATimeStampsUsed, UseUpperCase);
end;

// --------------------------------------------------------------------------------
procedure TImagePage.FormatImage;
var
    Info: TFileSystemInfo;
    Dialog: TFileDialog;
    BootFile, FileSystemLabel: string;
    TimeStampsUsed, UseUpperCase: boolean;
begin
    Info := FCpmTools.GetFileSystemInfo;

    try
        Dialog := TFileDialog.Create(self);
        Dialog.SetDialogType(cfdFormatCurrentImage);
        Dialog.SetDefaultPath(Info.FileName);
        Dialog.SetDefaultType(Info.FileType);
        Dialog.SetBoottracksUsed(StrToInt(Info.BootSectors) > 0);

        if (Dialog.ShowModal = mrOk) then begin
            BootFile := Dialog.GetBootFileimage;
            FileSystemLabel := Dialog.GetFilesystemLabel;
            TimeStampsUsed := Dialog.GetTimestampsUsed;

            with TXMLSettings.Create(SettingsFile) do begin

                try
                    OpenKey('Settings');
                    UseUpperCase := GetValue('UseUppercaseCharacters', False);
                    CloseKey;
                finally
                    Free;
                end;

            end;

            FCpmTools.CreateNewImage(Info.FileName, Info.FileType, BootFile, FileSystemLabel, TimeStampsUsed, UseUpperCase);
            OpenImage(Info.FileName, Info.FileType);

        end;

    finally
        FreeAndNil(Dialog);
    end;

end;

// --------------------------------------------------------------------------------
function TImagePage.CloseImage: boolean;
begin
    Result := FCpmTools.CloseImage;
end;

// --------------------------------------------------------------------------------
function TImagePage.GetFileName: string;
begin
    Result := FCpmTools.GetFileSystemInfo.FileName;
end;

// --------------------------------------------------------------------------------
procedure TImagePage.RefreshDirectory;
var
    UpperCase: boolean;
begin
    { #todo : evtl. selektierte Items nach dem Refresh wieder herstellen. }

    with TXMLSettings.Create(SettingsFile) do begin
        try
            UpperCase := GetValue('Settings/UseUppercaseCharacters', False);
        finally
            Free;
        end;
    end;

    FCpmTools.RefreshDirectory(UpperCase);

    if Assigned(FDirStatisticCallBack) then begin
        FDirStatisticCallBack(FCpmTools.GetDirectoryStatistic);
    end;
end;

// --------------------------------------------------------------------------------
procedure TImagePage.RenameFile;
var
    Dialog: TRenameFileDialog;
    OldName, NewName: string;
begin
    try
        Dialog := TRenameFileDialog.Create(self);
        OldName := DelSpace(FDirectoryList.Selected.Caption);
        Dialog.SetOldName(OldName);
        if (Dialog.ShowModal = mrOk) then begin
            NewName := Dialog.GetNewName;
            if (FCpmTools.RenameFile(OldName, NewName)) then begin
                RefreshDirectory;
            end;
        end;
    finally
        FreeAndNil(Dialog);
    end;
end;

// --------------------------------------------------------------------------------
procedure TImagePage.DeleteFile;
var
    FileList: TStringList;
    IndexI: integer;
begin

    if MessageDlg('Delete File/s', 'Are you sure you want delete selected File/s ?', mtConfirmation, [mbYes, mbNo], 0) =
        mrYes then begin

        try
            FileList := TStringList.Create;

            for IndexI := 0 to FDirectoryList.Items.Count - 1 do begin

                if (FDirectoryList.Items[IndexI].Selected) then begin
                    FileList.Add(DelSpace(FDirectoryList.Items[IndexI].Caption));
                end;

            end;

            FCpmTools.DeleteFile(FileList);
            RefreshDirectory;
        finally
            FreeAndNil(FileList);
        end;

    end;
end;

// --------------------------------------------------------------------------------
procedure TImagePage.ShowFileCharacteristics;
var
    Dialog: TCharacteristicsDialog;
    FileName: string;
begin

    try
        Dialog := TCharacteristicsDialog.Create(self);
        FileName := DelSpace(FDirectoryList.Selected.Caption);
        Dialog.SetFileInfo(FCpmTools.GetFileInfo(FileName));
        if (Dialog.ShowModal = mrOk) then begin
            FCpmTools.SetNewAttributes(FileName, dialog.GetNewAttributes);
            RefreshDirectory;
        end;
    finally
        FreeAndNil(Dialog);
    end;

end;

// --------------------------------------------------------------------------------
procedure TImagePage.CheckImage;
var
    Dialog: TCheckImageDialog;
begin

    try
        Dialog := TCheckImageDialog.Create(self);
        Dialog.SetImageFile(FCpmTools.GetFileSystemInfo.FileName);
        Dialog.SetCheckFunctionCallBack(@FCpmTools.CheckImage);
        Dialog.ShowModal;
    finally
        FreeAndNil(Dialog);
    end;

end;

// --------------------------------------------------------------------------------
procedure TImagePage.PasteFiles(const AFiles: TStringArray);
var
    IndexI: integer;
    FileToPaste, FileName, FileExtension: string;
    UserNumber: integer;
    PreserveTimeStamps: boolean;
    ConvertTextFiles: boolean;
    TextfileEndings: string;
    IsTextFile: boolean;
    UnixFile: file of byte;
    UnixFileSize: longword;
    CpmName: string[15];
    Buffer: array of byte;
    Times: TUTimeBuf;

    {$ifdef UNIX}
    StatBuf: stat;
    {$endif}

    {$ifdef WINDOWS}
    FileAttr: TWIN32FILEATTRIBUTEDATA;
    SystemTime, LocalTime: TSystemTime;
    {$endif}

begin

    with TXMLSettings.Create(SettingsFile) do begin

        try
            OpenKey('Settings');
            PreserveTimeStamps := GetValue('KeepTimestamps', True);
            UserNumber := GetValue('DefaultUserNumber', 0);
            ConvertTextFiles := GetValue('ConvertTextFiles', False);
            TextfileEndings := GetValue('TextFileEndings', 'txt pip pas');
            CloseKey;
        finally
            Free;
        end;

    end;

    for IndexI := 0 to (Length(AFiles) - 1) do begin
        FileToPaste := AFiles[IndexI];
        FileName := LeftStr(ExtractFileName(FileToPaste), Pos('.', ExtractFileName(FileToPaste)) - 1);
        FileExtension := RightStr(ExtractFileName(FileToPaste), (Length(ExtractFileName(FileToPaste)) -
            Pos('.', ExtractFileName(FileToPaste))));
        IsTextFile := (ConvertTextFiles and TextFileEndings.Contains(FileExtension));

        if (FileExists(FileToPaste)) then begin

            try
                AssignFile(UnixFile, FileToPaste);
                Reset(UnixFile, 1);
                UnixFileSize := FileSize(UnixFile);
            except

                on e: Exception do begin
                    MessageDlg(Format('can not open %s' + LineEnding + '%s', [ExtractFileName(FileToPaste), e.Message]),
                        mtError, [mbOK], 0);
                    break;
                end;

            end;

            CpmName := Format('%.2d%s.%s', [UserNumber, LeftStr(FileName, 8), LeftStr(FileExtension, 3)]);

            try
                SetLength(Buffer, UnixFileSize);
                BlockRead(UnixFile, Buffer[0], UnixFileSize);
            except

                on e: Exception do begin
                    MessageDlg(Format('can not read %s from disk' + LineEnding + '%s',
                        [ExtractFileName(FileToPaste), e.Message]),
                        mtError, [mbOK], 0);
                    exit;
                end;

            end;

            if (PreserveTimeStamps) then begin

                {$ifdef UNIX}
                FpStat(FileToPaste, StatBuf);
                Times.AcTime := FileDateToDateTime(StatBuf.st_atime);
                Times.ModTime := FileDateToDateTime(StatBuf.st_mtime);
                {$endif}

                {$ifdef WINDOWS}
                GetFileAttributesEx(PChar(FileToPaste), GetFileExInfoStandard, @FileAttr);
                FileTimeToSystemTime(FileAttr.ftLastAccessTime, SystemTime);
                SystemTimeToTzSpecificLocalTime(nil, SystemTime, LocalTime);
                Times.AcTime := SystemTimeToDateTime(LocalTime);
                FileTimeToSystemTime(FileAttr.ftLastWriteTime, SystemTime);
                SystemTimeToTzSpecificLocalTime(nil, SystemTime, LocalTime);
                Times.ModTime := SystemTimeToDateTime(LocalTime);
                {$endif}

            end;

            FCpmTools.WriteFileToImage(CpmName, @Buffer[0], UnixFileSize, IsTextFile, PreserveTimeStamps, Times);

            try
                CloseFile(UnixFile);
            except

                on e: Exception do begin
                    MessageDlg(Format('can not close %s' + LineEnding + '%s', [ExtractFileName(FileToPaste), e.Message]),
                        mtError, [mbOK], 0);
                end;

            end;

        end;

    end;

    RefreshDirectory;

end;

// --------------------------------------------------------------------------------
procedure TImagePage.CopyFiles(ADoCut: boolean);
var
    SelectedFile, TmpFile, CpmFile: string;
    FileExt, TextfileEndings: string;
    PreserveTimeStamps, ConvertTextFiles, IsTextFile: boolean;
    IndexI: integer;
    ClipbrdList: TStringList;
begin
    { #todo : 'ADoCut' auswerten. }
    with TXMLSettings.Create(SettingsFile) do begin

        try
            OpenKey('Settings');
            PreserveTimeStamps := GetValue('KeepTimestamps', True);
            ConvertTextFiles := GetValue('ConvertTextFiles', False);
            TextfileEndings := GetValue('TextFileEndings', 'txt pip pas');
            CloseKey;
        finally
            Free;
        end;

    end;

    if (ADoCut) then begin

        try
            FCuttedFiles := TStringList.Create;
            FCuttedFiles.Clear;
        except
            ADoCut := False;
        end;

    end;

    try
        ClipbrdList := TStringList.Create;
        ClipbrdList.Clear;

        for IndexI := 0 to FDirectoryList.Items.Count - 1 do begin

            if (FDirectoryList.Items[IndexI].Selected) then begin
                SelectedFile := DelSpace(FDirectoryList.Items[IndexI].Caption);
                TmpFile := RightStr(SelectedFile, Length(SelectedFile) - Pos(':', SelectedFile));
                CpmFile := Format('%.2d%s', [StrToInt(LeftStr(SelectedFile, Pos(':', SelectedFile) - 1)), TmpFile]);
                FileExt := RightStr(TmpFile, (Length(TmpFile) - Pos('.', TmpFile)));
                IsTextFile := (ConvertTextFiles and TextFileEndings.Contains(FileExt));
                ReadCpmFile(CpmFile, TmpFile, IsTextFile, PreserveTimeStamps);
                ClipbrdList.Add(FTempFolder + TmpFile);

                if (ADoCut) then begin
                    FCuttedFiles.Add(SelectedFile);
                end;

            end;

        end;

        CopyFilesToClipboard(ClipbrdList);

    finally
        FreeAndNil(ClipbrdList);
    end;

end;

// --------------------------------------------------------------------------------
procedure TImagePage.DeleteCopiedFiles;
begin
    FCpmTools.DeleteFile(FCuttedFiles);
    FreeAndNil(FCuttedFiles);
    RefreshDirectory;
end;

// --------------------------------------------------------------------------------
constructor TImagePage.Create(ATheOwner: TComponent);
var
    DiskdefsPath: string;
begin
    inherited Create(ATheOwner);
    OnResize := @DirectoryListResize;
    CreateDirectoryListView;

    with TXMLSettings.Create(SettingsFile) do begin

        try
            OpenKey('Settings');
            DiskdefsPath := GetValue('DiskdefsFile', '');
            CloseKey;
        finally
            Free;
        end;

    end;

    FCpmTools := TCpmTools.Create;
    FCpmTools.SetPrintDirectoryEntryCallBack(@PrintDirectoryEntry);
    FCpmTools.SetDiskDefsPath(DiskdefsPath);
    FTempFolder := GetTempDir(True) + CreateTempFolderName(13) + PathDelim;
    MkDir(FTempFolder);
    FEnableAction := [];
end;

// --------------------------------------------------------------------------------
destructor TImagePage.Destroy;
begin
    ClearFileSystemInfo;
    ClearDirectoryStatistics;
    FreeAndNil(FCpmTools);
    DeleteDirectory(FTempFolder, False);
    inherited Destroy;
end;

// --------------------------------------------------------------------------------
procedure TImagePage.DirectoryListResize(ASender: TObject);
var
    NewWidth, ColWidths, ActListViewWidth: integer;
begin
    NewWidth := 0;
    ColWidths := 0;
    FDirectoryList.BeginUpdate;
    ActListViewWidth := FDirectoryList.ClientWidth;
    NewWidth := Round(ActListViewWidth * 0.151);
    ColWidths := ColWidths + NewWidth;
    FDirectoryList.Columns[0].Width := NewWidth;
    NewWidth := Round(ActListViewWidth * 0.076);
    ColWidths := ColWidths + NewWidth;
    FDirectoryList.Columns[1].Width := NewWidth;
    NewWidth := Round(ActListViewWidth * 0.076);
    ColWidths := ColWidths + NewWidth;
    FDirectoryList.Columns[2].Width := NewWidth;
    NewWidth := Round(ActListViewWidth * 0.107);
    ColWidths := ColWidths + NewWidth;
    FDirectoryList.Columns[3].Width := NewWidth;
    NewWidth := Round(ActListViewWidth * 0.107);
    ColWidths := ColWidths + NewWidth;
    FDirectoryList.Columns[4].Width := NewWidth;
    NewWidth := Round(ActListViewWidth * 0.16);
    ColWidths := ColWidths + NewWidth;
    FDirectoryList.Columns[5].Width := NewWidth;
    NewWidth := Round(ActListViewWidth * 0.16);
    ColWidths := ColWidths + NewWidth;
    FDirectoryList.Columns[6].Width := NewWidth;
    NewWidth := Round(ActListViewWidth * 0.16);
    ColWidths := ColWidths + NewWidth;
    FDirectoryList.Columns[7].Width := (NewWidth + (ActListViewWidth - Colwidths));
    FDirectoryList.EndUpdate;
end;

// --------------------------------------------------------------------------------
procedure TImagePage.DirectorySelectItem(Sender: TObject; Item: TListItem; Selected: boolean);
var
    DirList: TListView;
begin
    DirList := TListView(Sender);

    if (DirList.SelCount > 0) then begin
        FEnableAction := FEnableAction + [MAcut, MAcopy, MAdelete];
        // - [MApaste];{ #todo : Paste nur aktivieren wenn gültige Clipboard Daten vorliegen }

        if (DirList.SelCount = 1) then begin
            FEnableAction := FEnableAction + [MAcharacteristic, MArename];
        end
        else begin
            FEnableAction := FEnableAction - [MAcharacteristic, MArename];
        end;

        Selected := True;
    end
    else begin
        FEnableAction := FEnableAction - [MAcut, MAcopy, MAdelete, MAcharacteristic, MArename];
        // + [MApaste];{ #todo : Paste nur aktivieren wenn gültige Clipboard Daten vorliegen }
    end;

    if Assigned(FMenuActionEnableCallBack) then begin
        FMenuActionEnableCallBack(FEnableAction);
    end;

end;

// --------------------------------------------------------------------------------
procedure TImagePage.DirectoryListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
    DirList: TListView;
begin
    DirList := TListView(Sender);

    if ((Button = mbRight)) then begin

        if ((ssCtrl in Shift) and (DirList.SelCount > 1)) then begin
            DirList.Items[DirList.GetItemAt(X, Y).Index].Selected := True;
        end;

        FDirPopupMenu.PopUp;
    end;
end;

// --------------------------------------------------------------------------------
procedure TImagePage.DirectoryItemDblClick(ASender: TObject);
var
    DirList: TListView;
    DirEntry, TmpFile, CpmFileName: string;
    FileLength: QWord;
    FileData: TBytes;
    FileTime: TUTimeBuf;
    DataStream: TMemoryStream;
    Dialog: TFileHexEditDialog;
begin
    DirList := TListView(ASender);
    DirEntry := DelSpace(DirList.Items[DirList.ItemIndex].Caption);
    TmpFile := RightStr(DirEntry, Length(DirEntry) - Pos(':', DirEntry));
    CpmFileName := Format('%.2d%s', [StrToInt(LeftStr(DirEntry, Pos(':', DirEntry) - 1)), TmpFile]);
    FCpmTools.ReadFileFromImage(CpmFileName, FileData, FileLength, False, FileTime);
    try
        DataStream := TMemoryStream.Create;
        DataStream.Write(FileData[0], FileLength);

        try
            Dialog := TFileHexEditDialog.Create(self);
            Dialog.SetFileData(DataStream, TmpFile);
            if (Dialog.ShowModal = mrOk) then begin
                DataStream.Clear;
                Dialog.GetFileData(DataStream);
                if (DataStream.Size <> FileLength) then begin
                    MessageDlg('Error retrieving data from HexEditor', mtError, [mbOK], 0);
                    exit;
                end;
                SetLength(FileData, 0);
                SetLength(FileData, FileLength);
                DataStream.Position := 0;
                DataStream.Read(FileData[0], FileLength);
                FileTime.ModTime := now;
                FCpmTools.WriteFileToImage(CpmFileName, FileData, FileLength, False, True, FileTime, True);
                RefreshDirectory;
            end;
        finally
            FreeAndNil(Dialog);
        end;
    finally
        DataStream.Free;
    end;

end;

// --------------------------------------------------------------------------------
procedure TImagePage.CreateDirectoryListView;
var
    DirColumn: TListColumn;
begin
    FDirectoryList := TListView.Create(self);

    with FDirectoryList do begin
        Parent := self;
        Align := alClient;
        BorderStyle := bsSingle;
        ReadOnly := True;
        MultiSelect := True;
        ScrollBars := ssAutoVertical;
        ViewStyle := vsReport;
        AutoSort := False;
        GridLines := False;
        ColumnClick := False;
        SortType := stNone;
        AutoWidthLastColumn := False;
        RowSelect := True;

        {$ifdef UNIX}
        Font.Name := 'Liberation Mono';
        {$endif}

        {$ifdef Windows}
        Font.Name := 'Consolas';
        {$endif}

        BeginUpdate;
        DirColumn := Columns.Add;
        DirColumn.Caption := 'User : Name';
        DirColumn.Alignment := taLeftJustify;
        DirColumn := Columns.Add;
        DirColumn.Caption := 'Bytes';
        DirColumn.Alignment := taRightJustify;
        DirColumn := Columns.Add;
        DirColumn.Caption := 'Recs';
        DirColumn.Alignment := taRightJustify;
        DirColumn := Columns.Add;
        DirColumn.Caption := 'Attributes';
        DirColumn.Alignment := taCenter;
        DirColumn := Columns.Add;
        DirColumn.Caption := 'Protections';
        DirColumn.Alignment := taCenter;
        DirColumn := Columns.Add;
        DirColumn.Caption := 'Updated';
        DirColumn.Alignment := taCenter;
        DirColumn := Columns.Add;
        DirColumn.Caption := 'Created';
        DirColumn.Alignment := taCenter;
        DirColumn := Columns.Add;
        DirColumn.Caption := 'Last Access';
        DirColumn.Alignment := taCenter;
        EndUpdate;
        OnSelectItem := @DirectorySelectItem;
        OnMouseUp := @DirectoryListMouseUp;
        OnDblClick := @DirectoryItemDblClick;
    end;

end;

// --------------------------------------------------------------------------------
procedure TImagePage.ClearFileSystemInfo;
var
    Info: TFileSystemInfo;
begin

    with Info do begin
        FileName := EmptyStr;
        FileType := EmptyStr;
        Tracks := EmptyStr;
        Sectors := EmptyStr;
        SecBytes := EmptyStr;
        BlockSize := EmptyStr;
        MaxDir := EmptyStr;
        BootSectors := EmptyStr;
        Offset := EmptyStr;
        skew := EmptyStr;
        System := EmptyStr;
    end;

    if Assigned(FFileSystemInfoCallBack) then begin
        FFileSystemInfoCallBack(Info);
    end;
end;

// --------------------------------------------------------------------------------
procedure TImagePage.ClearDirectoryStatistics;
var
    Statistics: TDirStatistic;
begin

    with Statistics do begin
        TotalBytes := EmptyStr;
        TotalRecords := EmptyStr;
        Total1KBlocks := EmptyStr;
        TotalFreeBytes := EmptyStr;
        TotalDiskBytes := EmptyStr;
        FilesFound := EmptyStr;
        MaxDirEntries := EmptyStr;
        UsedDirEntries := EmptyStr;
    end;

    if Assigned(FDirStatisticCallBack) then begin
        FDirStatisticCallBack(Statistics);
    end;
end;

// --------------------------------------------------------------------------------
procedure TImagePage.PrintDirectoryEntry(AColumn: integer; ARow: integer; AData: string);
var
    Item: TListItem;
    IndexI: integer;
begin
    if ((AColumn = -1) and (ARow = -1) and (AData = 'Begin')) then begin
        FDirectoryList.Clear;
        FDirectoryList.BeginUpdate;
        Exit;
    end;

    if ((AColumn = -1) and (ARow = -1) and (AData = 'End')) then begin
        FDirectoryList.EndUpdate;
        if (FDirectoryList.Items.Count > 0) then begin
            FDirectoryList.Items[0].MakeVisible(False);
        end;
        Exit;
    end;

    if (FDirectoryList.Items.Count < ARow) then begin
        Item := FDirectoryList.Items.Add;
        Item.Caption := AData;
        for IndexI := 1 to 7 do begin
            Item.SubItems.Add('');
        end;
    end
    else begin
        FDirectoryList.Items.Item[ARow - 1].SubItems[AColumn - 1] := AData;
    end;

end;

// --------------------------------------------------------------------------------
function TImagePage.CreateTempFolderName(ALength: integer): string;
const
    BaseStr = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
var
    IndexI: integer;
begin
    Result := '';
    Randomize;

    for IndexI := 1 to ALength do begin
        Result := Result + BaseStr[RandomRange(1, Length(BaseStr) + 1)];
    end;

end;

// --------------------------------------------------------------------------------
procedure TImagePage.ReadCpmFile(ACpmFileName: string; ATmpFileName: string; AIsTextFile: boolean; APreserveTimeStamps: boolean);
var
    FileLength, Count: QWord;
    FileData: TBytes;
    FileTime: TUTimeBuf;
    TmpFile: file of byte;
    WriteError: boolean;

    {$ifdef WINDOWS}
    WinFileTime: TFileTime;
    WinSystemTime: TSYSTEMTIME;
    FileHandle: THandle;
    {$endif}

begin
    WriteError := False;
    FCpmTools.ReadFileFromImage(ACpmFileName, FileData, FileLength, AIsTextFile, FileTime);

    if (FileLength > 0) then begin

        try
            AssignFile(TmpFile, FTempFolder + ATmpFileName);
            Rewrite(TmpFile);

            try
                BlockWrite(TmpFile, FileData[0], FileLength, int64(Count));



            except

                on e: Exception do begin
                    MessageDlg(Format('Error writing %s to Temp Folder' + LineEnding + '%s',
                        [ExtractFileName(ATmpFileName), e.Message]), mtError, [mbOK], 0);
                    WriteError := True;
                end;
            end;

        finally
            CloseFile(TmpFile);
        end;

        if (APreserveTimeStamps and not WriteError) then begin
            {$ifdef UNIX}
            FileSetDate((FTempFolder + ATmpFileName), DateTimeToUnix(FileTime.ModTime));
            {$endif}

            {$ifdef WINDOWS}
            FileHandle := FileOpen((FTempFolder + ATmpFileName), fmOpenWrite);

            if (FileHandle <> feInvalidHandle) then begin

                try
                    DateTimeToSystemTime(FileTime.ModTime, WinSystemTime);
                    SystemTimeToFileTime(@WinSystemTime, @WinFileTime);
                    SetFileTime(FileHandle, nil, @WinFileTime, @WinFileTime);
                finally
                    FileClose(FileHandle);
                end;

            end;
            {$endif}
        end;

    end;

end;

// --------------------------------------------------------------------------------
procedure TImagePage.CopyFilesToClipboard(AFileList: TStringList);
var
    {$ifdef UNIX}
    IndexI: integer;
    ClipbrdUri: TURI;
    UriList: string;
    {$endif}

    {$ifdef WINDOWS}
    ClipbrdData: PDropFiles;
    ClipbrdHandle: THandle;
    IndexI, FileListLength: integer;
    {$endif}

    FileList: string;
begin
    FileList := '';

    {$ifdef UNIX}
    UriList := '';

    for IndexI := 0 to AFileList.Count - 1 do begin
        ClipbrdUri.Document := ExtractFileName(AFileList[IndexI]);
        ClipbrdUri.Path := ExtractFileDir(AFileList[IndexI]);
        ClipbrdUri.Protocol := 'file';
        ClipbrdUri.HasAuthority := True;
        ClipbrdUri.Port := 0;
        UriList := UriList + EncodeURI(ClipbrdUri);
        FileList := FileList + AFileList[IndexI];
        if (IndexI < (AFileList.Count - 1)) then begin
            UriList := UriList + #$0A;
            FileList := FileList + #$0A;
        end;
    end;

    Clipboard.Open;
    Clipboard.Clear;
    Clipboard.AddFormat(RegisterClipboardFormat('text/plain'), FileList[1], Length(FileList));
    Clipboard.AddFormat(RegisterClipboardFormat('text/uri-list'), UriList[1], Length(UriList));
    Clipboard.AddFormat(RegisterClipboardFormat('application/x-kde4-urilist'), UriList[1], Length(UriList));
    UriList := 'copy' + #$0A + UriList;
    Clipboard.AddFormat(RegisterClipboardFormat('x-special/gnome-copied-files'), UriList[1], Length(UriList));
    Clipboard.AddFormat(RegisterClipboardFormat('x-special/mate-copied-files'), UriList[1], Length(UriList));
    Clipboard.Close;
    {$endif}


    {$ifdef WINDOWS}
    for IndexI := 0 to AFileList.Count - 1 do begin
        FileList := FileList + AFileList[IndexI] + #0;
    end;

    FileList := FileList + #0;
    FileListLength := Length(FileList);
    ClipbrdHandle := GlobalAlloc(GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT, SizeOf(TDropFiles) + FileListLength);

    if (ClipbrdHandle <> 0) then begin
        ClipbrdData := GlobalLock(ClipbrdHandle);
        ClipbrdData^.pFiles := SizeOf(TDropFiles);
        Move(FileList[1], (PChar(ClipbrdData) + SizeOf(TDropFiles))^, FileListLength);
        GlobalUnlock(ClipbrdHandle);
        OpenClipboard(self.Handle);
        EmptyClipboard;
        SetClipboardData(CF_HDROP, ClipbrdHandle);
        CloseClipboard;
    end;
    {$endif}

end;

// --------------------------------------------------------------------------------
end.
