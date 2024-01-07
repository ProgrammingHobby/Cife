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
unit CpmTools;

{$mode ObjFPC}
{$H+}

interface

uses
    Classes, SysUtils, CpmFileSystem, CpmDevice, CifeGlobals, CpmDefs;

type

    TPrintDirectoryEntryCB = procedure(AColumn: integer; ARow: integer; AData: string) of object;

    { TCpmTools }

    TCpmTools = class
    public    // Attribute

    public    // Methoden
        procedure SetPrintDirectoryEntryCallBack(APrintDirectoryEntryCB: TPrintDirectoryEntryCB);
        procedure SetDiskDefsPath(ADiskdefsPath: string);
        function OpenImage(const AFileName: string; const AFileType: string; AUpperCase: boolean): boolean;
        function CloseImage: boolean;
        procedure ShowDirectory;
        procedure RefreshDirectory(AUpperCase: boolean);
        function RenameFile(AOldName, ANewName: string): boolean;
        function DeleteFile(AFileNames: TStringList): boolean;
        function CreateNewImage(AImageFile: string; AImageType: string; ABootFile: string;
            AFileSystemLabel: string; ATimeStampsUsed: boolean; AUseUpperCase: boolean): boolean;
        function GetFileSystemInfo: TFileSystemInfo;
        function GetDirectoryStatistic: TDirStatistic;
        function GetFileInfo(AFileName: string): TFileInfo;
        procedure SetNewAttributes(AFileName: string; AAttributes: cpm_attr_t);
    public  // Konstruktor/Destruktor
        constructor Create; overload;
        destructor Destroy; override;

    protected // Attribute

    protected // Methoden

    private   // Attribute
        FCpmDevice: TCpmDevice;
        FCpmFileSystem: TCpmFileSystem;
        FFileName: string;
        FFileType: string;
        FDiskdefsPath: string;
        FDirStatistic: TDirStatistic;
        FPrintDirectoryEntry: TPrintDirectoryEntryCB;

    private   // Methoden
        function GetUserNumber(const AFileName: string): integer;
        function ConvertFilename(const AFileName: string): string;
        function SizeToKb(ASize: longint; ABlocksize: longint): longint;

    end;

implementation

{ TCpmTools }

uses Dialogs, Controls, StrUtils, QuickSort, Character;

// --------------------------------------------------------------------------------
procedure TCpmTools.SetPrintDirectoryEntryCallBack(APrintDirectoryEntryCB: TPrintDirectoryEntryCB);
begin
    FPrintDirectoryEntry := APrintDirectoryEntryCB;
end;

// --------------------------------------------------------------------------------
procedure TCpmTools.SetDiskDefsPath(ADiskdefsPath: string);
begin
    FDiskdefsPath := ADiskdefsPath;
end;

// --------------------------------------------------------------------------------
function TCpmTools.OpenImage(const AFileName: string; const AFileType: string; AUpperCase: boolean): boolean;
begin
    FFileName := AFileName;
    FFileType := AFileType;

    if not (FCpmDevice.Open(AFileName, dmOpenReadWrite)) then begin
        if MessageDlg(Format('cannot open %s' + LineEnding + '(%s)', [ExtractFileName(AFileName), FCpmDevice.GetErrorMsg()])
            , mtError, [mbOK], 0) = mrOk then begin
            Result := False;
            Exit;
        end;
    end;

    if not (FCpmFileSystem.ReadDiskdefData(AFileType, FDiskdefsPath)) then begin
        if MessageDlg(Format('cannot read superblock' + LineEnding + '(%s)', [FCpmFileSystem.GetErrorMsg()])
            , mtError, [mbOK], 0) = mrOk then begin
            Result := False;
            Exit;
        end;
    end;

    if not (FCpmFileSystem.InitDriveData(AUpperCase)) then begin
        if MessageDlg(Format('cannot init filesystem' + LineEnding + '(%s)', [FCpmFileSystem.GetErrorMsg()])
            , mtError, [mbOK], 0) = mrOk then begin
            Result := False;
            Exit;
        end;
    end;

    Result := True;
end;

// --------------------------------------------------------------------------------
function TCpmTools.CloseImage: boolean;
begin

    if not (FCpmFileSystem.Unmount) then begin
        if MessageDlg('error write back filesystem directory', mtError, [mbOK], 0) = mrOk then begin
            Result := False;
            Exit;
        end;
    end;

    if not (FCpmDevice.Close) then begin

        if MessageDlg(Format('cannot close image %s' + LineEnding + '(%s)',
            [ExtractFileName(FFileName), FCpmFileSystem.GetErrorMsg()]), mtError, [mbOK], 0) = mrOk then begin
            Result := False;
            Exit;
        end;

    end;

    Result := True;
end;

// --------------------------------------------------------------------------------
procedure TCpmTools.ShowDirectory;
var
    DirFile: TCpmInode;
    Buf: TCpmStatFS;
    StatBuf: TCpmStat;
    Gargc, Row: integer;
    IndexI, Attrib, User: integer;
    FilesCount, TotalBytes, TotalRecs: integer;
    Gargv: TStringList;
    Attribute: string[16];
begin
    Row := 1;
    Gargv := TStringList.Create;
    FCpmFileSystem.Glob('*', Gargc, Gargv);
    { #todo : Range-Check Fehler bei falsch angegebenem Image-Typ abfangen }

    if (Gargc > 0) then begin
        FilesCount := 0;
        TotalBytes := 0;
        TotalRecs := 0;
        QSort(Gargv, 0, Gargv.Count - 1);
        FCpmFileSystem.StatFs(Buf);

        for User := 0 to 31 do begin

            for IndexI := 0 to Gargc - 1 do begin

                if ((Gargv[IndexI].ToCharArray[0] = Chr(Ord('0') + (User div 10))) and
                    (Gargv[IndexI].ToCharArray[1] = Chr(Ord('0') + (User mod 10)))) then begin
                    FCpmFileSystem.Name2Inode(PChar(Gargv[IndexI]), DirFile);
                    FCpmFileSystem.Stat(DirFile, StatBuf);
                    FCpmFileSystem.AttrGet(DirFile, Attrib);
                    Inc(TotalBytes, StatBuf.Size);
                    Inc(TotalRecs, ((StatBuf.Size + 127) div 128));
                    //  user: name
                    FPrintDirectoryEntry(0, Row, Format('%2d: %s', [User, MidStr(Gargv[IndexI], 3, Length(Gargv[IndexI]))]));
                    //  bytes
                    FPrintDirectoryEntry(1, Row, Format('%5.1dK', [SizeToKb(StatBuf.Size, Buf.F_BSize)]));
                    //  records
                    FPrintDirectoryEntry(2, Row, Format('%6.1d', [StatBuf.Size div 128]));
                    //  attributes
                    Attribute := '';

                    if ((Attrib and CPM_ATTR_F1) <> 0) then begin
                        Attribute := Attribute + '1';
                    end
                    else begin
                        Attribute := Attribute + '-';
                    end;

                    if ((Attrib and CPM_ATTR_F2) <> 0) then begin
                        Attribute := Attribute + '2';
                    end
                    else begin
                        Attribute := Attribute + '-';
                    end;

                    if ((Attrib and CPM_ATTR_F3) <> 0) then begin
                        Attribute := Attribute + '3';
                    end
                    else begin
                        Attribute := Attribute + '-';
                    end;

                    if ((Attrib and CPM_ATTR_F4) <> 0) then begin
                        Attribute := Attribute + '4';
                    end
                    else begin
                        Attribute := Attribute + '-';
                    end;

                    Attribute := Attribute + ' ';

                    if ((Attrib and CPM_ATTR_RO) <> 0) then begin
                        Attribute := Attribute + 'r';
                    end
                    else begin
                        Attribute := Attribute + '-';
                    end;

                    if ((Attrib and CPM_ATTR_SYS) <> 0) then begin
                        Attribute := Attribute + 's';
                    end
                    else begin
                        Attribute := Attribute + '-';
                    end;

                    if ((Attrib and CPM_ATTR_ARCV) <> 0) then begin
                        Attribute := Attribute + 'a';
                    end
                    else begin
                        Attribute := Attribute + '-';
                    end;

                    FPrintDirectoryEntry(3, Row, Attribute);
                    //  protections
                    Attribute := '';

                    if ((Attrib and CPM_ATTR_PWREAD) <> 0) then begin
                        Attribute := Attribute + 'rd';
                    end
                    else begin
                        Attribute := Attribute + '--';
                    end;

                    Attribute := Attribute + ' ';

                    if ((Attrib and CPM_ATTR_PWWRITE) <> 0) then begin
                        Attribute := Attribute + 'wr';
                    end
                    else begin
                        Attribute := Attribute + '--';
                    end;

                    Attribute := Attribute + ' ';

                    if ((Attrib and CPM_ATTR_PWDEL) <> 0) then begin
                        Attribute := Attribute + 'del';
                    end
                    else begin
                        Attribute := Attribute + '---';
                    end;

                    FPrintDirectoryEntry(4, Row, Attribute);

                    //  updated

                    if (StatBuf.MTime <> 0) then begin
                        FPrintDirectoryEntry(5, Row, FormatDateTime('DD-MMM-YYYY HH:MM', StatBuf.MTime));
                    end;

                    //  created

                    if (StatBuf.CTime <> 0) then begin
                        FPrintDirectoryEntry(6, Row, FormatDateTime('DD-MMM-YYYY HH:MM', StatBuf.CTime));
                    end;

                    //  last access

                    if (StatBuf.ATime <> 0) then begin
                        FPrintDirectoryEntry(7, Row, FormatDateTime('DD-MMM-YYYY HH:MM', StatBuf.ATime));
                    end;

                    Inc(FilesCount);
                    Inc(Row);
                end;

            end;

        end;

        { #todo : Statistiken (Records und 1kblocks) korrigieren, evtl. noch um TotalFreeBytes erweitern }
        FDirStatistic.TotalBytes := ((TotalBytes + 1023) div 1024);
        FDirStatistic.TotalRecords := TotalRecs;
        FDirStatistic.FilesFound := FilesCount;
        FDirStatistic.Total1KBlocks := ((Buf.F_BUsed * Buf.F_BSize) div 1024);
        FDirStatistic.UsedDirEntries := (Buf.F_Files - Buf.F_FFree);
        FDirStatistic.MaxDirEntries := Buf.F_Files;
    end;

    FreeAndNil(Gargv);
end;

// --------------------------------------------------------------------------------
procedure TCpmTools.RefreshDirectory(AUpperCase: boolean);
begin
    if (FCpmFileSystem.InitDriveData(AUpperCase)) then begin
        ShowDirectory;
    end;
end;

// --------------------------------------------------------------------------------
function TCpmTools.RenameFile(AOldName, ANewName: string): boolean;
var
    Gargc: integer;
    Gargv: TStringList;
begin
    try
        Gargv := TStringList.Create;
        FCpmFileSystem.Glob(PChar(AOldName), Gargc, Gargv);

        if not ((Gargc > 0) and (FCpmFileSystem.Rename(PChar(Gargv[0]), PChar(ConvertFilename(ANewName))))) then begin

            if MessageDlg(Format('can not rename %s in %s' + LineEnding + '(%s)',
                [AOldName, ANewName, FCpmFileSystem.GetErrorMsg]), mtError, [mbOK], 0) = mrOk then begin
                Result := False;
                Exit;
            end;

        end;

    finally
        FreeAndNil(Gargv);
    end;

    FCpmFileSystem.Sync;
    Result := True;
end;

// --------------------------------------------------------------------------------
function TCpmTools.DeleteFile(AFileNames: TStringList): boolean;
var
    Gargc, IndexI: integer;
    Gargv: TStringList;
begin
    try
        Gargv := TStringList.Create;
        for IndexI := 0 to AFileNames.Count - 1 do begin

            FCpmFileSystem.Glob(PChar(AFileNames[IndexI]), Gargc, Gargv);

            if not ((Gargc > 0) and (FCpmFileSystem.Delete(PChar(Gargv[IndexI])))) then begin

                if MessageDlg(Format('can not erase %s' + LineEnding + '(%s)',
                    [Gargv[0], FCpmFileSystem.GetErrorMsg]), mtError, [mbOK], 0) = mrOk then begin
                    Result := False;
                    Exit;
                end;

            end;

        end;

    finally
        FreeAndNil(Gargv);
    end;

    FCpmFileSystem.Sync;
    Result := True;
end;

// --------------------------------------------------------------------------------
function TCpmTools.CreateNewImage(AImageFile: string; AImageType: string; ABootFile: string;
    AFileSystemLabel: string; ATimeStampsUsed: boolean; AUseUpperCase: boolean): boolean;
var
    BootTracks: array of byte = nil;
    BootTrackSize: integer;
    IndexI: integer;
    BootFile: file;
    BootFileSize, ReadSize: integer;
begin

    if not (FCpmFileSystem.ReadDiskdefData(AImageType, FDiskdefsPath)) then begin
        if MessageDlg(Format('cannot read superblock' + LineEnding + '(%s)', [FCpmFileSystem.GetErrorMsg()])
            , mtError, [mbOK], 0) = mrOk then begin
            Result := False;
            Exit;
        end;
    end;

    BootTrackSize := FCpmFileSystem.GetBootTrackSize;

    try
        SetLength(BootTracks, BootTrackSize);
    except
        on e: Exception do begin

            if MessageDlg(Format('can not allocate boot track buffer' + LineEnding + '(%s)', [e.Message]), mtError, [mbOK], 0) =
                mrOk then begin
                Result := False;
                Exit;
            end;
        end;
    end;

    for IndexI := Low(BootTracks) to High(BootTracks) do begin
        BootTracks[IndexI] := $E5;
    end;

    if ((Length(BootTracks) > 0) and (BootTrackSize > 0)) then begin

        try
            AssignFile(BootFile, ABootFile);
            Reset(BootFile, 1);
        except
            on e: Exception do begin

                if MessageDlg(Format('can not open %s' + LineEnding + '(%s)', [ExtractFileName(ABootFile), e.Message]),
                    mtError, [mbOK], 0) = mrOk then begin
                    Result := False;
                    Exit;
                end;

            end;
        end;

        BootFileSize := FileSize(BootFile);

        if (BootFileSize > BootTrackSize) then begin

            if MessageDlg('boottrack file is bigger than boottracks size.', mtError, [mbOK], 0) = mrOk then begin
                Result := False;
                Exit;
            end;

        end;

        try
            Reset(BootFile, 1);
            BlockRead(BootFile, BootTracks[0], BootFileSize, ReadSize);
        except
            on e: Exception do begin
                if MessageDlg(Format('error Reading Boottrack-File' + LineEnding + '(%s)', [e.Message]), mtError, [mbOK], 0) =
                    mrOk then begin
                    Result := False;
                    Exit;
                end;
            end;
        end;

        if (ReadSize < BootFileSize) then begin

            if MessageDlg('Boottrack-File is bigger than Boottrack space', mtError, [mbOK], 0) = mrOk then begin
                Result := False;
                Exit;
            end;

        end;

    end;

    if not (FCpmFileSystem.ReadDiskdefData(AImageType, FDiskdefsPath) and
        FCpmFileSystem.MakeFileSystem(AImageFile, BootTracks, AFileSystemLabel, ATimeStampsUsed, AUseUpperCase)) then begin

        if MessageDlg(Format('can not make new file system' + LineEnding + '(%s)', [FCpmFileSystem.GetErrorMsg]),
            mtError, [mbOK], 0) = mrOk then begin
            Result := False;
            Exit;
        end;

    end;

    MessageDlg(Format('new Image-File ''%s'' successful created.', [ExtractFileName(AImageFile)]), mtInformation, [mbOK], 0);
    Result := True;
end;

// --------------------------------------------------------------------------------
function TCpmTools.GetFileSystemInfo: TFileSystemInfo;
var
    Info: TFileSystemInfo;
begin
    Info := FCpmFileSystem.GetFileSystemInfo;
    Info.FileName := FFileName;
    Info.FileType := FFileType;
    Result := Info;
end;

// --------------------------------------------------------------------------------
function TCpmTools.GetDirectoryStatistic: TDirStatistic;
begin
    Result := FDirStatistic;
end;

// --------------------------------------------------------------------------------
function TCpmTools.GetFileInfo(AFileName: string): TFileInfo;
var
    Gargc: integer;
    Gargv: TStringList;
    DirFile: TCpmInode;
    StatBuf: TCpmStat;
    Index, Attrib: integer;
    FileInfo: TFileInfo;
begin
    try
        Gargv := TStringList.Create;
        FCpmFileSystem.Glob(PChar(AFileName), Gargc, Gargv);

        if (Gargc > 0) then begin
            FCpmFileSystem.Name2Inode(PChar(Gargv[0]), DirFile);
            FCpmFileSystem.Stat(DirFile, StatBuf);
            FCpmFileSystem.AttrGet(DirFile, Attrib);
            Index := Pos(':', AFileName);
            FileInfo.UserNumber := StrToInt(LeftStr(AFileName, Index - 1));
            FileInfo.Name := RightStr(AFileName, (Length(AFileName) - Index));
            FileInfo.UsedBytes := StatBuf.Size;
            FileInfo.UsedRecords := ((StatBuf.Size + 127) div 128);
            FileInfo.Attributes := Attrib;
            FileInfo.ATime := StatBuf.ATime;
            FileInfo.CTime := StatBuf.CTime;
            FileInfo.MTime := StatBuf.MTime;
        end;


    finally
        FreeAndNil(Gargv);
    end;

    Result := FileInfo;
end;

// --------------------------------------------------------------------------------
procedure TCpmTools.SetNewAttributes(AFileName: string; AAttributes: cpm_attr_t);
var
    Gargc: integer;
    Gargv: TStringList;
    DirFile: TCpmInode;
begin
    try
        Gargv := TStringList.Create;
        FCpmFileSystem.Glob(PChar(AFileName), Gargc, Gargv);

        if (FCpmFileSystem.Name2Inode(PChar(Gargv[0]), DirFile)) then begin
            FCpmFileSystem.AttrSet(DirFile, AAttributes);
        end
        else begin

            if MessageDlg(Format('can not find %s' + LineEnding + '(%s)',
                [AFileName, FCpmFileSystem.GetErrorMsg]), mtError, [mbOK], 0) = mrOk then begin
                Exit;
            end;

        end;

        FCpmFileSystem.Sync;
    finally
        FreeAndNil(Gargv);
    end;

end;

// --------------------------------------------------------------------------------
constructor TCpmTools.Create;
begin
    inherited Create;
    FCpmDevice := TCpmDevice.Create;
    FCpmFileSystem := TCpmFileSystem.Create(FCpmDevice);
end;

// --------------------------------------------------------------------------------
destructor TCpmTools.Destroy;
begin
    FreeAndNil(FCpmFileSystem);
    FreeAndNil(FCpmDevice);
    inherited Destroy;
end;

// --------------------------------------------------------------------------------
//  -- get User Number from Filename (UU:NNNNNNNN.EEE)
// --------------------------------------------------------------------------------
function TCpmTools.GetUserNumber(const AFileName: string): integer;
begin
    if (IsDigit(AFileName[1]) and (AFileName[2] = ':')) then begin
        Result := StrToIntDef(AFileName[1], -1);
    end
    else if (IsDigit(AFileName[1]) and IsDigit(AFileName[2]) and (AFileName[3] = ':')) then begin
        Result := StrToIntDef(AFileName[1] + AFileName[2], -1);
    end
    else begin
        Result := -1;
    end;
end;

// --------------------------------------------------------------------------------
//  -- convert Filename from UU:NNNNNNNN.EEE to UUNNNNNNNN.EEE
// --------------------------------------------------------------------------------
function TCpmTools.ConvertFilename(const AFileName: string): string;
begin
    Result := Format('%.2d%s', [GetUserNumber(AFileName), RightStr(AFileName, Length(AFileName) - Pos(':', AFileName))]);
end;

// --------------------------------------------------------------------------------
function TCpmTools.SizeToKb(ASize: longint; ABlocksize: longint): longint;
var
    Blocks, BlockedSize: longint;
begin
    // In DR CP/M, the minimal block size is 1024, but in CP/M-65 it may be lower.
    Blocks := ((ASize + ABlocksize - 1) div ABlocksize);
    BlockedSize := (Blocks * ABlocksize);
    Result := ((BlockedSize + 1023) div 1024);
end;

// --------------------------------------------------------------------------------
end.
