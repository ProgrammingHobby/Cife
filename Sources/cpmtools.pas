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
unit CpmTools;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, CpmFileSystem, CpmDevice, CifeGlobals;

type

    TPrintDirectoryEntryCB = procedure(AColumn: integer; ARow: integer; AData: string) of object;

    { TCpmTools }

    TCpmTools = class
    public    // Attribute

    public    // Methoden
        procedure SetPrintDirectoryEntryCallBack(APrintDirectoryEntryCB: TPrintDirectoryEntryCB);
        function OpenImage(const AFileName: string; const AFileType: string; AUpperCase: boolean): boolean;
        function CloseImage: boolean;
        procedure ShowDirectory;
        function RenameFile(AOldName, ANewName: string): boolean;
        function GetFileSystemInfo: TFileSystemInfo;
        function GetDirectoryStatistic: TDirStatistic;

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
        FDirStatistic: TDirStatistic;
        FPrintDirectoryEntry: TPrintDirectoryEntryCB;

    private   // Methoden
        function GetUserNumber(const AFileName: string): integer;
        function ConvertFilename(const AFileName: string): string;

    end;

implementation

{ TCpmTools }

uses Dialogs, Controls, StrUtils, CpmDefs, QuickSort, Character;

// --------------------------------------------------------------------------------
procedure TCpmTools.SetPrintDirectoryEntryCallBack(APrintDirectoryEntryCB: TPrintDirectoryEntryCB);
begin
    FPrintDirectoryEntry := APrintDirectoryEntryCB;
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

    if not (FCpmFileSystem.ReadDiskdefData(AFileType)) then begin
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
                    FPrintDirectoryEntry(0, Row, Format('%2d: %s', [User,
                        MidStr(Gargv[IndexI], 3, Length(Gargv[IndexI]))]));
                    //  bytes
                    FPrintDirectoryEntry(1, Row,
                        Format('%5.1dK', [(StatBuf.Size + Buf.F_BSize - 1) div Buf.F_BSize * (Buf.F_BSize div 1024)]));
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
                        Attribute := Attribute + '3';
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
end.
