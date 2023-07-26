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
unit CpmFileSystem;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, CpmDevice, CpmDefs, CifeGlobals;

type

    TCpmInode = record
        Ino: ino_t;
        Mode: mode_t;
        Size: off_t;
        Attr: cpm_attr_t;
        ATime: time_t;
        MTime: time_t;
        CTime: time_t;
    end;

    TCpmStat = record
        Ino: ino_t;
        Mode: mode_t;
        Size: off_t;
        ATime: time_t;
        MTime: time_t;
        CTime: time_t;
    end;

    TCpmStatFS = record
        F_BSize: longint;
        F_Blocks: longint;
        F_BFree: longint;
        F_BUsed: longint;
        F_BAvail: longint;
        F_Files: longint;
        F_FFree: longint;
        F_NameLen: longint;
    end;

    TCpmFile = record
        Mode: mode_t;
        Pos: off_t;
        Ino: TCpmInode;
    end;

    { TCpmFileSystem }

    TCpmFileSystem = class
    public    // Attribute

    public    // Methoden
        function ReadDiskdefData(const AImageType: string): boolean;
        function InitDriveData(AUpperCase: boolean): boolean;
        procedure Glob(const AArgv: PChar; var AGargc: integer; var AGargv: TStringList);
        procedure StatFs(var ABuffer: TCpmStatFS);
        function Name2Inode(const AFilename: PChar; var AInode: TCpmInode): boolean;
        procedure Stat(const AInode: TCpmInode; var ABuffer: TCpmStat);
        procedure AttrGet(const AInode: TCpmInode; var AAttrib: cpm_attr_t);
        function Unmount: boolean;
        function Rename(const AOldName: PChar; const ANewName: PChar): boolean;
        function Delete(const AFileName: PChar): boolean;
        function Sync: boolean;
        function GetErrorMsg: string;
        function GetFileSystemInfo: TFileSystemInfo;

    public  // Konstruktor/Destruktor
        constructor Create(ACpmDevice: TCpmDevice); overload;
        destructor Destroy; override;

    protected // Attribute

    protected // Methoden

    private   // Attribute
    type
        TCpmDirent = record
            Ino: ino_t;
            Off: off_t;
            RecLen: size_t;
            Name: array[0..13] of char;
        end;

        TPhysDirectoryEntry = packed record
            Status: byte;
            Name: array[0..7] of char;
            Ext: array[0..2] of char;
            Extnol: byte;
            Lrc: byte;
            Extnoh: byte;
            Blkcnt: byte;
            Pointers: array[0..15] of byte;
        end;

        TDsEntry = record
            Year: byte;
            Month: byte;
            Day: byte;
            Hour: byte;
            Minute: byte;
        end;

        TDateStamps = record
            Create: TDsEntry;
            Access: TDsEntry;
            Modify: TDsEntry;
            CheckSum: byte;
        end;

        TCpmSuperBlock = record
            UpperCase: boolean;
            SecLength: integer;
            Tracks: integer;
            SecTrk: integer;
            BlkSiz: integer;
            MaxDir: integer;
            DirBlks: integer;
            Skew: integer;
            BootSec: integer;
            BootTrk: integer;
            Offset: off_t;
            OsType: integer;
            Size: integer;
            Extents: integer; // logical extents per physical extent
            Extentsize: integer; // pretty much always 16384
            AlvSize: integer;
            CnotaTime: integer;
            DiskLabel: array of char;
            LabelLength: size_t;
            Passwd: array of char;
            PasswdLength: size_t;
            DirtyDirectory: boolean;
            DirtyDateStamp: boolean;
        end;

        TIntArray = array of integer;
        TDirArray = array of TPhysDirectoryEntry;
        TDsArray = array of TDateStamps;

    var
        FCpmDevice: TCpmDevice;
        FFileSystemError: string;
        FDrive: TCpmSuperBlock;
        FRoot: TCpmInode;
        FSkewTab: TIntArray;
        FDirectory: TDirArray;
        FAllocationVector: TIntArray;
        FDateStamps: TDsArray;

    private   // Methoden
        procedure AlvInit;
        function AmstradReadSuper(): boolean;
        function DiskdefsReadSuper(const AImageType: string): boolean;
        function BootOffset: integer;
        function ReadBlock(ABlockNr: integer; ABuffer: pbyte; AStart, AEnd: integer): boolean;
        function WriteBlock(ABlockNr: integer; const ABuffer: pbyte; AStart, AEnd: integer): boolean;
        function FindFileExtent(AUser: integer; const AName: array of char; const AExt: array of char;
            AStart: integer; AExtNo: integer): integer;
        function ReadTimeStamps(var AInode: TCpmInode; ALowestExt: integer): integer;
        procedure ReadDsStamps(var AInode: TCpmInode; ALowestExt: integer);
        function CheckDateStamps: boolean;
        function SplitFilename(AFullname: PChar; AOsType: integer; var AName: array of char;
            var AExt: array of char; var AUser: integer): boolean;
        function RecMatch(AEntry: PChar; APattern: PChar): boolean;
        function Match(const AEntry: PChar; APattern: PChar): boolean;
        function IsMatching(AUser1: integer; const AName1: array of char; const AExt1: array of char;
            AUser2: integer; const AName2: array of char; const AExt2: array of char): boolean;
        function IsFileChar(AChar: char; AType: integer): boolean;
        function Cpm2UnixTime(ADays: integer; AHour: integer; AMin: integer): time_t;
        function Ds2UnixTime(const AEntry: TDsEntry): time_t;
        function SyncDateStamps: boolean;
        function OpenDir(var ADir: TCpmFile): boolean;
        function ReadDir(var ADir: TCpmFile; var AEnt: TCpmDirent): boolean;

    end;

implementation

{ TCpmFileSystem }

uses Character, StrUtils, DateUtils;

// --------------------------------------------------------------------------------
//  -- get DPB
// --------------------------------------------------------------------------------
function TCpmFileSystem.ReadDiskdefData(const AImageType: string): boolean;
begin

    if (AImageType.Contains('Amstrad (PCW16)')) then begin
        Result := AmstradReadSuper();
    end
    else begin
        Result := DiskdefsReadSuper(AImageType);
    end;

end;

// --------------------------------------------------------------------------------
//  -- init in-core data for drive
// --------------------------------------------------------------------------------
function TCpmFileSystem.InitDriveData(AUpperCase: boolean): boolean;
var
    IndexI, IndexJ, IndexK, Value: integer;
    Blocks, Passwords: integer;
    DirectoryBuffer: array of byte = nil;
begin
    FDrive.UpperCase := AUpperCase;

    // optional field, compute based on directory size
    if (FDrive.DirBlks = 0) then begin
        FDrive.DirBlks := ((FDrive.MaxDir * 32 + (FDrive.BlkSiz - 1)) div FDrive.BlkSiz);
    end;

    FCpmDevice.SetGeometry(FDrive.SecLength, FDrive.SecTrk, FDrive.Tracks, FDrive.Offset);

    // generate skew table
    if (FSkewTab = nil) then begin

        try
            SetLength(FSkewTab, FDrive.SecTrk);
        except
            on e: Exception do begin
                FFileSystemError := e.Message;
                Result := False;
                exit;
            end;
        end;

        Value := 0;

        for IndexI := 0 to FDrive.SecTrk - 1 do begin

            while (True) do begin

                IndexK := 0;

                while ((IndexK < IndexI) and (FSkewTab[IndexK] <> Value)) do begin
                    Inc(IndexK);
                end;

                if (IndexK < IndexI) then begin
                    Value := ((Value + 1) mod FDrive.SecTrk);
                end
                else begin
                    break;
                end;

            end;

            FSkewTab[IndexI] := Value;
            Value := ((Value + FDrive.Skew) mod FDrive.SecTrk);
        end;
    end;

    // initialise allocation vector bitmap
    FDrive.AlvSize := (((((FDrive.SecTrk * FDrive.Tracks) - BootOffset) * FDrive.SecLength) div FDrive.BlkSiz) +
        INTBITS - 1) div INTBITS;

    try
        SetLength(FAllocationVector, FDrive.AlvSize);
    except
        on e: Exception do begin
            FFileSystemError := e.Message;
            Result := False;
            exit;
        end;
    end;

    // allocate directory buffer
    try
        SetLength(DirectoryBuffer, ((((FDrive.MaxDir * 32) + FDrive.BlkSiz - 1) div FDrive.BlkSiz) * FDrive.BlkSiz));
    except
        on e: Exception do begin
            FFileSystemError := e.Message;
            Result := False;
            exit;
        end;
    end;

    if not (FCpmDevice.IsOpen) then begin // create empty directory in core

        for IndexI := Low(DirectoryBuffer) to High(DirectoryBuffer) do begin
            DirectoryBuffer[IndexI] := $E5;
        end;

    end
    else begin  // read directory in core
        Blocks := (((FDrive.MaxDir * 32) + FDrive.BlkSiz - 1) div FDrive.BlkSiz);

        for IndexI := 0 to Blocks - 1 do begin

            if not (ReadBlock(IndexI, @DirectoryBuffer[IndexI * FDrive.BlkSiz], 0, -1)) then begin
                Result := False;
                exit;
            end;

        end;

    end;

    // copy Directory buffer to Directory Record-Array
    try
        SetLength(FDirectory, (Length(DirectoryBuffer) div SizeOf(TPhysDirectoryEntry)));
    except
        on e: Exception do begin
            FFileSystemError := e.Message;
            Result := False;
            exit;
        end;
    end;

    IndexI := Low(DirectoryBuffer);

    while (IndexI <= High(DirectoryBuffer)) do begin

        with (FDirectory[(IndexI div SizeOf(TPhysDirectoryEntry))]) do begin
            Status := DirectoryBuffer[IndexI + 0];

            for IndexJ := 0 to 7 do begin
                Name[IndexJ] := char(DirectoryBuffer[IndexI + 1 + IndexJ]);
            end;

            for IndexJ := 0 to 2 do begin
                Ext[IndexJ] := char(DirectoryBuffer[IndexI + 9 + IndexJ]);
            end;

            Extnol := DirectoryBuffer[IndexI + 12];
            Lrc := DirectoryBuffer[IndexI + 13];
            Extnoh := DirectoryBuffer[IndexI + 14];
            Blkcnt := DirectoryBuffer[IndexI + 15];

            for IndexJ := 0 to 15 do begin
                Pointers[IndexJ] := DirectoryBuffer[IndexI + 16 + IndexJ];
            end;

            Inc(IndexI, SizeOf(TPhysDirectoryEntry));
        end;

    end;

    AlvInit;

    // read additional superblock information
    if ((FDrive.OsType and CPMFS_CPM3_OTHER) <> 0) then begin
        // passwords
        Passwords := 0;

        for IndexI := 0 to FDrive.MaxDir - 1 do begin

            if ((FDirectory[IndexI].Status >= 16) and (FDirectory[IndexI].Status <= 31)) then begin
                Inc(Passwords);
            end;

        end;

        FDrive.PasswdLength := (Passwords * PASSWD_RECLEN);

        if (FDrive.PasswdLength > 0) then begin

            try
                SetLength(FDrive.Passwd, FDrive.PasswdLength);
            except
                on e: Exception do begin
                    FFileSystemError := e.Message;
                    Result := False;
                    exit;
                end;
            end;

            Passwords := 0;
            for IndexI := 0 to FDrive.MaxDir - 1 do begin

                if ((FDirectory[IndexI].Status >= 16) and (FDirectory[IndexI].Status <= 31)) then begin
                    Value := (Passwords * PASSWD_RECLEN);
                    Inc(Passwords);
                    FDrive.Passwd[Value + 0] := char(Ord('0') + ((FDirectory[IndexI].Status - 16) div 10));
                    FDrive.Passwd[Value + 1] := char(Ord('0') + ((FDirectory[IndexI].Status - 16) mod 10));

                    for IndexJ := 0 to 7 do begin
                        FDrive.Passwd[Value + 2 + IndexJ] := char((Ord(FDirectory[IndexI].Name[IndexJ]) and $7F));
                    end;

                    if ((Ord(FDirectory[IndexI].Ext[0]) and $7F) <> 0) then begin
                        FDrive.Passwd[Value + 10] := '.';
                    end
                    else begin
                        FDrive.Passwd[Value + 10] := ' ';
                    end;

                    for IndexJ := 0 to 2 do begin
                        FDrive.Passwd[Value + 11 + IndexJ] := char((Ord(FDirectory[IndexI].Ext[IndexJ]) and $7F));
                    end;

                    FDrive.Passwd[Value + 14] := ' ';

                    for IndexJ := 0 to 7 do begin
                        FDrive.Passwd[Value + 15 + IndexJ] :=
                            char(FDirectory[IndexI].Pointers[7 - IndexJ] xor FDirectory[IndexI].Lrc);
                    end;

                    FDrive.Passwd[Value + 23] := char(13);
                end;

            end;

        end;

        // disc label
        for IndexI := 0 to FDrive.MaxDir - 1 do begin

            if (FDirectory[IndexI].Status = $20) then begin
                FDrive.CnotaTime := (FDirectory[IndexI].Extnol and $10);

                if ((FDirectory[IndexI].Extnol and $01) <> 0) then begin
                    FDrive.LabelLength := 12;

                    try
                        SetLength(FDrive.DiskLabel, FDrive.LabelLength);
                    except
                        on e: Exception do begin
                            FFileSystemError := Format('out of memory  (%s)', [e.Message]);
                            Result := False;
                            exit;
                        end;
                    end;

                    for IndexJ := 0 to 7 do begin
                        FDrive.DiskLabel[IndexJ] := char(Ord(FDirectory[IndexI].Name[IndexJ]) and $7F);
                    end;

                    for IndexJ := 0 to 2 do begin
                        FDrive.DiskLabel[IndexJ + 8] := char(Ord(FDirectory[IndexI].Ext[IndexJ]) and $7F);
                    end;

                    FDrive.DiskLabel[11] := char(13);
                end
                else begin
                    FDrive.LabelLength := 0;
                end;

                break;
            end;

        end;

        if (IndexI = FDrive.MaxDir) then begin
            FDrive.CnotaTime := 1;
            FDrive.LabelLength := 0;
        end;

    end
    else begin
        FDrive.PasswdLength := 0;
        FDrive.CnotaTime := 1;
        FDrive.LabelLength := 0;
    end;

    FDrive.DirtyDirectory := False;
    FRoot.Ino := FDrive.MaxDir;
    FRoot.Mode := (S_IFDIR or &0777);
    FRoot.Size := 0;
    FRoot.ATime := 0;
    FRoot.MTime := 0;
    FRoot.CTime := 0;
    FDrive.DirtyDateStamp := False;

    if (CheckDateStamps) then begin
        FDrive.OsType := (FDrive.OsType or CPMFS_DS_DATES);
    end
    else begin
        FDateStamps := nil;
    end;

    Result := True;
end;

// --------------------------------------------------------------------------------
//  -- expand CP/M style wildcards
// --------------------------------------------------------------------------------
procedure TCpmFileSystem.Glob(const AArgv: PChar; var AGargc: integer; var AGargv: TStringList);
var
    Dir: TCpmFile;
    DirEnt: array of TCpmDirent = nil;
    Entries, DirSize: integer;
    IndexJ: integer;
begin
    AGargc := 0;
    OpenDir(Dir);
    Entries := 0;
    DirSize := 8;

    // allocate DirEntrys array
    try
        SetLength(DirEnt, DirSize);
    except
        on e: Exception do begin
            FFileSystemError := e.Message;
            exit;
        end;
    end;

    while (ReadDir(Dir, DirEnt[Entries])) do begin
        Inc(Entries);

        if (Entries = DirSize) then begin
            DirSize := (DirSize * 2);
            SetLength(DirEnt, DirSize);
        end;

    end;

    for IndexJ := 0 to Entries - 1 do begin

        if (Match(DirEnt[IndexJ].Name, AArgv)) then begin
            AGargv.Add(DirEnt[IndexJ].Name);
            Inc(AGargc);
        end;

    end;

end;

// --------------------------------------------------------------------------------
//  -- statfs
// --------------------------------------------------------------------------------
procedure TCpmFileSystem.StatFs(var ABuffer: TCpmStatFS);
var
    IndexI, IndexJ, Temp: integer;
begin
    ABuffer.F_BSize := FDrive.BlkSiz;
    ABuffer.F_Blocks := FDrive.Size;
    ABuffer.F_BFree := 0;
    ABuffer.F_BUsed := -(FDrive.DirBlks);

    for IndexI := 0 to FDrive.AlvSize - 1 do begin
        Temp := FAllocationVector[IndexI];

        for IndexJ := 0 to INTBITS - 1 do begin

            if (((IndexI * INTBITS) + IndexJ) < FDrive.Size) then begin

                if ((1 and temp) <> 0) then begin
                    Inc(ABuffer.F_BUsed);
                end
                else begin
                    Inc(ABuffer.F_BFree);
                end;

            end;

            Temp := (Temp shr 1);
        end;

    end;

    ABuffer.F_BAvail := ABuffer.F_BFree;
    ABuffer.F_Files := FDrive.MaxDir;
    ABuffer.F_FFree := 0;

    for IndexI := 0 to FDrive.MaxDir - 1 do begin

        if (FDirectory[IndexI].Status = $E5) then begin
            Inc(ABuffer.F_FFree);
        end;

    end;

    ABuffer.F_NameLen := 11;
end;

// --------------------------------------------------------------------------------
//  -- map name to inode
// --------------------------------------------------------------------------------
function TCpmFileSystem.Name2Inode(const AFilename: PChar; var AInode: TCpmInode): boolean;
var
    User, ProtectMode, Ext, ExtNo, Block: integer;
    HighestExtno, HighestExt, LowestExtno, LowestExt: integer;
    Name: array[0..7] of char;
    Extension: array[0..2] of char;
begin
    HighestExt := -1;
    LowestExt := -1;
    ProtectMode := 0;

    if not (S_ISDIR(FRoot.Mode)) then begin
        FFileSystemError := 'no such file';
        Result := False;
        exit;
    end;

    // root directory
    if ((AFilename = '.') or (AFilename = '..')) then begin
        AInode := FRoot;
        Result := True;
        exit;
    end
    // access passwords
    else if ((AFilename = '[passwd]') and (FDrive.PasswdLength > 0)) then begin
        AInode.Attr := 0;
        AInode.Ino := (FDrive.MaxDir + 1);
        AInode.Mode := (S_IFREG or &0444);
        AInode.ATime := 0;
        AInode.MTime := 0;
        AInode.CTime := 0;
        AInode.Size := FDrive.PasswdLength;
        Result := True;
        exit;
    end
    // access label
    else if ((AFilename = '[label]') and (FDrive.LabelLength > 0)) then begin
        AInode.Attr := 0;
        AInode.Ino := (FDrive.MaxDir + 2);
        AInode.Mode := (S_IFREG or &0444);
        AInode.ATime := 0;
        AInode.MTime := 0;
        AInode.CTime := 0;
        AInode.Size := FDrive.LabelLength;
        Result := True;
        exit;
    end;

    if not (SplitFilename(AFilename, FDrive.OsType, Name, Extension, User)) then begin
        Result := False;
        exit;
    end;

    // find highest and lowest extent
    AInode.Size := 0;
    Ext := -1;
    HighestExtno := -1;
    LowestExtno := 2049;
    Ext := FindFileExtent(User, Name, Extension, Ext + 1, -1);

    while (Ext <> -1) do begin
        ExtNo := EXTENT(FDirectory[Ext].Extnol, FDirectory[Ext].Extnoh);

        if (ExtNo > HighestExtno) then begin
            HighestExtno := ExtNo;
            HighestExt := Ext;
        end;

        if (ExtNo < LowestExtno) then begin
            LowestExtno := ExtNo;
            LowestExt := Ext;
        end;

        Ext := FindFileExtent(User, Name, Extension, Ext + 1, -1);
    end;

    if (HighestExtno = -1) then begin
        Result := False;
        exit;
    end;

    // calculate size
    AInode.Size := (HighestExtno * FDrive.Extentsize);

    if (FDrive.Size <= 256) then begin

        for Block := 15 downto 0 do begin

            if (FDirectory[HighestExt].Pointers[Block] <> 0) then begin
                break;
            end;

        end;

    end
    else begin

        for Block := 7 downto 0 do begin

            if ((FDirectory[HighestExt].Pointers[2 * Block] <> 0) or (FDirectory[HighestExt].Pointers[(2 * Block) + 1] <> 0))
            then begin
                break;
            end;

        end;

    end;

    if (FDirectory[HighestExt].Blkcnt <> 0) then begin
        AInode.Size := AInode.Size + (((FDirectory[HighestExt].Blkcnt and $FF) - 1) * 128);

        if ((FDrive.OsType and CPMFS_ISX) <> 0) then begin
            AInode.Size := AInode.Size + (128 - FDirectory[HighestExt].Lrc);
        end
        else begin

            if (FDirectory[HighestExt].Lrc <> 0) then begin
                AInode.Size := AInode.Size + (FDirectory[HighestExt].Lrc and $FF);
            end
            else begin
                AInode.Size := AInode.Size + 128;
            end;

        end;

    end;

    AInode.Ino := LowestExt;
    AInode.Mode := S_IFREG;
    // read timestamps
    ProtectMode := ReadTimeStamps(AInode, LowestExt);
    // Determine the inode attributes
    AInode.Attr := 0;

    if ((Ord(FDirectory[LowestExt].Name[0]) and $80) <> 0) then begin
        AInode.Attr := AInode.Attr or CPM_ATTR_F1;
    end;

    if ((Ord(FDirectory[LowestExt].Name[1]) and $80) <> 0) then begin
        AInode.Attr := AInode.Attr or CPM_ATTR_F2;
    end;

    if ((Ord(FDirectory[LowestExt].Name[2]) and $80) <> 0) then begin
        AInode.Attr := AInode.Attr or CPM_ATTR_F3;
    end;

    if ((Ord(FDirectory[LowestExt].Name[3]) and $80) <> 0) then begin
        AInode.Attr := AInode.Attr or CPM_ATTR_F4;
    end;

    if ((Ord(FDirectory[LowestExt].Ext[0]) and $80) <> 0) then begin
        AInode.Attr := AInode.Attr or CPM_ATTR_RO;
    end;

    if ((Ord(FDirectory[LowestExt].Ext[1]) and $80) <> 0) then begin
        AInode.Attr := AInode.Attr or CPM_ATTR_SYS;
    end;

    if ((Ord(FDirectory[LowestExt].Ext[2]) and $80) <> 0) then begin
        AInode.Attr := AInode.Attr or CPM_ATTR_ARCV;
    end;

    if ((ProtectMode and $20) <> 0) then begin
        AInode.Attr := AInode.Attr or CPM_ATTR_PWDEL;
    end;

    if ((ProtectMode and $40) <> 0) then begin
        AInode.Attr := AInode.Attr or CPM_ATTR_PWWRITE;
    end;

    if ((ProtectMode and $80) <> 0) then begin
        AInode.Attr := AInode.Attr or CPM_ATTR_PWREAD;
    end;

    if ((Ord(FDirectory[LowestExt].Ext[1]) and $80) <> 0) then begin
        AInode.Mode := AInode.Mode or &01000;
    end;

    AInode.Mode := AInode.Mode or &0444;

    if not ((Ord(FDirectory[LowestExt].Ext[0]) and $80) <> 0) then begin
        AInode.Mode := AInode.Mode or &0222;
    end;

    if ((Extension[0] = 'C') and (Extension[1] = 'O') and (Extension[2] = 'M')) then begin
        AInode.Mode := AInode.Mode or &0111;
    end;

    ReadDsStamps(AInode, LowestExt);
    Result := True;
end;

// --------------------------------------------------------------------------------
//  -- stat
// --------------------------------------------------------------------------------
procedure TCpmFileSystem.Stat(const AInode: TCpmInode; var ABuffer: TCpmStat);
begin
    ABuffer.Ino := AInode.Ino;
    ABuffer.Mode := AInode.Mode;
    ABuffer.Size := AInode.Size;
    ABuffer.ATime := AInode.ATime;
    ABuffer.MTime := AInode.MTime;
    ABuffer.CTime := AInode.CTime;
end;

// --------------------------------------------------------------------------------
//  -- get CP/M attributes
// --------------------------------------------------------------------------------
procedure TCpmFileSystem.AttrGet(const AInode: TCpmInode; var AAttrib: cpm_attr_t);
begin
    AAttrib := AInode.Attr;
end;

// --------------------------------------------------------------------------------
//  -- free actual drive
// --------------------------------------------------------------------------------
function TCpmFileSystem.Unmount: boolean;
begin
    Result := Sync;

    if ((FDrive.OsType and CPMFS_DS_DATES) <> 0) then begin
        SetLength(FDateStamps, 0);
        FDateStamps := nil;
    end;

    SetLength(FAllocationVector, 0);
    FAllocationVector := nil;
    SetLength(FSkewTab, 0);
    FSkewTab := nil;
    SetLength(FDirectory, 0);
    FDirectory := nil;

    if (FDrive.PasswdLength > 0) then begin
        SetLength(FDrive.Passwd, 0);
        FDrive.Passwd := nil;
    end;

    if (FDrive.LabelLength > 0) then begin
        SetLength(FDrive.DiskLabel, 0);
        FDrive.DiskLabel := nil;
    end;

end;

// --------------------------------------------------------------------------------
//  -- rename a file
// --------------------------------------------------------------------------------
function TCpmFileSystem.Rename(const AOldName: PChar; const ANewName: PChar): boolean;
var
    Extent, OldUser, NewUser: integer;
    OldName: array[0..7] of char;
    OldExt: array[0..2] of char;
    NewName: array[0..7] of char;
    NewExt: array[0..2] of char;
begin

    if not S_ISDIR(FRoot.Mode) then begin
        FFileSystemError := 'no such file';
        Result := False;
        exit;
    end;

    if not SplitFilename(AOldName, FDrive.OsType, OldName, OldExt, OldUser) then begin
        Result := False;
        exit;
    end;

    if not SplitFilename(ANewName, FDrive.OsType, NewName, NewExt, NewUser) then begin
        Result := False;
        exit;
    end;

    Extent := FindFileExtent(OldUser, OldName, OldExt, 0, -1);

    if (Extent = -1) then begin
        Result := False;
        exit;
    end;

    if (FindFileExtent(NewUser, NewName, NewExt, 0, -1) <> -1) then begin
        FFileSystemError := 'file already exists';
        Result := False;
        exit;
    end;

    repeat
        FDrive.DirtyDirectory := True;
        FDirectory[Extent].Status := NewUser;
        FDirectory[Extent].Name := NewName;
        FDirectory[Extent].Ext := NewExt;
        Extent := FindFileExtent(OldUser, OldName, OldExt, Extent + 1, -1);
    until (Extent = -1);

    Result := True;
end;

// --------------------------------------------------------------------------------
//  -- delete cp/m-file
// --------------------------------------------------------------------------------
function TCpmFileSystem.Delete(const AFileName: PChar): boolean;
var
    User, Extent: integer;
    Name: array[0..7] of char;
    Extension: array[0..2] of char;
begin

    if not S_ISDIR(FRoot.Mode) then begin
        FFileSystemError := 'no such file';
        Result := False;
        exit;
    end;

    if not SplitFilename(AFileName, FDrive.OsType, Name, Extension, User) then begin
        Result := False;
        exit;
    end;

    Extent := FindFileExtent(User, Name, Extension, 0, -1);

    if (Extent = -1) then begin
        Result := False;
        exit;
    end;

    FDrive.DirtyDirectory := True;

    repeat
        FDirectory[Extent].Status := $E5;
        Extent := FindFileExtent(User, Name, Extension, Extent + 1, -1);
    until (Extent <= 0);

    AlvInit;
    Result := True;
end;

// --------------------------------------------------------------------------------
//  -- write directory back
// --------------------------------------------------------------------------------
function TCpmFileSystem.Sync: boolean;
var
    IndexI, IndexJ, Blocks: integer;
    DirectoryBuffer: array of byte = nil;
begin

    if (FDrive.DirtyDirectory) then begin

        // allocate directory buffer
        try
            SetLength(DirectoryBuffer, ((((FDrive.MaxDir * 32) + FDrive.BlkSiz - 1) div FDrive.BlkSiz) * FDrive.BlkSiz));
        except
            on e: Exception do begin
                FFileSystemError := e.Message;
                Result := False;
                exit;
            end;
        end;

        // copy directory entries into buffer
        IndexI := Low(DirectoryBuffer);

        while (IndexI <= High(DirectoryBuffer)) do begin

            with (FDirectory[(IndexI div SizeOf(TPhysDirectoryEntry))]) do begin
                DirectoryBuffer[IndexI + 0] := Status;

                for IndexJ := 0 to 7 do begin
                    DirectoryBuffer[IndexI + 1 + IndexJ] := Ord(Name[IndexJ]);
                end;

                for IndexJ := 0 to 2 do begin
                    DirectoryBuffer[IndexI + 9 + IndexJ] := Ord(Ext[IndexJ]);
                end;

                DirectoryBuffer[IndexI + 12] := Extnol;
                DirectoryBuffer[IndexI + 13] := Lrc;
                DirectoryBuffer[IndexI + 14] := Extnoh;
                DirectoryBuffer[IndexI + 15] := Blkcnt;

                for IndexJ := 0 to 15 do begin
                    DirectoryBuffer[IndexI + 16 + IndexJ] := Pointers[IndexJ];
                end;

                Inc(IndexI, SizeOf(TPhysDirectoryEntry));
            end;

        end;

        Blocks := (((FDrive.MaxDir * 32) + (FDrive.BlkSiz - 1)) div FDrive.BlkSiz);

        for IndexI := 0 to Blocks - 1 do begin

            if not (WriteBlock(IndexI, @DirectoryBuffer[IndexI * FDrive.BlkSiz], 0, -1)) then begin
                Result := False;
                exit;
            end;

        end;

        FDrive.DirtyDirectory := False;

    end;

    if ((FDrive.OsType and CPMFS_DS_DATES) <> 0) then begin
        SyncDateStamps;
    end;

    Result := True;
end;

// --------------------------------------------------------------------------------
function TCpmFileSystem.GetErrorMsg: string;
begin
    Result := FFileSystemError;
end;

// --------------------------------------------------------------------------------
function TCpmFileSystem.GetFileSystemInfo: TFileSystemInfo;
var
    Info: TFileSystemInfo;
begin

    with (Info) do begin
        Tracks := IntToStr(FDrive.Tracks);
        Sectors := IntToStr(FDrive.SecTrk);
        SecBytes := IntToStr(FDrive.SecLength);
        BlockSize := IntToStr(FDrive.BlkSiz);
        MaxDir := IntToStr(FDrive.MaxDir);
        BootSectors := IntToStr(BootOffset);
        Offset := IntToStr(FDrive.Offset);
        skew := IntToStr(FDrive.Skew);

        case (FDrive.OsType and CPMFS_DR3) of
            CPMFS_DR22: System := 'CP/M 2.2';
            CPMFS_DR3: System := 'CP/M 3';
            CPMFS_ISX: System := 'ISX';
            CPMFS_P2DOS: System := 'P2DOS';
            else System := 'unknown';
        end;

    end;

    Result := Info;
end;

// --------------------------------------------------------------------------------
constructor TCpmFileSystem.Create(ACpmDevice: TCpmDevice);
begin
    inherited Create;
    FCpmDevice := ACpmDevice;
end;

// --------------------------------------------------------------------------------
destructor TCpmFileSystem.Destroy;
begin
    inherited Destroy;
end;

// --------------------------------------------------------------------------------
procedure TCpmFileSystem.AlvInit;
var
    IndexI, IndexJ: integer;
    Offset, Block: integer;
    MaxUser: integer;
begin
    // clean bitmap
    for IndexI := Low(FAllocationVector) to High(FAllocationVector) do begin
        FAllocationVector[IndexI] := 0;
    end;

    // mark directory blocks as used
    // A directory may cover more blocks than an int may hold bits, so a loop is needed.
    for Block := 0 to FDrive.DirBlks - 1 do begin
        Offset := (Block div INTBITS);
        FAllocationVector[Offset] := (FAllocationVector[Offset] or (1 shl (Block mod INTBITS)));
    end;

    if ((FDrive.OsType and CPMFS_HI_USER) <> 0) then begin
        MaxUser := 31;
    end
    else begin
        MaxUser := 15;
    end;

    // mark file blocks as used
    for IndexI := 0 to FDrive.MaxDir - 1 do begin

        if ((FDirectory[IndexI].Status >= 0) and (FDirectory[IndexI].Status <= MaxUser)) then begin
            IndexJ := 0;

            while (IndexJ < 16) do begin
                Block := FDirectory[IndexI].Pointers[IndexJ];

                if (FDrive.Size > 256) then begin
                    Inc(IndexJ);
                    Block := Block + (FDirectory[IndexI].Pointers[IndexJ] shl 8);
                end;

                if ((Block <> 0) and (Block < FDrive.Size)) then begin
                    Offset := (Block div INTBITS);
                    FAllocationVector[Offset] := FAllocationVector[Offset] or (1 shl (Block mod INTBITS));
                end;

                Inc(IndexJ);
            end;

        end;

    end;

end;

// --------------------------------------------------------------------------------
//  -- read super block from amstrad disk
// --------------------------------------------------------------------------------
function TCpmFileSystem.AmstradReadSuper: boolean;
var
    BootSector: array[0..511] of byte;
    BootSpec: integer;
    IDString1, IDString2, IDString3: string;
begin
    FCpmDevice.SetGeometry(512, 9, 40, 0);

    if (not (FCpmDevice.ReadSector(0, 0, BootSector))) then begin
        FFileSystemError := Format('Failed to read Amstrad superblock  (%s)', [FCpmDevice.GetErrorMsg()]);
        Result := False;
        Exit;
    end;

    if ((BootSector[0] = 0) or (BootSector[0] = 3)) then begin
        BootSpec := 0;
    end
    else begin
        BootSpec := -1;
    end;

    { Check for JCE's extension to allow Amstrad and MSDOS superblocks
      in the same sector (for the PCW16) }
    SetString(IDString1, pansichar(@BootSector[$2B]), 4);
    SetString(IDString2, pansichar(@BootSector[$33]), 3);
    SetString(IDString3, pansichar(@BootSector[$7C]), 4);

    if (((BootSector[0] = $E9) or (BootSector[0] = $EB)) and (IDString1 = 'CP/M') and
        (IDString2 = 'DSK') and (IDString3 = 'CP/M')) then begin
        BootSpec := 128;
    end;

    if (BootSpec = -1) then begin
        FFileSystemError := 'Amstrad superblock not present';
        Result := False;
        exit;
    end;

    { boot_spec[0] = format number: 0 for SS SD, 3 for DS DD
               [1] = single/double sided and density flags
               [2] = cylinders per side
               [3] = sectors per cylinder
               [4] = Physical sector shift, 2 => 512
               [5] = Reserved track count
               [6] = Block shift
               [7] = No. of directory blocks }

    FDrive.OsType := CPMFS_DR3;  // Amstrads are CP/M 3 systems
    FDrive.SecLength := (128 shl BootSector[BootSpec + 4]);
    FDrive.Tracks := BootSector[BootSpec + 2];

    if ((BootSector[BootSpec + 1] and 3) <> 0) then begin
        FDrive.Tracks := FDrive.Tracks * 2;
    end;

    FDrive.SecTrk := BootSector[BootSpec + 3];
    FDrive.BlkSiz := (128 shl BootSector[BootSpec + 6]);
    FDrive.MaxDir := ((FDrive.BlkSiz div 32) * BootSector[BootSpec + 7]);
    FDrive.DirBlks := 0;
    FDrive.Skew := 1;  // Amstrads skew at the controller level
    FSkewTab := nil;
    FDrive.BootTrk := BootSector[BootSpec + 5];
    FDrive.BootSec := -1;
    FDrive.Offset := 0;
    FDrive.Size := ((FDrive.SecLength * FDrive.SecTrk * (FDrive.Tracks - FDrive.BootTrk)) div FDrive.BlkSiz);
    if (FDrive.Size > 256) then begin
        FDrive.Extents := ((FDrive.BlkSiz * 8) div 16384);
    end
    else begin
        FDrive.Extents := ((FDrive.BlkSiz * 16) div 16384);
    end;
    FDrive.Extentsize := 16384;
    Result := True;
end;

// --------------------------------------------------------------------------------
//  -- read super block from diskdefs file
// --------------------------------------------------------------------------------
function TCpmFileSystem.DiskdefsReadSuper(const AImageType: string): boolean;
var
    DiskDefs: TStringList;
    FoundDefinition: boolean;
    DefinitionLine, SkewTabItems: TStringArray;
    LineNumber: integer;
    Pass, Sectors, SkewItem: integer;
    Value: off_t;
    Multiplier: cardinal;
    Spezifier: string;
begin
    FoundDefinition := False;
    FDrive.Skew := 1;
    FDrive.Extents := 0;
    FDrive.Extentsize := 16384;
    FDrive.OsType := CPMFS_DR22;
    FSkewTab := nil;
    FDrive.Offset := 0;
    FDrive.BlkSiz := -1;
    FDrive.BootTrk := -1;
    FDrive.BootSec := -1;
    FDrive.SecLength := -1;
    FDrive.SecTrk := -1;
    FDrive.Tracks := -1;
    FDrive.MaxDir := -1;
    FDrive.DirBlks := 0;
    Result := True;
    try
        DiskDefs := TStringList.Create;
        DiskDefs.LoadFromFile('diskdefs');
        for LineNumber := 1 to Diskdefs.Count do begin
            DefinitionLine := Diskdefs[LineNumber - 1].Trim.Split(' ');

            if (FoundDefinition) then begin

                if ((Length(DefinitionLine) >= 1) and (DefinitionLine[0] = 'end')) then begin
                    FDrive.Size := ((((FDrive.SecTrk * FDrive.Tracks) - BootOffset) * FDrive.SecLength) div FDrive.BlkSiz);

                    if (FDrive.Extents = 0) then begin

                        if (FDrive.Size > 256) then begin
                            FDrive.Extents := ((FDrive.BlkSiz * 8) div FDrive.Extentsize);
                        end
                        else begin
                            FDrive.Extents := ((FDrive.BlkSiz * 16) div FDrive.Extentsize);
                        end;

                    end;

                    if (FDrive.Extents = 0) then begin
                        FDrive.Extents := 1;
                    end;

                    break;
                end
                else if (Length(DefinitionLine) = 2) then begin

                    if (DefinitionLine[0] = 'seclen') then begin
                        FDrive.SecLength := StrToIntDef(DefinitionLine[1], -1);
                    end
                    else if (DefinitionLine[0] = 'tracks') then begin
                        FDrive.Tracks := StrToIntDef(DefinitionLine[1], -1);
                    end
                    else if (DefinitionLine[0] = 'sectrk') then begin
                        FDrive.SecTrk := StrToIntDef(DefinitionLine[1], -1);
                    end
                    else if (DefinitionLine[0] = 'blocksize') then begin
                        FDrive.BlkSiz := StrToIntDef(DefinitionLine[1], -1);

                        if (FDrive.BlkSiz <= 0) then begin
                            FFileSystemError :=
                                Format('invalid blocksize ''%s'' in line %d', [DefinitionLine[1], (LineNumber)]);
                            Result := False;
                            exit;
                        end;

                    end
                    else if (DefinitionLine[0] = 'maxdir') then begin
                        FDrive.MaxDir := StrToIntDef(DefinitionLine[1], -1);
                    end
                    else if (DefinitionLine[0] = 'dirblks') then begin
                        FDrive.DirBlks := StrToIntDef(DefinitionLine[1], -1);
                    end
                    else if (DefinitionLine[0] = 'skew') then begin
                        FDrive.Skew := StrToIntDef(DefinitionLine[1], -1);
                    end
                    else if (DefinitionLine[0] = 'skewtab') then begin
                        SkewTabItems := DefinitionLine[1].Trim.Split(',');

                        for Pass := 0 to 1 do begin

                            for Sectors := 0 to Length(SkewTabItems) - 1 do begin
                                SkewItem := StrToIntDef(SkewTabItems[Sectors], -1);

                                if (Pass = 1) then begin
                                    FSkewTab[Sectors] := SkewItem;
                                end;

                                if (SkewItem = -1) then begin
                                    FFileSystemError :=
                                        Format('invalid skewtab ''%s'' in line %d', [DefinitionLine[1], (LineNumber)]);
                                    Result := False;
                                    exit;
                                end;

                            end;

                            if (Pass = 0) then begin
                                SetLength(FSkewTab, Sectors + 1);
                            end;

                        end;

                    end
                    else if (DefinitionLine[0] = 'boottrk') then begin
                        FDrive.BootTrk := StrToIntDef(DefinitionLine[1], -1);
                    end
                    else if (DefinitionLine[0] = 'bootsec') then begin
                        FDrive.BootSec := StrToIntDef(DefinitionLine[1], -1);
                    end
                    else if (DefinitionLine[0] = 'offset') then begin
                        Multiplier := 1;

                        if not (DefinitionLine[1][1] in ['0'..'9']) then begin
                            FFileSystemError :=
                                Format('offset value ''%s'' is not a number in line %d', [DefinitionLine[1], (LineNumber)]);
                            Result := False;
                            exit;
                        end;

                        for Pass := 1 to DefinitionLine[1].Length do begin

                            if not (DefinitionLine[1][Pass] in ['0'..'9']) then begin
                                Spezifier := RightStr(DefinitionLine[1], (DefinitionLine[1].Length - Pass + 1));
                                break;
                            end;

                        end;

                        Value := StrToIntDef(LeftStr(DefinitionLine[1], (Pass - 1)), 0);

                        if ((Value >= MaxInt) or (Value <= (not MaxInt))) then begin
                            FFileSystemError :=
                                Format('invalid offset value ''%s'' in line %d', [DefinitionLine[1], (LineNumber)]);
                            Result := False;
                            exit;
                        end;


                        if (not Spezifier.IsEmpty) then begin

                            case (UpperCase(Spezifier[1])) of
                                'K': begin
                                    Multiplier := 1024;
                                end;
                                'M': begin
                                    Multiplier := 1024 * 1024;
                                end;
                                'T': begin

                                    if ((FDrive.SecTrk < 0) or (FDrive.Tracks < 0) or (FDrive.SecLength < 0)) then begin
                                        FFileSystemError :=
                                            Format('offset must be specified after sectrk, tracks and secLength in line %d',
                                            [(LineNumber)]);
                                        Result := False;
                                        exit;
                                    end;

                                    Multiplier := (FDrive.SecTrk * FDrive.SecLength);
                                end;
                                'S': begin

                                    if ((FDrive.SecTrk < 0) or (FDrive.Tracks < 0) or (FDrive.SecLength < 0)) then begin
                                        FFileSystemError :=
                                            Format('offset must be specified after sectrk, tracks and secLength in line %d',
                                            [(LineNumber)]);
                                        Result := False;
                                        exit;
                                    end;

                                    Multiplier := FDrive.SecLength;
                                end;
                                else begin
                                    FFileSystemError :=
                                        Format('unknown unit specifier ''%s'' in line %d',
                                        [DefinitionLine[1][Pass], (LineNumber)]);
                                    Result := False;
                                    exit;
                                end;
                            end;

                        end;

                        if ((Value * Multiplier) > MaxInt) then begin
                            FFileSystemError := Format('effective offset is out of range in line %d', [(LineNumber)]);
                            Result := False;
                            exit;
                        end;

                        FDrive.Offset := (Value * Multiplier);
                    end
                    else if (DefinitionLine[0] = 'logicalextents') then begin
                        FDrive.Extents := StrToIntDef(DefinitionLine[1], -1);
                    end
                    else if (DefinitionLine[0] = 'extentsize') then begin
                        FDrive.Extentsize := StrToIntDef(DefinitionLine[1], -1);

                        if (FDrive.Extentsize > 16384) then begin
                            FFileSystemError := Format('extentsize > 16384 in line %d', [(LineNumber)]);
                            Result := False;
                            exit;
                        end;

                    end

                    else if (DefinitionLine[0] = 'os') then begin

                        case (DefinitionLine[1]) of
                            '2.2': begin
                                FDrive.OsType := (FDrive.OsType or CPMFS_DR22);
                            end;
                            '3': begin
                                FDrive.OsType := (FDrive.OsType or CPMFS_DR3);
                            end;
                            'isx': begin
                                FDrive.OsType := (FDrive.OsType or CPMFS_ISX);
                            end;
                            'p2dos': begin
                                FDrive.OsType := (FDrive.OsType or CPMFS_P2DOS);
                            end;
                            'zsys': begin
                                FDrive.OsType := (FDrive.OsType or CPMFS_ZSYS);
                            end;
                            else begin
                                FFileSystemError :=
                                    Format('invalid OS type ''%s'' in line %d', [DefinitionLine[1], (LineNumber)]);
                                Result := False;
                                exit;
                            end;
                        end;

                    end;
                end
                else if ((Length(DefinitionLine) > 0) and not (DefinitionLine[0][1] = '#') and not (DefinitionLine[0][1] = ';'))
                then begin
                    FFileSystemError := Format('invalid keyword ''%s'' in line %d', [DefinitionLine[0], (LineNumber)]);
                    Result := False;
                    exit;
                end;
            end
            else if ((Length(DefinitionLine) >= 2) and (DefinitionLine[0] = 'diskdef') and (DefinitionLine[1] = AImageType)) then
            begin
                FoundDefinition := True;
            end;
        end;
    finally
        FreeAndNil(DiskDefs);
    end;

    if (not FoundDefinition) then begin
        FFileSystemError := Format('unknown format %s', [AImageType]);
        Result := False;
        exit;
    end;

    if ((FDrive.BootTrk < 0) and (FDrive.BootSec < 0)) then begin
        FFileSystemError := 'boottrk/bootsec parameter invalid or missing from diskdef';
        Result := False;
        exit;
    end;

    if (FDrive.SecLength < 0) then begin
        FFileSystemError := 'secLength parameter invalid or missing from diskdef';
        Result := False;
        exit;
    end;

    if (FDrive.SecTrk < 0) then begin
        FFileSystemError := 'sectrk parameter invalid or missing from diskdef';
        Result := False;
        exit;
    end;

    if (FDrive.Tracks < 0) then begin
        FFileSystemError := 'tracks parameter invalid or missing from diskdef';
        Result := False;
        exit;
    end;

    if (FDrive.BlkSiz < 0) then begin
        FFileSystemError := 'blocksize parameter invalid or missing from diskdef';
        Result := False;
        exit;
    end;

    if (FDrive.MaxDir < 0) then begin
        FFileSystemError := 'maxdir parameter invalid or missing from diskdef';
        Result := False;
        exit;
    end;
end;

// --------------------------------------------------------------------------------
//  -- find the logical sector number of the CP/M directory
// --------------------------------------------------------------------------------
function TCpmFileSystem.BootOffset: integer;
begin
    if (FDrive.BootSec >= 0) then begin
        Result := FDrive.BootSec;
    end
    else begin
        Result := (FDrive.BootTrk * FDrive.SecTrk);
    end;
end;

// --------------------------------------------------------------------------------
//  -- read a (partial) block
// --------------------------------------------------------------------------------
function TCpmFileSystem.ReadBlock(ABlockNr: integer; ABuffer: pbyte; AStart, AEnd: integer): boolean;
var
    Sect, Track, Counter: integer;
begin

    if (ABlockNr >= FDrive.Size) then begin
        FFileSystemError := 'attempting to access block beyond end of disk';
        Result := False;
        exit;
    end;

    if (AEnd < 0) then begin
        AEnd := ((FDrive.BlkSiz div FDrive.SecLength) - 1);
    end;

    Sect := (((ABlockNr * (FDrive.BlkSiz div FDrive.SecLength)) + BootOffset) mod FDrive.SecTrk);
    Track := (((ABlockNr * (FDrive.BlkSiz div FDrive.SecLength)) + BootOffset) div FDrive.SecTrk);

    for Counter := 0 to AEnd do begin

        if (Counter >= AStart) then begin

            if not (FCpmDevice.ReadSector(Track, FSkewTab[Sect], ABuffer[FDrive.SecLength * Counter])) then begin
                FFileSystemError := FCpmDevice.GetErrorMsg;
                Result := False;
                exit;
            end;

        end;

        Inc(Sect);

        if (Sect >= FDrive.SecTrk) then begin
            Sect := 0;
            Inc(Track);
        end;

    end;

    Result := True;
end;

// --------------------------------------------------------------------------------
//  -- write a (partial) block
// --------------------------------------------------------------------------------
function TCpmFileSystem.WriteBlock(ABlockNr: integer; const ABuffer: pbyte; AStart, AEnd: integer): boolean;
var
    Sect, Track, Counter: integer;
begin

    if (AEnd < 0) then begin
        AEnd := ((FDrive.BlkSiz div FDrive.SecLength) - 1);
    end;

    Sect := (((ABlockNr * (FDrive.BlkSiz div FDrive.SecLength)) + BootOffset) mod FDrive.SecTrk);
    Track := (((ABlockNr * (FDrive.BlkSiz div FDrive.SecLength)) + BootOffset) div FDrive.SecTrk);

    for Counter := 0 to AEnd do begin

        if (Counter >= AStart) then begin

            if not (FCpmDevice.WriteSector(Track, FSkewTab[Sect], ABuffer[FDrive.SecLength * Counter])) then begin
                FFileSystemError := FCpmDevice.GetErrorMsg;
                Result := False;
                exit;
            end;

        end;

        Inc(Sect);

        if (Sect >= FDrive.SecTrk) then begin
            Sect := 0;
            Inc(Track);
        end;

    end;

    Result := True;
end;

// --------------------------------------------------------------------------------
//  -- find first/next extent for a file
// --------------------------------------------------------------------------------
function TCpmFileSystem.FindFileExtent(AUser: integer; const AName: array of char; const AExt: array of char;
    AStart: integer; AExtNo: integer): integer;
var
    MaxUser: integer;
begin
    FFileSystemError := 'file already exists';

    if ((FDrive.OsType and CPMFS_HI_USER) <> 0) then begin
        MaxUser := 31;
    end
    else begin
        MaxUser := 15;
    end;

    while (AStart < FDrive.MaxDir) do begin

        if ((FDirectory[AStart].Status <= MaxUser) and ((AExtNo = -1) or
            ((EXTENT(FDirectory[AStart].Extnol, FDirectory[AStart].Extnoh) div FDrive.Extents) =
            (AExtNo div FDrive.Extents))) and IsMatching(AUser, AName, AExt, FDirectory[AStart].Status,
            FDirectory[AStart].Name, FDirectory[AStart].Ext)) then begin
            Result := (AStart);
            exit;
        end;

        Inc(AStart);
    end;

    FFileSystemError := 'file not found';
    Result := (-1);
end;

// --------------------------------------------------------------------------------
//  -- read CP/M time stamp
// --------------------------------------------------------------------------------
function TCpmFileSystem.ReadTimeStamps(var AInode: TCpmInode; ALowestExt: integer): integer;
var
    DirDate: TPhysDirectoryEntry;
    U_Days, U_Hour, U_Min: integer;
    Ca_Days, Ca_Hour, Ca_Min: integer;
    ProtectMode: integer;
begin
    U_Days := 0;
    U_Hour := 0;
    U_Min := 0;
    Ca_Days := 0;
    Ca_Hour := 0;
    Ca_Min := 0;
    ProtectMode := 0;

    DirDate := FDirectory[ALowestExt or 3];

    if (((FDrive.OsType and CPMFS_CPM3_DATES) <> 0) and (DirDate.Status = $21)) then begin

        case (ALowestExt and 3) of
            // first entry of the four
            0: begin
                Ca_Days := (Ord(DirDate.Name[0]) + (Ord(DirDate.Name[1]) shl 8));
                Ca_Hour := Ord(DirDate.Name[2]);
                Ca_Min := Ord(DirDate.Name[3]);
                U_Days := (Ord(DirDate.Name[4]) + (Ord(DirDate.Name[5]) shl 8));
                U_Hour := Ord(DirDate.Name[6]);
                U_Min := Ord(DirDate.Name[7]);
                ProtectMode := Ord(DirDate.Ext[0]);
            end;
            // second entry
            1: begin
                Ca_Days := (Ord(DirDate.Ext[2]) + (Ord(DirDate.Extnol) shl 8));
                Ca_Hour := DirDate.Lrc;
                Ca_Min := DirDate.Extnoh;
                U_Days := (DirDate.Blkcnt + (DirDate.Pointers[0] shl 8));
                U_Hour := DirDate.Pointers[1];
                U_Min := DirDate.Pointers[2];
                ProtectMode := DirDate.Pointers[3];
            end;
            // third one
            2: begin
                Ca_Days := (DirDate.Pointers[5] + (DirDate.Pointers[6] shl 8));
                Ca_Hour := DirDate.Pointers[7];
                Ca_Min := DirDate.Pointers[8];
                U_Days := (DirDate.Pointers[9] + (DirDate.Pointers[10] shl 8));
                U_Hour := DirDate.Pointers[11];
                U_Min := DirDate.Pointers[12];
                ProtectMode := DirDate.Pointers[13];
            end;
        end;

        if (FDrive.CnotaTime <> 0) then begin
            AInode.CTime := Cpm2UnixTime(Ca_Days, Ca_Hour, Ca_Min);
            AInode.ATime := 0;
        end
        else begin
            AInode.CTime := 0;
            AInode.ATime := Cpm2UnixTime(Ca_Days, Ca_Hour, Ca_Min);
        end;

        AInode.MTime := Cpm2UnixTime(U_Days, U_Hour, U_Min);
    end
    else begin
        AInode.ATime := 0;
        AInode.MTime := 0;
        AInode.CTime := 0;
        ProtectMode := 0;
    end;

    Result := (ProtectMode);
end;

// --------------------------------------------------------------------------------
//  -- read datestamper time stamp
// --------------------------------------------------------------------------------
procedure TCpmFileSystem.ReadDsStamps(var AInode: TCpmInode; ALowestExt: integer);
var
    Stamp: TDateStamps;
begin

    if not ((FDrive.OsType and CPMFS_DS_DATES) <> 0) then begin
        exit;
    end;

    // Get datestamp
    Stamp := FDateStamps[ALowestExt];
    AInode.MTime := Ds2UnixTime(Stamp.Modify);
    AInode.CTime := Ds2UnixTime(Stamp.Create);
    AInode.ATime := Ds2UnixTime(Stamp.Access);
end;

// --------------------------------------------------------------------------------
//  -- read all datestamper timestamps
// --------------------------------------------------------------------------------
function TCpmFileSystem.CheckDateStamps: boolean;
var
    DSOffset, DSBlocks, DSRecords: integer;
    IndexI, IndexJ, CheckSum, Offset: integer;
    DateStampsBuffer: array of byte = nil;
begin

    if (not IsMatching(0, '!!!TIME&', 'DAT', FDirectory[0].Status, FDirectory[0].Name, FDirectory[0].Ext)) then begin
        Result := False;
        exit;
    end;

    // Offset to ds file in alloc blocks
    DSOffset := (((FDrive.MaxDir * 32) + (FDrive.BlkSiz - 1)) div FDrive.BlkSiz);
    DSRecords := ((FDrive.MaxDir + 7) div 8);
    DSBlocks := (((DSRecords * 128) + (FDrive.BlkSiz - 1)) div FDrive.BlkSiz);

    // allocate datestamps buffer
    try
        SetLength(DateStampsBuffer, (FDrive.MaxDir * DSRecords));
    except
        on e: Exception do begin
            FFileSystemError := e.Message;
            Result := False;
            exit;
        end;
    end;

    Offset := 0;

    // Read ds file in its entirety
    for IndexI := DSOffset to (DSOffset + DSBlocks) - 1 do begin

        if (not ReadBlock(IndexI, @DateStampsBuffer[Offset], 0, -1)) then begin
            Result := False;
            exit;
        end;

        Inc(Offset, FDrive.BlkSiz);

    end;

    // allocate datestamps record
    try
        SetLength(FDateStamps, FDrive.MaxDir);
    except
        on e: Exception do begin
            FFileSystemError := e.Message;
            Result := False;
            exit;
        end;
    end;

    // copy buffer into datestamps record
    IndexI := Low(DateStampsBuffer);

    while (IndexI <= High(DateStampsBuffer)) do begin

        with (FDateStamps[(IndexI div SizeOf(TDateStamps))]) do begin
            Create.Year := DateStampsBuffer[IndexI + 0];
            Create.Month := DateStampsBuffer[IndexI + 1];
            Create.Day := DateStampsBuffer[IndexI + 2];
            Create.Hour := DateStampsBuffer[IndexI + 3];
            Create.Minute := DateStampsBuffer[IndexI + 4];
            Access.Year := DateStampsBuffer[IndexI + 5];
            Access.Month := DateStampsBuffer[IndexI + 6];
            Access.Day := DateStampsBuffer[IndexI + 7];
            Access.Hour := DateStampsBuffer[IndexI + 8];
            Access.Minute := DateStampsBuffer[IndexI + 9];
            Modify.Year := DateStampsBuffer[IndexI + 10];
            Modify.Month := DateStampsBuffer[IndexI + 11];
            Modify.Day := DateStampsBuffer[IndexI + 12];
            Modify.Hour := DateStampsBuffer[IndexI + 13];
            Modify.Minute := DateStampsBuffer[IndexI + 14];
            CheckSum := DateStampsBuffer[IndexI + 15];
            Inc(IndexI, SizeOf(TDateStamps));
        end;

    end;

    // Verify checksums
    Offset := 0;

    for IndexI := 0 to DSRecords - 1 do begin
        CheckSum := 0;

        for IndexJ := 0 to 126 do begin
            CheckSum := CheckSum + DateStampsBuffer[IndexJ + Offset];
        end;

        if (DateStampsBuffer[IndexJ + Offset + 1] <> (CheckSum and $FF)) then begin
            SetLength(FDateStamps, 0);
            FDateStamps := nil;
            Result := False;
            exit;
        end;

        Inc(Offset, 128);
    end;

    Result := True;
end;

// --------------------------------------------------------------------------------
//  -- split file name into name and extension
// --------------------------------------------------------------------------------
function TCpmFileSystem.SplitFilename(AFullname: PChar; AOsType: integer; var AName: array of char;
    var AExt: array of char; var AUser: integer): boolean;
var
    IndexI, IndexJ, MaxUser: integer;
begin
    FillChar(AName, 8, ' ');
    FillChar(AExt, 3, ' ');

    if ((not IsDigit(AFullname[0])) and (not IsDigit(AFullname[1]))) then begin
        FFileSystemError := 'illegal CP/M filename';
        Result := False;
        exit;
    end;

    AUser := (10 * (Ord(AFullname[0]) - Ord('0'))) + (Ord(AFullname[1]) - Ord('0'));
    AFullname := AFullname + 2;

    if ((AOsType and CPMFS_HI_USER) <> 0) then begin
        MaxUser := 32;
    end
    else begin
        MaxUser := 16;
    end;

    if ((AFullname[0] = Chr(0)) or (AUser >= MaxUser)) then begin
        FFileSystemError := 'illegal CP/M filename';
        Result := False;
        exit;
    end;

    IndexI := 0;

    while ((IndexI < 8) and (AFullname[IndexI] <> char(0)) and (AFullname[IndexI] <> '.')) do begin

        //if not (ISFILECHAR(IndexI, AFullname[IndexI])) then begin
        if not (IsFileChar(ToUpper(AFullname[IndexI]), AOsType)) then begin
            FFileSystemError := 'illegal CP/M filename';
            Result := False;
            exit;
        end
        else begin
            AName[IndexI] := ToUpper(AFullname[IndexI]);
        end;

        Inc(IndexI);
    end;

    if (IndexI = 0) then begin
        // no filename after user or extension without filename
        FFileSystemError := 'illegal CP/M filename';
        Result := False;
        exit;
    end;

    if (AFullname[IndexI] = '.') then begin
        Inc(IndexI);
        IndexJ := 0;

        while ((IndexJ < 3) and (AFullname[IndexI] <> char(0))) do begin

            if not (IsFileChar(ToUpper(AFullname[IndexI]), AOsType)) then begin
                FFileSystemError := 'illegal CP/M filename';
                Result := False;
                exit;
            end
            else begin
                AExt[IndexJ] := ToUpper(AFullname[IndexI]);
            end;

            Inc(IndexI);
            Inc(IndexJ);
        end;

        if (AFullname[IndexI] <> char(0)) then begin
            FFileSystemError := 'illegal CP/M filename';
            Result := False;
            exit;
        end;

    end;

    Result := True;
end;

// --------------------------------------------------------------------------------
//  -- match filename against a pattern
// --------------------------------------------------------------------------------
function TCpmFileSystem.RecMatch(AEntry: PChar; APattern: PChar): boolean;
var
    First: integer;
begin
    First := 1;

    while (APattern[0] <> CHR(0)) do begin

        case (APattern[0]) of
            '*': begin

                if ((AEntry[0] = '.') and (First <> 0)) then begin
                    Result := True;
                    exit;
                end;

                Inc(APattern);

                while (AEntry[0] <> Chr(0)) do begin

                    if (RecMatch(AEntry, APattern)) then begin
                        Result := True;
                        exit;
                    end
                    else begin
                        Inc(AEntry);
                    end;
                end;

            end;
            '?': begin

                if (AEntry[0] <> Chr(0)) then begin
                    Inc(AEntry);
                    Inc(APattern);
                end
                else begin
                    Result := False;
                    exit;
                end;

            end;
            else begin

                if (LowerCase(AEntry) = LowerCase(APattern)) then begin
                    Inc(AEntry);
                    Inc(APattern);
                end
                else begin
                    Result := False;
                    exit;
                end;

            end;

        end;

        First := 0;
    end;

    Result := ((APattern[0] = Chr(0)) and (AEntry[0] = Chr(0)));
end;

// --------------------------------------------------------------------------------
//  -- match filename against a pattern
// --------------------------------------------------------------------------------
function TCpmFileSystem.Match(const AEntry: PChar; APattern: PChar): boolean;
var
    User: integer;
    Pat: PChar;
begin

    if (IsDigit(APattern[0]) and (APattern[1] = ':')) then begin
        User := StrToIntDef(APattern[0], -1);
        APattern := APattern + 2;
    end
    else if (IsDigit(APattern[0]) and IsDigit(APattern[1]) and (APattern[2] = ':')) then begin
        User := StrToIntDef(APattern[0] + APattern[1], -1);
        APattern := APattern + 3;
    end
    else begin
        User := -1;
    end;

    if (User = -1) then begin
        Pat := PChar(Format('??%s', [APattern]));
    end
    else begin
        Pat := PChar(Format('%.2d%s', [User, APattern]));
    end;

    Result := (RecMatch(AEntry, Pat));
end;

// --------------------------------------------------------------------------------
//  -- do two file names match?
// --------------------------------------------------------------------------------
function TCpmFileSystem.IsMatching(AUser1: integer; const AName1: array of char; const AExt1: array of char;
    AUser2: integer; const AName2: array of char; const AExt2: array of char): boolean;
var
    IndexI: integer;
begin

    if (AUser1 <> AUser2) then begin
        Result := False;
        exit;
    end;

    for IndexI := 0 to 7 do begin

        if ((Ord(AName1[IndexI]) and $7F) <> (Ord(AName2[IndexI]) and $7F)) then begin
            Result := False;
            exit;
        end;

    end;

    for IndexI := 0 to 2 do begin

        if ((Ord(AExt1[IndexI]) and $7F) <> (Ord(AExt2[IndexI]) and $7F)) then begin
            Result := False;
            exit;
        end;

    end;

    Result := True;
end;

// --------------------------------------------------------------------------------
//  -- is character allowed in a name?
// --------------------------------------------------------------------------------
function TCpmFileSystem.IsFileChar(AChar: char; AType: integer): boolean;
begin

    if ((Ord(AChar) and $80) <> 0) then begin
        Result := False;
    end;

    if (AType = CPMFS_DR3) then begin
        Result := ((Ord(AChar) > Ord(' ')) and (Ord(AChar) <> Ord('<')) and (Ord(AChar) <> Ord('>')) and
            (Ord(AChar) <> Ord('.')) and (Ord(AChar) <> Ord(',')) and (Ord(AChar) <> Ord(';')) and
            (Ord(AChar) <> Ord(':')) and (Ord(AChar) <> Ord('=')) and (Ord(AChar) <> Ord('?')) and
            (Ord(AChar) <> Ord('*')) and (Ord(AChar) <> Ord('[')) and (Ord(AChar) <> Ord(']')) and
            (Ord(AChar) <> Ord('|')) and not IsLower(AChar));
    end

    else begin
        Result := ((Ord(AChar) > Ord(' ')) and (Ord(AChar) <> Ord('<')) and (Ord(AChar) <> Ord('>')) and
            (Ord(AChar) <> Ord('.')) and (Ord(AChar) <> Ord(',')) and (Ord(AChar) <> Ord(':')) and
            (Ord(AChar) <> Ord('=')) and (Ord(AChar) <> Ord('?')) and (Ord(AChar) <> Ord('*')) and
            (Ord(AChar) <> Ord('_')) and not IsLower(AChar));
    end;
end;

// --------------------------------------------------------------------------------
//  -- convert CP/M time to UTC
// --------------------------------------------------------------------------------
function TCpmFileSystem.Cpm2UnixTime(ADays: integer; AHour: integer; AMin: integer): time_t;
var
    DateTime: TDateTime;
begin
    ///* CP/M stores timestamps in local time.  We don't know which     */
    ///* timezone was used and if DST was in effect.  Assuming it was   */
    ///* the current offset from UTC is most sensible, but not perfect. */
    DateTime := EncodeDate(1978, 1, 1);
    DateTime := IncDay(DateTime, ADays - 1);
    DateTime := IncHour(DateTime, BCDToInt(AHour));
    Result := IncMinute(DateTime, BCDToInt(AMin));
end;

// --------------------------------------------------------------------------------
//  -- convert DateStamper to Unix time
// --------------------------------------------------------------------------------
function TCpmFileSystem.Ds2UnixTime(const AEntry: TDsEntry): time_t;
var
    Year: integer;
begin

    if ((AEntry.Minute = 0) and (AEntry.Hour = 0) and (AEntry.Day = 0) and (AEntry.Month = 0) and (AEntry.Year = 0)) then begin
        Result := (0);
        exit;
    end;

    Year := BCDToInt(AEntry.Year);

    if (Year < 70) then begin
        Inc(Year, 2000);
    end;

    Result := EncodeDateTime(Year, BCDToInt(AEntry.Month), BCDToInt(AEntry.Day), BCDToInt(AEntry.Hour),
        BCDToInt(AEntry.Minute), 0, 0);
end;

// --------------------------------------------------------------------------------
//  -- write all datestamper timestamps
// --------------------------------------------------------------------------------
function TCpmFileSystem.SyncDateStamps: boolean;
var
    DSOffset, DSBlocks, DSRecords: integer;
    IndexI, IndexJ, CheckSum, Offset: integer;
    DateStampsBuffer: array of byte = nil;
begin

    if (FDrive.DirtyDateStamp) then begin

        DSRecords := ((FDrive.MaxDir + 7) div 8);

        // allocate DateStamps buffer
        try
            SetLength(DateStampsBuffer, (FDrive.MaxDir * DSRecords));
        except
            on e: Exception do begin
                FFileSystemError := e.Message;
                Result := False;
                exit;
            end;
        end;

        // copy DateStamps into buffer
        IndexI := Low(DateStampsBuffer);

        while (IndexI <= High(DateStampsBuffer)) do begin

            with (FDateStamps[(IndexI div SizeOf(TDateStamps))]) do begin
                DateStampsBuffer[IndexI + 0] := Create.Year;
                DateStampsBuffer[IndexI + 1] := Create.Month;
                DateStampsBuffer[IndexI + 2] := Create.Day;
                DateStampsBuffer[IndexI + 3] := Create.Hour;
                DateStampsBuffer[IndexI + 4] := Create.Minute;
                DateStampsBuffer[IndexI + 5] := Access.Year;
                DateStampsBuffer[IndexI + 6] := Access.Month;
                DateStampsBuffer[IndexI + 7] := Access.Day;
                DateStampsBuffer[IndexI + 8] := Access.Hour;
                DateStampsBuffer[IndexI + 9] := Access.Minute;
                DateStampsBuffer[IndexI + 10] := Modify.Year;
                DateStampsBuffer[IndexI + 11] := Modify.Month;
                DateStampsBuffer[IndexI + 12] := Modify.Day;
                DateStampsBuffer[IndexI + 13] := Modify.Hour;
                DateStampsBuffer[IndexI + 14] := Modify.Minute;
                DateStampsBuffer[IndexI + 15] := CheckSum;
                Inc(IndexI, SizeOf(TDateStamps));
            end;

        end;

        // Re-calculate checksums
        Offset := 0;

        for IndexI := 0 to DSRecords - 1 do begin
            CheckSum := 0;

            for IndexJ := 0 to 126 do begin
                CheckSum := CheckSum + DateStampsBuffer[IndexJ + Offset];
            end;

            DateStampsBuffer[IndexJ + Offset + 1] := (CheckSum and $FF);
            Inc(Offset, 128);

        end;

        DSOffset := (((FDrive.MaxDir * 32) + (FDrive.BlkSiz - 1)) div FDrive.BlkSiz);
        DSBlocks := (((DSRecords * 128) + (FDrive.BlkSiz - 1)) div FDrive.BlkSiz);
        Offset := 0;

        for IndexI := DSOffset to (DSOffset + DSBlocks) - 1 do begin

            if not (WriteBlock(IndexI, @DateStampsBuffer[Offset], 0, -1)) then begin
                Result := False;
                exit;
            end;

            Inc(Offset, FDrive.BlkSiz);

        end;

    end;

    Result := True;
end;

// --------------------------------------------------------------------------------
//  -- opendir
// --------------------------------------------------------------------------------
function TCpmFileSystem.OpenDir(var ADir: TCpmFile): boolean;
begin

    if not (S_ISDIR(FRoot.Mode)) then begin
        FFileSystemError := 'no such file';
        Result := False;
        exit;
    end;

    ADir.Ino := FRoot;
    ADir.Pos := 0;
    ADir.Mode := O_RDONLY;
    Result := True;
end;

// --------------------------------------------------------------------------------
//  -- readdir
// --------------------------------------------------------------------------------
function TCpmFileSystem.ReadDir(var ADir: TCpmFile; var AEnt: TCpmDirent): boolean;
var
    IndexI, First, MaxUser: integer;
    Current: TPhysDirectoryEntry;
    Buffer: array[0..13] of char;
    BufferIndex: integer;
    HasExt: boolean;
begin

    if not (S_ISDIR(ADir.Ino.Mode)) then begin      // error: not a directory
        FFileSystemError := 'not a directory';
        Result := False;
        exit;
    end;

    while (True) do begin
        FillChar(Buffer, Length(Buffer), 0);

        if (ADir.Pos = 0) then begin     // first entry is .
            AEnt.Ino := FDrive.MaxDir;
            AEnt.RecLen := 1;
            StrPCopy(AEnt.Name, '.');
            AEnt.Off := ADir.Pos;
            Inc(ADir.Pos);
            Result := True;
            break;
        end
        else if (ADir.Pos = 1) then begin   // next entry is ..
            AEnt.Ino := FDrive.MaxDir;
            AEnt.RecLen := 2;
            StrPCopy(AEnt.Name, '..');
            AEnt.Off := ADir.Pos;
            Inc(ADir.Pos);
            Result := True;
            break;
        end
        else if (ADir.Pos = 2) then begin   // next entry is [passwd]

            if (FDrive.PasswdLength > 0) then begin
                AEnt.Ino := (FDrive.MaxDir + 1);
                AEnt.RecLen := 8;
                StrPCopy(AEnt.Name, '[passwd]');
                AEnt.Off := ADir.Pos;
                Inc(ADir.Pos);
                Result := True;
                break;
            end;

        end
        else if (ADir.Pos = 3) then begin       // next entry is [label]

            if (FDrive.LabelLength > 0) then begin
                AEnt.Ino := (FDrive.MaxDir + 2);
                AEnt.RecLen := 7;
                StrPCopy(AEnt.Name, '[label]');
                AEnt.Off := ADir.Pos;
                Inc(ADir.Pos);
                Result := True;
                break;
            end;

        end
        else if ((ADir.Pos >= RESERVED_ENTRIES) and (ADir.Pos < (FDrive.MaxDir + RESERVED_ENTRIES))) then begin
            First := (ADir.Pos - RESERVED_ENTRIES);
            Current := FDirectory[ADir.Pos - RESERVED_ENTRIES];

            if ((FDrive.OsType and CPMFS_HI_USER) <> 0) then begin
                MaxUser := 31;
            end
            else begin
                MaxUser := 15;
            end;

            if ((Current.Status >= 0) and (Current.Status <= MaxUser)) then begin

                // determine first extent for the current file
                for IndexI := 0 to FDrive.MaxDir - 1 do begin

                    if (IndexI <> (ADir.Pos - RESERVED_ENTRIES)) then begin

                        if (IsMatching(Current.Status, Current.Name, Current.Ext, FDirectory[IndexI].Status,
                            FDirectory[IndexI].Name, FDirectory[IndexI].Ext) and
                            (EXTENT(Current.Extnol, Current.Extnoh) > EXTENT(FDirectory[IndexI].Extnol,
                            FDirectory[IndexI].Extnoh))) then begin
                            First := IndexI;
                        end;

                    end;

                end;

                if (First = (ADir.Pos - RESERVED_ENTRIES)) then begin
                    AEnt.Ino := (ADir.Pos - RESERVED_ENTRIES);
                    // convert file name to UNIX style
                    Buffer[0] := Chr(Ord('0') + (Current.Status div 10));
                    Buffer[1] := Chr(Ord('0') + (Current.Status mod 10));
                    BufferIndex := 2;
                    IndexI := 0;

                    while ((IndexI < 8) and ((Ord(Current.Name[IndexI]) and $7F) <> Ord(' '))) do begin

                        if (FDrive.UpperCase) then begin
                            Buffer[BufferIndex] := Chr(Ord(Current.Name[IndexI]) and $7F);
                        end
                        else begin
                            Buffer[BufferIndex] := LowerCase(Chr(Ord(Current.Name[IndexI]) and $7F));
                        end;

                        Inc(BufferIndex);
                        Inc(IndexI);
                    end;

                    HasExt := False;
                    IndexI := 0;

                    while ((IndexI < 3) and ((Ord(Current.Ext[IndexI]) and $7F) <> Ord(' '))) do begin

                        if not (HasExt) then begin
                            Buffer[BufferIndex] := '.';
                            Inc(BufferIndex);
                            HasExt := True;
                        end;

                        if (FDrive.UpperCase) then begin
                            Buffer[BufferIndex] := Chr(Ord(Current.Ext[IndexI]) and $7F);
                        end
                        else begin
                            Buffer[BufferIndex] := LowerCase(Chr(Ord(Current.Ext[IndexI]) and $7F));
                        end;

                        Inc(BufferIndex);
                        Inc(IndexI);
                    end;

                    AEnt.RecLen := BufferIndex;
                    StrPCopy(AEnt.Name, Buffer);
                    AEnt.Off := ADir.Pos;
                    Inc(ADir.Pos);
                    Result := True;
                    break;
                end;

            end;

        end
        else begin
            Result := False;
            break;
        end;

        Inc(ADir.Pos);
    end;
end;

// --------------------------------------------------------------------------------
end.
