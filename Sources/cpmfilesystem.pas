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
unit CpmFileSystem;

{$mode ObjFPC}
{$H+}

interface

uses
    Classes, SysUtils, CpmDevice, CpmDefs, CifeGlobals;

type
    TCheckMessageCallBack = procedure(AMessage: string) of object;

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
        function OpenImage(const AFileName: string): boolean;
        function CloseImage: boolean;
        function ReadDiskdefData(const AImageType: string; ADiskdefsPath: string): boolean;
        function InitDriveData(AUpperCase: boolean): boolean;
        procedure Glob(const AArgv: PChar; var AGargc: integer; var AGargv: TStringList);
        procedure StatFs(var ABuffer: TCpmStatFS);
        function Name2Inode(const AFilename: PChar; var AInode: TCpmInode): boolean;
        procedure Stat(const AInode: TCpmInode; var ABuffer: TCpmStat);
        procedure AttrGet(const AInode: TCpmInode; var AAttrib: cpm_attr_t);
        procedure AttrSet(var AInode: TCpmInode; const AAttrib: cpm_attr_t);
        function MakeFileSystem(const AImageName: string; const ABootTracks: array of byte;
            AFileSystemLabel: string; ATimeStampsUsed: boolean; AUseUpperCase: boolean): boolean;
        function UnmountFile: boolean;
        function RenameFile(const AOldName: PChar; const ANewName: PChar): boolean;
        function DeleteFile(const AFileName: PChar): boolean;
        function CreateFile(ADirEntry: TCpmInode; const AFileName: string; var AInode: TCpmInode; AMode: mode_t): boolean;
        function OpenFile(AInode: TCpmInode; var AFile: TCpmFile; AMode: mode_t): boolean;
        function ReadFile(var AFile: TCpmFile; ABuffer: pbyte; ACount: size_t): ssize_t;
        function WriteFile(var AFile: TCpmFile; ABuffer: pbyte; ACount: size_t): ssize_t;
        function CloseFile(AFile: TCpmFile): boolean;
        procedure UpdateTime(AInode: TCpmInode; ATimes: TUTimeBuf);
        function CheckFileSystem(ADoRepair: boolean; AMessage: TCheckMessageCallBack): integer;
        function SyncDirectory: boolean;
        function IsFileExisting(ACpmFileName: string): boolean;
        function GetErrorMsg: string;
        function GetFileSystemInfo: TFileSystemInfo;
        function GetBootTrackSize: integer;
        function GetDirectoryRoot: TCpmInode;
        function GetFreeFileSpace: size_t;
        function GetFileSize(AExtent: integer): size_t;
    public  // Konstruktor/Destruktor
        constructor Create; overload;
        destructor Destroy; override;

    protected // Attribute

    protected // Methoden

    private   // Attribute
    type
        TSideOrder = (soAlt, soOutOut, soOutBack);

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

        TDsEntry = packed record
            Year: byte;
            Month: byte;
            Day: byte;
            Hour: byte;
            Minute: byte;
        end;

        TDateStamps = packed record
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
            SideOrder: TSideOrder;
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
            LibdskGeometry: TLibdskGeometry;
            LibdskDeviceOptions: TLibdskDeviceOptions;
        end;

        TIntArray = array of integer;
        TDirArray = packed array of TPhysDirectoryEntry;
        TDsArray = packed array of TDateStamps;

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
        function AllocBlock: integer;
        procedure MemCpy7(var ADestination: array of char; const ASource: array of char; ACount: integer);
        function AmstradReadSuper(): boolean;
        function DiskdefsReadSuper(const AImageType: string; ADiskdefsPath: string): boolean;
        function BootOffset: integer;
        function ReadBlock(ABlockNr: integer; ABuffer: pbyte; AStart, AEnd: integer): boolean;
        function WriteBlock(ABlockNr: integer; const ABuffer: pbyte; AStart, AEnd: integer): boolean;
        function FindFileExtent(AUser: integer; const AName: array of char; const AExt: array of char;
            AStart: integer; AExtNo: integer): integer;
        function FindFreeExtent: integer;
        procedure UpdateTimeStamps(const AInode: TCpmInode; AExtent: integer);
        procedure UpdateDateStamper(const AInode: TCpmInode; AExtent: integer);
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
        procedure Unix2CpmTime(ANow: TDateTime; var ADays: integer; var AHour: integer; var AMin: integer);
        function Ds2UnixTime(const AEntry: TDsEntry): time_t;
        procedure Unix2DsTime(ANow: TDateTime; var AEntry: TDsEntry);
        function SyncDateStamps: boolean;
        function OpenDir(var ADir: TCpmFile): boolean;
        function ReadDir(var ADir: TCpmFile; var AEnt: TCpmDirent): boolean;
        function BcdCheck(AValue: integer; AMax: integer): boolean;
        function PwdCheck(APassword: array of byte; ADecode: byte; var APassWd: string): boolean;
        function DirCheck(AStr: array of char; ALen: size_t; AAllowEmpty: boolean; AType: integer): boolean;
        function PrintFile(AExtent: integer): string;
        procedure CalcSecTrack(ABlockNr: integer; AOffset: integer; var ASect: integer; var ATrack: integer);

    end;

implementation

{ TCpmFileSystem }

uses Character, StrUtils, DateUtils, CpmDevice_Libdsk, CpmDevice_Posix;

// --------------------------------------------------------------------------------
//  -- Open Image
// --------------------------------------------------------------------------------
function TCpmFileSystem.OpenImage(const AFileName: string): boolean;
begin
    Result := True;

    try
        if (FDrive.LibdskGeometry[0] = #0) then begin
            FCpmDevice := TCpmDevice_Posix.Create;
        end
        else begin
            FCpmDevice := TCpmDevice_Libdsk.Create;
        end;

    except
        FFileSystemError := 'Cannot open Image-Device';
        Result := False;
        exit;
    end;

    if not (FCpmDevice.Open(AFileName, FDrive.LibdskDeviceOptions)) then begin
        FFileSystemError := FCpmDevice.GetErrorMsg();
        Result := False;
    end;

end;

// --------------------------------------------------------------------------------
//  -- Close Image
// --------------------------------------------------------------------------------
function TCpmFileSystem.CloseImage: boolean;
begin
    Result := True;

    if not (FCpmDevice.Close) then begin
        FFileSystemError := FCpmDevice.GetErrorMsg();
        Result := False;
    end;

end;

// --------------------------------------------------------------------------------
//  -- get DPB
// --------------------------------------------------------------------------------
function TCpmFileSystem.ReadDiskdefData(const AImageType: string; ADiskdefsPath: string): boolean;
begin

    if (AImageType.Contains('Amstrad (PCW16)')) then begin
        Result := AmstradReadSuper();
    end
    else begin
        Result := DiskdefsReadSuper(AImageType, ADiskdefsPath);
    end;

end;

// --------------------------------------------------------------------------------
//  -- init in-core data for drive
// --------------------------------------------------------------------------------
function TCpmFileSystem.InitDriveData(AUpperCase: boolean): boolean;
var
    IndexI, IndexJ, IndexK, Value: integer;
    Blocks, Passwords: integer;
    PDirectory: pbyte;
begin
    FDrive.UpperCase := AUpperCase;

    // optional field, compute based on directory size
    if (FDrive.DirBlks = 0) then begin
        FDrive.DirBlks := ((FDrive.MaxDir * 32 + (FDrive.BlkSiz - 1)) div FDrive.BlkSiz);
    end;

    FCpmDevice.SetGeometry(FDrive.SecLength, FDrive.SecTrk, FDrive.Tracks, FDrive.Offset, FDrive.LibdskGeometry);

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
        SetLength(FDirectory, FDrive.MaxDir);
        IndexI := Length(FDirectory);
        PDirectory := @FDirectory[0];
    except
        on e: Exception do begin
            FFileSystemError := e.Message;
            Result := False;
            exit;
        end;
    end;

    if not (FCpmDevice.IsOpen) then begin // create empty directory in core

        for IndexI := 0 to (FDrive.MaxDir * 32) do begin
            (PDirectory +IndexI)^ := $E5;
        end;

    end
    else begin  // ReadFile directory in core
        Blocks := (((FDrive.MaxDir * 32) + FDrive.BlkSiz - 1) div FDrive.BlkSiz);

        for IndexI := 0 to Blocks - 1 do begin

            if not (ReadBlock(IndexI, (PDirectory + (IndexI * FDrive.BlkSiz)), 0, -1)) then begin
                Result := False;
                exit;
            end;

        end;

    end;

    AlvInit;

    // ReadFile additional superblock information
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

    // ReadFile timestamps
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
//  -- get CP/M attributes and protections
// --------------------------------------------------------------------------------
procedure TCpmFileSystem.AttrGet(const AInode: TCpmInode; var AAttrib: cpm_attr_t);
begin
    AAttrib := AInode.Attr;
end;

// --------------------------------------------------------------------------------
//  -- set CP/M attributes and protections
// --------------------------------------------------------------------------------
procedure TCpmFileSystem.AttrSet(var AInode: TCpmInode; const AAttrib: cpm_attr_t);
var
    ExtentNo, LowestExt: integer;
    Name: array[0..7] of char;
    Extension: array[0..2] of char;
    User: byte;
    ProtectMode: integer;
begin
    FillChar(Name, 8, Chr(0));
    FillChar(Extension, 3, Chr(0));
    ExtentNo := AInode.Ino;
    FDrive.DirtyDirectory := True;
    MemCpy7(Name, FDirectory[ExtentNo].Name, 8);
    MemCpy7(Extension, FDirectory[ExtentNo].Ext, 3);
    User := FDirectory[ExtentNo].Status;

    // And set new ones
    if ((AAttrib and CPM_ATTR_F1) <> 0) then begin
        Name[0] := Chr(Ord(Name[0]) or $80);
    end;

    if ((AAttrib and CPM_ATTR_F2) <> 0) then begin
        Name[1] := Chr(Ord(Name[1]) or $80);
    end;

    if ((AAttrib and CPM_ATTR_F3) <> 0) then begin
        Name[2] := Chr(Ord(Name[2]) or $80);
    end;

    if ((AAttrib and CPM_ATTR_F4) <> 0) then begin
        Name[3] := Chr(Ord(Name[3]) or $80);
    end;

    if ((AAttrib and CPM_ATTR_RO) <> 0) then begin
        Extension[0] := Chr(Ord(Extension[0]) or $80);
    end;

    if ((AAttrib and CPM_ATTR_SYS) <> 0) then begin
        Extension[1] := Chr(Ord(Extension[1]) or $80);
    end;

    if ((AAttrib and CPM_ATTR_ARCV) <> 0) then begin
        Extension[2] := Chr(Ord(Extension[2]) or $80);
    end;

    repeat
        FDirectory[ExtentNo].Name := Name;
        FDirectory[ExtentNo].Ext := Extension;
        ExtentNo := FindFileExtent(User, Name, Extension, ExtentNo + 1, -1);
    until (ExtentNo = -1);

    // Update the stored (inode) copies of the file attributes and mode
    AInode.Attr := AAttrib;

    if ((AAttrib and CPM_ATTR_RO) <> 0) then begin
        AInode.Mode := (AInode.Mode and not (S_IWUSR or S_IWGRP or S_IWOTH));
    end
    else begin
        AInode.Mode := (AInode.Mode or (S_IWUSR or S_IWGRP or S_IWOTH));
    end;

    ProtectMode := 0;
    LowestExt := AInode.Ino;

    if ((AAttrib and CPM_ATTR_PWDEL) <> 0) then begin
        ProtectMode := (ProtectMode or $20);
    end;

    if ((AAttrib and CPM_ATTR_PWWRITE) <> 0) then begin
        ProtectMode := (ProtectMode or $40);
    end;

    if ((AAttrib and CPM_ATTR_PWREAD) <> 0) then begin
        ProtectMode := (ProtectMode or $80);
    end;

    if (((FDrive.OsType and CPMFS_CPM3_DATES) <> 0) and (FDirectory[LowestExt or 3].Status = $21)) then begin

        case (LowestExt and 3) of

            0: begin
                FDirectory[LowestExt or 3].Ext[0] := Chr(ProtectMode);
            end;

            1: begin
                FDirectory[LowestExt or 3].Pointers[3] := ProtectMode;
            end;

            2: begin
                FDirectory[LowestExt or 3].Pointers[13] := ProtectMode;
            end;

        end;

    end;

end;

// --------------------------------------------------------------------------------
//  -- create new empty binary Image-File
// --------------------------------------------------------------------------------
function TCpmFileSystem.MakeFileSystem(const AImageName: string; const ABootTracks: array of byte;
    AFileSystemLabel: string; ATimeStampsUsed: boolean; AUseUpperCase: boolean): boolean;
var
    ImageFile: file;
    TrackBytes: longword;
    IndexI, IndexJ, Count, DirBytes: longword;
    Buffer, FirstBuffer: array[0..127] of byte;
    DateTime: TDateTime;
    Minute, Hour, Days, Offset: integer;
    ImageSize, Records, CheckSum: longword;
    Inode: TCpmInode;
    DateStampFile: TCpmFile;
    Times: TUTimeBuf;
    PDateStamps: pbyte;
const
    Signature = '!!!TIME';
begin
    // check for already opened image
    if FCpmDevice.IsOpen then begin
        FCpmDevice.Close;
    end;

    // check for empty label
    if (AFileSystemLabel.IsEmpty) then begin
        AFileSystemLabel := 'UNLABELED';
    end;

    // OpenFile image file
    try
        FileMode := fmOpenWrite;
        AssignFile(ImageFile, AImageName);
        Rewrite(ImageFile, 1);
    except
        on e: Exception do begin
            FFileSystemError := e.Message;
            Result := False;
            exit;
        end;
    end;


    ///* WriteFile system tracks */
    ///* this initialises only whole tracks, so it skew is not an issue */
    TrackBytes := (FDrive.SecLength * FDrive.SecTrk);
    IndexI := 0;

    while (IndexI < (TrackBytes * FDrive.BootTrk)) do begin

        try
            BlockWrite(ImageFile, ABootTracks[IndexI], FDrive.SecLength, Count);
        except
            on e: Exception do begin

                if (Count <> FDrive.SecLength) then begin
                    FFileSystemError := e.Message;
                    Close(ImageFile);
                    Result := False;
                    exit;
                end;

            end;
        end;

        Inc(IndexI, FDrive.SecLength);
    end;

    // WriteFile directory
    for IndexI := Low(Buffer) to High(Buffer) do begin
        Buffer[IndexI] := $E5;
    end;

    DirBytes := (FDrive.MaxDir * 32);

    if ((DirBytes mod TrackBytes) <> 0) then begin
        DirBytes := (((DirBytes + TrackBytes) div TrackBytes) * TrackBytes);
    end;

    if (ATimeStampsUsed and ((FDrive.OsType = CPMFS_P2DOS) or (FDrive.OsType = CPMFS_DR3))) then begin
        Buffer[3 * 32] := $21;
    end;

    FirstBuffer := Buffer;

    if (FDrive.OsType = CPMFS_DR3) then begin
        FirstBuffer[0] := $20;

        for IndexI := 1 to 11 do begin

            if (IndexI <= Length(AFileSystemLabel)) then begin
                FirstBuffer[IndexI] := (Ord(UpCase(AFileSystemLabel[IndexI])) and $7F);
            end
            else begin
                FirstBuffer[IndexI] := Ord(' ');
            end;

        end;

        // label set and first time stamp is creation date
        if (ATimeStampsUsed) then begin
            FirstBuffer[12] := $11;
        end
        else begin
            FirstBuffer[12] := $01;
        end;

        FillByte(FirstBuffer[13], 1 + 2 + 8, 0);

        if (ATimeStampsUsed) then begin
            DateTime := Now;
            Minute := (((MinuteOf(DateTime) div 10) shl 4) or (MinuteOf(DateTime) mod 10));
            Hour := (((HourOf(DateTime) div 10) shl 4) or (HourOf(DateTime) mod 10));
            Days := DaysBetween(DateTime, EncodeDate(1978, 1, 1)) + 1;
            FirstBuffer[24] := (Days and $FF);
            FirstBuffer[28] := (Days and $FF);
            FirstBuffer[25] := (Days shr 8);
            FirstBuffer[29] := (Days shr 8);
            FirstBuffer[26] := Hour;
            FirstBuffer[30] := Hour;
            FirstBuffer[27] := Minute;
            FirstBuffer[31] := Minute;
        end;

    end;

    IndexI := 0;
    while (IndexI < DirBytes) do begin

        try

            if (IndexI = 0) then begin
                BlockWrite(ImageFile, FirstBuffer[0], 128, Count);
            end
            else begin
                BlockWrite(ImageFile, Buffer[0], 128, Count);
            end;

        except
            on e: Exception do begin

                if (Count <> 128) then begin
                    FFileSystemError := e.Message;
                    Close(ImageFile);
                    Result := False;
                    exit;
                end;

            end;
        end;

        Inc(IndexI, 128);

    end;

    // fill remaining size
    for IndexI := Low(Buffer) to High(Buffer) do begin
        Buffer[IndexI] := $E5;
    end;

    ImageSize := (FDrive.SecLength * FDrive.SecTrk * FDrive.Tracks);

    IndexI := 0;
    while (IndexI < (ImageSize - (DirBytes + (TrackBytes * FDrive.BootTrk)))) do begin
        try
            BlockWrite(ImageFile, Buffer[0], 128, Count);
        except
            on e: Exception do begin

                if (Count <> 128) then begin
                    FFileSystemError := e.Message;
                    Close(ImageFile);
                    Result := False;
                    exit;
                end;

            end;
        end;

        Inc(IndexI, 128);

    end;

    // CloseFile image file
    try
        Close(ImageFile);
    except
        on e: Exception do begin

            if (Count <> 128) then begin
                FFileSystemError := e.Message;
                Result := False;
                exit;
            end;

        end;
    end;

    if (ATimeStampsUsed and not ((FDrive.OsType = CPMFS_P2DOS) or (FDrive.OsType = CPMFS_DR3))) then begin

        if not FCpmDevice.Open(AImageName, '') then begin
            FFileSystemError := Format('Cannot open %s  (%s)', [ExtractFileName(AImageName), FCpmDevice.GetErrorMsg()]);
            Result := False;
            exit;
        end;

        InitDriveData(AUseUpperCase);
        Records := ((FDrive.MaxDir + 7) div 8);

        try
            SetLength(FDateStamps, (Records * 128));
            PDateStamps := @FDateStamps[0];
        except
            SyncDirectory;
            Result := False;
            exit;
        end;

        FillByte(PDateStamps^, (Records * 128 * SizeOf(TDateStamps)), 0);
        Offset := 15;

        for IndexI := 0 to (Records - 1) do begin

            for IndexJ := 0 to 6 do begin
                (PDateStamps +Offset)^ := Ord(Signature[IndexJ + 1]);
                Inc(Offset, 16);
            end;

            // skip checksum byte
            Inc(Offset, 16);
            CheckSum := 0;

            for IndexJ := 0 to 126 do begin
                CheckSum := CheckSum + (PDateStamps + (IndexI * 128) + IndexJ)^;
            end;

            (PDateStamps +((IndexI * 128) + IndexJ + 1))^ := (CheckSum and $FF);
            Inc(IndexJ);

        end;

        // The filesystem does not know about datestamper yet, because it was not there when it was mounted.
        if (not CreateFile(FRoot, '00!!!TIME&.DAT', Inode, &0222)) then begin
            FFileSystemError := Format('Unable to create DateStamper file.  (%s)', [FFileSystemError]);
            Result := False;
            exit;
        end;

        if ((not OpenFile(Inode, DateStampFile, O_WRONLY)) or (WriteFile(DateStampFile, PDateStamps, (Records * 128)) <>
            (Records * 128)) or (not CloseFile(DateStampFile))) then begin
            FFileSystemError := Format('Unable to write DateStamper file.  (%s)', [FFileSystemError]);
            Result := False;
            exit;
        end;

        AttrSet(Inode, 0);
        Name2Inode('00!!!TIME&.DAT', Inode);
        Times.AcTime := Now;
        times.ModTime := Now;
        UpdateTime(Inode, Times);
        FDrive.DirtyDateStamp := True;
        SyncDirectory;

    end;

    Result := True;
end;

// --------------------------------------------------------------------------------
//  -- free actual drive
// --------------------------------------------------------------------------------
function TCpmFileSystem.UnmountFile: boolean;
begin
    Result := SyncDirectory;

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
function TCpmFileSystem.RenameFile(const AOldName: PChar; const ANewName: PChar): boolean;
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
        FFileSystemError := 'file name already exists';
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
function TCpmFileSystem.DeleteFile(const AFileName: PChar): boolean;
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
//  -- creat new CP/M file
// --------------------------------------------------------------------------------
function TCpmFileSystem.CreateFile(ADirEntry: TCpmInode; const AFileName: string; var AInode: TCpmInode; AMode: mode_t): boolean;
type
    PPhysDirectoryEntry = ^TPhysDirectoryEntry;
var
    User: integer;
    Name: array[0..7] of char;
    Extension: array[0..2] of char;
    Ext: integer;
    DirEntry: PPhysDirectoryEntry;
begin

    if not (S_ISDIR(ADirEntry.Mode)) then begin
        FFileSystemError := 'no such file or directory';
        Result := False;
        exit;
    end;

    if not (SplitFilename(PChar(AFileName), FDrive.OsType, Name, Extension, User)) then begin
        Result := False;
        exit;
    end;

    if (FindFileExtent(User, Name, Extension, 0, -1) <> -1) then begin
        Result := False;
        exit;
    end;

    Ext := FindFreeExtent;
    if (Ext = -1) then begin
        Result := False;
        exit;
    end;

    DirEntry := @FDirectory[Ext];
    FDrive.DirtyDirectory := True;
    FillByte(DirEntry^, 32, 0);
    DirEntry^.Status := User;
    DirEntry^.Name := Name;
    DirEntry^.Ext := Extension;
    AInode.Ino := Ext;
    AInode.Mode := (S_IFREG or AMode);
    AInode.Size := 0;
    AInode.ATime := Now;
    AInode.MTime := Now;
    AInode.CTime := Now;
    UpdateTimeStamps(AInode, Ext);
    UpdateDateStamper(AInode, Ext);
    Result := True;
end;

// --------------------------------------------------------------------------------
//  -- open a CP/M file
// --------------------------------------------------------------------------------
function TCpmFileSystem.OpenFile(AInode: TCpmInode; var AFile: TCpmFile; AMode: mode_t): boolean;
begin

    if (S_ISREG(AInode.Mode)) then begin

        if (((AMode and O_WRONLY) <> 0) and ((AInode.Mode and &222) = 0)) then begin
            FFileSystemError := 'permission denied';
            Result := False;
            exit;
        end;

        AFile.Pos := 0;
        AFile.Ino := AInode;
        AFile.Mode := AMode;
        Result := True;
    end
    else begin
        FFileSystemError := 'not a regular file';
        Result := False;
    end;

end;

// --------------------------------------------------------------------------------
//  -- read a file from CP/M filesystem
// --------------------------------------------------------------------------------
function TCpmFileSystem.ReadFile(var AFile: TCpmFile; ABuffer: pbyte; ACount: size_t): ssize_t;
var
    FindExt, FindBlock, Ext, Block, Extentno, Got, NextBlockPos, NextExtPos: integer;
    BlockSize, ExtCap, Ptr, RdStart, RdEnd: integer;
    Buffer: array of byte;
begin
    FindExt := 1;
    FindBlock := 1;
    Ext := -1;
    Block := -1;
    Extentno := -1;
    Got := 0;
    NextBlockPos := -1;
    NextExtPos := -1;
    BlockSize := FDrive.BlkSiz;

    try
        SetLength(Buffer, BlockSize);
    except

        on e: Exception do begin
            FFileSystemError := Format('error creating block buffer.' + LineEnding + '%s', [e.Message]);
            Result := -1;
            exit;
        end;

    end;

    if (FDrive.Size <= 256) then begin
        ExtCap := (16 * BlockSize);
    end
    else begin
        ExtCap := (8 * BlockSize);
    end;

    if (ExtCap > 16384) then begin
        ExtCap := (16384 * FDrive.Extents);
    end;

    if (AFile.Ino.Ino = (FDrive.MaxDir + 1)) then begin    // [passwd]

        if ((AFile.Pos + ACount) > AFile.Ino.Size) then begin
            ACount := (AFile.Ino.Size - AFile.Pos);
        end;

        if (ACount <> 0) then begin
            Move(FDrive.Passwd[AFile.Pos], ABuffer^, ACount);
        end;

        Inc(AFile.Pos, ACount);
        Result := (ACount);
        Exit;
    end
    else if (AFile.Ino.Ino = (FDrive.MaxDir + 2)) then begin    // [label]

        if ((AFile.Pos + ACount) > AFile.Ino.Size) then begin
            ACount := (AFile.Ino.Size - AFile.Pos);
        end;

        if (ACount <> 0) then begin
            Move(FDrive.DiskLabel[AFile.Pos], ABuffer^, ACount);
        end;

        Inc(AFile.Pos, ACount);
        Result := (ACount);
        Exit;
    end
    else begin
        while ((ACount > 0) and (AFile.Pos < AFile.Ino.Size)) do begin

            if (FindExt <> 0) then begin
                Extentno := (AFile.Pos div FDrive.Extentsize);
                Ext := FindFileExtent(FDirectory[AFile.Ino.Ino].Status, FDirectory[AFile.Ino.Ino].Name,
                    FDirectory[AFile.Ino.Ino].Ext, 0, Extentno);
                NextExtPos := (((AFile.Pos div ExtCap) * ExtCap) + ExtCap);
                FindExt := 0;
                FindBlock := 1;
            end;

            if (FindBlock <> 0) then begin

                if (Ext <> -1) then begin
                    Ptr := ((AFile.Pos mod ExtCap) div BlockSize);

                    if (FDrive.Size > 256) then begin
                        Ptr := (Ptr * 2);
                    end;

                    Block := FDirectory[Ext].Pointers[Ptr];

                    if (FDrive.Size > 256) then begin
                        Block := (Block + (FDirectory[Ext].Pointers[Ptr + 1] shl 8));
                    end;

                    if (Block = 0) then begin
                        FillByte(Buffer[0], BlockSize, 0);
                    end
                    else begin
                        RdStart := ((AFile.Pos mod BlockSize) div FDrive.SecLength);

                        if (((AFile.Pos mod BlockSize) + ACount) > BlockSize) then begin
                            RdEnd := ((BlockSize - 1) div FDrive.SecLength);
                        end
                        else begin
                            RdEnd := (((AFile.Pos mod BlockSize) + ACount - 1) div FDrive.SecLength);
                        end;

                        if (Block < FDrive.DirBlks) then begin
                            FFileSystemError := 'Attempting to access block before beginning of data';

                            if (Got = 0) then begin
                                Got := -1;
                            end;

                            Break;
                        end;

                        if (not ReadBlock(Block, @Buffer[0], RdStart, RdEnd)) then begin

                            if (Got = 0) then begin
                                Got := -1;
                            end;

                        end;

                    end;

                end;

                NextBlockPos := (((AFile.Pos div BlockSize) * BlockSize) + BlockSize);
                FindBlock := 0;
            end;

            if (AFile.Pos < NextBlockPos) then begin

                if (Ext = -1) then begin
                    ABuffer^ := 0;
                    Inc(ABuffer);
                end
                else begin
                    ABuffer^ := Buffer[AFile.Pos mod BlockSize];
                    Inc(ABuffer);
                end;

                Inc(AFile.Pos);
                Inc(Got);
                Dec(ACount);
            end
            else if (AFile.Pos = NextExtPos) then begin
                FindExt := 1;
            end
            else begin
                FindBlock := 1;
            end;

        end;

    end;

    Result := (Got);
end;

// --------------------------------------------------------------------------------
//  -- write a file to CP/M filesystem
// --------------------------------------------------------------------------------
function TCpmFileSystem.WriteFile(var AFile: TCpmFile; ABuffer: pbyte; ACount: size_t): ssize_t;
var
    FindExt, FindBlock, Ext, ExtNo, Got, NextBlockPos, NextExtPos: integer;
    Block, WrStart, WrEnd, DataPtr, Last: integer;
    BlockSize, ExtCap: integer;
    Buffer: array[0..16383] of byte;
begin
    FindExt := 1;
    FindBlock := -1;
    Ext := -1;
    ExtNo := -1;
    Got := 0;
    NextBlockPos := -1;
    NextExtPos := -1;
    BlockSize := FDrive.BlkSiz;
    Block := -1;
    WrStart := -1;
    WrEnd := -1;
    DataPtr := -1;
    Last := -1;

    if (FDrive.Size <= 256) then begin
        ExtCap := (16 * BlockSize);
    end
    else begin
        ExtCap := (8 * BlockSize);
    end;

    if (ExtCap > 16384) then begin
        ExtCap := (16384 * FDrive.Extents);
    end;

    while (ACount > 0) do begin

        if (FindExt <> 0) then begin
            ExtNo := (AFile.Pos div FDrive.Extentsize);
            Ext := FindFileExtent(FDirectory[AFile.Ino.Ino].Status, FDirectory[AFile.Ino.Ino].Name,
                FDirectory[AFile.Ino.Ino].Ext, 0, ExtNo);
            NextExtPos := (((AFile.Pos div ExtCap) * ExtCap) + ExtCap);

            if (Ext = -1) then begin
                Ext := FindFreeExtent;

                if (Ext = -1) then begin

                    if (Got = 0) then begin
                        Result := (-1);
                    end
                    else begin
                        Result := (Got);
                    end;

                    exit;
                end;

                FDirectory[Ext] := FDirectory[AFile.Ino.Ino];
                FillByte(FDirectory[Ext].Pointers, 16, 0);
                FDirectory[Ext].Extnol := EXTENTL(ExtNo);
                FDirectory[Ext].Extnoh := EXTENTH(ExtNo);
                FDirectory[Ext].Blkcnt := 0;
                FDirectory[Ext].Lrc := 0;
                AFile.Ino.CTime := Now;
                UpdateTimeStamps(AFile.Ino, Ext);
                UpdateDateStamper(AFile.Ino, Ext);
                //}
            end;

            FindExt := 0;
            FindBlock := 1;
        end;

        if (FindBlock <> 0) then begin
            DataPtr := ((AFile.Pos mod ExtCap) div BlockSize);

            if (FDrive.Size > 256) then begin
                DataPtr := (DataPtr * 2);
            end;

            Block := FDirectory[Ext].Pointers[DataPtr];

            if (FDrive.Size > 256) then begin
                Block := (Block + (FDirectory[Ext].Pointers[DataPtr + 1] shl 8));
            end;

            // allocate new block, set start/end to cover it
            if (Block = 0) then begin
                Block := AllocBlock;

                if (Block = -1) then begin

                    if (Got = 0) then begin
                        Result := (-1);
                    end
                    else begin
                        Result := (Got);
                    end;

                    exit;
                end;

                FDirectory[Ext].Pointers[DataPtr] := (Block and $FF);

                if (FDrive.Size > 256) then begin
                    FDirectory[Ext].Pointers[DataPtr + 1] := ((Block shr 8) and $FF);
                end;

                WrStart := 0;
                // By setting end to the end of the block and not the end
                // of the currently written data, the whole block gets
                // wiped from the disk, which is slow, but convenient in
                // case of sparse files.
                WrEnd := ((BlockSize - 1) div FDrive.SecLength);
                FillByte(Buffer, BlockSize, 0);
                AFile.Ino.CTime := Now;
                UpdateTimeStamps(AFile.Ino, Ext);
                UpdateDateStamper(AFile.Ino, Ext);
            end
            // ReadFile existing block and set start/end to cover modified parts
            else begin
                WrStart := ((AFile.Pos mod BlockSize) div FDrive.SecLength);

                if (((AFile.Pos mod BlockSize) + ACount) >= BlockSize) then begin
                    WrEnd := ((BlockSize - 1) div FDrive.SecLength);
                end
                else begin
                    WrEnd := (((AFile.Pos mod BlockSize) + ACount - 1) div FDrive.SecLength);
                end;

                if ((AFile.Pos mod FDrive.SecLength) <> 0) then begin

                    if not ReadBlock(Block, @Buffer[0], WrStart, WrStart) then begin

                        if (Got = 0) then begin
                            Got := -1;
                        end;

                        break;
                    end;

                end;

                if ((WrEnd <> WrStart) and (((AFile.Pos mod BlockSize) + ACount) < BlockSize) and
                    (((AFile.Pos + ACount) mod FDrive.SecLength) <> 0)) then begin

                    if not ReadBlock(Block, @Buffer[0], WrEnd, WrEnd) then begin

                        if (Got = 0) then begin
                            Got := -1;
                        end;

                        break;
                    end;

                end;

            end;

            NextBlockPos := (((AFile.Pos div BlockSize) * BlockSize) + BlockSize);
            FindBlock := 0;
        end;

        // fill block and WriteFile it
        FDrive.DirtyDirectory := True;

        while ((AFile.Pos <> NextBlockPos) and (ACount <> 0)) do begin
            Buffer[AFile.Pos mod BlockSize] := ABuffer^;
            Inc(ABuffer);
            Inc(AFile.Pos);

            if (AFile.Ino.Size < AFile.Pos) then begin
                AFile.Ino.Size := AFile.Pos;
            end;

            Inc(Got);
            Dec(ACount);
        end;

        // In case the data only fills part of a sector, the rest is
        // still initialized: A new block was cleared and the boundaries
        // of an existing block were ReadFile.
        WriteBlock(Block, @Buffer[0], WrStart, WrEnd);
        AFile.Ino.MTime := Now;

        if (FDrive.Size <= 256) then begin
            Last := 15;

            while (Last >= 0) do begin

                if (FDirectory[Ext].Pointers[Last] <> 0) then begin
                    break;
                end;

                Dec(Last);
            end;

        end
        else begin
            Last := 14;

            while (Last > 0) do begin

                if ((FDirectory[Ext].Pointers[Last] <> 0) or (FDirectory[Ext].Pointers[Last + 1] <> 0)) then begin
                    Last := (Last div 2);
                    break;
                end;

                Dec(Last, 2);
            end;
        end;

        if (Last > 0) then begin
            ExtNo := (ExtNo + ((Last * BlockSize) div ExtCap));
        end;

        FDirectory[Ext].Extnol := EXTENTL(ExtNo);
        FDirectory[Ext].Extnoh := EXTENTH(ExtNo);
        FDirectory[Ext].Blkcnt := ((((AFile.Pos - 1) mod FDrive.Extentsize) div 128) + 1);

        if ((FDrive.OsType and CPMFS_EXACT_SIZE) <> 0) then begin
            FDirectory[Ext].Lrc := ((128 - (AFile.Pos mod 128)) and $7F);
        end
        else begin
            FDirectory[Ext].Lrc := (AFile.Pos mod 128);
        end;

        UpdateTimeStamps(AFile.Ino, Ext);
        UpdateDateStamper(AFile.Ino, Ext);

        if (AFile.Pos = NextExtPos) then begin
            FindExt := 1;
        end
        else if (AFile.Pos = NextBlockPos) then begin
            FindBlock := 1;
        end;

    end;

    Result := (Got);
end;

// --------------------------------------------------------------------------------
//  -- close
// --------------------------------------------------------------------------------
function TCpmFileSystem.CloseFile(AFile: TCpmFile): boolean;
begin
    Result := True;
end;

// --------------------------------------------------------------------------------
//  -- set timestamps
// --------------------------------------------------------------------------------
procedure TCpmFileSystem.UpdateTime(AInode: TCpmInode; ATimes: TUTimeBuf);
begin
    AInode.ATime := ATimes.AcTime;
    AInode.MTime := ATimes.ModTime;
    AInode.CTime := Now;
    UpdateTimeStamps(AInode, AInode.Ino);
    UpdateDateStamper(AInode, AInode.Ino);
end;

// --------------------------------------------------------------------------------
//  -- file system check
// --------------------------------------------------------------------------------
//int CpmTools::fsck(const char *image, bool repair) {
function TCpmFileSystem.CheckFileSystem(ADoRepair: boolean; AMessage: TCheckMessageCallBack): integer;
type
    PPhysDirectoryEntry = ^TPhysDirectoryEntry;
var
    Ret: integer;
    Extent1, Extent2: integer;
    UsedBlocks, MaxUser: integer;
    IndexI, IndexJ: integer;
    Dir1, Dir2: PPhysDirectoryEntry;
    Status: pbyte;
    MinValue, MaxValue, Block1, Block2, RecordsInBlocks: integer;
    ShouldSize, HasSize, Fragmented, Borders, Previous: integer;
    CATimeName, PassWd: string;
    Created, Modified: longword;
    StatFsBuf: TCpmStatFS;
begin
    Ret := FS_OK;

    if (FDrive.OsType = CPMFS_P2DOS) then begin
        MaxUser := 31;
    end
    else begin
        MaxUser := 15;
    end;

    if (FDrive.CnotaTime <> 0) then begin
        CATimeName := 'creation date';
    end
    else begin
        CATimeName := 'access date';
    end;

    // Phase 1: check extent fields
    AMessage('================================================================================');
    AMessage('  Phase 1: check extent fields');

    for Extent1 := 0 to (FDrive.MaxDir - 1) do begin
        Dir1 := @FDirectory[Extent1];
        Status := @Dir1^.Status;

        if ((Status^ >= 0) and (Status^ <= MaxUser)) then begin    // directory entry

            // check name and extension
            if not DirCheck(Dir1^.Name, 8, False, FDrive.OsType) then begin
                AMessage(Format('    Error: Bad name (extent=%d, name=''%s'')', [Extent1, PrintFile(Extent1)]));

                if (ADoRepair) then begin
                    AMessage('      File removed');
                    Status^ := $E5;
                    Ret := (Ret or FS_MODIFIED);
                    Continue;
                end
                else begin
                    Ret := (Ret or FS_BROKEN);
                end;

            end;

            if not DirCheck(Dir1^.Ext, 3, True, FDrive.OsType) then begin
                AMessage(Format('    Error: Bad extension (extent=%d, name=''%s'')', [Extent1, PrintFile(Extent1)]));

                if (ADoRepair) then begin
                    AMessage('      File removed');
                    Status^ := $E5;
                    Ret := (Ret or FS_MODIFIED);
                    Continue;
                end
                else begin
                    Ret := (Ret or FS_BROKEN);
                end;

            end;

            // check extent number
            if ((Dir1^.Extnol and $FF) > $1F) then begin
                AMessage(Format('    Error: Bad lower bits of extent number (extent=%d, name=''%s'', low bits=%d)',
                    [Extent1, PrintFile(Extent1), (Dir1^.Extnol and $FF)]));

                if (ADoRepair) then begin
                    AMessage('      File removed');
                    Status^ := $E5;
                    Ret := (Ret or FS_MODIFIED);
                end
                else begin
                    Ret := (Ret or FS_BROKEN);
                end;

            end;

            if (Status^ = $E5) then begin
                Continue;
            end;

            if ((Dir1^.Extnoh and $FF) > $3F) then begin
                AMessage(Format('    Error: Bad higher bits of extent number (extent=%d, name=''%s'', high bits=%d)',
                    [Extent1, PrintFile(Extent1), (Dir1^.Extnoh and $FF)]));

                if (ADoRepair) then begin
                    AMessage('      File removed');
                    Status^ := $E5;
                    Ret := (Ret or FS_MODIFIED);
                end
                else begin
                    Ret := (Ret or FS_BROKEN);
                end;

            end;

            if (Status^ = $E5) then begin
                Continue;
            end;

            // check last record byte count
            if ((Dir1^.Lrc and $FF) > 128) then begin
                AMessage(Format('    Error: Bad last record byte count (extent=%d, name=''%s'', lrc=%d)',
                    [Extent1, PrintFile(Extent1), (Dir1^.Lrc and $FF)]));

                if (ADoRepair) then begin
                    AMessage('      last record byte count cleared');
                    Dir1^.Lrc := 0;
                    Ret := (Ret or FS_MODIFIED);
                end
                else begin
                    Ret := (Ret or FS_BROKEN);
                end;

            end;

            if (Status^ = $E5) then begin
                Continue;
            end;

            // check block number range
            MinValue := FDrive.DirBlks;
            MaxValue := FDrive.Size;
            IndexI := 0;

            while (IndexI < 16) do begin
                Block1 := (Dir1^.Pointers[IndexI] and $FF);

                if (FDrive.Size > 256) then begin
                    Inc(IndexI);
                    Block1 := Block1 + ((Dir1^.Pointers[IndexI] and $FF) shl 8);
                end;

                if (Block1 > 0) then begin

                    if ((Block1 < MinValue) or (Block1 > MaxValue)) then begin
                        AMessage(Format('    Error: Bad block number (extent=%d, name=''%s'', block=%d)',
                            [Extent1, PrintFile(Extent1), Block1]));

                        if (ADoRepair) then begin
                            AMessage('      File removed');
                            Status^ := $E5;
                            Ret := (Ret or FS_MODIFIED);
                            Break;
                        end
                        else begin
                            Ret := (Ret or FS_BROKEN);
                        end;
                    end;

                end;

                Inc(IndexI);
            end;

            if (Status^ = $E5) then begin
                Continue;
            end;

            // check number of used blocks ?
            // check record count
            UsedBlocks := 0;
            MinValue := (((Dir1^.Extnol mod FDrive.Extents) * 16) div FDrive.Extents);
            MaxValue := ((((Dir1^.Extnol mod FDrive.Extents) + 1) * 16) div FDrive.Extents);
            IndexI := MinValue;

            while (IndexI < MaxValue) do begin

                if ((Dir1^.Pointers[IndexI] <> 0) or ((FDrive.Size > 256) and (Dir1^.Pointers[IndexI + 1] <> 0))) then begin
                    Inc(UsedBlocks);
                end;

                if (FDrive.Size > 256) then begin
                    Inc(IndexI);
                end;

                Inc(IndexI);
            end;

            RecordsInBlocks := (((Dir1^.Blkcnt * 128) + FDrive.BlkSiz - 1) div FDrive.BlkSiz);

            if (RecordsInBlocks <> UsedBlocks) then begin
                AMessage(Format('    Error: Bad record count (extent=%d, name=''%s'', record count=%d)',
                    [Extent1, PrintFile(Extent1), (Dir1^.Blkcnt and $FF)]));

                if (ADoRepair) then begin
                    AMessage('      File removed');
                    Status^ := $E5;
                    Ret := (Ret or FS_MODIFIED);
                end
                else begin
                    Ret := (Ret or FS_BROKEN);
                end;

            end;

            if (Status^ = $E5) then begin
                Continue;
            end;

            // check for too large .com files
            if ((((EXTENT(Dir1^.Extnol, Dir1^.Extnoh) = 3) and (Dir1^.Blkcnt >= 126)) or
                (EXTENT(Dir1^.Extnol, Dir1^.Extnoh) >= 4)) and ((Ord(Dir1^.Ext[0]) and $7F) = Ord('C')) and
                ((Ord(Dir1^.Ext[1]) and $7F) = Ord('O')) and ((Ord(Dir1^.Ext[2]) and $7F) = Ord('M'))) then begin
                AMessage(Format('    Warning: Oversized .COM file (extent=%d, name=''%s'')', [Extent1, PrintFile(Extent1)]));
            end;

            // check DateStamper file
            if (((Ord(Dir1^.Name[0]) and $7F) = Ord('!')) and ((Ord(Dir1^.Name[1]) and $7F) = Ord('!')) and
                ((Ord(Dir1^.Name[2]) and $7F) = Ord('!')) and ((Ord(Dir1^.Name[3]) and $7F) = Ord('T')) and
                ((Ord(Dir1^.Name[4]) and $7F) = Ord('I')) and ((Ord(Dir1^.Name[5]) and $7F) = Ord('M')) and
                ((Ord(Dir1^.Name[6]) and $7F) = Ord('E')) and ((Ord(Dir1^.Name[7]) and $7F) = Ord('&')) and
                ((Ord(Dir1^.Ext[0]) and $7F) = Ord('D')) and ((Ord(Dir1^.Ext[1]) and $7F) = Ord('A')) and
                ((Ord(Dir1^.Ext[2]) and $7F) = Ord('T'))) then begin

                if (Extent1 <> 0) then begin
                    AMessage(Format('    Warning: DateStamper file not first file (extent=%d, name=''%s'')',
                        [Extent1, PrintFile(Extent1)]));
                end;

                if not ((Ord(Dir1^.Ext[0]) and $80) <> 0) then begin
                    AMessage(Format('    Warning: DateStamper file not read-only (extent=%d, name=''%s'')',
                        [Extent1, PrintFile(Extent1)]));
                end;

                ShouldSize := (FDrive.MaxDir * 16);
                HasSize := GetFileSize(Extent1);

                if (HasSize <> ShouldSize) then begin
                    AMessage(Format('    Warning: DateStamper file is %d, should be %d (extent=%d, name=''%s'')',
                        [HasSize, ShouldSize, Extent1, PrintFile(Extent1)]));
                end;

            end;

        end

        // check time stamps ?
        else if (((FDrive.OsType = CPMFS_P2DOS) or (FDrive.OsType = CPMFS_DR3)) and (Status^ = 33)) then begin
            Extent2 := (Extent1 and not 3);

            // time stamps for first of the three extents
            if ((FDirectory[Extent2].Status >= 0) and (FDirectory[Extent2].Status <= MaxUser)) then begin

                if not (BcdCheck(Ord(Dir1^.Name[2]), 24)) then begin
                    AMessage(Format('    Bad %s hour (extent=%d/%d, hour=%02x)',
                        [CATimeName, Extent1, Extent2, (Ord(Dir1^.Name[2]) and $FF)]));
                end;

                if not (BcdCheck(Ord(Dir1^.Name[3]), 60)) then begin
                    AMessage(Format('    Bad %s minute (extent=%d/%d, minute=%02x)',
                        [CATimeName, Extent1, Extent2, (Ord(Dir1^.Name[3]) and $FF)]));
                end;

                if not (BcdCheck(Ord(Dir1^.Name[6]), 24)) then begin
                    AMessage(Format('    Bad modification date hour (extent=%d/%d, hour=%02x)',
                        [Extent1, Extent2, (Ord(Dir1^.Name[6]) and $FF)]));
                end;

                if not (BcdCheck(Ord(Dir1^.Name[7]), 60)) then begin
                    AMessage(Format('    Bad modification date minute (extent=%d/%d, minute=%02x)',
                        [Extent1, Extent2, (Ord(Dir1^.Name[7]) and $FF)]));
                end;

                Created := ((Ord(Dir1^.Name[4]) + (Ord(Dir1^.Name[1]) shl 8)) * ($60 * $60) +
                    (Ord(Dir1^.Name[2]) * $60) + Ord(Dir1^.Name[3]));
                Modified := ((Ord(Dir1^.Name[0]) + (Ord(Dir1^.Name[5]) shl 8)) * ($60 * $60) +
                    (Ord(Dir1^.Name[6]) * $60) + Ord(Dir1^.Name[7]));

                if ((FDrive.CnotaTime <> 0) and (Modified < Created)) then begin
                    AMessage(Format('    Warning: Modification date earlier than creation date (extent=%d/%d)',
                        [Extent1, Extent2]));
                end;

            end;

            Extent2 := ((Extent1 and not 3) + 1);

            // time stamps for second
            if ((FDirectory[Extent2].Status >= 0) and (FDirectory[Extent2].Status <= MaxUser)) then begin

                if not (BcdCheck(Dir1^.Lrc, 24)) then begin
                    AMessage(Format('    Bad %s hour (extent=%d/%d, hour=%02x)',
                        [CATimeName, Extent1, Extent2, (Dir1^.Lrc and $FF)]));
                end;

                if not (BcdCheck(Dir1^.Extnoh, 60)) then begin
                    AMessage(Format('    Bad %s minute (extent=%d/%d, minute=%02x)',
                        [CATimeName, Extent1, Extent2, (Dir1^.Extnoh and $FF)]));
                end;

                if not (BcdCheck(Dir1^.Pointers[1], 24)) then begin
                    AMessage(Format('    Bad modification date hour (extent=%d/%d, hour=%02x)',
                        [Extent1, Extent2, (Dir1^.Pointers[1] and $FF)]));
                end;

                if not (BcdCheck(Dir1^.Pointers[2], 60)) then begin
                    AMessage(Format('    Bad modification date minute (extent=%d/%d, minute=%02x)',
                        [Extent1, Extent2, (Dir1^.Pointers[2] and $FF)]));
                end;

                Created := ((Ord(Dir1^.Ext[2]) + (Dir1^.Extnol shl 8)) * ($60 * $60) + (Dir1^.Lrc * $60) + Dir1^.Extnoh);
                Modified := ((Dir1^.Blkcnt + (Dir1^.Pointers[0] shl 8)) * ($60 * $60) +
                    (Dir1^.Pointers[1] * $60) + Dir1^.Pointers[2]);

                if ((FDrive.CnotaTime <> 0) and (Modified < Created)) then begin
                    AMessage(Format('    Warning: Modification date earlier than creation date (extent=%d/%d)',
                        [Extent1, Extent2]));
                end;

            end;

            Extent2 := ((Extent1 and not 3) + 2);

            // time stamps for third
            if ((FDirectory[Extent2].Status >= 0) and (FDirectory[Extent2].Status <= MaxUser)) then begin

                if not (BcdCheck(Dir1^.Pointers[7], 24)) then begin
                    AMessage(Format('    Bad %s hour (extent=%d/%d, hour=%02x)',
                        [CATimeName, Extent1, Extent2, (Dir1^.Pointers[7] and $FF)]));
                end;

                if not (BcdCheck(Dir1^.Pointers[8], 60)) then begin
                    AMessage(Format('    Bad %s minute (extent=%d/%d, minute=%02x)',
                        [CATimeName, Extent1, Extent2, (Dir1^.Pointers[8] and $FF)]));
                end;

                if not (BcdCheck(Dir1^.Pointers[11], 24)) then begin
                    AMessage(Format('    Bad modification date hour (extent=%d/%d, hour=%02x)',
                        [Extent1, Extent2, (Dir1^.Pointers[11] and $FF)]));
                end;

                if not (BcdCheck(Dir1^.Pointers[12], 60)) then begin
                    AMessage(Format('    Bad modification date minute (extent=%d/%d, minute=%02x)',
                        [Extent1, Extent2, (Dir1^.Pointers[12] and $FF)]));
                end;

                Created := ((Dir1^.Pointers[5] + (Dir1^.Pointers[6] shl 8)) * ($60 * $60) +
                    (Dir1^.Pointers[7] * $60) + Dir1^.Pointers[8]);
                Modified := ((Dir1^.Pointers[9] + (Dir1^.Pointers[10] shl 8)) * ($60 * $60) +
                    (Dir1^.Pointers[11] * $60) + Dir1^.Pointers[12]);

                if ((FDrive.CnotaTime <> 0) and (Modified < Created)) then begin
                    AMessage(Format('    Warning: Modification date earlier than creation date (extent=%d/%d)',
                        [Extent1, Extent2]));
                end;

            end;

        end
        // disc label
        else if ((FDrive.OsType = CPMFS_DR3) and (Status^ = 32)) then begin

            if not (BcdCheck(Dir1^.Pointers[10], 24)) then begin
                AMessage(Format('    Bad Label %s hour (extent=%d, hour=%02x)',
                    [CATimeName, Extent1, (Dir1^.Pointers[10] and $FF)]));
            end;

            if not (BcdCheck(Dir1^.Pointers[11], 60)) then begin
                AMessage(Format('    Bad Label %s minute (extent=%d, minute=%02x)',
                    [CATimeName, Extent1, (Dir1^.Pointers[11] and $FF)]));
            end;

            if not (BcdCheck(Dir1^.Pointers[14], 24)) then begin
                AMessage(Format('    Bad Label modification date hour (extent=%d, hour=%02x)',
                    [Extent1, (Dir1^.Pointers[14] and $FF)]));
            end;

            if not (BcdCheck(Dir1^.Pointers[15], 60)) then begin
                AMessage(Format('    Bad Label modification date minute (extent=%d, minute=%02x)',
                    [Extent1, (Dir1^.Pointers[15] and $FF)]));
            end;

            Created := ((Dir1^.Pointers[8] + (Dir1^.Pointers[9] shl 8)) * ($60 * $60) +
                (Dir1^.Pointers[10] * $60) + Dir1^.Pointers[11]);
            Modified := ((Dir1^.Pointers[12] + (Dir1^.Pointers[13] shl 8)) * ($60 * $60) +
                (Dir1^.Pointers[14] * $60) + Dir1^.Pointers[15]);

            if ((FDrive.CnotaTime <> 0) and (Modified < Created)) then begin
                AMessage(Format('    Warning: Label modification date earlier than creation date (extent=%d)', [Extent1]));
            end;

            //if ((dir->extnol & 0x40) && (dir->extnol & 0x10)) {
            if (((Dir1^.Extnol and $40) <> 0) and ((Dir1^.Extnol and $10) <> 0)) then begin
                AMessage(Format('    Error: Bit 4 and 6 can only be exclusively be set (extent=%d, label byte=0x%02x)',
                    [Extent1, Dir1^.Extnol]));

                if (ADoRepair) then begin

                    if (FDrive.CnotaTime <> 0) then begin
                        AMessage('      Set Timestamps to creation date');
                        Dir1^.Extnol := (Dir1^.Extnol and not (1 shl $40));
                    end
                    else begin
                        AMessage('      Set Timestamps to access date');
                        Dir1^.Extnol := (Dir1^.Extnol and not (1 shl $10));
                    end;

                    Ret := (Ret or FS_MODIFIED);
                end
                else begin
                    Ret := (Ret or FS_BROKEN);
                end;

            end;

            if (((Dir1^.Extnol and $80) <> 0) and not PwdCheck(Dir1^.Pointers, Dir1^.Lrc, PassWd)) then begin
                AMessage(Format('    Non-printable character in password (extent=%d, password=%s)', [Extent1, PassWd]));

                if (ADoRepair) then begin
                    AMessage(Format('      Set password to %s%s%s%s%s%s%s%s', [T0, T1, T2, T3, T4, T5, T6, T7]));
                    Dir1^.Pointers[0] := P0;
                    Dir1^.Pointers[1] := P1;
                    Dir1^.Pointers[2] := P2;
                    Dir1^.Pointers[3] := P3;
                    Dir1^.Pointers[4] := P4;
                    Dir1^.Pointers[5] := P5;
                    Dir1^.Pointers[6] := P6;
                    Dir1^.Pointers[7] := P7;
                    Dir1^.Lrc := PB;
                    Ret := (Ret or FS_MODIFIED);
                end
                else begin
                    Ret := (Ret or FS_BROKEN);
                end;

            end;

        end
        // password
        else if ((FDrive.OsType = CPMFS_DR3) and (Status^ >= 16) and (Status^ <= 31)) then begin

            // check name and extension
            if not DirCheck(Dir1^.Name, 8, False, FDrive.OsType) then begin
                AMessage(Format('    Error: Bad name (extent=%d, name=''%s'')', [Extent1, PrintFile(Extent1)]));

                if (ADoRepair) then begin
                    AMessage('      Clear password entry');
                    Status^ := $E5;
                    Ret := (Ret or FS_MODIFIED);
                    Continue;
                end
                else begin
                    Ret := (Ret or FS_BROKEN);
                end;

            end;

            if not DirCheck(Dir1^.Ext, 3, True, FDrive.OsType) then begin
                AMessage(Format('    Error: Bad extension (extent=%d, name=''%s'')', [Extent1, PrintFile(Extent1)]));

                if (ADoRepair) then begin
                    AMessage('      Clear password entry');
                    Status^ := $E5;
                    Ret := (Ret or FS_MODIFIED);
                    Continue;
                end
                else begin
                    Ret := (Ret or FS_BROKEN);
                end;

            end;

            // check password
            if ((Dir1^.Extnol and ($80 or $40 or $20)) <> 0) and not PwdCheck(Dir1^.Pointers, Dir1^.Lrc, PassWd) then begin
                AMessage(Format('    Non-printable character in password (extent=%d, password=%s)', [Extent1, PassWd]));

                if (ADoRepair) then begin
                    AMessage(Format('      Set password to %s%s%s%s%s%s%s%s', [T0, T1, T2, T3, T4, T5, T6, T7]));
                    Dir1^.Pointers[0] := P0;
                    Dir1^.Pointers[1] := P1;
                    Dir1^.Pointers[2] := P2;
                    Dir1^.Pointers[3] := P3;
                    Dir1^.Pointers[4] := P4;
                    Dir1^.Pointers[5] := P5;
                    Dir1^.Pointers[6] := P6;
                    Dir1^.Pointers[7] := P7;
                    Dir1^.Lrc := PB;
                    Ret := (Ret or FS_MODIFIED);
                end
                else begin
                    Ret := (Ret or FS_BROKEN);
                end;

            end;

        end
        // bad status
        else if (Status^ <> $E5) then begin
            AMessage(Format('    Error: Bad status (extent=%d, name=''%s'', status=0x%02x)',
                [Extent1, PrintFile(Extent1), (Status^ and $FF)]));

            if (ADoRepair) then begin
                AMessage('      Clear entry');
                Status^ := $E5;
                Ret := (Ret or FS_MODIFIED);
            end
            else begin
                Ret := (Ret or FS_BROKEN);
            end;

            Continue;
        end;

    end;

    // Phase 2: check extent connectivity
    AMessage('  Phase 2: check extent connectivity');

    // check multiple allocated blocks
    for Extent1 := 0 to (FDrive.MaxDir - 1) do begin
        Dir1 := @FDirectory[Extent1];

        if ((Dir1^.Status >= 0) and (Dir1^.Status <= MaxUser)) then begin
            IndexI := 0;

            while (IndexI < 16) do begin
                Block1 := (Dir1^.Pointers[IndexI] and $FF);

                if (FDrive.Size > 256) then begin
                    Inc(IndexI);
                    Block1 := Block1 + ((Dir1^.Pointers[IndexI] and $FF) shl 8);
                end;

                for Extent2 := 0 to (FDrive.MaxDir - 1) do begin
                    Dir2 := @FDirectory[Extent2];

                    if ((Dir2^.Status >= 0) and (Dir2^.Status <= MaxUser)) then begin
                        IndexJ := 0;

                        while (IndexJ < 16) do begin
                            Block2 := (Dir2^.Pointers[IndexJ] and $FF);

                            if (FDrive.Size > 256) then begin
                                Inc(IndexJ);
                                Block2 := Block2 + ((Dir2^.Pointers[IndexJ] and $FF) shl 8);
                            end;

                            if ((Block1 <> 0) and (Block2 <> 0) and (Block1 = Block2) and not
                                ((Extent1 = Extent2) and (IndexI = IndexJ))) then begin
                                AMessage(Format(
                                    '    Error: Multiple allocated block (extent=%d,%d, name=''%s'',''%s'' block=%d)',
                                    [Extent1, Extent2, PrintFile(Extent1), PrintFile(Extent2), Block1]));
                                Ret := (Ret or FS_BROKEN);
                            end;

                            Inc(IndexJ);
                        end;

                    end;

                end;

                Inc(IndexI);
            end;

        end;

    end;

    // check multiple extents
    for Extent1 := 0 to (FDrive.MaxDir - 1) do begin
        Dir1 := @FDirectory[Extent1];

        if ((Dir1^.Status >= 0) and (Dir1^.Status <= MaxUser)) then begin

            for Extent2 := 0 to (FDrive.MaxDir - 1) do begin
                Dir2 := @FDirectory[Extent2];

                if ((Dir2^.Status >= 0) and (Dir2^.Status <= MaxUser)) then begin

                    if ((Extent1 <> Extent2) and (EXTENT(Dir1^.Extnol, Dir1^.Extnoh) = EXTENT(Dir2^.Extnol, Dir2^.Extnoh)) and
                        (Dir1^.Status = Dir2^.Status)) then begin
                        IndexI := 0;

                        while ((IndexI < 8) and ((Ord(Dir1^.Name[IndexI]) and $7F) = (Ord(Dir2^.Name[IndexI]) and $7F))) do begin
                            Inc(IndexI);
                        end;

                        if (IndexI = 8) then begin
                            IndexI := 0;

                            while ((IndexI < 3) and ((Ord(Dir1^.Ext[IndexI]) and $7F) = (Ord(Dir2^.Ext[IndexI]) and $7F)))
                                do begin
                                Inc(IndexI);
                            end;

                            if (IndexI = 3) then begin
                                AMessage(Format('    Error: Duplicate extent (extent=%d,%d)', [Extent1, Extent2]));
                                Ret := (Ret or FS_BROKEN);
                            end;

                        end;

                    end;

                end;

            end;

        end;

    end;

    // print statistics
    if (Ret = FS_OK) then begin
        Fragmented := 0;
        Borders := 0;
        StatFs(StatFsBuf);

        for Extent1 := 0 to (FDrive.MaxDir - 1) do begin
            Dir1 := @FDirectory[Extent1];

            if ((Dir1^.Status >= 0) and (Dir1^.Status <= MaxUser)) then begin
                Previous := -1;
                IndexI := 0;

                while (IndexI < 16) do begin
                    Block1 := (Dir1^.Pointers[IndexI] and $FF);

                    if (FDrive.Size > 256) then begin
                        Inc(IndexI);
                        Block1 := Block1 + ((Dir1^.Pointers[IndexI] and $FF) shl 8);
                    end;

                    if (Previous <> -1) then begin

                        if ((Block1 <> 0) and (Block1 <> (Previous + 1))) then begin
                            Inc(Fragmented);
                        end;

                        Inc(Borders);
                    end;

                    Previous := Block1;
                    Inc(IndexI);
                end;

            end;

        end;

        if (Borders <> 0) then begin
            Fragmented := ((Fragmented * 1000) div Borders);
        end
        else begin
            Fragmented := 0;
        end;

        AMessage(Format('  %d/%d files (%d.%d%% non-contigous), %d/%d blocks',
            [(StatFsBuf.F_Files - StatFsBuf.F_FFree), StatFsBuf.F_Files, (Fragmented div 10),
            (Fragmented mod 10), (StatFsBuf.F_Blocks - StatFsBuf.F_BFree), StatFsBuf.F_Blocks]));
    end;

    if ((Ret and FS_MODIFIED) <> 0) then begin
        FDrive.DirtyDirectory := True;
    end;

    AMessage('================================================================================');
    Result := (Ret);
end;

// --------------------------------------------------------------------------------
//  -- write directory back
// --------------------------------------------------------------------------------
function TCpmFileSystem.SyncDirectory: boolean;
var
    IndexI, Blocks: integer;
    PDirectory: pbyte;
begin

    if (FDrive.DirtyDirectory) then begin
        PDirectory := @FDirectory[0];
        Blocks := (((FDrive.MaxDir * 32) + (FDrive.BlkSiz - 1)) div FDrive.BlkSiz);

        for IndexI := 0 to (Blocks - 1) do begin

            if not (WriteBlock(IndexI, (PDirectory + (IndexI * FDrive.BlkSiz)), 0, -1)) then begin
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
//  -- check if file already exists
// --------------------------------------------------------------------------------
function TCpmFileSystem.IsFileExisting(ACpmFileName: string): boolean;
var
    User: integer;
    Name: array[0..7] of char;
    Extension: array[0..2] of char;
    Inode: TCpmInode;
begin
    SplitFilename(PChar(ACpmFileName), FDrive.OsType, Name, Extension, User);
    Name2Inode(PChar(ACpmFileName), Inode);
    Result := (FindFileExtent(User, Name, Extension, 0, -1) <> -1);
end;

// --------------------------------------------------------------------------------
function TCpmFileSystem.GetErrorMsg: string;
begin
    Result := FFileSystemError;
    FFileSystemError := '';
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
function TCpmFileSystem.GetBootTrackSize: integer;
begin

    if (FDrive.BootTrk >= 0) then begin
        Result := (FDrive.BootTrk * FDrive.SecLength * FDrive.SecTrk);
    end
    else if (FDrive.BootSec >= 0) then begin
        Result := (FDrive.BootSec * FDrive.SecLength);
    end
    else begin
        Result := 0;
    end;
end;

// --------------------------------------------------------------------------------
function TCpmFileSystem.GetDirectoryRoot: TCpmInode;
begin
    Result := FRoot;
end;

// --------------------------------------------------------------------------------
function TCpmFileSystem.GetFreeFileSpace: size_t;
var
    FileSystemStats: TCpmStatFS;
begin
    StatFs(FileSystemStats);
    Result := ((FileSystemStats.F_BSize * FileSystemStats.F_Blocks) - (FileSystemStats.F_BUsed *
        FileSystemStats.F_BSize) - (FDrive.MaxDir * 32));
end;

// --------------------------------------------------------------------------------
constructor TCpmFileSystem.Create;
begin
    inherited Create;
end;

// --------------------------------------------------------------------------------
destructor TCpmFileSystem.Destroy;
begin
    FreeAndNil(FCpmDevice);
    inherited Destroy;
end;

// --------------------------------------------------------------------------------
//  -- init allocation vector
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
//  -- allocate a new disk block
// --------------------------------------------------------------------------------
function TCpmFileSystem.AllocBlock: integer;
var
    IndexI, IndexJ: integer;
    Bits, Block: integer;
begin

    for IndexI := 0 to (FDrive.AlvSize - 1) do begin
        Bits := FAllocationVector[IndexI];

        for IndexJ := 0 to (INTBITS - 1) do begin

            if ((Bits and $01) = 0) then begin
                Block := ((IndexI * INTBITS) + IndexJ);

                if (Block >= FDrive.Size) then begin
                    FFileSystemError := 'device full';
                    Result := (-1);
                    exit;
                end;

                FAllocationVector[IndexI] := (FAllocationVector[IndexI] or (1 shl IndexJ));
                Result := (Block);
                exit;
            end;

            Bits := (Bits shr 1);
        end;

    end;

    FFileSystemError := 'device full';
    Result := (-1);
end;

// --------------------------------------------------------------------------------
//  -- Copy string, leaving 8th bit alone
// --------------------------------------------------------------------------------
procedure TCpmFileSystem.MemCpy7(var ADestination: array of char; const ASource: array of char; ACount: integer);
begin

    while (ACount > 0) do begin
        ADestination[ACount - 1] := Chr((Ord(ADestination[ACount - 1]) and $80) or (Ord(ASource[ACount - 1]) and $7F));
        Dec(ACount);
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
    FCpmDevice.SetGeometry(512, 9, 40, 0, 'pcw180');

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
    FDrive.LibdskGeometry[0] := #0;  // LibDsk can recognise an Amstrad superblock and autodetect
    FDrive.LibdskDeviceOptions[0] := #0;
    Result := True;
end;

// --------------------------------------------------------------------------------
//  -- read super block from diskdefs file
// --------------------------------------------------------------------------------
function TCpmFileSystem.DiskdefsReadSuper(const AImageType: string; ADiskdefsPath: string): boolean;
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
    FDrive.SideOrder := soAlt;
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
    FDrive.LibdskGeometry[0] := #0;
    FDrive.LibdskDeviceOptions[0] := #0;
    Result := True;

    try
        DiskDefs := TStringList.Create;
        DiskDefs.LoadFromFile(ADiskdefsPath);

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
                else if ((Length(DefinitionLine) = 2) or ((Length(DefinitionLine) > 2) and
                    ((DefinitionLine[2] = '#') or (DefinitionLine[2][1] = '#')))) then begin

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

                        if ((FDrive.BlkSiz <= 0) or ((FDrive.BlkSiz mod 2) <> 0)) then begin
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

                    else if (DefinitionLine[0] = 'sideorder') then begin

                        if (DefinitionLine[1] = 'alternate') then begin
                            FDrive.SideOrder := soAlt;
                        end
                        else if (DefinitionLine[1] = 'outout') then begin
                            FDrive.SideOrder := soOutOut;
                        end
                        else if (DefinitionLine[1] = 'outback') then begin
                            FDrive.SideOrder := soOutBack;
                        end
                        else begin
                            FFileSystemError := Format('sideorder invalid value in line %d', [(LineNumber)]);
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

                    end
                    else if (DefinitionLine[0] = 'libdsk:format') then begin
                        FDrive.SideOrder := soAlt;

                        if (DefinitionLine[1].Length < SizeOf(FDrive.LibdskGeometry)) then begin

                            for Pass := 0 to DefinitionLine[1].Length - 1 do begin
                                FDrive.LibdskGeometry[Pass] := DefinitionLine[1][Pass + 1];
                            end;

                            FDrive.LibdskGeometry[DefinitionLine[1].Length] := #0;

                        end;
                    end
                    else if (DefinitionLine[0] = 'libdsk:devopts') then begin

                        if (DefinitionLine[1].Length < SizeOf(FDrive.LibdskDeviceOptions)) then begin

                            for Pass := 0 to DefinitionLine[1].Length - 1 do begin
                                FDrive.LibdskDeviceOptions[Pass] := DefinitionLine[1][Pass + 1];
                            end;

                            FDrive.LibdskDeviceOptions[DefinitionLine[1].Length] := #0;

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

    for Counter := 0 to AEnd do begin
        CalcSecTrack(ABlockNr, (BootOffset + Counter), Sect, Track);

        if (Counter >= AStart) then begin

            if not (FCpmDevice.ReadSector(Track, FSkewTab[Sect], (ABuffer + (FDrive.SecLength * Counter))^)) then begin
                FFileSystemError := FCpmDevice.GetErrorMsg;
                Result := False;
                exit;
            end;

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

    for Counter := 0 to AEnd do begin
        CalcSecTrack(ABlockNr, (BootOffset + Counter), Sect, Track);

        if (Counter >= AStart) then begin

            if not (FCpmDevice.WriteSector(Track, FSkewTab[Sect], (ABuffer + (FDrive.SecLength * Counter))^)) then begin
                FFileSystemError := FCpmDevice.GetErrorMsg;
                Result := False;
                exit;
            end;

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
//  -- find first free extent
// --------------------------------------------------------------------------------
function TCpmFileSystem.FindFreeExtent: integer;
var
    IndexI: integer;
begin

    for IndexI := 0 to FDrive.MaxDir - 1 do begin

        if (FDirectory[IndexI].Status = $E5) then begin
            Result := (IndexI);
            exit;
        end;

    end;

    FFileSystemError := 'directory full';
    Result := (-1);
end;

// --------------------------------------------------------------------------------
//  -- convert time stamps to CP/M format
// --------------------------------------------------------------------------------
procedure TCpmFileSystem.UpdateTimeStamps(const AInode: TCpmInode; AExtent: integer);
type
    PPhysDirectoryEntry = ^TPhysDirectoryEntry;
var
    CpmDate: PPhysDirectoryEntry;
    Ca_Min, Ca_Hour, Ca_Days: integer;
    U_Min, U_Hour, U_Days: integer;
begin

    if not S_ISREG(AInode.Mode) then begin
        exit;
    end;

    if (FDrive.CnotaTime <> 0) then begin
        Unix2CpmTime(AInode.CTime, Ca_Days, Ca_Hour, Ca_Min);
    end
    else begin
        Unix2CpmTime(AInode.ATime, Ca_Days, Ca_Hour, Ca_Min);
    end;

    Unix2CpmTime(AInode.MTime, U_Days, U_Hour, U_Min);
    CpmDate := @FDirectory[AExtent or 3];

    if (((FDrive.OsType and CPMFS_CPM3_DATES) <> 0) and (CpmDate^.Status = $21)) then begin
        FDrive.DirtyDirectory := True;

        case (AExtent and 3) of

            0: begin  // first entry
                CpmDate^.Name[0] := Chr(Ca_Days and $FF);
                CpmDate^.Name[1] := Chr(Ca_Days shr 8);
                CpmDate^.Name[2] := Chr(Ca_Hour);
                CpmDate^.Name[3] := Chr(Ca_Min);
                CpmDate^.Name[4] := Chr(U_Days and $FF);
                CpmDate^.Name[5] := Chr(U_Days shr 8);
                CpmDate^.Name[6] := Chr(U_Hour);
                CpmDate^.Name[7] := Chr(U_Min);
            end;

            1: begin  // second entry
                CpmDate^.Ext[2] := Chr(Ca_Days and $FF);
                CpmDate^.Extnol := (Ca_Days shr 8);
                CpmDate^.Lrc := Ca_Hour;
                CpmDate^.Extnoh := Ca_Min;
                CpmDate^.Blkcnt := (U_Days and $FF);
                CpmDate^.Pointers[0] := (U_Days shr 8);
                CpmDate^.Pointers[1] := U_Hour;
                CpmDate^.Pointers[2] := U_Min;
            end;

            2: begin  // third entry
                CpmDate^.Pointers[5] := (Ca_Days and $FF);
                CpmDate^.Pointers[6] := (Ca_Days shr 8);
                CpmDate^.Pointers[7] := Ca_Hour;
                CpmDate^.Pointers[8] := Ca_Min;
                CpmDate^.Pointers[9] := (U_Days and $FF);
                CpmDate^.Pointers[10] := (U_Days shr 8);
                CpmDate^.Pointers[11] := U_Hour;
                CpmDate^.Pointers[12] := U_Min;
            end;

        end;

    end;
end;

// --------------------------------------------------------------------------------
//  -- set time in datestamper file
// --------------------------------------------------------------------------------
procedure TCpmFileSystem.UpdateDateStamper(const AInode: TCpmInode; AExtent: integer);
type
    PDateStamps = ^TDateStamps;
var
    Stamp: PDateStamps;
begin

    if not S_ISREG(AInode.Mode) then begin
        exit;
    end;

    if not ((FDrive.OsType and CPMFS_DS_DATES) <> 0) then begin
        exit;
    end;

    // Get datestamp struct
    Stamp := @FDateStamps[AExtent];
    Unix2DsTime(AInode.MTime, Stamp^.Modify);
    Unix2DsTime(AInode.CTime, Stamp^.Create);
    Unix2DsTime(AInode.ATime, Stamp^.Access);
    FDrive.DirtyDateStamp := True;
end;

// --------------------------------------------------------------------------------
//  -- read CP/M time stamp
// --------------------------------------------------------------------------------
function TCpmFileSystem.ReadTimeStamps(var AInode: TCpmInode; ALowestExt: integer): integer;
type
    PPhysDirectoryEntry = ^TPhysDirectoryEntry;
var
    DirDate: PPhysDirectoryEntry;
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

    DirDate := @FDirectory[ALowestExt or 3];

    if (((FDrive.OsType and CPMFS_CPM3_DATES) <> 0) and (DirDate^.Status = $21)) then begin

        case (ALowestExt and 3) of
            // first entry of the four
            0: begin
                Ca_Days := (Ord(DirDate^.Name[0]) + (Ord(DirDate^.Name[1]) shl 8));
                Ca_Hour := Ord(DirDate^.Name[2]);
                Ca_Min := Ord(DirDate^.Name[3]);
                U_Days := (Ord(DirDate^.Name[4]) + (Ord(DirDate^.Name[5]) shl 8));
                U_Hour := Ord(DirDate^.Name[6]);
                U_Min := Ord(DirDate^.Name[7]);
                ProtectMode := Ord(DirDate^.Ext[0]);
            end;
            // second entry
            1: begin
                Ca_Days := (Ord(DirDate^.Ext[2]) + (Ord(DirDate^.Extnol) shl 8));
                Ca_Hour := DirDate^.Lrc;
                Ca_Min := DirDate^.Extnoh;
                U_Days := (DirDate^.Blkcnt + (DirDate^.Pointers[0] shl 8));
                U_Hour := DirDate^.Pointers[1];
                U_Min := DirDate^.Pointers[2];
                ProtectMode := DirDate^.Pointers[3];
            end;
            // third one
            2: begin
                Ca_Days := (DirDate^.Pointers[5] + (DirDate^.Pointers[6] shl 8));
                Ca_Hour := DirDate^.Pointers[7];
                Ca_Min := DirDate^.Pointers[8];
                U_Days := (DirDate^.Pointers[9] + (DirDate^.Pointers[10] shl 8));
                U_Hour := DirDate^.Pointers[11];
                U_Min := DirDate^.Pointers[12];
                ProtectMode := DirDate^.Pointers[13];
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
type
    PDateStamps = ^TDateStamps;
var
    Stamp: PDateStamps;
begin

    if not ((FDrive.OsType and CPMFS_DS_DATES) <> 0) then begin
        exit;
    end;

    // Get datestamp
    Stamp := @FDateStamps[ALowestExt];
    AInode.MTime := Ds2UnixTime(Stamp^.Modify);
    AInode.CTime := Ds2UnixTime(Stamp^.Create);
    AInode.ATime := Ds2UnixTime(Stamp^.Access);
end;

// --------------------------------------------------------------------------------
//  -- read all datestamper timestamps
// --------------------------------------------------------------------------------
function TCpmFileSystem.CheckDateStamps: boolean;
var
    DSOffset, DSBlocks, DSRecords: integer;
    IndexI, IndexJ, CheckSum, Offset: integer;
    PDateStamps: pbyte;
begin

    if (not IsMatching(0, '!!!TIME&', 'DAT', FDirectory[0].Status, FDirectory[0].Name, FDirectory[0].Ext)) then begin
        Result := False;
        exit;
    end;

    // Offset to ds file in alloc blocks
    DSOffset := (((FDrive.MaxDir * 32) + (FDrive.BlkSiz - 1)) div FDrive.BlkSiz);
    DSRecords := ((FDrive.MaxDir + 7) div 8);
    DSBlocks := (((DSRecords * 128) + (FDrive.BlkSiz - 1)) div FDrive.BlkSiz);

    try
        SetLength(FDateStamps, ((FDrive.MaxDir * DSRecords) div SizeOf(TDateStamps)));
        PDateStamps := @FDateStamps[0];
    except
        on e: Exception do begin
            FFileSystemError := e.Message;
            Result := False;
            exit;
        end;
    end;

    Offset := 0;

    for IndexI := DSOffset to ((DSOffset + DSBlocks) - 1) do begin

        if (not ReadBlock(IndexI, (PDateStamps + Offset), 0, -1)) then begin
            Result := False;
            exit;
        end;

        Inc(Offset, FDrive.BlkSiz);

    end;

    Offset := 0;

    for IndexI := 0 to (DSRecords - 1) do begin
        CheckSum := 0;

        for IndexJ := 0 to 126 do begin
            CheckSum := CheckSum + (PDateStamps + IndexJ + Offset)^;
        end;

        if ((PDateStamps + IndexJ + Offset + 1)^ <> (CheckSum and $FF)) then begin
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
    DateTime := IncHour(DateTime, BCD2BIN(AHour));
    Result := IncMinute(DateTime, BCD2BIN(AMin));
end;

// --------------------------------------------------------------------------------
//  -- convert UTC to CP/M time
// --------------------------------------------------------------------------------
procedure TCpmFileSystem.Unix2CpmTime(ANow: TDateTime; var ADays: integer; var AHour: integer; var AMin: integer);
var
    DateTime: TDateTime;
begin
    DateTime := ANow;
    AMin := (((MinuteOf(DateTime) div 10) shl 4) or (MinuteOf(DateTime) mod 10));
    AHour := (((HourOf(DateTime) div 10) shl 4) or (HourOf(DateTime) mod 10));
    ADays := DaysBetween(DateTime, EncodeDate(1978, 1, 1)) + 1;
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

    Year := BCD2BIN(AEntry.Year);

    if (Year < 70) then begin
        Inc(Year, 2000);
    end;

    Result := EncodeDateTime(Year, BCD2BIN(AEntry.Month), BCD2BIN(AEntry.Day), BCD2BIN(AEntry.Hour),
        BCD2BIN(AEntry.Minute), 0, 0);
end;

// --------------------------------------------------------------------------------
//  -- convert Unix to DS time
// --------------------------------------------------------------------------------
procedure TCpmFileSystem.Unix2DsTime(ANow: TDateTime; var AEntry: TDsEntry);
var
    Hour, Min, Sec, MilliSec: word;
    Year, Month, Day: word;
begin

    if (ANow = 0) then begin
        AEntry.Minute := 0;
        AEntry.Hour := 0;
        AEntry.Day := 0;
        AEntry.Month := 0;
        AEntry.Year := 0;
    end
    else begin
        DecodeDate(ANow, Year, Month, Day);
        DecodeTime(ANow, Hour, Min, Sec, MilliSec);

        AEntry.Minute := BIN2BCD(Min);
        AEntry.Hour := BIN2BCD(Hour);
        AEntry.Day := BIN2BCD(Day);
        AEntry.Month := BIN2BCD(Month);
        Dec(Year, 1900);

        if (Year > 100) then begin
            Dec(Year, 100);
        end;

        AEntry.Year := BIN2BCD(Year);
    end;
end;

// --------------------------------------------------------------------------------
//  -- write all datestamper timestamps
// --------------------------------------------------------------------------------
function TCpmFileSystem.SyncDateStamps: boolean;
var
    DSOffset, DSBlocks, DSRecords: integer;
    IndexI, IndexJ, CheckSum, Offset: integer;
    PDateStamps: pbyte;
begin

    if (FDrive.DirtyDateStamp) then begin

        DSRecords := ((FDrive.MaxDir + 7) div 8);
        PDateStamps := @FDateStamps[0];

        // Re-calculate checksums
        Offset := 0;

        for IndexI := 0 to (DSRecords - 1) do begin
            CheckSum := 0;

            for IndexJ := 0 to 126 do begin
                CheckSum := CheckSum + (PDateStamps + IndexJ + Offset)^;
            end;

            (PDateStamps +IndexJ + Offset + 1)^ := (CheckSum and $FF);
            Inc(Offset, 128);

        end;

        DSOffset := (((FDrive.MaxDir * 32) + (FDrive.BlkSiz - 1)) div FDrive.BlkSiz);
        DSBlocks := (((DSRecords * 128) + (FDrive.BlkSiz - 1)) div FDrive.BlkSiz);
        Offset := 0;

        for IndexI := DSOffset to ((DSOffset + DSBlocks) - 1) do begin

            if not (WriteBlock(IndexI, (PDateStamps + Offset), 0, -1)) then begin
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
type
    PPhysDirectoryEntry = ^TPhysDirectoryEntry;
var
    IndexI, First, MaxUser: integer;
    Current: PPhysDirectoryEntry;
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
            Current := @FDirectory[ADir.Pos - RESERVED_ENTRIES];

            if ((FDrive.OsType and CPMFS_HI_USER) <> 0) then begin
                MaxUser := 31;
            end
            else begin
                MaxUser := 15;
            end;

            if ((Current^.Status >= 0) and (Current^.Status <= MaxUser)) then begin

                // determine first extent for the current file
                for IndexI := 0 to FDrive.MaxDir - 1 do begin

                    if (IndexI <> (ADir.Pos - RESERVED_ENTRIES)) then begin

                        if (IsMatching(Current^.Status, Current^.Name, Current^.Ext, FDirectory[IndexI].Status,
                            FDirectory[IndexI].Name, FDirectory[IndexI].Ext) and
                            (EXTENT(Current^.Extnol, Current^.Extnoh) > EXTENT(FDirectory[IndexI].Extnol,
                            FDirectory[IndexI].Extnoh))) then begin
                            First := IndexI;
                        end;

                    end;

                end;

                if (First = (ADir.Pos - RESERVED_ENTRIES)) then begin
                    AEnt.Ino := (ADir.Pos - RESERVED_ENTRIES);
                    // convert file name to UNIX style
                    Buffer[0] := Chr(Ord('0') + (Current^.Status div 10));
                    Buffer[1] := Chr(Ord('0') + (Current^.Status mod 10));
                    BufferIndex := 2;
                    IndexI := 0;

                    while ((IndexI < 8) and ((Ord(Current^.Name[IndexI]) and $7F) <> Ord(' '))) do begin

                        if (FDrive.UpperCase) then begin
                            Buffer[BufferIndex] := Chr(Ord(Current^.Name[IndexI]) and $7F);
                        end
                        else begin
                            Buffer[BufferIndex] := LowerCase(Chr(Ord(Current^.Name[IndexI]) and $7F));
                        end;

                        Inc(BufferIndex);
                        Inc(IndexI);
                    end;

                    HasExt := False;
                    IndexI := 0;

                    while ((IndexI < 3) and ((Ord(Current^.Ext[IndexI]) and $7F) <> Ord(' '))) do begin

                        if not (HasExt) then begin
                            Buffer[BufferIndex] := '.';
                            Inc(BufferIndex);
                            HasExt := True;
                        end;

                        if (FDrive.UpperCase) then begin
                            Buffer[BufferIndex] := Chr(Ord(Current^.Ext[IndexI]) and $7F);
                        end
                        else begin
                            Buffer[BufferIndex] := LowerCase(Chr(Ord(Current^.Ext[IndexI]) and $7F));
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
//  -- check format and range of BCD digit
// --------------------------------------------------------------------------------
function TCpmFileSystem.BcdCheck(AValue: integer; AMax: integer): boolean;
begin
    Result := True;

    if ((((AValue shr 4) and $0F) > 10) or ((AValue and $0F) > 10) or
        ((((AValue shr 4) and $0F) * 10 + (AValue and $0F)) >= AMax)) then begin
        Result := False;
    end;

end;

// --------------------------------------------------------------------------------
//  -- check password
// --------------------------------------------------------------------------------
function TCpmFileSystem.PwdCheck(APassword: array of byte; ADecode: byte; var APassWd: string): boolean;
var
    Value: byte;
    IndexI, IndexJ: integer;
begin
    Result := True;
    APassWd := '';

    for IndexI := 0 to 7 do begin
        Value := (APassword[7 - IndexI] xor ADecode);

        if ((Value < Ord(' ')) or ((Value and $80) <> 0)) then begin
            for IndexJ := 0 to 7 do begin
                Value := (APassword[7 - IndexJ] xor ADecode);

                if ((Value < Ord(' ')) or ((Value and $80) <> 0)) then begin
                    APassWd := APassWd + '\';
                    APassWd := APassWd + Chr(Ord('0') + ((Value shr 6) and $01));
                    APassWd := APassWd + Chr(Ord('0') + ((Value shr 3) and $03));
                    APassWd := APassWd + Chr(Ord('0') + (Value and $03));
                    APassWd := APassWd + '\';
                end
                else begin
                    APassWd := APassWd + Chr(Value);
                end;

            end;

            Result := False;
        end;

    end;

end;

// --------------------------------------------------------------------------------
//  -- check name or extension
// --------------------------------------------------------------------------------
function TCpmFileSystem.DirCheck(AStr: array of char; ALen: size_t; AAllowEmpty: boolean; AType: integer): boolean;
var
    Value: byte;
    IndexI: integer;
    InName: boolean;
begin
    InName := True;
    Result := True;

    for IndexI := 0 to (ALen - 1) do begin
        Value := (Ord(AStr[IndexI]) and $7F);

        if (InName) then begin

            if (IsLower(Chr(Value))) then begin
                Result := False;
                exit;
            end;

            if ((IndexI = 0) and (Value = Ord(' ')) and not AAllowEmpty) then begin
                Result := False;
                exit;
            end;

            if (Value = Ord(' ')) then begin
                InName := False;
            end
            else if not IsFileChar(Chr(Value), AType) then begin
                Result := False;
                exit;
            end;

        end
        else begin

            if (Value <> Ord(' ')) then begin
                Result := False;
                exit;
            end;

        end;

    end;

end;

// --------------------------------------------------------------------------------
//  -- return file size
// --------------------------------------------------------------------------------
function TCpmFileSystem.GetFileSize(AExtent: integer): size_t;
type
    PPhysDirectoryEntry = ^TPhysDirectoryEntry;
var
    Dir: PPhysDirectoryEntry;
    Block, Size: integer;
begin
    Dir := @FDirectory[AExtent];
    Size := (EXTENT(Dir^.Extnol, Dir^.Extnoh) * FDrive.Extentsize);

    if (FDrive.Size <= 256) then begin

        for Block := 15 downto 0 do begin

            if (Dir^.Pointers[Block] <> 0) then begin
                Break;
            end;

        end;

    end
    else begin

        for Block := 7 downto 0 do begin

            if ((Dir^.Pointers[Block * 2] <> 0) or (Dir^.Pointers[(Block * 2) + 1] <> 0)) then begin
                Break;
            end;

        end;

    end;

    if (Dir^.Blkcnt <> 0) then begin
        Size := (Size + (((Dir^.Blkcnt and $FF) - 1) * 128));

        if ((FDrive.OsType and CPMFS_ISX) <> 0) then begin
            Size := (Size + (128 - Dir^.Lrc));
        end
        else begin

            if (Dir^.Lrc <> 0) then begin
                Size := (Size + (Dir^.Lrc and $FF));
            end
            else begin
                Size := (Size + 128);
            end;

        end;

    end;

    Result := (Size);
end;

// --------------------------------------------------------------------------------
//  -- print file name
// --------------------------------------------------------------------------------
function TCpmFileSystem.PrintFile(AExtent: integer): string;
type
    PPhysDirectoryEntry = ^TPhysDirectoryEntry;
var
    Dir: PPhysDirectoryEntry;
    IndexI: integer;
    FileChar: char;
begin
    Result := '';
    Dir := @FDirectory[AExtent];

    for IndexI := 0 to 7 do begin
        FileChar := Dir^.Name[IndexI];

        if ((Ord(FileChar) and $7F) < Ord(' ')) then begin
            Result := Result + '\';
            Result := Result + Chr(Ord('0') + ((Ord(FileChar) shr 6) and $01));
            Result := Result + Chr(Ord('0') + ((Ord(FileChar) shr 3) and $03));
            Result := Result + Chr(Ord('0') + (Ord(FileChar) and $03));
            Result := Result + '\';
        end
        else begin
            Result := Result + Chr(Ord(FileChar) and $7F);
        end;

    end;

    Result := Result + '.';

    for IndexI := 0 to 2 do begin
        FileChar := Dir^.Ext[IndexI];

        if ((Ord(FileChar) and $7F) < Ord(' ')) then begin
            Result := Result + '\';
            Result := Result + Chr(Ord('0') + ((Ord(FileChar) shr 6) and $01));
            Result := Result + Chr(Ord('0') + ((Ord(FileChar) shr 3) and $03));
            Result := Result + Chr(Ord('0') + (Ord(FileChar) and $03));
            Result := Result + '\';
        end
        else begin
            Result := Result + Chr(Ord(FileChar) and $7F);
        end;

    end;

end;

// --------------------------------------------------------------------------------
procedure TCpmFileSystem.CalcSecTrack(ABlockNr: integer; AOffset: integer; var ASect: integer; var ATrack: integer);
var
    TmpBlkNr: integer;
begin
    TmpBlkNr := ((ABlockNr * (FDrive.BlkSiz div FDrive.SecLength)) + AOffset);
    ASect := (TmpBlkNr mod FDrive.SecTrk);

    case (FDrive.SideOrder) of

        soAlt: begin
            ATrack := (TmpBlkNr div FDrive.SecTrk);
        end;

        soOutOut: begin
            if ((TmpBlkNr div FDrive.SecTrk) < (FDrive.Tracks div 2)) then begin
                ATrack := ((TmpBlkNr div FDrive.SecTrk) * 2);
            end
            else begin
                ATrack := (((TmpBlkNr div FDrive.SecTrk) * 2) - FDrive.Tracks + 1);
            end;
        end;

        soOutBack: begin
            if ((TmpBlkNr div FDrive.SecTrk) < (FDrive.Tracks div 2)) then begin
                ATrack := ((TmpBlkNr div FDrive.SecTrk) * 2);
            end
            else begin
                ATrack := (FDrive.Tracks - Abs(FDrive.Tracks - ((TmpBlkNr div FDrive.SecTrk) * 2)) - 1);
            end;
        end;

    end;

end;

// --------------------------------------------------------------------------------
end.
