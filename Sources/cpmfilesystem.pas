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
        Ino: array of TCpmInode;
    end;

    { TCpmFileSystem }

    TCpmFileSystem = class
    public    // Attribute

    public    // Methoden
        function ReadDiskdefData(const AImageType: string): boolean;
        function InitDriveData(AUpperCase: boolean): boolean;
        function Unmount: boolean;
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
            Extents: integer; { logical extents per physical extent }
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
        function CheckDateStamps: boolean;
        function IsMatching(AUser1: integer; const AName1: array of char; const AExt1: array of char;
            AUser2: integer; const AName2: array of char; const AExt2: array of char): boolean;
        function SyncDateStamps: boolean;
    end;

implementation

{ TCpmFileSystem }

// --------------------------------------------------------------------------------
//  -- get DPB
// --------------------------------------------------------------------------------
function TCpmFileSystem.ReadDiskdefData(const AImageType: string): boolean;
begin
    Result := False;

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
    IndexI, IndexJ, Value: integer;
    Blocks, Passwords: integer;
    DirectoryBuffer: array of byte = nil;
begin
    Result := True;
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

                IndexJ := 0;

                while ((IndexJ < IndexI) and (FSkewTab[IndexJ] <> Value)) do begin
                    Inc(IndexJ);
                end;

                if (IndexJ < IndexI) then begin
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
                            FFileSystemError := e.Message;
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
    Result := True;
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
    FDrive.Offset := 0;
    FDrive.Size := ((FDrive.SecLength * FDrive.SecTrk * (FDrive.Tracks - FDrive.BootTrk)) div FDrive.BlkSiz);
    if (FDrive.Size > 256) then begin
        FDrive.Extents := ((FDrive.BlkSiz * 8) div 16384);
    end
    else begin
        FDrive.Extents := ((FDrive.BlkSiz * 16) div 16384);
    end;
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
    FDrive.OsType := 0;
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
                            FDrive.Extents := ((FDrive.BlkSiz * 8) div 16384);
                        end
                        else begin
                            FDrive.Extents := ((FDrive.BlkSiz * 16) div 16384);
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
                            break;
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
                            break;
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
                            break;
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
                                        break;
                                    end;

                                    Multiplier := (FDrive.SecTrk * FDrive.SecLength);
                                end;
                                'S': begin

                                    if ((FDrive.SecTrk < 0) or (FDrive.Tracks < 0) or (FDrive.SecLength < 0)) then begin
                                        FFileSystemError :=
                                            Format('offset must be specified after sectrk, tracks and secLength in line %d',
                                            [(LineNumber)]);
                                        Result := False;
                                        break;
                                    end;

                                    Multiplier := FDrive.SecLength;
                                end;
                                else begin
                                    FFileSystemError :=
                                        Format('unknown unit specifier ''%s'' in line %d',
                                        [DefinitionLine[1][Pass], (LineNumber)]);
                                    Result := False;
                                    break;
                                end;
                            end;

                        end;

                        if ((Value * Multiplier) > MaxInt) then begin
                            FFileSystemError := Format('effective offset is out of range in line %d', [(LineNumber)]);
                            Result := False;
                            break;
                        end;

                        FDrive.Offset := (Value * Multiplier);
                    end
                    else if (DefinitionLine[0] = 'logicalextents') then begin
                        FDrive.Extents := StrToIntDef(DefinitionLine[1], -1);
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
                                break;
                            end;
                        end;

                    end;
                end
                else if ((Length(DefinitionLine) > 0) and not (DefinitionLine[0][1] = '#') and not (DefinitionLine[0][1] = ';'))
                then begin
                    FFileSystemError := Format('invalid keyword ''%s'' in line %d', [DefinitionLine[0], (LineNumber)]);
                    Result := False;
                    break;
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

    for IndexI := Low(AName1) to High(AName2) do begin

        if ((Ord(AName1[IndexI]) and $7F) <> (Ord(AName2[IndexI]) and $7F)) then begin
            Result := False;
            exit;
        end;

    end;

    for IndexI := Low(AExt1) to High(AExt2) do begin

        if ((Ord(AExt1[IndexI]) and $7F) <> (Ord(AExt2[IndexI]) and $7F)) then begin
            Result := False;
            exit;
        end;

    end;

    Result := True;
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

        DSOffset := (((FDrive.MaxDir * 32) + (FDrive.BlkSiz - 1)) div FDrive.BlkSiz);
        DSRecords := ((FDrive.MaxDir + 7) div 8);
        DSBlocks := (((DSRecords * 128) + (FDrive.BlkSiz - 1)) div FDrive.BlkSiz);

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
end.
