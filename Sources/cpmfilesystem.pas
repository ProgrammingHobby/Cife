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
    Classes, SysUtils, CpmDevice, CpmDefs;

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
            Name: string[14];
        end;

        TPhysDirectoryEntry = record
            Status: byte;
            Name: string[8];
            Ext: string[3];
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

        TDateStamperDate = record
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
            DiskLabel: string[8];
            LabelLength: size_t;
            Passwd: string[8];
            PasswdLength: size_t;
            DirtyDirectory: boolean;
            DirtyDs: boolean;
        end;

        TIntArray = array of integer;
        TByteArray = array of byte;
        TDirArray = array of TPhysDirectoryEntry;
        TDsArray = array of TDateStamperDate;

    var
        FCpmDevice: TCpmDevice;
        FFileSystemError: string;
        FDrive: TCpmSuperBlock;
        FSkewTab: TIntArray;
        FDirectory: TDirArray;
        FAllocationVector: TIntArray;
        FDateStamper: TDsArray;

    private   // Methoden
        function AmstradReadSuper(): boolean;
        function DiskdefsReadSuper(const AImageType: string): boolean;
        function BootOffset: integer;

    end;

implementation

{ TCpmFileSystem }

// --------------------------------------------------------------------------------
function TCpmFileSystem.ReadDiskdefData(const AImageType: string): boolean;
begin

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
end.
