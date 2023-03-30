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
begin

end;

// --------------------------------------------------------------------------------
function TCpmFileSystem.DiskdefsReadSuper(const AImageType: string): boolean;
begin

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
