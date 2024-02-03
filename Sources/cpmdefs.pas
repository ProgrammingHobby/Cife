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
unit CpmDefs;

{$mode ObjFPC}
{$H+}

interface

type
    off_t = longint;
    size_t = cardinal;
    ssize_t = longint;
    ino_t = cardinal;
    mode_t = nativeuint;
    cpm_attr_t = integer;
    time_t = TDateTime;

const
    INTBITS = (sizeof(integer) * 8);

    CPMFS_HI_USER = ($01 shl 0);     // has user numbers up to 31
    CPMFS_CPM3_DATES = ($01 shl 1);  // has CP/M+ style time stamps
    CPMFS_CPM3_OTHER = ($01 shl 2);  // has passwords and disc label
    CPMFS_DS_DATES = ($01 shl 3);    // has datestamper timestamps
    CPMFS_EXACT_SIZE = ($01 shl 4);  // has reverse exact file size

    CPMFS_DR22 = (CPMFS_HI_USER);
    CPMFS_P2DOS = (CPMFS_CPM3_DATES or CPMFS_HI_USER);
    CPMFS_DR3 = (CPMFS_CPM3_DATES or CPMFS_CPM3_OTHER or CPMFS_HI_USER);
    CPMFS_ISX = (CPMFS_EXACT_SIZE);
    CPMFS_ZSYS = (CPMFS_HI_USER);

    // CP/M file attributes
    CPM_ATTR_F1 = 1;
    CPM_ATTR_F2 = 2;
    CPM_ATTR_F3 = 4;
    CPM_ATTR_F4 = 8;
    // F5-F8 are banned in CP/M 2 & 3, F7 is used by ZSDOS

    CPM_ATTR_RO = 256;        // Read-only
    CPM_ATTR_SYS = 512;       // System
    CPM_ATTR_ARCV = 1024;     // Archive
    CPM_ATTR_PWDEL = 2048;    // Password required to delete
    CPM_ATTR_PWWRITE = 4096;  // Password required to write
    CPM_ATTR_PWREAD = 8192;   // Password required to read

    PASSWD_RECLEN = 24;
    RESERVED_ENTRIES = 4;
    RESERVED_INODES = 3;

    FS_OK = 0;
    FS_MODIFIED = 1;
    FS_BROKEN = 2;

    S_IFDIR = 16384;
    S_IFREG = 32768;

    S_IWUSR = &0200;
    S_IWGRP = (S_IWUSR shr 3);
    S_IWOTH = (S_IWGRP shr 3);

    O_RDONLY = 00;
    O_WRONLY = 01;
    O_RDWR = 02;

    T0 = 'S';
    T1 = 'E';
    T2 = 'C';
    T3 = 'R';
    T4 = 'E';
    T5 = 'T';
    T6 = ' ';
    T7 = ' ';

    PB = ((Ord(T0) + Ord(T1) + Ord(T2) + Ord(T3) + Ord(T4) + Ord(T5) + Ord(T6) + Ord(T7)) and $FF);
    P0 = (Ord(T7) xor PB);
    P1 = (Ord(T6) xor PB);
    P2 = (Ord(T5) xor PB);
    P3 = (Ord(T4) xor PB);
    P4 = (Ord(T3) xor PB);
    P5 = (Ord(T2) xor PB);
    P6 = (Ord(T1) xor PB);
    P7 = (Ord(T0) xor PB);

function BCD2BIN(AValue: integer): integer;
function BIN2BCD(AValue: integer): integer;
function S_ISDIR(AMode: mode_t): boolean; inline;
function S_ISREG(AMode: mode_t): boolean; inline;
function EXTENT(ALow: byte; AHigh: byte): integer; inline;
function EXTENTL(AExtent: integer): integer; inline;
function EXTENTH(AExtent: integer): integer; inline;

implementation

// --------------------------------------------------------------------------------
function BCD2BIN(AValue: integer): integer;
begin
    Result := ((((AValue shr 4) and $0F) * 10) + (AValue and $0F));
end;

// --------------------------------------------------------------------------------
function BIN2BCD(AValue: integer): integer;
begin
    Result := ((((AValue div 10) and $0F) shl 4) + ((AValue mod 10) and $0f));
end;

// --------------------------------------------------------------------------------
function S_ISDIR(AMode: mode_t): boolean;
begin
    Result := ((AMode and &0170000) = &040000);
end;

// --------------------------------------------------------------------------------
function S_ISREG(AMode: mode_t): boolean;
begin
    Result := ((AMode and &0170000) = &0100000);
end;

// --------------------------------------------------------------------------------
function EXTENT(ALow: byte; AHigh: byte): integer;
begin
    Result := ((ALow and $1F) or ((AHigh and $3F) shl 5));
end;

// --------------------------------------------------------------------------------
function EXTENTL(AExtent: integer): integer;
begin
    Result := (AExtent and $1F);
end;

// --------------------------------------------------------------------------------
function EXTENTH(AExtent: integer): integer;
begin
    Result := ((AExtent shr 5) and $3F);
end;

end.
