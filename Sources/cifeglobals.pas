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
unit CifeGlobals;

{$mode ObjFPC}
{$H+}

interface

uses
    Classes, SysUtils, StdCtrls;

type
    TFileSystemInfo = record
        FileName: string;
        FileType: string;
        Tracks: string;
        Sectors: string;
        SecBytes: string;
        BlockSize: string;
        MaxDir: string;
        BootSectors: string;
        Offset: string;
        skew: string;
        System: string;
    end;

    TDirStatistic = record
        TotalBytes: string;
        TotalRecords: string;
        Total1KBlocks: string;
        TotalFreeBytes: string;
        TotalDiskBytes: string;
        FilesFound: string;
        MaxDirEntries: string;
        UsedDirEntries: string;
    end;

    TFileInfo = record
        Name: string;
        UserNumber: integer;
        UsedBytes: integer;
        UsedRecords: integer;
        MTime: double;
        ATime: double;
        CTime: double;
        Attributes: integer;
    end;

    TUTimeBuf = record
        AcTime: TDateTime;
        ModTime: TDatetime;
    end;

function SettingsFile: string;
procedure GetDiskDefsList(APath: string; ADiskDefsList: TComboBox);
function CheckDiskdefsFile(ADiskdefsFile: string): boolean;
function CheckLibdskLibrary(ALibdskFile: string): boolean;

implementation

uses ImageTypeInfo, dynlibs, libdsk;

// --------------------------------------------------------------------------------
function SettingsFile: string;
begin
    Result := GetAppConfigDir(False);

    if (not DirectoryExists(Result, False)) then begin
        MkDir(Result);
    end;

    Result := Result + 'cife.xml';
end;

// --------------------------------------------------------------------------------
procedure GetDiskDefsList(APath: string; ADiskDefsList: TComboBox);
var
    Diskdefs: TStringList;
    Line: TStringArray;
    Index, BootTrk, BootSec: integer;
    LabelUsed, BootTrackUsed: boolean;
    ImageTypeName, OsType: string;
begin
    ADiskDefsList.Items.Clear;
    ADiskDefsList.Items.BeginUpdate;

    try
        Diskdefs := TStringList.Create;
        DiskDefs.LoadFromFile(APath);
        ADiskDefsList.AddItem('Amstrad (PCW16)', TImageTypeInfo.Create(True, False));
        Index := 0;

        while (Index < Diskdefs.Count) do begin
            Line := Diskdefs[Index].Trim.Split(' ');

            if ((Length(Line) = 2) and (Line[0] = 'diskdef')) then begin
                ImageTypeName := Line[1];
                BootTrk := -1;
                BootSec := -1;
                OsType := '';

                while not ((Length(Line) >= 1) and (Line[0] = 'end')) do begin
                    Inc(Index);
                    Line := Diskdefs[Index].Trim.Split(' ');

                    if (Line[0] = 'boottrk') then begin
                        BootTrk := StrToIntDef(Line[1], -1);
                    end
                    else if (Line[0] = 'bootsec') then begin
                        BootSec := StrToIntDef(Line[1], -1);
                    end
                    else if (Line[0] = 'os') then begin
                        OsType := Line[1];
                    end;

                end;

                LabelUsed := (OsType = '3');
                BootTrackUsed := ((BootTrk > 0) or (BootSec > 0));
                ADiskDefsList.AddItem(ImageTypeName, TImageTypeInfo.Create(LabelUsed, BootTrackUsed));
            end;

            Inc(Index);
        end;

    finally
        FreeAndNil(Diskdefs);
    end;

    ADiskDefsList.Items.EndUpdate;
end;

// --------------------------------------------------------------------------------
function CheckDiskdefsFile(ADiskdefsFile: string): boolean;
var
    Diskdefs: TStringList;
    Line: TStringArray;
    BeginCount, EndCount: integer;
    IndexI: integer;
begin
    Result := True;

    if not FileExists(ADiskdefsFile) then begin
        Result := False;
        exit;
    end;

    try
        Diskdefs := TStringList.Create;
        Diskdefs.LoadFromFile(ADiskdefsFile);
        BeginCount := 0;
        EndCount := 0;
        IndexI := 0;

        while (IndexI < Diskdefs.Count) do begin
            Line := Diskdefs[IndexI].Trim.Split(' ');

            if (Line[0] = 'diskdef') then begin
                Inc(BeginCount);
            end;

            if (Line[0] = 'end') then begin
                Inc(EndCount);
            end;

            Inc(IndexI);
        end;

    finally
        FreeAndNil(Diskdefs);
    end;

    if (BeginCount <> EndCount) then begin
        Result := False;
    end;

end;

// --------------------------------------------------------------------------------
function CheckLibdskLibrary(ALibdskFile: string): boolean;
var
    LibdskHandle: TLibHandle;
    dg_stdformat: Tdg_stdformat;
    dsk_open: Tdsk_open;
    dsk_close: Tdsk_close;
    dsk_strerror: Tdsk_strerror;
    dsk_getgeom: Tdsk_getgeom;
    dsk_lread: Tdsk_lread;
    dsk_lwrite: Tdsk_lwrite;
begin
    Result := True;

    if not FileExists(ALibdskFile) then begin
        Result := False;
        exit;
    end;

    LibdskHandle := LoadLibrary(ALibdskFile);

    if (LibdskHandle <> dynlibs.NilHandle) then begin
        {$ifdef UNIX}
        dg_stdformat := Tdg_stdformat(GetProcedureAddress(LibdskHandle, 'dg_stdformat'));
        dsk_open := Tdsk_open(GetProcedureAddress(LibdskHandle, 'dsk_open'));
        dsk_close := Tdsk_close(GetProcedureAddress(LibdskHandle, 'dsk_close'));
        dsk_strerror := Tdsk_strerror(GetProcedureAddress(LibdskHandle, 'dsk_strerror'));
        dsk_getgeom := Tdsk_getgeom(GetProcedureAddress(LibdskHandle, 'dsk_getgeom'));
        dsk_lread := Tdsk_lread(GetProcedureAddress(LibdskHandle, 'dsk_lread'));
        dsk_lwrite := Tdsk_lwrite(GetProcedureAddress(LibdskHandle, 'dsk_lwrite'));
        {$endif}

        {$ifdef WINDOWS}
        dg_stdformat := Tdg_stdformat(GetProcedureAddress(LibdskHandle, '_dg_stdformat@16'));
        dsk_open := Tdsk_open(GetProcedureAddress(LibdskHandle, '_dsk_open@16'));
        dsk_close := Tdsk_close(GetProcedureAddress(LibdskHandle, '_dsk_close@4'));
        dsk_strerror := Tdsk_strerror(GetProcedureAddress(LibdskHandle, '_dsk_strerror@4'));
        dsk_getgeom := Tdsk_getgeom(GetProcedureAddress(LibdskHandle, '_dsk_getgeom@8'));
        dsk_lread := Tdsk_lread(GetProcedureAddress(LibdskHandle, '_dsk_lread@16'));
        dsk_lwrite := Tdsk_lwrite(GetProcedureAddress(LibdskHandle, '_dsk_lwrite@16'));
        {$endif}
    end;

    if ((LibdskHandle = dynlibs.NilHandle) or not (Assigned(dg_stdformat) and Assigned(dsk_open) and
        Assigned(dsk_close) and Assigned(dsk_strerror) and Assigned(dsk_getgeom) and Assigned(dsk_lread) and
        Assigned(dsk_lwrite))) then begin
        Result := False;
        exit;
    end;

    if (LibdskHandle <> dynlibs.NilHandle) then begin

        if (FreeLibrary(LibdskHandle)) then begin
            LibdskHandle := dynlibs.NilHandle;
        end;

    end;

end;

// --------------------------------------------------------------------------------
end.
