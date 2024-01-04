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
        TotalBytes: integer;
        TotalRecords: integer;
        Total1KBlocks: integer;
        FilesFound: integer;
        MaxDirEntries: integer;
        UsedDirEntries: integer;
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

function SettingsFile: string;
procedure GetDiskDefsList(APath: string; ADiskDefsList: TComboBox);

implementation

uses ImageTypeInfo;

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
end;

// --------------------------------------------------------------------------------
end.
