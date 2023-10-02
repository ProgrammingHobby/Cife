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

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils;

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
procedure GetDiskDefsList(APath: string; ADiskDefsList: TStrings);

implementation

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
procedure GetDiskDefsList(APath: string; ADiskDefsList: TStrings);
var
    Diskdefs: TStringList;
    Line: TStringArray;
    Index: integer;
begin
    try
        Diskdefs := TStringList.Create;
        DiskDefs.LoadFromFile(APath);
        ADiskDefsList.Add('Amstrad (PCW16)');
        for Index := 0 to Diskdefs.Count - 1 do begin
            Line := Diskdefs[Index].Trim.Split(' ');
            if ((Length(Line) = 2) and (Line[0] = 'diskdef')) then begin
                ADiskDefsList.Add(Line[1]);
            end;
        end;
    finally
        FreeAndNil(Diskdefs);
    end;
end;

// --------------------------------------------------------------------------------
end.
