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
unit CifeGlobals;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils;

function SettingsFile: string;
procedure GetDiskDefsList(DiskDefsList: TStrings);

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
procedure GetDiskDefsList(DiskDefsList: TStrings);
var
    Diskdefs: TStringList;
    Line: TStringArray;
    Index: integer;
begin
    try
        Diskdefs := TStringList.Create;
        DiskDefs.LoadFromFile('diskdefs');
        for Index := 0 to Diskdefs.Count - 1 do begin
            Line := Diskdefs[Index].Trim.Split(' ');
            if ((Length(Line) = 2) and (Line[0] = 'diskdef')) then begin
                DiskDefsList.Add(Line[1]);
            end;
        end;
    finally
        FreeAndNil(Diskdefs);
    end;
end;

// --------------------------------------------------------------------------------
end.
