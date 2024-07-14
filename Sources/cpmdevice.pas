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
unit CpmDevice;

{$mode ObjFPC}
{$H+}

interface

uses
    Classes, SysUtils;

type

    { TCpmDevice }

    TLibdskGeometry = array[0..255] of char;
    TLibdskDeviceOptions = array[0..79] of char;

    TCpmDevice = class
    public    // Attribute

    public    // Methoden
        function Open(const AFilename: string; const ADeviceOptions: TLibdskDeviceOptions): boolean; virtual;
        procedure SetGeometry(const ASecLength, ASecTrk, ATracks, AOffset: integer;
            const ALibdskGeometry: TLibdskGeometry); virtual;
        function Close: boolean; virtual;
        function ReadSector(const ATrack, ASector: integer; var ABuffer: array of byte): boolean; virtual;
        function WriteSector(const ATrack, ASector: integer; const ABuffer: array of byte): boolean; virtual;
        function IsOpen(): boolean; virtual;
        function GetErrorMsg(): string; virtual;
    public  // Konstruktor/Destruktor

    protected // Attribute

    protected // Methoden

    private   // Attribute

    private   // Methoden

    end;

implementation

{ TCpmDevice }

// --------------------------------------------------------------------------------
function TCpmDevice.Open(const AFilename: string; const ADeviceOptions: TLibdskDeviceOptions): boolean;
begin
    Result := False;
end;

// --------------------------------------------------------------------------------
procedure TCpmDevice.SetGeometry(const ASecLength, ASecTrk, ATracks, AOffset: integer; const ALibdskGeometry: TLibdskGeometry);
begin

end;

// --------------------------------------------------------------------------------
function TCpmDevice.Close: boolean;
begin
    Result := False;
end;

// --------------------------------------------------------------------------------
function TCpmDevice.ReadSector(const ATrack, ASector: integer; var ABuffer: array of byte): boolean;
begin
    Result := False;
end;

// --------------------------------------------------------------------------------
function TCpmDevice.WriteSector(const ATrack, ASector: integer; const ABuffer: array of byte): boolean;
begin
    Result := False;
end;

// --------------------------------------------------------------------------------
function TCpmDevice.IsOpen: boolean;
begin
    Result := False;
end;

// --------------------------------------------------------------------------------
function TCpmDevice.GetErrorMsg: string;
begin
    Result := '';
end;

// --------------------------------------------------------------------------------
end.
