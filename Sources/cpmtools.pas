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
unit CpmTools;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, CpmFileSystem;

type

    { TCpmTools }

    TCpmTools = class(TCpmFileSystem)
    public    // Attribute

    public    // Methoden
        function OpenImage(const AFileName: string; const AFileType: string; AUpperCase: boolean): boolean;
        function GetFileName: string;
        function GetFileType: string;

    public  // Konstruktor/Destruktor
        constructor Create; overload;
        destructor Destroy; override;

    protected // Attribute

    protected // Methoden

    private   // Attribute
        FFileName: string;
        FFileType: string;

    private   // Methoden

    end;

implementation

{ TCpmTools }

// --------------------------------------------------------------------------------
function TCpmTools.OpenImage(const AFileName: string; const AFileType: string; AUpperCase: boolean): boolean;
begin
    FFileName := AFileName;
    FFileType := AFileType;
    Result := True;
end;

// --------------------------------------------------------------------------------
function TCpmTools.GetFileName: string;
begin
    Result := FFileName;
end;

// --------------------------------------------------------------------------------
function TCpmTools.GetFileType: string;
begin
    Result := FFileType;
end;

// --------------------------------------------------------------------------------
constructor TCpmTools.Create;
begin
    inherited Create;
end;

// --------------------------------------------------------------------------------
destructor TCpmTools.Destroy;
begin
    inherited Destroy;
end;

// --------------------------------------------------------------------------------
end.
