{*
 *  Copyright (C) 2024  Uwe Merker
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
unit ImageTypeInfo;

{$mode ObjFPC}
{$H+}

interface

uses
    Classes, SysUtils;

type

    { TImageTypeInfo }

    TImageTypeInfo = class
    public    // Attribute

    private   // Attribute
        FLabelUsed: boolean;
        FBootTrackUsed: boolean;

    public    // Methoden
        property LabelUsed: boolean read FLabelUsed write FLabelUsed;
        property BootTrackUsed: boolean read FBootTrackUsed write FBootTrackUsed;

    private   // Methoden

    public  // Konstruktor/Destruktor
        constructor Create(ALabelUsed: boolean; ABootTrackUsed: boolean);

    protected // Attribute

    protected // Methoden

    end;

implementation

{ TImageTypeInfo }

// --------------------------------------------------------------------------------
constructor TImageTypeInfo.Create(ALabelUsed: boolean; ABootTrackUsed: boolean);
begin
    FLabelUsed := ALabelUsed;
    FBootTrackUsed := ABootTrackUsed;
end;

end.
