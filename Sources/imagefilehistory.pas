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
unit ImageFileHistory;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Menus;

type

    { TImageFileHistory }

    TImageFileHistory = class
    public    // Attribute

    public    // Methoden

    public  // Konstruktor/Destruktor
        constructor Create(RecentMenu: TMenuItem); overload;
        destructor Destroy; override;

    protected // Attribute

    protected // Methoden

    private   // Attribute

    private   // Methoden

    end;

implementation

{ TImageFileHistory }

// --------------------------------------------------------------------------------
constructor TImageFileHistory.Create(RecentMenu: TMenuItem);
begin

end;

// --------------------------------------------------------------------------------
destructor TImageFileHistory.Destroy;
begin
    inherited Destroy;
end;

// --------------------------------------------------------------------------------
end.