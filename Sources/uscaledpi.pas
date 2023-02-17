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
unit UscaleDPI;

{$mode objfpc}{$H+}

interface

uses
    Forms, Graphics, Controls;

procedure HighDPI(FromDPI: integer);
procedure ScaleDPI(Control: TControl; FromDPI: integer);

implementation

procedure HighDPI(FromDPI: integer);
var
    i: integer;
begin
    for i := 0 to Screen.FormCount - 1 do
        ScaleDPI(Screen.Forms[i], FromDPI);
end;

procedure ScaleDPI(Control: TControl; FromDPI: integer);
var
    n: integer;
    WinControl: TWinControl;
begin
    if Screen.PixelsPerInch = FromDPI then
        exit;

    with Control do begin
        Left := ScaleX(Left, FromDPI);
        Top := ScaleY(Top, FromDPI);
        Width := ScaleX(Width, FromDPI);
        Height := ScaleY(Height, FromDPI);
        Font.Height := ScaleY(Font.GetTextHeight('Hg'), FromDPI);
        end;

    if Control is TWinControl then begin
        WinControl := TWinControl(Control);
        if WinControl.ControlCount > 0 then begin
            for n := 0 to WinControl.ControlCount - 1 do begin
                if WinControl.Controls[n] is TControl then begin
                    ScaleDPI(WinControl.Controls[n], FromDPI);
                    end;
                end;
            end;
        end;
end;

end.
