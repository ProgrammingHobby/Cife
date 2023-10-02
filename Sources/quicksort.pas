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
unit QuickSort;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils;

procedure QSort(var arr: TStringList; Start: longint; Finish: longint; SortUp: boolean = True);

implementation

// --------------------------------------------------------------------------------
procedure QSort(var arr: TStringList; Start: longint; Finish: longint; SortUp: boolean = True);
var
    i, j: longint;
    x, temp: ansistring;
    u: array[0..255] of byte;  {look up table for speed}

    // --------------------------------------------------------------------------------
    function lessthan(const a: ansistring; const b: ansistring): integer;
    var
        n, lim, lena, lenb: longint;
    begin
        lena := length(a);
        lenb := length(b);
        if (lena < lenb) then
            lim := lena
        else
            lim := lenb;
        for n := 1 to lim do begin
            if u[Ord(a[n])] < u[Ord(b[n])] then
                exit(-1);
            if u[Ord(a[n])] > u[Ord(b[n])] then
                exit(0);
        end;
        exit(0);
    end;

    // --------------------------------------------------------------------------------
    function morethan(const a: ansistring; const b: ansistring): integer;
    var
        n, lim, lena, lenb: longint;
    begin
        lena := length(a);
        lenb := length(b);
        if (lena < lenb) then
            lim := lena
        else
            lim := lenb;
        for n := 1 to lim do begin
            if u[Ord(a[n])] > u[Ord(b[n])] then
                exit(-1);
            if u[Ord(a[n])] < u[Ord(b[n])] then
                exit(0);
        end;
        exit(0);
    end;

    // --------------------------------------------------------------------------------
    procedure setlookuparray(var u: array of byte); // must run
    var
        x, r: integer;
    begin
        for x := 0 to 255 do begin
            r := x;
   {$ifdef caseinsensitive}
            if (r < 91) and (r > 64) then
                r := r + 32;
   {$endif}
            u[x] := r;
        end;
    end;

begin
    i := Start;
    j := finish;
    x := arr[(Start + finish) div 2];
    setlookuparray(u);
    while I <= J do begin
        if SortUp then begin
            while lessthan(arr[I], X) = -1 do
                I += 1;
            while morethan(arr[J], X) = -1 do
                J -= 1;
        end;

        if not SortUp then begin
            while morethan(arr[I], X) = -1 do
                I += 1;
            while lessthan(arr[J], X) = -1 do
                J -= 1;
        end;

        if I <= J then begin
            temp := arr[j];
            arr[j] := arr[i];
            arr[i] := temp;
            I += 1;
            J -= 1;
        end;
    end;
    if J > Start then
        QSort(arr, Start, J, SortUp);
    if I < Finish then
        QSort(arr, I, Finish, SortUp);
end;
// --------------------------------------------------------------------------------

end.
