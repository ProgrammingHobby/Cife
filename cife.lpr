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
program cife;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
    cthreads,
 {$ENDIF}
 {$IFDEF HASAMIGA}
    athreads,
 {$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms,
    Dialogs,
    Main_Window { you can add units after this };

{$R *.res}

begin
    RequireDerivedFormResource := True;
  Application.Scaled:=True;
    Application.Initialize;
    Application.CreateForm(TMainWindow, MainWindow);
    Application.Run;
end.
