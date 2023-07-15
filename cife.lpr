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
    Main_Window,
    CifeGlobals { you can add units after this };

{$R *.res}

begin
    RequireDerivedFormResource := True;
    Application.Scaled := True;
    Application.Initialize;
    if (not IsDiskdefsFilePresent) then begin
        MessageDlg('Diskdefinitions File not found. Please copy ''diskdefs'' into Application directory and restart CP/M Image-File Explorer.'
            , mtError, [mbOK], 0);
        exit;
    end;
    Application.CreateForm(TMainWindow, MainWindow);
    Application.Run;
end.
