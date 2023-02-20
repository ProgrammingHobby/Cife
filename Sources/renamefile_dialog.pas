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
unit RenameFile_Dialog;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel,
    ExtCtrls, StdCtrls;

type

    { TRenameFileDialog }

    TRenameFileDialog = class(TForm)
        ButtonPanel1: TButtonPanel;
        checkboxChangeUserNumber: TCheckBox;
        editOldUserNumer: TEdit;
        editOldFileName: TEdit;
        editNewUserNumber: TEdit;
        editNewFileName: TEdit;
        Label1: TLabel;
        Label2: TLabel;
        Panel1: TPanel;
        Panel2: TPanel;
        Panel3: TPanel;
        Panel4: TPanel;
        Panel5: TPanel;
    private

    public

    end;

var
    RenameFileDialog: TRenameFileDialog;

implementation

{$R *.lfm}

end.
