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
        editOldUserNumber: TEdit;
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
        procedure checkboxChangeUserNumberClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    private
        FOldName: string;
        FNewName: string;

    public
        procedure SetOldName(AOldName: string);
        function GetNewName: string;

    end;

var
    RenameFileDialog: TRenameFileDialog;

implementation

{$R *.lfm}
uses StrUtils;

{ TRenameFileDialog }

// --------------------------------------------------------------------------------
procedure TRenameFileDialog.checkboxChangeUserNumberClick(Sender: TObject);
begin
    editNewUserNumber.Enabled := checkboxChangeUserNumber.Checked;
end;

procedure TRenameFileDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    if (ModalResult = mrOk) then begin
        FNewName := editNewUserNumber.Text + ':' + editNewFileName.Text;
        if (FNewName <> FOldName) then begin
            ModalResult := mrOk;
        end
        else begin
            FNewName := '';
            ModalResult := mrCancel;
        end;
    end;
    CloseAction := caFree;
end;

// --------------------------------------------------------------------------------
procedure TRenameFileDialog.SetOldName(AOldName: string);
begin
    FOldName := AOldName;
    editOldUserNumber.Text := LeftStr(FOldName, Pos(':', FOldName) - 1);
    editNewUserNumber.Text := LeftStr(FOldName, Pos(':', FOldName) - 1);
    editOldFileName.Text := RightStr(FOldName, Length(FOldName) - Pos(':', FOldName));
    editNewFileName.Text := RightStr(FOldName, Length(FOldName) - Pos(':', FOldName));
end;

// --------------------------------------------------------------------------------
function TRenameFileDialog.GetNewName: string;
begin
    Result := FNewName;
end;

end.
