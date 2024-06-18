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
unit FileHexEdit_Dialog;

{$mode ObjFPC}
{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls,
    ExtCtrls, Buttons;

type

    { TFileHexEditDialog }

    TFileHexEditDialog = class(TForm)
        buttonPanel: TButtonPanel;
        checkboxShowRuler: TCheckBox;
        checkboxReadOnly: TCheckBox;
        imageList: TImageList;
        labelPosition: TLabel;
        panelToolButtons: TPanel;
        buttonUndo: TSpeedButton;
        buttonRedo: TSpeedButton;
    private

    public
        procedure SetFileData(AFileData: TMemoryStream; ALength: QWord; AName: string);
    end;

var
    FileHexEditDialog: TFileHexEditDialog;

implementation

{$R *.lfm}

{ TFileHexEditDialog }

// --------------------------------------------------------------------------------
procedure TFileHexEditDialog.SetFileData(AFileData: TMemoryStream; ALength: QWord; AName: string);
begin
    Caption := AName;
end;

end.
