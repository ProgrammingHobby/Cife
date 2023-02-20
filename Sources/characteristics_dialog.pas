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
unit Characteristics_Dialog;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
    ButtonPanel;

type

    { TCharacteristicsDialog }

    TCharacteristicsDialog = class(TForm)
        ButtonPanel1: TButtonPanel;
        checkboxChangeProtect: TCheckBox;
        checkgroupProtections: TCheckGroup;
        checkgroupAttributes: TCheckGroup;
        Label1: TLabel;
        labelLastAccess: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        labelFileName: TLabel;
        labelFileSize: TLabel;
        labelCreated: TLabel;
        labelUpdated: TLabel;
        Panel1: TPanel;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure Panel1Paint(Sender: TObject);
    private

    public

    end;

var
    CharacteristicsDialog: TCharacteristicsDialog;

implementation

{$R *.lfm}

{ TCharacteristicsDialog }
// --------------------------------------------------------------------------------
procedure TCharacteristicsDialog.Panel1Paint(Sender: TObject);
const
    R = 4;  // Rundungsradius
var
    panel: TPanel;
begin
    if not (Sender is TPanel) then begin
        exit;
    end;
    panel := TPanel(Sender);
    panel.Canvas.Brush.Style := bsClear;
    panel.Canvas.Pen.Color := clSilver;
    panel.Canvas.Pen.Width := 1;
    panel.Canvas.RoundRect(0, 0, panel.ClientWidth, panel.ClientHeight, R, R);
end;

// --------------------------------------------------------------------------------
procedure TCharacteristicsDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    CloseAction := caFree;
end;

end.
