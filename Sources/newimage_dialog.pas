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
unit NewImage_Dialog;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel,
    ExtCtrls, StdCtrls;

type

    { TNewImageDialog }

    TNewImageDialog = class(TForm)
        buttonOpenImageFile: TButton;
        buttonOpenBootTrackFile: TButton;
        ButtonPanel1: TButtonPanel;
        checkboxUseTimestamps: TCheckBox;
        editImageFile: TEdit;
        editImageType: TEdit;
        editBootTrackFile: TEdit;
        editFileSystemLabel: TEdit;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        labelCreateNotice: TLabel;
        Panel1: TPanel;
        Panel2: TPanel;
        Panel3: TPanel;
        Panel4: TPanel;
        Panel5: TPanel;
        procedure PanelPaint(Sender: TObject);
    private

    public

    end;

var
    NewImageDialog: TNewImageDialog;

implementation

{$R *.lfm}

{ TNewImageDialog }

// --------------------------------------------------------------------------------
procedure TNewImageDialog.PanelPaint(Sender: TObject);
const
    radius = 4;
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
    panel.Canvas.RoundRect(0, 0, panel.ClientWidth, panel.ClientHeight, radius, radius);
end;

// --------------------------------------------------------------------------------
end.