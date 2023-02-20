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
unit File_Dialog;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel,
    ComCtrls, ExtCtrls, Buttons, ShellCtrls, StdCtrls;

type

    { TFileDialog }

    TFileDialog = class(TForm)
        ButtonPanel: TButtonPanel;
        comboboxImageTypes: TComboBox;
        comboboxFileTypes: TComboBox;
        editFileName: TEdit;
        ImageList: TImageList;
        Label1: TLabel;
        labelFilePath: TLabel;
        Panel1: TPanel;
        buttonListView: TSpeedButton;
        buttonDetailView: TSpeedButton;
        buttonFolderUp: TSpeedButton;
        buttonHomeFolder: TSpeedButton;
        buttonNewFolder: TSpeedButton;
        Panel10: TPanel;
        Panel11: TPanel;
        Panel2: TPanel;
        Panel3: TPanel;
        Panel4: TPanel;
        Panel5: TPanel;
        Panel6: TPanel;
        Panel7: TPanel;
        Panel8: TPanel;
        Panel9: TPanel;
        ShellListView: TShellListView;
        ShellTreeView: TShellTreeView;
        Splitter: TSplitter;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure Panel4Paint(Sender: TObject);
    private

    public

    end;

var
    FileDialog: TFileDialog;

implementation

{$R *.lfm}

{ TFileDialog }
// --------------------------------------------------------------------------------
procedure TFileDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    CloseAction := caFree;
end;

// --------------------------------------------------------------------------------
procedure TFileDialog.Panel4Paint(Sender: TObject);
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

end.
