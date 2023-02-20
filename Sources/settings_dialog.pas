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
unit Settings_Dialog;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
    ButtonPanel, ExtCtrls, StdCtrls, Spin;

type

    { TSettingsDialog }

    TSettingsDialog = class(TForm)
        ButtonPanel: TButtonPanel;
        checkboxKeepTimeStamps: TCheckBox;
        checkboxOpenLastImage: TCheckBox;
        checkboxUppercaseCpmCharacters: TCheckBox;
        Label1: TLabel;
        Label2: TLabel;
        memoTextfileEndings: TMemo;
        Notebook: TNotebook;
        Page1: TPage;
        Page2: TPage;
        Panel1: TPanel;
        Panel4: TPanel;
        Panel2: TPanel;
        Panel3: TPanel;
        Panel5: TPanel;
        Panel6: TPanel;
        spineditUserNumber: TSpinEdit;
        Splitter: TSplitter;
        treeviewSettingPages: TTreeView;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure PanelPaint(Sender: TObject);
        procedure treeviewSettingPagesSelectionChanged(Sender: TObject);
    private

    public

    end;

var
    SettingsDialog: TSettingsDialog;

implementation

{$R *.lfm}

{ TSettingsDialog }
// --------------------------------------------------------------------------------
procedure TSettingsDialog.PanelPaint(Sender: TObject);
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
procedure TSettingsDialog.treeviewSettingPagesSelectionChanged(Sender: TObject);
begin
    Notebook.PageIndex := (Sender as TTreeView).Selected.AbsoluteIndex;
end;

// --------------------------------------------------------------------------------
procedure TSettingsDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    CloseAction := caFree;
end;

end.
