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
    ExtCtrls, Buttons, MPHexEditor;

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
        procedure buttonRedoClick(Sender: TObject);
        procedure buttonUndoClick(Sender: TObject);
        procedure checkboxReadOnlyChange(Sender: TObject);
        procedure checkboxShowRulerChange(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormResize(Sender: TObject);
        procedure FormShow(Sender: TObject);
    private
        FHexEditor: TMPHexEditor;
        procedure AppIdle(Sender: TObject; var Done: boolean);
    public
        procedure SetFileData(AFileData: TMemoryStream; AName: string);
    end;

var
    FileHexEditDialog: TFileHexEditDialog;

implementation

{$R *.lfm}

{ TFileHexEditDialog }

uses LCLIntf, LCLType;

// --------------------------------------------------------------------------------
procedure TFileHexEditDialog.FormCreate(Sender: TObject);
begin

    FHexEditor := TMPHexEditor.Create(self);

    with FHexEditor do begin
        Parent := self;
        Align := alClient;
        BytesPerColumn := 1;
        BytesPerBlock := -1;
        BytesPerRow := 16;
        DrawGridLines := True;
        ReadOnlyView := True;
        ScrollBars := ssVertical;
        BorderSpacing.Around := 4;
        AllowInsertMode := False;
        DragMode := dmManual;
        MaxUndo := 1024;
        GutterWidth := -1;
        DrawGutter3D := True;
        ShowRuler := False;
    end;

    Application.OnIdle := @AppIdle;
end;

// --------------------------------------------------------------------------------
procedure TFileHexEditDialog.checkboxReadOnlyChange(Sender: TObject);
begin
    FHexEditor.ReadOnlyView := checkboxReadOnly.Checked;
end;

// --------------------------------------------------------------------------------
procedure TFileHexEditDialog.buttonRedoClick(Sender: TObject);
begin
    FHexEditor.Redo;
end;

// --------------------------------------------------------------------------------
procedure TFileHexEditDialog.buttonUndoClick(Sender: TObject);
begin
    FHexEditor.Undo;
end;

// --------------------------------------------------------------------------------
procedure TFileHexEditDialog.checkboxShowRulerChange(Sender: TObject);
begin
    FHexEditor.ShowRuler := checkboxShowRuler.Checked;
end;

// --------------------------------------------------------------------------------
procedure TFileHexEditDialog.FormDestroy(Sender: TObject);
begin
    Application.OnIdle := nil;
end;

// --------------------------------------------------------------------------------
procedure TFileHexEditDialog.FormResize(Sender: TObject);
var
    test: integer;
begin
    test := 0;
end;

// --------------------------------------------------------------------------------
procedure TFileHexEditDialog.FormShow(Sender: TObject);
var
    HexEditorWidth, HexEditorHeight: integer;
begin
    HexEditorWidth := FHexEditor.DisplayWidth + GetSystemMetrics(SM_CXVSCROLL) + (2 * FHexEditor.BorderSpacing.Around) +
        (GetSystemMetrics(SM_CXFRAME) div 2);
    Self.Width := HexEditorWidth;
    Self.Constraints.MinWidth := HexEditorWidth;
    Self.Constraints.MaxWidth := HexEditorWidth;
    HexEditorHeight := (16 * FHexEditor.RowHeight) + panelToolButtons.Height + buttonPanel.Height +
        (2 * FHexEditor.BorderSpacing.Around) + (GetSystemMetrics(SM_CYHSCROLL) div 4) +
        (GetSystemMetrics(SM_CYFIXEDFRAME) * ((GetSystemMetrics(SM_CYHSCROLL) + GetSystemMetrics(SM_CYFIXEDFRAME) + 1) div 7));
    Self.Height := HexEditorHeight;
    Self.Constraints.MinHeight := HexEditorHeight;
end;

// --------------------------------------------------------------------------------
procedure TFileHexEditDialog.AppIdle(Sender: TObject; var Done: boolean);
begin
    Done := True;

    with FHexEditor do begin
        labelPosition.Caption := Format('Position : %s', [Trim(GetAnyOffsetString(GetCursorPos))]);
        buttonUndo.Enabled := CanUndo;
        buttonUndo.ShowHint := CanUndo;
        buttonUndo.Hint := Format('Undo: %s', [UndoDescription]);
        buttonRedo.Enabled := CanRedo;
        buttonPanel.OKButton.Enabled:=Modified;
    end;

end;

// --------------------------------------------------------------------------------
procedure TFileHexEditDialog.SetFileData(AFileData: TMemoryStream; AName: string);
begin
    Caption := AName;
    FHexEditor.LoadFromStream(AFileData);
end;

end.
