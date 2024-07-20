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
unit DiskdefsParameter_Dialog;

{$mode ObjFPC}
{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ComCtrls,
    StdCtrls;

type

    { TDiskdefsParameterDialog }

    TDiskdefsParameterDialog = class(TForm)
        ButtonPanel: TButtonPanel;
        Memo1: TMemo;
        Memo2: TMemo;
        Memo3: TMemo;
        PageControl1: TPageControl;
        TabSheet1: TTabSheet;
        TabSheet2: TTabSheet;
        TabSheet3: TTabSheet;
        procedure FormShow(Sender: TObject);
    private

    public

    end;

var
    DiskdefsParameterDialog: TDiskdefsParameterDialog;

implementation

{$R *.lfm}

{ TDiskdefsParameterDialog }

// --------------------------------------------------------------------------------
procedure TDiskdefsParameterDialog.FormShow(Sender: TObject);
var
    CalcWidth, CalcHeight: integer;
begin
    {$ifdef UNIX}
    Memo1.Font.Name := 'Liberation Mono';
    Memo2.Font.Name := 'Liberation Mono';
    Memo3.Font.Name := 'Liberation Mono';
    CalcWidth := Memo1.font.GetTextWidth('#') * 45;
    CalcHeight := Memo1.Font.GetTextHeight('#') * 29;
    {$endif}

    {$ifdef Windows}
    Memo1.Font.Name := 'Consolas';
    Memo2.Font.Name := 'Consolas';
    Memo3.Font.Name := 'Consolas';
    CalcWidth := Memo1.font.GetTextWidth('#') * 40;
    CalcHeight := Memo1.Font.GetTextHeight('#') * 24;
    {$endif}

    Width := CalcWidth;
    Height := CalcHeight;
    Constraints.MinWidth := CalcWidth;
    Constraints.MaxWidth := CalcWidth;
    Constraints.MinHeight := CalcHeight;
end;

end.
