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
unit About_Dialog;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
    ButtonPanel;

type

    { TAboutDialog }

    TAboutDialog = class(TForm)
        ButtonPanel1: TButtonPanel;
        Image1: TImage;
        Label1: TLabel;
        labelIconHyperlink: TLabel;
        labelDeveloper: TLabel;
        labelHyperlink: TLabel;
        labelToolsCopyright: TLabel;
        labelSystem: TLabel;
        labelVersion: TLabel;
        labelTitle: TLabel;
        Panel1: TPanel;
        Panel2: TPanel;
        Panel3: TPanel;
        Panel4: TPanel;
        Panel5: TPanel;
        Panel6: TPanel;
        Panel7: TPanel;
        Panel8: TPanel;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
        procedure labelHyperlinkDblClick(Sender: TObject);
        procedure labelHyperlinkMouseEnter(Sender: TObject);
        procedure labelHyperlinkMouseLeave(Sender: TObject);
    private

    public

    end;

var
    AboutDialog: TAboutDialog;

implementation

{$R *.lfm}

uses VersionInfo, LCLIntf;

{ TAboutDialog }

// --------------------------------------------------------------------------------
procedure TAboutDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    CloseAction := caFree;
end;

// --------------------------------------------------------------------------------
procedure TAboutDialog.FormShow(Sender: TObject);
begin
    labelVersion.Caption := GetFileVersion;
    labelSystem.Caption := 'Written with Lazarus / Free Pascal' + ^M + ^J + GetLCLVersion + '   &&   ' +
        GetCompilerInfo + '  ' + IntToStr(SizeOf(PtrUInt) * 8) + 'bit';
    Constraints.MinWidth := Width;
    Constraints.MaxWidth := Width;
    Constraints.MinHeight := Height;
    Constraints.MaxHeight := Height;
end;

// --------------------------------------------------------------------------------
procedure TAboutDialog.labelHyperlinkDblClick(Sender: TObject);
begin
    OpenURL((Sender as TLabel).Caption);
end;

// --------------------------------------------------------------------------------
procedure TAboutDialog.labelHyperlinkMouseEnter(Sender: TObject);
begin
    (Sender as TLabel).Cursor := crHandPoint;
end;

// --------------------------------------------------------------------------------
procedure TAboutDialog.labelHyperlinkMouseLeave(Sender: TObject);
begin
    (Sender as TLabel).Cursor := crDefault;
end;

// --------------------------------------------------------------------------------
end.
