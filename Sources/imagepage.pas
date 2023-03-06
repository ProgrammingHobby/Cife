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
unit ImagePage;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, ComCtrls;

type

    { TImagePage }

    TImagePage = class(TTabSheet)
    public    // Attribute

    public    // Methoden
        procedure SetFile(ImageFile: string);
        procedure SetType(ImageType: string);
        function GetFile: string;
        function GetType: string;

    public  // Konstruktor/Destruktor
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;

    protected // Attribute

    protected // Methoden

    private   // Attribute
        m_ImageFile: string;
        m_ImageType: string;
        m_DirectoryList: TListView;

    private   // Methoden
        procedure CreateDirectoryListView;
        procedure DirectoryListResize(Sender: TObject);

    end;

implementation

{ TImagePage }

uses Controls, StdCtrls;

// --------------------------------------------------------------------------------
procedure TImagePage.SetFile(ImageFile: string);
begin
    m_ImageFile := ImageFile;
end;

// --------------------------------------------------------------------------------
procedure TImagePage.SetType(ImageType: string);
begin
    m_ImageType := ImageType;
end;

// --------------------------------------------------------------------------------
function TImagePage.GetFile: string;
begin
    Result := m_ImageFile;
end;

// --------------------------------------------------------------------------------
function TImagePage.GetType: string;
begin
    Result := m_ImageType;
end;

// --------------------------------------------------------------------------------
constructor TImagePage.Create(TheOwner: TComponent);
begin
    inherited Create(TheOwner);
    CreateDirectoryListView;
end;

// --------------------------------------------------------------------------------
destructor TImagePage.Destroy;
begin
    inherited Destroy;
end;

procedure TImagePage.CreateDirectoryListView;
var
    DirColumn: TListColumn;
begin
    m_DirectoryList := TListView.Create(self);
    with m_DirectoryList do begin
        Parent := self;
        Align := alClient;
        BorderStyle := bsSingle;
        ReadOnly := True;
        ScrollBars := ssAutoVertical;
        ViewStyle := vsReport;
        GridLines := True;
        ColumnClick := False;
        SortDirection := sdAscending;
        SortType := stText;
        BeginUpdate;
        DirColumn := Columns.Add;
        DirColumn.Caption := 'User : Name';
        DirColumn.Alignment := taLeftJustify;
        DirColumn := Columns.Add;
        DirColumn.Caption := 'Bytes';
        DirColumn.Alignment := taRightJustify;
        DirColumn := Columns.Add;
        DirColumn.Caption := 'Recs';
        DirColumn.Alignment := taRightJustify;
        DirColumn := Columns.Add;
        DirColumn.Caption := 'Attributes';
        DirColumn.Alignment := taCenter;
        DirColumn := Columns.Add;
        DirColumn.Caption := 'Protections';
        DirColumn.Alignment := taCenter;
        DirColumn := Columns.Add;
        DirColumn.Caption := 'Updated';
        DirColumn.Alignment := taCenter;
        DirColumn := Columns.Add;
        DirColumn.Caption := 'Created';
        DirColumn.Alignment := taCenter;
        DirColumn := Columns.Add;
        DirColumn.Caption := 'Last Access';
        DirColumn.Alignment := taCenter;
        EndUpdate;
        OnResize := @DirectoryListResize;
    end;
end;

// --------------------------------------------------------------------------------
procedure TImagePage.DirectoryListResize(Sender: TObject);
var
    NewWidth, ColWidths, ActListViewWidth: integer;
    dlv: TListView;
begin
    NewWidth := 0;
    ColWidths := 0;
    dlv := TListView(Sender);
    dlv.BeginUpdate;
{$ifdef Windows}
    ActListViewWidth := (ClientWidth - 4);
{$else}
    ActListViewWidth := (ClientWidth - 1);
{$endif}
    NewWidth := Round(ActListViewWidth * 0.151);
    ColWidths := ColWidths + NewWidth;
    dlv.Columns[0].Width := NewWidth;
    NewWidth := Round(ActListViewWidth * 0.076);
    ColWidths := ColWidths + NewWidth;
    dlv.Columns[1].Width := NewWidth;
    NewWidth := Round(ActListViewWidth * 0.076);
    ColWidths := ColWidths + NewWidth;
    dlv.Columns[2].Width := NewWidth;
    NewWidth := Round(ActListViewWidth * 0.107);
    ColWidths := ColWidths + NewWidth;
    dlv.Columns[3].Width := NewWidth;
    NewWidth := Round(ActListViewWidth * 0.107);
    ColWidths := ColWidths + NewWidth;
    dlv.Columns[4].Width := NewWidth;
    NewWidth := Round(ActListViewWidth * 0.16);
    ColWidths := ColWidths + NewWidth;
    dlv.Columns[5].Width := NewWidth;
    NewWidth := Round(ActListViewWidth * 0.16);
    ColWidths := ColWidths + NewWidth;
    dlv.Columns[6].Width := NewWidth;
    NewWidth := Round(ActListViewWidth * 0.16);
    ColWidths := ColWidths + NewWidth;
    dlv.Columns[7].Width := (NewWidth + (ActListViewWidth - Colwidths));
    dlv.EndUpdate;
end;

// --------------------------------------------------------------------------------
end.
