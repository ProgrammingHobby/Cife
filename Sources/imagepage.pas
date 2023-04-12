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
    Classes, SysUtils, ComCtrls, CpmTools, CifeGlobals;

type

    { TImagePage }

    TImagePage = class(TTabSheet)
    public    // Attribute
        type
            TFileSystemInfoCB = procedure (const Info:TFileSystemInfo) of object;
            TDirectoryStatisticsCB = procedure (const Statistics:TDirStatistics) of object;

    public    // Methoden
        procedure DoShow; override;
        procedure SetFileSystemInfoCallBack(AFileSystemInfoCB: TFileSystemInfoCB);
        function Open(const AFileName: string; const AFileType: string; AUpperCase: boolean = False): boolean;
        function GetFileName: string;

    public  // Konstruktor/Destruktor
        constructor Create(ATheOwner: TComponent); override;
        destructor Destroy; override;

    protected // Attribute

    protected // Methoden

    private   // Attribute
        FDirectoryList: TListView;
        FCpmTools: TCpmTools;
        FFileSystemInfoCallBack: TFileSystemInfoCB;
        FDirStatisticsCallBack: TDirectoryStatisticsCB;

    private   // Methoden
        procedure CreateDirectoryListView;
        procedure DirectoryListResize(ASender: TObject);
        procedure ClearFileSystemInfo;

    end;

implementation

{ TImagePage }

uses Controls, StdCtrls;

// --------------------------------------------------------------------------------
procedure TImagePage.DoShow;
var
    Info: TFileSystemInfo;
begin
    inherited DoShow;
    Info := FCpmTools.GetFileSystemInfo;
    if Assigned(FFileSystemInfoCallBack) then begin
        FFileSystemInfoCallBack(Info);
    end;
end;

// --------------------------------------------------------------------------------
procedure TImagePage.SetFileSystemInfoCallBack(AFileSystemInfoCB: TFileSystemInfoCB);
begin
    FFileSystemInfoCallBack := AFileSystemInfoCB;
end;

// --------------------------------------------------------------------------------
function TImagePage.Open(const AFileName: string; const AFileType: string; AUpperCase: boolean): boolean;
begin
    Result := FCpmTools.OpenImage(AFileName, AFileType, AUpperCase);
end;

// --------------------------------------------------------------------------------
function TImagePage.GetFileName: string;
begin
    Result := FCpmTools.GetFileSystemInfo.FileName;
end;

// --------------------------------------------------------------------------------
constructor TImagePage.Create(ATheOwner: TComponent);
begin
    inherited Create(ATheOwner);
    CreateDirectoryListView;
    FCpmTools := TCpmTools.Create;
end;

// --------------------------------------------------------------------------------
destructor TImagePage.Destroy;
begin
    FCpmTools.CloseImage;
    ClearFileSystemInfo;
    FreeAndNil(FCpmTools);
    inherited Destroy;
end;

procedure TImagePage.CreateDirectoryListView;
var
    DirColumn: TListColumn;
begin
    FDirectoryList := TListView.Create(self);
    with FDirectoryList do begin
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
procedure TImagePage.DirectoryListResize(ASender: TObject);
var
    NewWidth, ColWidths, ActListViewWidth: integer;
    dlv: TListView;
begin
    NewWidth := 0;
    ColWidths := 0;
    dlv := TListView(ASender);
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
procedure TImagePage.ClearFileSystemInfo;
var
    Info: TFileSystemInfo;
begin
    with Info do begin
        FileName := EmptyStr;
        FileType := EmptyStr;
        Tracks := EmptyStr;
        Sectors := EmptyStr;
        SecBytes := EmptyStr;
        BlockSize := EmptyStr;
        MaxDir := EmptyStr;
        BootSectors := EmptyStr;
        Offset := EmptyStr;
        skew := EmptyStr;
        System := EmptyStr;
    end;
    if Assigned(FFileSystemInfoCallBack) then begin
        FFileSystemInfoCallBack(Info);
    end;
end;

// --------------------------------------------------------------------------------
end.
