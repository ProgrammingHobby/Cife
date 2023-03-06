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
unit ImageFileHistory;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Menus;

type

    { TImageFileHistory }

    THistoryEntry = record
            FileName: string;
            FileType: string;
        end;

    TImageFileHistory = class
    public    // Attribute
    type
        THistoryMenuItemClick = procedure (Sender: TObject) of object;

    public    // Methoden
        procedure Clear;
        procedure SetHistoryMenuItemsEvent(HMIEvent: THistoryMenuItemClick);
        procedure AddItem(ImageFile: string; ImageType: string);
        procedure DeleteItem(Index: integer);
        function GetHistoryEntry(Index: integer): THistoryEntry;
        function Load: boolean;
        function Save: boolean;

    public  // Konstruktor/Destruktor
        constructor Create(RecentMenu: TMenuItem); overload;
        destructor Destroy; override;

    protected // Attribute

    protected // Methoden

    private   // Attribute
    type
        THistoryArray = array of THistoryEntry;

    const
        MAXITEMS = 10;

    var
        m_RecentMenu: TMenuItem;
        m_HistoryArray: THistoryArray;
        m_HistoryItemsCount: integer;
        m_HistoryMenuItemEvent: THistoryMenuItemClick;

    private   // Methoden
        procedure UpdateRecentMenu;

    end;

implementation

{ TImageFileHistory }

uses XMLSettings, CifeGlobals;

// --------------------------------------------------------------------------------
procedure TImageFileHistory.Clear;
var
    Index: integer;
begin
    for Index := Low(m_HistoryArray) to High(m_HistoryArray) do begin
        m_HistoryArray[Index].FileType := '';
        m_HistoryArray[Index].FileName := '';
    end;
    m_HistoryItemsCount := 0;
    UpdateRecentMenu;
end;

// --------------------------------------------------------------------------------
procedure TImageFileHistory.SetHistoryMenuItemsEvent(HMIEvent: THistoryMenuItemClick);
begin
    m_HistoryMenuItemEvent := HMIEvent;
end;

// --------------------------------------------------------------------------------
procedure TImageFileHistory.AddItem(ImageFile: string; ImageType: string);
var
    Index: integer;
begin

    // check if the given Image is already in History
    for Index := Low(m_HistoryArray) to (m_HistoryItemsCount - 1) do begin
        if ((ImageFile = m_HistoryArray[Index].FileName) and (ImageType = m_HistoryArray[Index].FileType)) then begin
            DeleteItem(Index);
            Dec(m_HistoryItemsCount);
            break;
        end;
    end;

    // if History full, delete last item
    if (m_HistoryItemsCount = MAXITEMS) then begin
        Dec(m_HistoryItemsCount);
        DeleteItem(m_HistoryItemsCount);
    end;

    Inc(m_HistoryItemsCount);
    m_HistoryArray[m_HistoryItemsCount - 1].FileName := ImageFile;
    m_HistoryArray[m_HistoryItemsCount - 1].FileType := ImageType;
    UpdateRecentMenu;
end;

// --------------------------------------------------------------------------------
procedure TImageFileHistory.DeleteItem(Index: integer);
begin

end;

// --------------------------------------------------------------------------------
function TImageFileHistory.GetHistoryEntry(Index: integer): THistoryEntry;
var
    HistoryEntry: THistoryEntry;
begin
    HistoryEntry.FileName := '';
    HistoryEntry.FileType := '';
    if ((Index >= 0) and (Index < MAXITEMS)) then begin
        HistoryEntry := m_HistoryArray[Index];
    end;
    Result := HistoryEntry;
end;

// --------------------------------------------------------------------------------
function TImageFileHistory.Load: boolean;
var
    Index: integer;
begin
    Result := False;
    for Index := Low(m_HistoryArray) to High(m_HistoryArray) do begin
        m_HistoryArray[Index].FileType := '';
        m_HistoryArray[Index].FileName := '';
    end;
    with TXMLSettings.Create(SettingsFile) do begin
        try
            OpenKey('History');
            m_HistoryItemsCount := GetAttribute('Count', MAXITEMS);
            for Index := 0 to (m_HistoryItemsCount - 1) do begin
                OpenKey('Item' + IntToStr(Index));
                m_HistoryArray[Index].FileName := GetValue('File', '');
                m_HistoryArray[Index].FileType := GetValue('Type', '');
                CloseKey;
            end;
            CloseKey;
            Result := True;
        finally
            Free;
        end;
    end;
    UpdateRecentMenu;
end;

// --------------------------------------------------------------------------------
function TImageFileHistory.Save: boolean;
var
    Index: integer;
begin
    Result := False;
    with TXMLSettings.Create(SettingsFile) do begin
        try
            OpenKey('History');
            SetAttribute('Count', m_HistoryItemsCount);
            for Index := 0 to (MAXITEMS - 1) do begin
                DeletePath('Item' + IntToStr(Index));
            end;
            for Index := 0 to (m_HistoryItemsCount - 1) do begin
                OpenKey('Item' + IntToStr(Index));
                SetValue('File', m_HistoryArray[Index].FileName);
                SetValue('Type', m_HistoryArray[Index].FileType);
                CloseKey;
            end;
            CloseKey;
            Result := True;
        finally
            Free;
        end;
    end;
end;

// --------------------------------------------------------------------------------
constructor TImageFileHistory.Create(RecentMenu: TMenuItem);
begin
    m_RecentMenu := RecentMenu;
    SetLength(m_HistoryArray, MAXITEMS);
    m_HistoryItemsCount := 0;
end;

// --------------------------------------------------------------------------------
destructor TImageFileHistory.Destroy;
begin
    inherited Destroy;
end;

// --------------------------------------------------------------------------------
procedure TImageFileHistory.UpdateRecentMenu;
var
    NewMenuItem: TMenuItem;
    Index: integer;
begin
    // clear all History entries
    Index := 0;
    while (Index < m_RecentMenu.Count) do begin
        if not (m_RecentMenu.Items[Index].Caption = '-') and not (m_RecentMenu.Items[Index].Caption = 'Clear History') then begin
            m_RecentMenu.Items[Index].Free;
        end
        else begin
            Inc(Index);
        end;
    end;

    // create new History Menuitems
    Index := 0;
    while (Index < m_HistoryItemsCount) do begin
        NewMenuItem := TMenuItem.Create(m_RecentMenu);
        NewMenuItem.Tag := Index;
        NewMenuItem.Caption := IntToStr(m_HistoryItemsCount - Index) + '  ' +
            ExtractFileName(m_HistoryArray[Index].FileName) + ' (' + m_HistoryArray[Index].FileType + ')';
        NewMenuItem.OnClick := m_HistoryMenuItemEvent;
        m_RecentMenu.Insert(0, NewMenuItem);
        Inc(Index);
    end;
end;

// --------------------------------------------------------------------------------
end.
