{*
 *  Copyright (C) 2023  Uwe Merker
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
unit ImageFileHistory;

{$mode ObjFPC}
{$H+}

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
        THistoryMenuItemClick = procedure (ASender: TObject) of object;

    public    // Methoden
        procedure Clear;
        procedure SetHistoryMenuItemsEvent(AHmiEvent: THistoryMenuItemClick);
        procedure AddItem(AImageFile: string; AImageType: string);
        procedure DeleteItem(AItem: integer);
        function GetHistoryEntry(AItem: integer): THistoryEntry;
        function Load: boolean;
        function Save: boolean;
    public  // Konstruktor/Destruktor
        constructor Create(ARecentMenu: TMenuItem); overload;
        destructor Destroy; override;

    protected // Attribute

    protected // Methoden

    private   // Attribute
    type
        THistoryArray = array of THistoryEntry;

    const
        MAXITEMS = 10;

    var
        FRecentMenu: TMenuItem;
        FHistoryArray: THistoryArray;
        FHistoryItemsCount: integer;
        FHistoryMenuItemEvent: THistoryMenuItemClick;

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

    for Index := Low(FHistoryArray) to High(FHistoryArray) do begin
        FHistoryArray[Index].FileType := '';
        FHistoryArray[Index].FileName := '';
    end;

    FHistoryItemsCount := 0;
    UpdateRecentMenu;
end;

// --------------------------------------------------------------------------------
procedure TImageFileHistory.SetHistoryMenuItemsEvent(AHmiEvent: THistoryMenuItemClick);
begin
    FHistoryMenuItemEvent := AHmiEvent;
end;

// --------------------------------------------------------------------------------
procedure TImageFileHistory.AddItem(AImageFile: string; AImageType: string);
var
    Index: integer;
    Entry: THistoryEntry;
begin

    // check if the given Image is already in History
    for Index := Low(FHistoryArray) to (FHistoryItemsCount - 1) do begin

        if ((AImageFile = FHistoryArray[Index].FileName) and (AImageType = FHistoryArray[Index].FileType)) then begin
            DeleteItem(Index);
            Dec(FHistoryItemsCount);
            break;
        end;

    end;

    // if History full, delete last item
    if (FHistoryItemsCount = MAXITEMS) then begin
        Dec(FHistoryItemsCount);
        DeleteItem(FHistoryItemsCount);
    end;

    Inc(FHistoryItemsCount);

    Entry.FileName := AImageFile;
    Entry.FileType := AImageType;

    for Index := MAXITEMS - 2 downto 0 do begin
        FHistoryArray[Index + 1] := FHistoryArray[Index];
    end;

    FHistoryArray[0] := Entry;
    UpdateRecentMenu;
end;

// --------------------------------------------------------------------------------
procedure TImageFileHistory.DeleteItem(AItem: integer);
var
    Index: integer;
begin

    for Index := AItem to (MAXITEMS - 2) do begin
        FHistoryArray[Index] := FHistoryArray[Index + 1];
    end;

    FHistoryArray[MAXITEMS - 1].FileName := '';
    FHistoryArray[MAXITEMS - 1].FileType := '';
    Dec(FHistoryItemsCount);
    UpdateRecentMenu;
end;

// --------------------------------------------------------------------------------
function TImageFileHistory.GetHistoryEntry(AItem: integer): THistoryEntry;
var
    HistoryEntry: THistoryEntry;
begin
    HistoryEntry.FileName := '';
    HistoryEntry.FileType := '';

    if ((AItem >= 0) and (AItem < MAXITEMS)) then begin
        HistoryEntry := FHistoryArray[AItem];
    end;

    Result := HistoryEntry;
end;

// --------------------------------------------------------------------------------
function TImageFileHistory.Load: boolean;
var
    Index: integer;
begin
    Result := False;

    for Index := Low(FHistoryArray) to High(FHistoryArray) do begin
        FHistoryArray[Index].FileType := '';
        FHistoryArray[Index].FileName := '';
    end;

    with TXMLSettings.Create(SettingsFile) do begin

        try
            OpenKey('History');
            FHistoryItemsCount := GetAttribute('Count', 0);

            for Index := 0 to (FHistoryItemsCount - 1) do begin
                OpenKey('Item' + IntToStr(Index));
                FHistoryArray[Index].FileName := GetValue('File', '');
                FHistoryArray[Index].FileType := GetValue('Type', '');
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
            SetAttribute('Count', FHistoryItemsCount);

            for Index := 0 to (MAXITEMS - 1) do begin
                DeletePath('Item' + IntToStr(Index));
            end;

            for Index := 0 to (FHistoryItemsCount - 1) do begin
                OpenKey('Item' + IntToStr(Index));
                SetValue('File', FHistoryArray[Index].FileName);
                SetValue('Type', FHistoryArray[Index].FileType);
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
constructor TImageFileHistory.Create(ARecentMenu: TMenuItem);
begin
    FRecentMenu := ARecentMenu;
    SetLength(FHistoryArray, MAXITEMS);
    FHistoryItemsCount := 0;
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

    while (Index < FRecentMenu.Count) do begin
        if not (FRecentMenu.Items[Index].Caption = '-') and not (FRecentMenu.Items[Index].Caption = 'Clear History') then begin
            FRecentMenu.Items[Index].Free;
        end
        else begin
            Inc(Index);
        end;
    end;

    // create new History Menuitems
    for Index := 0 to FHistoryItemsCount - 1 do begin
        NewMenuItem := TMenuItem.Create(FRecentMenu);
        NewMenuItem.Tag := Index;
        NewMenuItem.Caption := IntToStr(Index + 1) + '  ' + ExtractFileName(FHistoryArray[Index].FileName) +
            ' (' + FHistoryArray[Index].FileType + ')';
        NewMenuItem.OnClick := FHistoryMenuItemEvent;
        FRecentMenu.Insert(Index, NewMenuItem);
    end;
end;

// --------------------------------------------------------------------------------
end.
