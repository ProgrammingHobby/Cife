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
unit Main_Window;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
    ActnList, ExtCtrls, StdCtrls, ImageFileHistory;

type

    { TMainWindow }

    TMainWindow = class(TForm)
        actionClearHistory: TAction;
        actionNew: TAction;
        actionDelete: TAction;
        actionCharacteristics: TAction;
        actionFormatCurrent: TAction;
        actionRefresh: TAction;
        actionCheckImage: TAction;
        actionSettings: TAction;
        actionAbout: TAction;
        actionOpen: TAction;
        actionClose: TAction;
        actionQuit: TAction;
        actionCut: TAction;
        actionCopy: TAction;
        actionPaste: TAction;
        actionSelectAll: TAction;
        actionRename: TAction;
        ActionList: TActionList;
        groupImageFile: TGroupBox;
        groupDirStatistics: TGroupBox;
        groupDriveData: TGroupBox;
        ImageList: TImageList;
        Label1: TLabel;
        Label10: TLabel;
        Label11: TLabel;
        Label12: TLabel;
        Label13: TLabel;
        Label14: TLabel;
        Label15: TLabel;
        Label16: TLabel;
        Label17: TLabel;
        labelType: TLabel;
        labelUsedDirEntries: TLabel;
        labelMaxDirEntries: TLabel;
        labelFilesFound: TLabel;
        labelTotalRecords: TLabel;
        labelTotal1kBlocks: TLabel;
        labelTotalBytes: TLabel;
        labelFile: TLabel;
        labelOs: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        Label9: TLabel;
        labelSkew: TLabel;
        labelOffset: TLabel;
        labelBoottracks: TLabel;
        labelMaxdir: TLabel;
        labelBlocksize: TLabel;
        labelSectorBytes: TLabel;
        labelSectors: TLabel;
        labelTracks: TLabel;
        MainMenu: TMainMenu;
        menuFile: TMenuItem;
        menuitemClearHistory: TMenuItem;
        menuitemRecentFiles: TMenuItem;
        PageControl: TPageControl;
        Panel1: TPanel;
        Panel2: TPanel;
        Panel3: TPanel;
        Panel4: TPanel;
        Panel5: TPanel;
        Panel6: TPanel;
        popupItemRefresh: TMenuItem;
        Separator10: TMenuItem;
        Separator8: TMenuItem;
        popupItemAttributes: TMenuItem;
        Separator9: TMenuItem;
        popupItemCheckImage: TMenuItem;
        Separator6: TMenuItem;
        popupItemCut: TMenuItem;
        popupItemCopy: TMenuItem;
        popupItemPaste: TMenuItem;
        popupItemSelectAll: TMenuItem;
        Separator7: TMenuItem;
        popupItemRename: TMenuItem;
        popupItemDelete: TMenuItem;
        menuItemCopy: TMenuItem;
        menuItemPaste: TMenuItem;
        menuItemSelectAll: TMenuItem;
        PopupMenu1: TPopupMenu;
        Separator3: TMenuItem;
        menuItemRename: TMenuItem;
        menuItemDelete: TMenuItem;
        Separator4: TMenuItem;
        menuItemCharacteristics: TMenuItem;
        Separator5: TMenuItem;
        menuItemNew: TMenuItem;
        menuItemFormatCurrent: TMenuItem;
        menuView: TMenuItem;
        menuItemRefresh: TMenuItem;
        menuTools: TMenuItem;
        menuItemCheckImage: TMenuItem;
        menuOptions: TMenuItem;
        menuSettings: TMenuItem;
        menuHelp: TMenuItem;
        menuItemAbout: TMenuItem;
        menuItemOpen: TMenuItem;
        Separator1: TMenuItem;
        menuItemClose: TMenuItem;
        Separator2: TMenuItem;
        menuItemQuit: TMenuItem;
        menuEdit: TMenuItem;
        menuItemCut: TMenuItem;
        StatusBar1: TStatusBar;
        procedure actionAboutExecute(Sender: TObject);
        procedure actionCharacteristicsExecute(Sender: TObject);
        procedure actionClearHistoryExecute(Sender: TObject);
        procedure actionCloseExecute(Sender: TObject);
        procedure actionNewExecute(Sender: TObject);
        procedure actionOpenExecute(Sender: TObject);
        procedure actionRenameExecute(Sender: TObject);
        procedure actionSettingsExecute(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
        procedure PageControlChange(Sender: TObject);
        procedure PageControlCloseTabClicked(Sender: TObject);
    private
        m_ImageFileHistory: TImageFileHistory;
        procedure AddImagePage(ImageFile: string; ImageType: string);
        procedure HistoryMenuItemClick(Sender: TObject);

    public

    end;

var
    MainWindow: TMainWindow;

implementation

{$R *.lfm}

uses NewImage_Dialog, File_Dialog, RenameFile_Dialog, Characteristics_Dialog, Settings_Dialog,
    About_Dialog, CifeGlobals, XMLSettings, ImagePage;

{ TMainWindow }

// --------------------------------------------------------------------------------
procedure TMainWindow.actionNewExecute(Sender: TObject);
var
    dialog: TNewImageDialog;
begin
    try
        dialog := TNewImageDialog.Create(self);
        dialog.ShowModal;
    finally
        FreeAndNil(dialog);
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionCharacteristicsExecute(Sender: TObject);
var
    dialog: TCharacteristicsDialog;
begin
    try
        dialog := TCharacteristicsDialog.Create(self);
        dialog.ShowModal;
    finally
        FreeAndNil(dialog);
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionClearHistoryExecute(Sender: TObject);
begin
    m_ImageFileHistory.Clear;
    actionClearHistory.Enabled := False;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionCloseExecute(Sender: TObject);
begin
    PageControl.ActivePage.Free;
    if (PageControl.PageCount <= 0) then begin
        actionClose.Enabled := False;
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionAboutExecute(Sender: TObject);
var
    dialog: TAboutDialog;
begin
    try
        dialog := TAboutDialog.Create(self);
        dialog.ShowModal;
    finally
        FreeAndNil(dialog);
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionOpenExecute(Sender: TObject);
var
    dialog: TFileDialog;
    ImageFile, ImageType: string;
begin
    try
        dialog := TFileDialog.Create(self);
        dialog.SetDialogTitle('Open CP/M Disk Image File');
        dialog.SetRootPath(GetUserDir);
        dialog.SetFileWildcards('Image Files (*.img,*.fdd,*.dsk)|*.img;*.IMG;*.fdd;*.FDD;*.dsk;*.DSK|all Files (*.*)|*');
        if (dialog.ShowModal = mrOk) then begin
            ImageFile := dialog.GetFullFileName;
            ImageType := dialog.GetImageType;
            AddImagePage(ImageFile, ImageType);
        end;
    finally
        FreeAndNil(dialog);
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionRenameExecute(Sender: TObject);
var
    dialog: TRenameFileDialog;
begin
    try
        dialog := TRenameFileDialog.Create(self);
        dialog.ShowModal;
    finally
        FreeAndNil(dialog);
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionSettingsExecute(Sender: TObject);
var
    dialog: TSettingsDialog;
begin
    try
        dialog := TSettingsDialog.Create(self);
        dialog.ShowModal;
    finally
        FreeAndNil(dialog);
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    m_ImageFileHistory.Save;
    FreeAndNil(m_ImageFileHistory);
    with TXMLSettings.Create(SettingsFile) do begin
        try
            SaveFormState(TForm(self));
        finally
            Free;
        end;
    end;
    CloseAction := caFree;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.FormShow(Sender: TObject);
var
    LabelWidth: integer;
begin

    LabelWidth := labelTotalBytes.Canvas.GetTextWidth('######');
    labelTotalBytes.Constraints.MinWidth := LabelWidth;
    labelTotalRecords.Constraints.MinWidth := LabelWidth;
    labelMaxDirEntries.Constraints.MinWidth := LabelWidth;

    LabelWidth := labelTracks.Canvas.GetTextWidth('######');
    labelTracks.Constraints.MinWidth := LabelWidth;

    SetAutoSize(True);
    Constraints.MinWidth := Width;
    Constraints.MinHeight := Height;
    SetAutoSize(False);

    with TXMLSettings.Create(SettingsFile) do begin
        try
            RestoreFormState(TForm(self));
        finally
            Free;
        end;
    end;
    actionClose.Enabled := False;
    m_ImageFileHistory := TImageFileHistory.Create(menuitemRecentFiles);
    m_ImageFileHistory.SetHistoryMenuItemsEvent(@HistoryMenuItemClick);
    actionClearHistory.Enabled := m_ImageFileHistory.Load;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.PageControlChange(Sender: TObject);
var
    ImagePage: TImagePage;
    PgCtrl: TPageControl;
begin
    if (Sender is TPageControl) then begin
        PgCtrl := TPageControl(Sender);
        ImagePage := TImagePage(PgCtrl.Pages[PgCtrl.PageIndex]);
        labelFile.Caption := ImagePage.GetFile;
        labelType.Caption := ImagePage.GetType;
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.PageControlCloseTabClicked(Sender: TObject);
begin
    if (Sender is TTabSheet) then begin
        TTabSheet(Sender).Free;
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.AddImagePage(ImageFile: string; ImageType: string);
var
    ImagePage: TImagePage;
begin
    ImagePage := TImagePage.Create(PageControl);
    ImagePage.PageControl := PageControl;
    ImagePage.Caption := ExtractFileName(ImageFile);
    ImagePage.SetFile(ImageFile);
    ImagePage.SetType(ImageType);
    labelFile.Caption := ImageFile;
    labelType.Caption := ImageType;
    PageControl.ActivePage := ImagePage;
    if (PageControl.PageCount > 0) then begin
        actionClose.Enabled := True;
        actionClearHistory.Enabled := True;
    end;
    m_ImageFileHistory.AddItem(ImageFile, ImageType);
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.HistoryMenuItemClick(Sender: TObject);
var
    HistoryMenuItem: TMenuItem;
    HistoryEntry: THistoryEntry;
    Index: integer;
    TabExisting: boolean;
    ImagePage: TImagePage;
begin
    if (Sender is TMenuItem) then begin
        HistoryMenuItem := TMenuItem(Sender);
        HistoryEntry := m_ImageFileHistory.GetHistoryEntry(HistoryMenuItem.Tag);
        TabExisting := False;
        for Index := 0 to (PageControl.PageCount - 1) do begin
            ImagePage := TImagePage(PageControl.Pages[Index]);
            if (ImagePage.GetFile = HistoryEntry.FileName) then begin
                TabExisting := True;
            end;
        end;
        if not TabExisting then begin
            AddImagePage(HistoryEntry.FileName, HistoryEntry.FileType);
        end;
    end;
end;

// --------------------------------------------------------------------------------
end.
