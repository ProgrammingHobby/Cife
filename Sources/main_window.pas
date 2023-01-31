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
unit main_window;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ActnList;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    actionNew: TAction;
    actionDelete: TAction;
    actionCharacteristics: TAction;
    actionFormatCurrent: TAction;
    actionRefresh: TAction;
    actionCheckImage: TAction;
    actionSettings: TAction;
    actionAbout: TAction;
    actionClearMessages: TAction;
    actionSaveMessages: TAction;
    actionOpen: TAction;
    actionClose: TAction;
    actionQuit: TAction;
    actionCut: TAction;
    actionCopy: TAction;
    actionPaste: TAction;
    actionSelectAll: TAction;
    actionRename: TAction;
    ActionList: TActionList;
    ImageList: TImageList;
    listDirectory: TListView;
    MainMenu: TMainMenu;
    menuFile: TMenuItem;
    popupItemRefresh: TMenuItem;
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
    tabDirectory: TTabControl;
  private

  public

  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}

end.

