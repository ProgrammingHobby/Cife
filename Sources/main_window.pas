unit main_window;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ActnList, StdCtrls;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    actionNew: TAction;
    actionDelete: TAction;
    actionAttributes: TAction;
    actionProtections: TAction;
    actionFormatCurrent: TAction;
    actionRefresh: TAction;
    actionCheckImage: TAction;
    actionCopySettings: TAction;
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
    ActionList1: TActionList;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    menuFile: TMenuItem;
    menuItemCopy: TMenuItem;
    menuItemPaste: TMenuItem;
    menuItemSelectAll: TMenuItem;
    Separator3: TMenuItem;
    menuItemRename: TMenuItem;
    menuItemDelete: TMenuItem;
    Separator4: TMenuItem;
    menuItemAttributes: TMenuItem;
    menuItemProtections: TMenuItem;
    Separator5: TMenuItem;
    menuItemNew: TMenuItem;
    menuItemFormatCurrent: TMenuItem;
    menuView: TMenuItem;
    menuItemRefresh: TMenuItem;
    menuTools: TMenuItem;
    menuItemCheckImage: TMenuItem;
    menuOptions: TMenuItem;
    menuCopySettings: TMenuItem;
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
  private

  public

  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}

end.

