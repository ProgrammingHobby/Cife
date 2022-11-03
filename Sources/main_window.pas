unit main_window;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    MainMenu: TMainMenu;
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
    menuItemCreateNew: TMenuItem;
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
    statusbarMainWindow: TStatusBar;
  private

  public

  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}

end.

