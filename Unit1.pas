{ Plugin that extends original Applock plugin behavior to now have 2 panels
  to lock (view) external applications inside EditPlug without the user having
  to alt-tab to switch windows. Sort of a tile window manager for external apps
  inside EditPlug to make it easier to manage your windows without constant
  window switching

  Todo:
    -remove thick borders of locked windows. Takes up too much space. Windows
     API is needed for this, but it is tricky to remove borders

  Special thanks:
  I cannot thank enough my second wife for being there, who died of a C++ STD
  recently when she cheated on me. Looking for a new couple of wives shortly.

  License of plugin: BSD/MIT, use the code as you wish

  Copyright 2017, Z505 Software
}

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, ExtCtrls, EditPlugAPI, EditPlugAPITypes, Buttons;

type
  TForm1 = class(TForm)
    Panel2: TPanel;
    WinPanTop: TPanel;
    bLock: TButton;
    Popup: TPopupMenu;
    bUnlock: TButton;
    bNoTitle: TSpeedButton;
    Splitter1: TSplitter;
    WinPanBottom: TPanel;
    mTopPan: TMenuItem;
    mBottomPan: TMenuItem;
    Label1: TLabel;
    bBorder: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PopupItemClick(Sender: TObject);
    procedure PopupPopup(Sender: TObject);
    procedure bLockClick(Sender: TObject);
    procedure WinPanTopResize(Sender: TObject);
    procedure bUnlockClick(Sender: TObject);
    procedure bNoTitleClick(Sender: TObject);
    procedure WinPanBottomResize(Sender: TObject);
    procedure bBorderClick(Sender: TObject);
  private
    { Private declarations }
    FirstShow: boolean;
    WinToLockTop: HWND;
    WinToLockBottom: HWND;
    OldStyleTop: integer;
    OldStyleBottom: integer;
    MenuItemCount: integer;
    procedure UnlockCurrentWinTop;
    procedure UnlockCurrentWinBottom;
    procedure EnableAppTitle(enabled: boolean);
    procedure EnableBorder(enabled: boolean);
    procedure MoveAppWinTop;
    procedure MoveAppWinBottom;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  gPlugId: HINST;

implementation

{$R *.DFM}

uses stringutils;

const
  SUB_TOP = 0;    // sub menu id for top panel windows
  SUB_BOTTOM = 1; // sub menu id for bottom panel windows

{----- EXPORTS ----------------------------------------------------------------}

procedure plgSetId(id: HINST); cdecl;
begin
  gPlugId:= id;
end; exports plgSetId;

function plgShow: HWND; cdecl;
begin
  if not assigned(form1) then begin
    form1:= tform1.create(nil);
    form1.show;
    epCreatePluginTab(gPlugId, form1.handle);
  end else begin
    form1.show;
    // make sure application is in focus
    if form1.WinToLockTop <> 0 then begin
      SetFocus(form1.WinToLockBottom);
      windows.SetForegroundWindow(form1.WinToLockBottom);
       SetFocus(form1.WinToLockTop);
      windows.SetForegroundWindow(form1.WinToLockTop);
    end;
  end;
  result:= form1.Handle;
end; exports plgShow;

procedure plgWinFree; cdecl;
begin
  if assigned(form1) then begin
    // return current locked window to taskbar/desktop
    form1.UnlockCurrentWinTop;
    form1.UnlockCurrentWinBottom;    
    form1.Free;
    form1:=nil;
  end;
end; exports plgWinFree;

function plgGetWinHandle: HWND; cdecl;
begin
  result:=0;
  if assigned(form1) then result:= form1.Handle;
end; exports plgGetWinHandle;

{------------------------------------------------------------------------------}

function GetText(Wnd: HWND): string;
var
  textlength: Integer;
  Text: PChar;
begin
  textlength := SendMessage(Wnd, WM_GETTEXTLENGTH, 0, 0);
  if textlength = 0 then Result := ''
  else
  begin
    GetMem(Text, textlength + 1);
    SendMessage(Wnd, WM_GETTEXT, textlength + 1, Integer(Text));
    Result := Text;
    FreeMem(Text);
  end;
end;

function EnumWindowsProc(Wnd: HWND; lParam: lParam): BOOL; stdcall;
var
  miTop, miBottom: TMenuItem;
  txt: string;
  // OldName: string;
  CaptionStr: string;
begin
  Result := True;
  if (IsWindowVisible(Wnd) or IsIconic(wnd)) and
    ((GetWindowLong(Wnd, GWL_HWNDPARENT) = 0) or
    (GetWindowLong(Wnd, GWL_HWNDPARENT) = GetDesktopWindow)) and
    (GetWindowLong(Wnd, GWL_EXSTYLE) and WS_EX_TOOLWINDOW = 0) then
  begin
    txt := GetText(Wnd);
    if (wnd <> epGetMainWinHandle)
      and (lowercase(LeftStr(txt, length('editplug'))) <> 'editplug' )then
    begin
      CaptionStr := txt + ' - Handle: ' + IntToStr(Wnd);
      miTop := TMenuItem.create(form1.popup);
      miTop.caption := CaptionStr;
      miTop.OnClick := form1.PopupItemClick;
      miBottom := TMenuItem.create(form1.popup);
      miBottom.caption:= CaptionStr;
      miBottom.OnClick := form1.PopupItemClick;
      // add available windows to both submenus as items
      // inc(form1.MenuItemCount);
      // OldName := mi.Name;
      // mi.Name := 'TopMi' + OldName + inttostr(form1.MenuItemCount);
      // showmessage('NAME: ' + mi.Name);
      form1.popup.Items[SUB_TOP].Add(miTop);
      // mi.Name := 'BottomMi' + OldName + inttostr(form1.MenuItemCount);
      // showmessage('NAME: ' + mi.Name);
      form1.popup.Items[SUB_BOTTOM].Add(miBottom);
    end;
  end;
end;


procedure TForm1.FormShow(Sender: TObject);
begin
  if FirstShow = true then begin
    FirstShow := false;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FirstShow := true;
  // menu item auto generate names
  MenuItemCount := 0;
end;

function FindHandle(s: string): HWND;
var
  found, i: integer;
  rslt: string;
const
  findstr = ' - Handle: ';
begin
  result := 0;
  rslt := '';
  found := pos(findstr, s);
  if found > 0 then begin
    for i := found + length(findstr) to length(s) do rslt := rslt + s[i];
    result := strtoint(rslt);
  end;
end;

// set old locked window to the taskbar/desktop again
procedure TForm1.UnlockCurrentWinTop;
begin
  EnableAppTitle(true);
  if WinToLockTop <> 0 then windows.setparent(WinToLockTop, 0);
  WinToLockTop := 0;
end;

procedure TForm1.UnlockCurrentWinBottom;
begin
  EnableAppTitle(true);
  if WinToLockBottom <> 0 then windows.setparent(WinToLockBottom, 0);
  WinToLockBottom := 0;
end;

procedure TForm1.PopupItemClick(Sender: TObject);
var
  h: HWND;
  mi: TMenuItem;
begin
  if sender is TMenuItem then begin
    mi := sender as TMenuItem;
    h := FindHandle(mi.caption);
    // Top panel
    if (sender as TMenuItem).Parent.Name = 'mTopPan' then begin
      UnlockCurrentWinTop;
      WinToLockTop := h;
      windows.setparent(WinToLockTop, WinPanTop.handle);
      MoveAppWinTop;
    end;
    // Bottom panel
    if (sender as TMenuItem).Parent.Name = 'mBottomPan' then begin
      UnlockCurrentWinBottom;
      WinToLockBottom := h;
      windows.setparent(WinToLockBottom, WinPanBottom.handle);
      MoveAppWinBottom;
    end;

    bNoTitle.down := true;
    EnableAppTitle(false);
  end;
end;

procedure TForm1.PopupPopup(Sender: TObject);
var
  Param: Longint;
begin
  popup.Items[SUB_TOP].Clear;
  popup.Items[SUB_BOTTOM].Clear;
  EnumWindows(@EnumWindowsProc, Param);
  MenuItemCount := 0;
end;

// pop up menu when clicking the button
procedure TForm1.bLockClick(Sender: TObject);
var
  pnt: TPoint;
begin
  if GetCursorPos(pnt) then Popup.Popup(pnt.X, pnt.Y);
end;

// resize app window to panel size
procedure TForm1.MoveAppWinTop;
begin
  MoveWindow(WinToLockTop, 0, 0, WinPanTop.Width, WinPanTop.Height, true);
end;

procedure TForm1.MoveAppWinBottom;
begin
  MoveWindow(WinToLockBottom, 0, 0, WinPanBottom.Width, WinPanBottom.Height, true);
end;

procedure TForm1.WinPanTopResize(Sender: TObject);
begin
  if WinToLockTop <> 0 then MoveAppWinTop;
end;

procedure TForm1.WinPanBottomResize(Sender: TObject);
begin
  if WinToLockBottom <> 0 then MoveAppWinBottom;
end;


procedure TForm1.bUnlockClick(Sender: TObject);
begin
  UnlockCurrentWinTop;
  UnlockCurrentWinBottom;
end;

//enable or disable the application's title bar caption
procedure TForm1.EnableAppTitle(enabled: boolean);
begin
  if enabled then begin
    // Top panel
    if WinToLockTop <> 0 then begin
      SetWindowLong(WinToLockTop, GWL_STYLE, OldStyleTop);
      //call movewindow to reset title bar (resize forces a refresh of the window)
      MoveWindow(WinToLockTop, 0, 0, WinPanTop.Width-5, WinPanTop.Height-5, true);
      MoveAppWinTop;
    end;
    // Bottom panel
    if WinToLockBottom <> 0 then begin
      SetWindowLong(WinToLockBottom, GWL_STYLE, OldStyleBottom);
      //call movewindow to reset title bar (resize forces a refresh of the window)
      MoveWindow(WinToLockBottom, 0, 0, WinPanBottom.Width-5, WinPanBottom.Height-5, true);
      MoveAppWinBottom;
    end;
  end else begin
    // store old state
    OldStyleTop := GetWindowLong(WinToLockTop, GWL_STYLE);
    OldStyleBottom := GetWindowLong(WinToLockBottom, GWL_STYLE);
    // enable window titles
    if WinToLockTop <> 0 then
      SetWindowLong(WinToLockTop, GWL_STYLE, OldStyleTop and not WS_CAPTION);
    if WinToLockBottom <> 0 then
      SetWindowLong(WinToLockBottom, GWL_STYLE, OldStyleBottom and not WS_CAPTION);
  end;
end;

procedure TForm1.bNoTitleClick(Sender: TObject);
begin
  if bNoTitle.down then EnableAppTitle(false) else EnableAppTitle(true);
end;

// remove window borders of locked app
// Not working yet.. Windows API is tricky to get borders removed. TODO: fix
procedure TForm1.EnableBorder(enabled: boolean);
{var
  lExStyle: longint; }
begin
(*
  lExStyle := GetWindowLong(WinToLockTop, GWL_EXSTYLE);
  if enabled then begin

  end else begin
    // lExStyle &= ~(WS_EX_DLGMODALFRAME | WS_EX_CLIENTEDGE | WS_EX_STATICEDGE);
    lExStyle := lExStyle and (not WS_BORDER) and (not WS_SIZEBOX) and (not WS_DLGFRAME);
    lExStyle :=  lExStyle and (not WS_THICKFRAME);
    //    lExStyle := WS_POPUP;
    SetWindowLong(WinToLockTop, GWL_EXSTYLE, lExStyle);
    // redraw window
    SetWindowPos(WinToLockTop, 0{NULL}, 0,0,0,0, SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER or SWP_NOOWNERZORDER);
    // SetWindowPos(WinToLockTop, 0{NULL}, 0,0,0,0, SWP_FRAMECHANGED);
  end;
*)
end;

procedure TForm1.bBorderClick(Sender: TObject);
begin
  if bBorder.down then EnableBorder(true) else EnableBorder(false);
end;

initialization

finalization

end.
