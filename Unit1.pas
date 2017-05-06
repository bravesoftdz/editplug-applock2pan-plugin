{ Plugin that extends original Applock plugin behavior to now have 2 panels
  to lock (view) external applications inside EditPlug without the user having
  to alt-tab to switch windows. Sort of a tile window manager for external apps
  inside EditPlug to make it easier to manage your windows without constant
  window switching

  StayOnTop feature available also, just to make any window stay on top

  Filter feature: enter in a filter (search term) to only show windows with that
  search term. I.e. "total" in the filter box would find Total Commander windows

  ISSUES:
    -cmd.exe windows (consoles) may have some painting issues. Instead, use
     the stay on top feature if cmd.exe windows are not painting or drawing on the
     screen nice after resizing. Might be able to fix this.

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
    HeadPanel: TPanel;
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
    mStayOnTop: TMenuItem;
    mUndoStayOnTop: TMenuItem;
    edFilter: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PopupItemClick(Sender: TObject);
    procedure PopupItemStayOnTopClick(Sender: TObject);
    procedure PopupItemUndoStay(Sender: TObject);
    procedure PopupPopup(Sender: TObject);
    procedure bLockClick(Sender: TObject);
    procedure WinPanTopResize(Sender: TObject);
    procedure bUnlockClick(Sender: TObject);
    procedure bNoTitleClick(Sender: TObject);
    procedure WinPanBottomResize(Sender: TObject);
    procedure bBorderClick(Sender: TObject);
    procedure edFilterKeyPress(Sender: TObject; var Key: Char);
    procedure edFilterEnter(Sender: TObject);
  private
    { Private declarations }
    FirstShow: boolean;
    WinToLockTop: HWND;
    WinToLockBottom: HWND;
    OldStyleTop: integer;
    OldStyleBottom: integer;
    procedure UnlockCurrentWinTop;
    procedure UnlockCurrentWinBottom;
    procedure EnableAppTitle(enabled: boolean);
    procedure EnableBorder(enabled: boolean);
    procedure MoveAppWinTop;
    procedure MoveAppWinBottom;
    // After resize finished event
    procedure WMExitSizeMove(var Msg: TMessage); message WM_EXITSIZEMOVE;
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
  SUB_STAYONTOP = 2; // just a stay on top feature, doesn't lock window in place
  SUB_UNDOSTAY = 3; // undo stay on top of a window

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
    if form1.WinToLockBottom <> 0 then begin
      SetFocus(form1.WinToLockBottom);
      windows.SetForegroundWindow(form1.WinToLockBottom);
    end;

    if form1.WinToLockTop <> 0 then begin
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

procedure AddMenuItem(CaptionStr: string; ID: integer);
var mi: TMenuItem;
begin
  mi := TMenuItem.create(form1.popup);
  mi.caption := CaptionStr;
  case ID of
    SUB_STAYONTOP: mi.OnClick := form1.PopupItemStayOnTopClick;
    SUB_UNDOSTAY: mi.OnClick := form1.PopupItemUndoStay;
  else
    // default click event
    mi.OnClick := form1.PopupItemClick;
  end;
  form1.popup.Items[ID].Add(mi);
end;

procedure AddMenuItems(CaptionStr: string);
begin
  AddMenuItem(CaptionStr, SUB_TOP);
  AddMenuItem(CaptionStr, SUB_BOTTOM);
  AddMenuItem(CaptionStr, SUB_STAYONTOP);
  AddMenuItem(CaptionStr, SUB_UNDOSTAY);
end;

function MakeCaption(txt: string; wnd: HWND): string;
begin
  result := txt + ' - Handle: ' + IntToStr(wnd);
end;

function EnumWindowsProc(Wnd: HWND; lParam: lParam): BOOL; stdcall;
var
  miTop, miBottom: TMenuItem;
  txt: string;
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
      CaptionStr := MakeCaption(txt, Wnd);
      AddMenuItems(CaptionStr);
    end;
  end;
end;

// finds all open windows, but applies filter (searches for text)
function EnumWinProcWithFilter(Wnd: HWND; lParam: lParam): BOOL; stdcall;
var
  miTop, miBottom: TMenuItem;
  txt: string;
  // OldName: string;
  CaptionStr: string;
  substr: string;
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
      substr := lowercase(form1.edFilter.text);
      if pos(substr, lowercase(txt)) > 0 then begin
        CaptionStr := MakeCaption(txt, Wnd);
        AddMenuItems(CaptionStr);
      end;
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

// use windows api to make a window stay on top of all other windows
procedure StayOnTop(h: HWND);
begin
  SetWindowPos(h, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_SHOWWINDOW);
end;

// undo stayontop with windows api
procedure UndoStayOnTop(h: HWND);
begin
  SetWindowPos(h, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_SHOWWINDOW);
end;

// Stay on top feature
procedure TForm1.PopupItemStayOnTopClick(Sender: TObject);
var
  h: HWND;
  mi: TMenuItem;
begin
  if sender is TMenuItem then begin
    mi := sender as TMenuItem;
    h := FindHandle(mi.caption);
    // force window to stay on top
    if mi.Parent.Name = 'mStayOnTop' then StayOnTop(h);
  end;
end;

procedure TForm1.PopupItemUndoStay(Sender: TObject);
var
  h: HWND;
  mi: TMenuItem;
begin
  if sender is TMenuItem then begin
    mi := sender as TMenuItem;
    h := FindHandle(mi.caption);
    // force window to stay on top
    if mi.Parent.Name = 'mUndoStayOnTop' then UndoStayOnTop(h);
  end;
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
    if mi.Parent.Name = 'mTopPan' then begin
      UnlockCurrentWinTop;
      WinToLockTop := h;
      windows.setparent(WinToLockTop, WinPanTop.handle);
      MoveAppWinTop;
    end;
    // Bottom panel
    if mi.Parent.Name = 'mBottomPan' then begin
      UnlockCurrentWinBottom;
      WinToLockBottom := h;
      windows.setparent(WinToLockBottom, WinPanBottom.handle);
      MoveAppWinBottom;
    end;

    bNoTitle.down := true;
    EnableAppTitle(false);
  end;
end;

procedure MenuClearItems;
begin
  form1.popup.Items[SUB_TOP].Clear;
  form1.popup.Items[SUB_BOTTOM].Clear;
  form1.popup.Items[SUB_STAYONTOP].Clear;
  form1.popup.Items[SUB_UNDOSTAY].Clear;
end;

procedure ApplyFilter;
var Param: longint;
begin
  MenuClearItems;
  EnumWindows(@EnumWinProcWithFilter, Param);
end;

procedure FillMenuItems(filter: boolean);
var
  Param: Longint;
begin
  if filter then begin
    ApplyFilter;
  end else begin
    MenuClearItems;
    EnumWindows(@EnumWindowsProc, Param);
  end;
end;

procedure TForm1.PopupPopup(Sender: TObject);
begin
  // only filter enumerated windows with search string if filter is entered into editbox
  if (edFilter.text <> 'f i l t e r') and (edFilter.text <> '') then begin
    FillMenuItems(true);
  end else begin
    FillMenuItems(false);  
  end;
end;

// pop up menu when clicking the button
procedure TForm1.bLockClick(Sender: TObject);
var
  pnt: TPoint;
begin
  //  if GetCursorPos(pnt) then Popup.Popup(pnt.X, pnt.Y);
  // popup at location of form 1 0,0 position underneath HeadPanel
  pnt := Form1.ClientToScreen(point(0,0 + HeadPanel.Height + 2));
  Popup.Popup(pnt.x, pnt.y);
end;

// force refresh of screen, otherwise window is not redrawn nicely
procedure ForceWinRedraw(pan: TPanel; han: HWND);
var h: HDC;
begin
//  pan.Invalidate;
//  InvalidateRect(pan.Handle, nil, false);
//  InvalidateRect(han, nil, false);
//  pan.Update;
//  pan.Repaint;
//  Application.ProcessMessages;
//  RedrawWindow(han, nil, 0, RDW_INVALIDATE or RDW_NOCHILDREN);

  h := GetWindowDC(han);
  SendMessage(han, WM_PAINT, h, 0);
//  SendMessage(han, WM_
  ReleaseDC(han, h);
  SetFocus(han);
end;

// resize app window to panel size
procedure TForm1.MoveAppWinTop;
var h: hdc;
begin
{  h := GetWindowDC(WinPanTop.Handle);
  SendMessage(WinPanTop.Handle, WM_PAINT, h, 0);
  ReleaseDC(WinPanTop.Handle, h);
}
//  ForceWinRedraw(WinPanTop, WinToLockTop);
  MoveWindow(WinToLockTop, 0, 0, WinPanTop.Width, WinPanTop.Height, true);
//  ForceWinRedraw(WinPanTop, WinToLockTop);
end;

procedure TForm1.MoveAppWinBottom;
begin
  MoveWindow(WinToLockBottom, 0, 0, WinPanBottom.Width, WinPanBottom.Height, true);
  ForceWinRedraw(WinPanBottom, WinToLockBottom);
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

procedure TForm1.WMExitSizeMove(var Msg: TMessage); {message WM_EXITSIZEMOVE;}
begin
  ShowMessage('Panel Resize!');
end;

procedure TForm1.edFilterKeyPress(Sender: TObject; var Key: Char);
begin
  if ord(Key) = VK_RETURN then begin
    Key := #0; // prevent beeping
    bLock.Click;
  end;
end;

procedure TForm1.edFilterEnter(Sender: TObject);
begin
  if edFilter.text = 'f i l t e r' then edFilter.text := '';
end;

initialization

finalization

end.
