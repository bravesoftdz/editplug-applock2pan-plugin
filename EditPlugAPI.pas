unit EditPlugAPI;

interface
uses windows, EditPlugAPITypes, forms;

const exe = 'editplug.exe';

procedure epSetActiveEdSelText(p: pchar); cdecl;
external exe;
function epGetActiveEdSelLen: longint; cdecl;
external exe;
function epGetActiveEdTextLen: longint; cdecl;
external exe;
function epGetActiveEdSelText(buf: pchar; len: longint): longint; cdecl;
external exe;
function epGetActiveEdText(buf: pchar; len: longint): longint; cdecl;
external exe;

function epSetActiveEdSelStart(pos: longint): longbool; cdecl;
external exe;
function epSetActiveEdSelEnd(pos: longint): longbool; cdecl;
external exe;
function epGetActiveEdSelStart: longint; cdecl;
external exe;
function epGetActiveEdSelEnd: longint; cdecl;
external exe;

function epGetEdCount: longint; cdecl;
external exe;
function epGetEdIndex: longint; cdecl;
external exe;
function epSetActiveEd(idx: longint): longbool; cdecl;
external exe;
function epSetEdSelText(idx: longint; txt: pchar): longbool; cdecl;
external exe;
function epSetEdText(idx: longint; txt: pchar): longbool; cdecl;
external exe;
procedure epNewEd; cdecl;
external exe;
function epGetEdFileName(idx: longint; buf: pchar; len: longint): longint; cdecl;
external exe;
function epGetEdFileNameLen(idx: longint): longint; cdecl;
external exe;
procedure epActiveEdRemoveSel; cdecl;
external exe;
procedure epSetParentToMainWin(child: HWND); cdecl;
external exe;
function epGetActiveEdFileNameLen: longint; cdecl;
external exe;
function epGetActiveEdFileName(buf: pchar; len: longint): longint; cdecl;
external exe;
procedure epAddSearchResult(fname: pchar; s: pchar; coord: TBufCoord; len: longint); cdecl;
external exe;
procedure epClearSearchResults; cdecl;
external exe;
procedure epClearOutputConsole(freeobjects: longbool); cdecl;
external exe;
procedure epAddOutputConsoleMsg(s: pchar; IgnoreForParsing: longbool); cdecl;
external exe;
procedure epGetMainWinPos(out rslt: TWinPos); cdecl;
external exe;
function epGetMainWinHandle: HWND; cdecl;
external exe;
function epFilesModified: longbool; cdecl;
external exe;
function epFileModified(idx: longint): longbool; cdecl;
external exe;
procedure epSearchResultsBeginUpdate; cdecl;
external exe;
procedure epSearchResultsEndUpdate; cdecl;
external exe;
procedure epAppProcessMessages; cdecl;
external exe;
function epRemovePluginTab(plugid: HINST): longbool; cdecl;
external exe;
function epCreatePluginTabEx(plugid: HINST; winhandle: HWND; adjustheight: longint): longbool; cdecl;
external exe;
function epCreatePluginTab(plugid: HINST; winhandle: HWND): longbool; cdecl;
external exe;
procedure epSaveActiveEdFile; cdecl;
external exe;
procedure epSaveAllFiles; cdecl;
external exe;
function epGetActiveEdTmpHtmFileLen: longint; cdecl;
external exe;
function epGetActiveEdTmpHtmFile(buf: pchar; len: longint): longint; cdecl;
external exe;
procedure epMakeActiveEdTmpHtmFile; cdecl;
external exe;
function epActiveEdUnnamed: longbool; cdecl;
external exe;
function epOpenFile(fname: pchar): longbool; cdecl;
external exe;

function epGetEdSelStart(idx: longint): longint; cdecl;
external exe;
function epGetEdSelLen(idx: longint): longint; cdecl;
external exe;
function epGetEdSelEnd(idx: longint): longint; cdecl;
external exe;


function epGetEdTextLen(idx: longint): longint; cdecl;
external exe;
function epGetEdText(buf: pchar; len: longint; idx: longint): longint; cdecl;
external exe;

function epSetEdSelStart(idx: longint; pos: longint): longbool; cdecl;
external exe;
function epSetEdSelEnd(idx: longint; pos: longint): longbool; cdecl;
external exe;

procedure epEdRemoveSel(idx: longint); cdecl;
external exe;

function epLoadPlugin(name: pchar): longbool; cdecl;
external exe;

procedure epSetActiveEdFocus; cdecl;
external exe;
procedure epSetMainWinFocus; cdecl;
external exe;

function epGetAppExePathLen: longint; cdecl;
external exe;
function epGetAppExePath(buf: pchar; len: longint): longint; cdecl;
external exe;

function epGetActiveEdLine: longint; cdecl;
external exe;
function epGetActiveEdColumn: longint; cdecl;
external exe;
function epGetActiveEdWordLen: longint; cdecl;
external exe;
function epGetActiveEdWord(buf: pchar; len: longint): longint; cdecl;
external exe;

procedure epConsoleExecCmd(CmdLine: pchar; StartInDir: pchar; ParseRule: pchar; ScrollToLastLine: longbool); cdecl;
external exe;
procedure epConsoleJumpToLastLine; cdecl;
external exe;

function epGetFilePanDirLen: longint; cdecl;
external exe;
function epGetFilePanDir(buf: pchar; len: longint): longint; cdecl;
external exe;
function epGetFilePanSelFilesLen: longint; cdecl;
external exe;
function epGetFilePanSelFiles(buf: pchar; len: longint): longint; cdecl;
external exe;

procedure epGetActiveEdBlockBegin(var coord: TBufCoord); cdecl;
external exe;
procedure epGetActiveEdBlockEnd(var coord: TBufCoord); cdecl;
external exe;

function epGetActiveEdTextAsString: string;
function epGetActiveEdSelTextAsString: string;
function epGetActiveEdFilenameAsString: string;
procedure epMoveWinToTopRight(form: TForm);
function epGetEdFileNameAsString(idx: integer): string;
function epGetActiveEdTmpHtmFileAsString: string;
function epGetEdTextAsString(idx: integer): string;
function epGetAppExePathAsString: string;
function epGetActiveEdWordAsString: string;
function epGetFilePanSelFilesAsString: string;
function epGetFilePanDirAsString: string;

function GetDesktopPos(h: HWND): TRect;
procedure SetDesktopPos(h: HWND; rect: TRect; width: integer; height: integer);

function LibPath: string;

implementation

function epGetActiveEdTextAsString: string;
var
  len: integer;
  buffer: string;
begin
  result := '';
  len := epGetActiveEdTextLen;
  if len < 1 then exit;
  setlength(buffer, len);
  epGetActiveEdText(pchar(buffer), length(buffer)+1);
  buffer := pchar(buffer);
  result := buffer;
end;

function epGetActiveEdSelTextAsString: string;
var
  len: integer;
  buffer: string;
begin
  result := '';
  len := epGetActiveEdSelLen;
  if len < 1 then exit;
  setlength(buffer, len);
  epGetActiveEdSelText(pchar(buffer), length(buffer)+1);
  buffer := pchar(buffer);
  result := buffer;
end;

function epGetActiveEdFileNameAsString: string;
var
  len: integer;
  buffer: string;
begin
  result := '';
  len := epGetActiveEdFileNameLen;
  if len < 1 then exit;
  setlength(buffer, len);
  epGetActiveEdFilename(pchar(buffer), length(buffer)+1);
  buffer := pchar(buffer);
  result := buffer;
end;

function epGetEdFileNameAsString(idx: integer): string;
var
  len: integer;
  buffer: string;
begin
  result := '';
  if idx < 0 then exit;
  len := epGetEdFileNameLen(idx);
  if len < 1 then exit;
  setlength(buffer, len);
  epGetEdFileName(idx, pchar(buffer), length(buffer)+1);
  buffer := pchar(buffer);
  result := buffer;
end;

procedure epMoveWinToTopRight(form: TForm);
var
  winpos: TWinPos;
  left: integer;
begin
  epGetMainWinPos(winpos);
  left := winpos.width - form.width - 10;
  MoveWindow(form.handle, left, 0, form.width, form.height, true);
end;


function LibPath: string;
var
  Buffer: Array[0..260] of Char;
begin
  GetModuleFileName(hInstance, Buffer, Length(Buffer));
  result := buffer;
end;

function epGetActiveEdTmpHtmFileAsString: string;
var
  len: integer;
  buffer: string;
begin
  result := '';
  len := epGetActiveEdTmpHtmFileLen;
  if len < 1 then exit;
  setlength(buffer, len);
  epGetActiveEdTmpHtmFile(pchar(buffer), length(buffer)+1);
  buffer := pchar(buffer);
  result := buffer;
end;

function epGetEdTextAsString(idx: integer): string;
var
  len: integer;
  buffer: string;
begin
  result := '';
  len := epGetEdTextLen(idx);
  if len < 1 then exit;
  setlength(buffer, len);
  epGetEdText(pchar(buffer), length(buffer)+1, idx);
  buffer := pchar(buffer);
  result := buffer;
end;

function epGetAppExePathAsString: string;
var
  len: integer;
  buffer: string;
begin
  result := '';
  len := epGetAppExePathLen;
  if len < 1 then exit;
  setlength(buffer, len);
  epGetAppExePath(pchar(buffer), length(buffer)+1);
  buffer := pchar(buffer);
  result := buffer;
end;

function epGetActiveEdWordAsString: string;
var
  len: integer;
  buffer: string;
begin
  result := '';
  len := epGetActiveEdWordLen;
  if len < 1 then exit;
  setlength(buffer, len);
  epGetAppExePath(pchar(buffer), length(buffer)+1);
  buffer := pchar(buffer);
  result := buffer;
end;

function epGetFilePanSelFilesAsString: string;
var
  len: integer;
  buffer: string;
begin
  result := '';
  len := epGetFilePanSelFilesLen;
  if len < 1 then exit;
  setlength(buffer, len);
  epGetFilePanSelFiles(pchar(buffer), length(buffer)+1);
  buffer := pchar(buffer);
  result := buffer;
end;

function epGetFilePanDirAsString: string;
var
  len: integer;
  buffer: string;
begin
  result := '';
  len := epGetFilePanDirLen;
  if len < 1 then exit;
  setlength(buffer, len);
  epGetFilePanDir(pchar(buffer), length(buffer)+1);
  buffer := pchar(buffer);
  result := buffer;
end;

function GetDesktopPos(h: HWND): TRect;
begin
  GetWindowRect(h, result);
end;

procedure SetDesktopPos(h: HWND; rect: TRect; width: integer; height: integer);
begin
  MoveWindow(h, rect.Left, rect.Top, width, height, true);
end;

end.
