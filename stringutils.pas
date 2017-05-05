unit stringutils;

interface

function RightStr(const AText: AnsiString; const ACount: Integer): AnsiString;
function LeftStr(const AText: AnsiString; const ACount: Integer): AnsiString;

implementation

function RightStr(const AText: AnsiString; const ACount: Integer): AnsiString;
begin
  Result := Copy(WideString(AText), Length(WideString(AText)) + 1 - ACount, ACount);
end;

function LeftStr(const AText: AnsiString; const ACount: Integer): AnsiString;
begin
  Result := Copy(WideString(AText), 1, ACount);
end;



end.
