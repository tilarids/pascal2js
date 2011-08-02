unit graph;

interface

procedure SetColor(color:integer);
procedure Rectangle(left:integer; top:integer; right:integer; bottom:integer);
procedure SetFillStyle(pattern:integer; color:integer);
procedure FloodFill(x:integer; y:integer; color:integer);
function ImageSize(left:integer; top:integer; right:integer; bottom:integer): integer;
{
procedure GetImage(left:integer; top:integer; right:integer; bottom:integer; p:pointer);
}
procedure ClearDevice;
procedure Circle(x:integer; y:integer; r:integer);
function Detect: integer;
procedure InitGraph(gd:integer;gm:integer;bgipath:string);
procedure TextColor(color:integer);
procedure SetTextStyle(Font, Direction : Word; CharSize : Word);
procedure OutTextXY(X, Y : Integer; TextString : String);
procedure Line(X1, Y1, X2, Y2 : Integer);
function GetMaxX : Integer;
function GetMaxY : Integer;
function KeyPressed: boolean;
function ReadKey : Char;
function GetPixel(x:integer; y:integer):integer;
procedure PutPixel(x:integer; y:integer; c:integer);
procedure CloseGraph;

const
    Blue = 1;
    Green = 2;
    Cyan = 3;
    Red = 4;
    Magenta = 5;
    Brown = 6;
    LightGray = 7;
    DarkGray = 8;
    LightBlue = 9;
    LightGreen = 10;
    LightCyan = 11;
    LightRed=12;
    LightMagenta=13;
    Yellow=14;
    White=15;

implementation

procedure SetColor(color:integer);
begin
end;

procedure Rectangle(left:integer; top:integer; right:integer; bottom:integer);
begin
end;

procedure SetFillStyle(pattern:integer; color:integer);
begin
end;

procedure FloodFill(x:integer; y:integer; color:integer);
begin
end;

function ImageSize(left:integer; top:integer; right:integer; bottom:integer): integer;
begin
end;

{
procedure GetImage(left:integer; top:integer; right:integer; bottom:integer; p:pointer);
begin
end;
}

procedure ClearDevice;
begin
end;

procedure Circle(x:integer; y:integer; r:integer);
begin
end;

function Detect: integer;
begin
end;

procedure InitGraph(gd:integer;gm:integer;bgipath:string);
begin
end;

procedure TextColor(color:integer);
begin
end;

procedure SetTextStyle(Font, Direction : Word; CharSize : Word);
begin
end;

procedure OutTextXY(X, Y : Integer; TextString : String);
begin
end;

procedure Line(X1, Y1, X2, Y2 : Integer);
begin
end;

function GetMaxX : Integer;
begin
end;

function GetMaxY : Integer;
begin
end;

function KeyPressed: boolean;
begin
end;

function ReadKey : Char;
begin
end;

function GetPixel(x:integer; y:integer):integer;
begin
end;

procedure CloseGraph;
begin
end;

procedure PutPixel(x:integer; y:integer; c:integer);
begin
end;

begin
end.
