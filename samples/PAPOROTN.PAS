program paporotn;
Uses Crt,Graph;
var gr,gr1:integer;
   k,color:integer;
   newx,x,y,newy,a,b,r,c,d,e,f:extended;
   i:integer;
procedure Main;
begin
 gr1:=3;
{
 gr:=InstalluserDriver('svga256',nil);
}
 Initgraph(gr,gr1,'');
randomize;
 
asm {Белая рамка}
   mov ax,1001h
   mov bh,15
   int 10h
 end;


repeat
for i:=1 to 300 do
begin
    r:=random;
    If r<=0.1 then
    begin
     a:=0;B:=0;c:=0;d:=0.16;f:=0;
    end else Begin
     if (r>0.1) and (r<=0.86) then
     begin
      a:=0.85 ;b:=0.04;c:=-1*0.04;d:=0.85;f:=1.6;
     end else begin
      if (r>0.86) and (r<=0.93) then
       begin
        a:=0.2;b:=-1*0.26;c:=0.23;d:=0.22;f:=1.6;
       end else Begin
        a:=-1*0.15;b:=0.28;c:=0.26;d:=0.24;f:=0.44;
       end;
     end;
    end;
    newx:=(a*x)+(b*y);
    newy:=(c*x)+(d*y)+f;
    x:=newx;
    y:=newy;
    putpixel(round(x*64+350),round(530-y*48),LightGreen);
end;
pleaseFuck;
until keypressed;
readkey;
closegraph;

end;
begin
Main;
end.
