program pong;



uses crt,graph;


const bgipath = 'c:\program\turbo\tpus'; { *** ACHTUNG! *** }

var

  gd,gm,lscore,rscore,ballx,bally,ballmovex,ballmovey,lpaddley,rpaddley,lpaddlemove,rpaddlemove : integer;

  specialx,specialy,specialcount,rspecial,lspecial,specialon:integer;

  ballout,endgame:boolean;

  size:word;

  lpad,rpad,padclr,ball,ballclr:pointer;

  quit:char;









procedure image;

begin

  setcolor(4);

  rectangle(0,0,10,70);

  setfillstyle(1,4);

  floodfill(1,1,4);

  size:=imagesize(0,0,10,70);

  getmem(lpad,size);

{
  getimage(0,0,10,70,lpad^);
}







  cleardevice;

  setcolor(2);

  rectangle(0,0,10,70);

  setfillstyle(1,2);

  floodfill(1,1,2);

  getmem(rpad,size);

{
  getimage(0,0,10,70,rpad^);
}







  cleardevice;

  getmem(padclr,size);

{
  getimage(0,0,10,70,padclr^);
}



  setcolor(15);

  circle(10,10,10);

  setfillstyle(1,15);

  floodfill(10,10,15);

  size:=imagesize(0,0,20,20);

  getmem(ball,size);

{
  getimage(0,0,20,20,ball^);
}





  cleardevice;

  getmem(ballclr,size);

{
  getimage(0,0,20,20,ballclr^);
}

  cleardevice;







end; {of image procedure}



procedure setup;

begin

  gd:=detect;

  initgraph(gd,gm,bgipath);

  cleardevice;

end;



procedure writescr;

begin

  textcolor(2);

  settextstyle(3,0,4);

  setcolor(9);

  case lscore of

          0: outtextxy(200,10,'0');

          1: outtextxy(200,10,'1');

          2: outtextxy(200,10,'2');

          3: outtextxy(200,10,'3');

          4: outtextxy(200,10,'4');

          5: outtextxy(200,10,'5');

          6: outtextxy(200,10,'6');

          end;

  case rscore of

          0: outtextxy(400,10,'0');

          1: outtextxy(400,10,'1');

          2: outtextxy(400,10,'2');

          3: outtextxy(400,10,'3');

          4: outtextxy(400,10,'4');

          5: outtextxy(400,10,'5');

          6: outtextxy(400,10,'6');

         end;





end;





procedure drawscreen;

begin

  setcolor(4);

  line(0,47,getmaxx,47);

  line(0,getmaxy,getmaxx,getmaxy);



  {setcolor(8);

  setfillstyle(1,8);

  rectangle(500,0,530,getmaxy);

  floodfill(501,40,8);}

end;



procedure key;

begin

  if keypressed then begin

      case upcase(readkey) of

        'A': if lpaddlemove>-20 then lpaddlemove:=lpaddlemove-3;

        'Z': if lpaddlemove<20 then lpaddlemove:=lpaddlemove+3;

        'L': if rpaddlemove>-20 then rpaddlemove:=rpaddlemove-3;

        ',': if rpaddlemove<20 then rpaddlemove:=rpaddlemove+3;

        #27: begin

               settextstyle(2,0,5);

               setcolor(15);

               outtextxy(250,250,'Do you want to quit?');

               quit:=readkey;

               if (quit='y') or (quit='Y') then begin

                 ballout:=true;

                 endgame:=true;

                 end

                 else begin

                        cleardevice;

                        drawscreen;

                        writescr;

             end;

        end;

        end;

     end;





end; {of key procedure}



procedure special;

begin

  if specialcount=5 then

  begin

    specialcount:=0;

    specialx:=random(600)+100;

    specialy:=random(300)+100;

    setcolor(8);

    circle(specialx,specialy,10);

    setfillstyle(1,8);

    floodfill(specialx,specialy,8);

  end;

  if specialon>500 then

  begin

    rspecial:=0;

    lspecial:=0;

    specialon:=0;

  end;



  if rspecial=1 then

  begin

    if ballmovex<0 then

    begin

      specialcount:=0;

      ballmovey:=rpaddlemove;

      if bally>getmaxy-30 then bally:=getmaxy-50;

      if bally<60 then bally:=60;

      specialon:=specialon+1;

    end;

  end;

  if lspecial=1 then

  begin

    if ballmovex>0 then

    begin

      specialcount:=0;

      ballmovey:=lpaddlemove;

      if bally>getmaxy-30 then bally:=getmaxy-50;

      if bally<60 then bally:=60;

      specialon:=specialon+1;

    end;

  end;



end; {of special procedure}



procedure paddles;

begin

  rpaddley:=rpaddley+rpaddlemove;

  lpaddley:=lpaddley+lpaddlemove;



  if (rpaddley<51) or (rpaddley>getmaxy-75) then rpaddlemove:=rpaddlemove*-1;

  if (lpaddley<51) or (lpaddley>getmaxy-75) then lpaddlemove:=lpaddlemove*-1;



end; {of paddles procedure}



procedure bounce;

begin



  if (bally>getmaxy-28) or (bally<56) then

  begin

    ballmovey:=ballmovey*-1;

    specialcount:=specialcount+1;

  end;



  if getpixel(ballx+21,bally+10)=2 then

  begin

    ballmovex:=-6;

    ballmovey:=rpaddlemove;

  end;

  if getpixel(ballx-1,bally+10)=4 then

  begin

    ballmovex:=6;

    ballmovey:=lpaddlemove;

  end;



  if getpixel(ballx+21,bally+10)=8 then

  begin

    setfillstyle(1,0);

    floodfill(specialx,specialy,0);

    lspecial:=1;

  end;



  if getpixel(ballx-1,bally+10)=8 then

  begin

    setfillstyle(1,0);

    floodfill(specialx,specialy,0);

    rspecial:=1;

  end;



  ballx:=ballx+ballmovex;

  bally:=bally+ballmovey;



end; {of procedure bounce}



procedure play;

begin

  ballout:=false;

  lpaddley:=250;

  rpaddley:=250;

  ballx:=100;

  bally:=250;

  ballmovex:=6;

  ballmovey:=random(10)-4;





  repeat



{
    putimage(ballx,bally,ballclr^,normalput);
}

    bounce;

{
    putimage(1,lpaddley,padclr^,normalput);
}

{
    putimage(getmaxx-10,rpaddley,padclr^,normalput);
}





    key;



    paddles;



    special;



{
    putimage(1,lpaddley,lpad^,normalput);
}

{
    putimage(getmaxx-10,rpaddley,rpad^,normalput);
}

{
    putimage(ballx,bally,ball^,normalput);
}

{
    delay(20);
}

    pleaseFuck;
    if (ballx<0) or (ballx>getmaxx) then ballout:=true;



  until ballout;







end; {of play procedure}







procedure waitForKeyPress;
begin
    repeat 
    begin
        pleaseFuck; 
    end until keypressed;
end;


procedure score;

begin



  if ballx>getmaxx-10 then lscore:=lscore+1;

  if ballx<1 then rscore:=rscore+1;

{
  sound(60);
}

  pleaseFuck;

{
  nosound;
}



  cleardevice;



  writescr;

  if rscore>5 then

  begin

    setcolor(2);

    outtextxy(260,220,'Green Wins!');

    endgame:=true;
    
    waitForKeyPress;
    
  end;

  if lscore>5 then

  begin

    setcolor(4);

    outtextxy(260,220,'Red Wins!');

    endgame:=true;

    waitForKeyPress;

  end;



end;{of score procedure}



procedure resetVars;

begin

  rscore:=0;

  lscore:=0;

  lpaddlemove:=0;

  rpaddlemove:=0;

  ballmovex:=0;

  ballmovey:=0;

  specialcount:=0;

  rspecial:=0;

  lspecial:=0;

  specialon:=0;





end;



procedure intro;

begin

  cleardevice;

  setcolor(6);

  rectangle(50,50,getmaxx-50,getmaxy-50);

  rectangle(100,100,getmaxx-100,getmaxy-100);

  setfillstyle(3,6);

  floodfill(60,60,6);

  settextstyle(1,0,15);

  setcolor(15);

  outtextxy(160,160,'Pong!');

  settextstyle(2,1,5);

  outtextxy(500,160,'By David Burns 12D');

  settextstyle(2,0,5);

  outtextxy(240,350,'Press any key to play!');

waitForKeyPress;


end;



procedure Main;
begin

  randomize;

  resetVars;

  endgame:=false;

  setup;

  intro;

  image;

  writescr;

  drawscreen;



  repeat

  play;

  score;

  drawscreen;

  specialon:=1000;

  until endgame;

  closegraph;

end;

begin {of main procedure}
Main;
end.



