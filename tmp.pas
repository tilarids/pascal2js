procedure HighScores;
begin
   while true do
   begin
       ClearHighScoresDisplay;
       for i:= 1 to 10 do 
       begin
            ShowHighScoreElement(i)
            for j := 1 to 20 do PleaseYield;
       end;
   end
end;

procedure ChooseMenu;
const
  selected: integer  = -1;
  current: integer = 0;
begin
   while selected = -1 do
   begin
       if (keyPressed[keyUp]) then dec(current);
       if (keyPressed[keyDown]) then inc(current);
       if (keyPressed[keyEnter]) then selected := current;
       displaySelection(current);
       PleaseYield;
   end
end;

procedure MainProc;
begin
        TaskManager.addTask(@HighScores);
        TaskManager.addTask(@ChooseMenu);
        TaskManager.runTasks();
end;
