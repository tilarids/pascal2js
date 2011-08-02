var mainGen = PAPOROTN$MAIN();

function yielder1() {
    while (true) {
        alert("yield1!");
        yield 0;
    }
}

GRAPH$PUTPIXEL = function(x,y,c) {
    game.putPixel(x,y,c);
}
GRAPH$KEYPRESSED = function() {
    return game.checkKey(27);
}

taskManager.addTask("MAIN", mainGen);
//taskManager.addTask("YIELDER1", yielder1());
taskManager.runTasks();
