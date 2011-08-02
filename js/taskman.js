function Task(name, gen) {
    this.name = name;
    this.gen = gen;
    this.state = "ok";
}

Task.prototype.tick = function() {
    if (this.state == "ok") {
        try {
            return this.gen.next();
        } catch(e) {
            console.log("Caught something! Suspending");
            this.suspend();
            //if (e != StopIteration) {
            //console.log(e);
                //throw e;
            //}
        }

    }
}

Task.prototype.suspend = function() {
    this.state = "susp";
}

Task.prototype.resume = function() {
    this.state = "ok";
}

function TaskManager() {
    this.tasks = {}
    this.fps = 30;
}

TaskManager.prototype.addTask = function(name, gen) {
    this.tasks[name] = new Task(name, gen);
}

TaskManager.prototype.suspendTask = function(name) {
    this.tasks[name].suspend();
}

TaskManager.prototype.resumeTask = function(name) {
    this.tasks[name].resume();
}

TaskManager.prototype.removeTask = function(name) {
    delete this.tasks[name];
}

function runTasks_() {
    var n=new Date();
    var sMS=n.getTime();
    for (var task in this.tasks) {
        if (this.tasks.hasOwnProperty(task)) {
            this.tasks[task].tick();
        }
    }

    var w=new Date();
    var wMS=w.getTime();
    var delay = 1;
    if (1000 / this.fps > (wMS - sMS)) {
        delay = (1000 / this.fps) - (wMS - sMS);
    }
    var self = this;
    window.setTimeout(function() { runTasks_.bind(self)() }, delay);
}
TaskManager.prototype.runTasks = function() { runTasks_.bind(this)(); } 

TaskManager.prototype.setFPS = function(fps) {
    this.fps = fps;
}

var taskManager = new TaskManager();


