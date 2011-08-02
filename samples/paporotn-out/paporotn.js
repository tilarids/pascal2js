var GRAPH$BLUE = 1;
var GRAPH$GREEN = 2;
var GRAPH$CYAN = 3;
var GRAPH$RED = 4;
var GRAPH$MAGENTA = 5;
var GRAPH$BROWN = 6;
var GRAPH$LIGHTGRAY = 7;
var GRAPH$DARKGRAY = 8;
var GRAPH$LIGHTBLUE = 9;
var GRAPH$LIGHTGREEN = 10;
var GRAPH$LIGHTCYAN = 11;
var GRAPH$LIGHTRED = 12;
var GRAPH$LIGHTMAGENTA = 13;
var GRAPH$YELLOW = 14;
var GRAPH$WHITE = 15;
var PAPOROTN$GR = 0;
var PAPOROTN$GR1 = 0;
var PAPOROTN$K = 0;
var PAPOROTN$COLOR = 0;
var PAPOROTN$NEWX = 0;
var PAPOROTN$X = 0;
var PAPOROTN$Y = 0;
var PAPOROTN$NEWY = 0;
var PAPOROTN$A = 0;
var PAPOROTN$B = 0;
var PAPOROTN$R = 0;
var PAPOROTN$C = 0;
var PAPOROTN$D = 0;
var PAPOROTN$E = 0;
var PAPOROTN$F = 0;
var PAPOROTN$I = 0;
var PAPOROTN$MAIN = function() {
        PAPOROTN$GR1 = 3;
        var yi = GRAPH$INITGRAPH(PAPOROTN$GR, PAPOROTN$GR1, '');
        if (RT_isGenerator(yi)) {
            for (var i in yi) yield i;
        };
        RANDOMIZE();
        NOTIMPL( /* AsmSt "mov ax,1001h\ */ );
        do {
            for (PAPOROTN$I = 1; PAPOROTN$I <= 300; PAPOROTN$I++) {
                PAPOROTN$R = RANDOM();
                if (((PAPOROTN$R) <= (0.1))) {
                    PAPOROTN$A = 0;
                    PAPOROTN$B = 0;
                    PAPOROTN$C = 0;
                    PAPOROTN$D = 0.16;
                    PAPOROTN$F = 0;
                } else {
                    if (((((PAPOROTN$R) > (0.1))) && (((PAPOROTN$R) <= (0.86))))) {
                        PAPOROTN$A = 0.85;
                        PAPOROTN$B = 4.0e-2;
                        PAPOROTN$C = ((-1) * (4.0e-2));
                        PAPOROTN$D = 0.85;
                        PAPOROTN$F = 1.6;
                    } else {
                        if (((((PAPOROTN$R) > (0.86))) && (((PAPOROTN$R) <= (0.93))))) {
                            PAPOROTN$A = 0.2;
                            PAPOROTN$B = ((-1) * (0.26));
                            PAPOROTN$C = 0.23;
                            PAPOROTN$D = 0.22;
                            PAPOROTN$F = 1.6;
                        } else {
                            PAPOROTN$A = ((-1) * (0.15));
                            PAPOROTN$B = 0.28;
                            PAPOROTN$C = 0.26;
                            PAPOROTN$D = 0.24;
                            PAPOROTN$F = 0.44;
                        };
                    };
                };
                PAPOROTN$NEWX = ((((PAPOROTN$A) * (PAPOROTN$X))) + (((PAPOROTN$B) * (PAPOROTN$Y))));
                PAPOROTN$NEWY = ((((PAPOROTN$C) * (PAPOROTN$X))) + (((((PAPOROTN$D) * (PAPOROTN$Y))) + (PAPOROTN$F))));
                PAPOROTN$X = PAPOROTN$NEWX;
                PAPOROTN$Y = PAPOROTN$NEWY;
                var yi = GRAPH$PUTPIXEL(ROUND(((((PAPOROTN$X) * (64))) + (350))), ROUND(((530) - (((PAPOROTN$Y) * (48))))), GRAPH$LIGHTGREEN);
                if (RT_isGenerator(yi)) {
                    for (var i in yi) yield i;
                };
            };
            yield 0;
        } while (!(GRAPH$KEYPRESSED()));
        GRAPH$READKEY();
        var yi = GRAPH$CLOSEGRAPH();
        if (RT_isGenerator(yi)) {
            for (var i in yi) yield i;
        };
    };
var GRAPH$BLUE = 1;
var GRAPH$GREEN = 2;
var GRAPH$CYAN = 3;
var GRAPH$RED = 4;
var GRAPH$MAGENTA = 5;
var GRAPH$BROWN = 6;
var GRAPH$LIGHTGRAY = 7;
var GRAPH$DARKGRAY = 8;
var GRAPH$LIGHTBLUE = 9;
var GRAPH$LIGHTGREEN = 10;
var GRAPH$LIGHTCYAN = 11;
var GRAPH$LIGHTRED = 12;
var GRAPH$LIGHTMAGENTA = 13;
var GRAPH$YELLOW = 14;
var GRAPH$WHITE = 15;
var GRAPH$SETCOLOR = function(COLOR) {};
var GRAPH$RECTANGLE = function(LEFT, TOP, RIGHT, BOTTOM) {};
var GRAPH$SETFILLSTYLE = function(PATTERN, COLOR) {};
var GRAPH$FLOODFILL = function(X, Y, COLOR) {};
var GRAPH$IMAGESIZE = function(LEFT, TOP, RIGHT, BOTTOM) {
        var _rt_retval = 0;
        return _rt_retval;
    };
var GRAPH$CLEARDEVICE = function() {};
var GRAPH$CIRCLE = function(X, Y, R) {};
var GRAPH$DETECT = function() {
        var _rt_retval = 0;
        return _rt_retval;
    };
var GRAPH$INITGRAPH = function(GD, GM, BGIPATH) {};
var GRAPH$TEXTCOLOR = function(COLOR) {};
var GRAPH$SETTEXTSTYLE = function(FONT, DIRECTION, CHARSIZE) {};
var GRAPH$OUTTEXTXY = function(X, Y, TEXTSTRING) {};
var GRAPH$LINE = function(X1, Y1, X2, Y2) {};
var GRAPH$GETMAXX = function() {
        var _rt_retval = 0;
        return _rt_retval;
    };
var GRAPH$GETMAXY = function() {
        var _rt_retval = 0;
        return _rt_retval;
    };
var GRAPH$KEYPRESSED = function() {
        var _rt_retval = false;
        return _rt_retval;
    };
var GRAPH$READKEY = function() {
        var _rt_retval = '\0';
        return _rt_retval;
    };
var GRAPH$GETPIXEL = function(X, Y) {
        var _rt_retval = 0;
        return _rt_retval;
    };
var GRAPH$CLOSEGRAPH = function() {};
var GRAPH$PUTPIXEL = function(X, Y, C) {};
var PAPOROTN$GR = 0;
var PAPOROTN$GR1 = 0;
var PAPOROTN$K = 0;
var PAPOROTN$COLOR = 0;
var PAPOROTN$NEWX = 0;
var PAPOROTN$X = 0;
var PAPOROTN$Y = 0;
var PAPOROTN$NEWY = 0;
var PAPOROTN$A = 0;
var PAPOROTN$B = 0;
var PAPOROTN$R = 0;
var PAPOROTN$C = 0;
var PAPOROTN$D = 0;
var PAPOROTN$E = 0;
var PAPOROTN$F = 0;
var PAPOROTN$I = 0;
var PAPOROTN$MAIN = function() {
        PAPOROTN$GR1 = 3;
        var yi = GRAPH$INITGRAPH(PAPOROTN$GR, PAPOROTN$GR1, '');
        if (RT_isGenerator(yi)) {
            for (var i in yi) yield i;
        };
        RANDOMIZE();
        NOTIMPL( /* AsmSt "mov ax,1001h\ */ );
        do {
            for (PAPOROTN$I = 1; PAPOROTN$I <= 300; PAPOROTN$I++) {
                PAPOROTN$R = RANDOM();
                if (((PAPOROTN$R) <= (0.1))) {
                    PAPOROTN$A = 0;
                    PAPOROTN$B = 0;
                    PAPOROTN$C = 0;
                    PAPOROTN$D = 0.16;
                    PAPOROTN$F = 0;
                } else {
                    if (((((PAPOROTN$R) > (0.1))) && (((PAPOROTN$R) <= (0.86))))) {
                        PAPOROTN$A = 0.85;
                        PAPOROTN$B = 4.0e-2;
                        PAPOROTN$C = ((-1) * (4.0e-2));
                        PAPOROTN$D = 0.85;
                        PAPOROTN$F = 1.6;
                    } else {
                        if (((((PAPOROTN$R) > (0.86))) && (((PAPOROTN$R) <= (0.93))))) {
                            PAPOROTN$A = 0.2;
                            PAPOROTN$B = ((-1) * (0.26));
                            PAPOROTN$C = 0.23;
                            PAPOROTN$D = 0.22;
                            PAPOROTN$F = 1.6;
                        } else {
                            PAPOROTN$A = ((-1) * (0.15));
                            PAPOROTN$B = 0.28;
                            PAPOROTN$C = 0.26;
                            PAPOROTN$D = 0.24;
                            PAPOROTN$F = 0.44;
                        };
                    };
                };
                PAPOROTN$NEWX = ((((PAPOROTN$A) * (PAPOROTN$X))) + (((PAPOROTN$B) * (PAPOROTN$Y))));
                PAPOROTN$NEWY = ((((PAPOROTN$C) * (PAPOROTN$X))) + (((((PAPOROTN$D) * (PAPOROTN$Y))) + (PAPOROTN$F))));
                PAPOROTN$X = PAPOROTN$NEWX;
                PAPOROTN$Y = PAPOROTN$NEWY;
                var yi = GRAPH$PUTPIXEL(ROUND(((((PAPOROTN$X) * (64))) + (350))), ROUND(((530) - (((PAPOROTN$Y) * (48))))), GRAPH$LIGHTGREEN);
                if (RT_isGenerator(yi)) {
                    for (var i in yi) yield i;
                };
            };
            yield 0;
        } while (!(GRAPH$KEYPRESSED()));
        GRAPH$READKEY();
        var yi = GRAPH$CLOSEGRAPH();
        if (RT_isGenerator(yi)) {
            for (var i in yi) yield i;
        };
    }; /*  -------- empty BODY!*/
;
