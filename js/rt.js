Function.prototype.bind = function(scope) {
  var _function = this;
  
  return function() {
    return _function.apply(scope, arguments);
  }
}

function Game(canvas, palette, screenWidth, screenHeight) {
    this.canvas = document.getElementById(canvas);
    this.ctx = this.canvas.getContext("2d");
    this.palette = palette;
    this.data = new Data(getDefaultDataArray());
    this.fonts = {}
    this.defColor = 1;
    this.defBackColor = 3;
    this.defTransp = true;
    this.width = 800;
    this.height = 600;
    this.realWidth = 800;
    this.realHeight = 2400;
    this.screenWidth = screenWidth;
    this.screenHeight = screenHeight;
    
    this.ctx.scale(this.screenWidth/this.width, this.screenHeight/this.height);
    function loadFont(game, offset, length, name) {
        game.fonts[name] = game.data.readData(offset, length);
    }
    
    //LoadNFont; // readData(334045,sizeof(Font),Font);
    loadFont(this, 64768,991,'PC9');
    loadFont(this, 65759,1486,'LCDFONT');
    loadFont(this, 67245,1486,'ITALIC');
    loadFont(this, 68731,1486,'FRAKTUR');
    loadFont(this, 70217,1486,'FUTURE');
    loadFont(this, 71703,991,'LIGHT9');
    loadFont(this, 72694,793,'FONT07');
    loadFont(this, 203023,1486,'COUNTDOWN');
    loadFont(this, 400605,1486,'LIGHT14');

    this.keys = {}
    
    this.buffer = this.ctx.createImageData(this.screenWidth*2, this.screenHeight*2);
    window.addEventListener('keydown', function(evt) { this.keys[evt.keyCode] = true; }.bind(this), true);
    window.addEventListener('keyup', function(evt) { this.keys[evt.keyCode] = false; }.bind(this), true);
}

Game.prototype.putPixel = function(x,y,c) {
    this.drawBar(x % this.realWidth, y % this.realHeight, 1, 1, c);
}

Game.prototype.getPixel = function(x,y) {
}

Game.prototype.getPaletteColor = function(c) {
    return "rgb("+this.palette[c*3]+","+this.palette[c*3+1]+","+this.palette[c*3+2]+")";
}
Game.prototype.drawLine = function(x1,y1,x2,y2,c) {
    this.ctx.save();
    this.strokeStyle = this.getPaletteColor(c);
    this.ctx.beginPath();
    this.ctx.moveTo(x1,y1);
    this.ctx.lineTo(x2,y2);
    this.ctx.closePath();
    this.ctx.stroke();
    this.ctx.restore();
}

Game.prototype.drawBar = function(x1,y1,x2,y2,c) {
    this.ctx.save();
    this.ctx.fillStyle = this.getPaletteColor(c);
    this.ctx.fillRect(x1,y1,x2,y2);
    this.ctx.restore();
}

Game.prototype.getPalette = function() {
    return this.palette;
}

Game.prototype.setPalette = function(p) {
    this.palette = p;
}

Game.prototype.drawChar = function(fontName, x, y, cr, color, backColor, transp) {
    var font = this.fonts[fontName];
    var fn = 0;
    var charByte = font[fn];
    fn += 1;
    var p = (1+charByte)*(cr.charCodeAt(0)-28)+fn;
    var mi = font[p];
    for (var j = 0; j<charByte; ++j) {
        p += 1;
        var di = x;
        var b = font[p];
        for (var i = 0;i<mi; ++i)
        {
            var c;
            b *= 2;
            if (b & 0x100) {
                c = color;
            } else if (!transp) {
                c = backColor;
            }
            else {
                di+=1;
                continue;
            }
            this.putPixel(di,y,c);
            di+=1;
        }
        y += 1;
    }
    return mi;
}

Game.prototype.drawText = function(fontName, x, y, text) {
    var font = this.fonts[fontName];
    var startX = x;
    var color = this.defColor;
    var backColor = this.defBackColor;
    var transp = this.defTransp;
    for (var i = 0; i<text.length; ++i) {
        if (x > this.width - 8) {
            x = 0;
            y = y + font[0];
        }
        if (text[i] != '%') {
            x += this.drawChar(fontName, x,y, text[i], color, backColor, transp);
        } else {
            ++i;
            switch(text[i].toUpperCase()) {
            case "%": x += this.drawChar(fontName, x,y, text[i], color, backColor, transp);
                      break;
            case "F": color = parseInt(text[i+1] + text[i+2] + text[i+3]);
                      i += 3;
                      break;
            case "B": backColor = parseInt(text[i+1] + text[i+2] + text[i+3]);
                      i += 3;
                      break;
            case "T": transp = text[i+1]=='1';
                      i += 1;
                      break;
            case "1":
            case "2":
            case "3":
            case "4":
            case "5":
            case "6":
            case "7":
            case "8":
                     color = text.charCodeAt(i)+105-1;
                     break;
            }
        }
        
    }
    return x - startX;
}

Game.prototype.drawData = function(x, y, offset) {
    var imgData = this.data.readData(offset, 64000);
    var palette = this.data.readData(offset + 64000, 768);
    for (var i = 0; i<768; ++i) {
        palette[i] *= 4;
        if (palette[i] > 255) {
            palette[i] = 255;
        }
    }   
    this.setPalette(palette);
    for (var i = 0; i < 4; ++i ) {
        for (var j = 0; j < 4; ++j) {
            for (var k = 0; k < 80; ++k) {
                for (var l = 0; l < 50; ++l) {
                    this.putPixel(x + k*4 + i, y + l*4 + j, imgData[(j*80+k)+(i*50+l)*320]);
                }
            }
        }
    }
    //for (var i =0; i<this.width; ++i) {
        //for (var j=0; j<this.height; ++j) {
            //var xi = (i % 4) * 80 + (i / 4);
            //var xj = (j % 4) * 50 + (j / 4);
            //this.putPixel(i, j, imgData[xj * this.width + xi]);
        //}
    //}
    //for (var i = 0; i < 64000; ++i) {
        //this.putPixel(i % this.width, i / this.width, imgData[i]);
    //}
}

function getDefaultPalette() {
    return [0, 0, 0, 0, 0, 168, 0, 168, 0, 0, 168, 168, 168, 0, 0, 168, 0, 
    168, 168, 84, 0, 168, 168, 168, 84, 84, 84, 84, 84, 252, 84, 252, 84, 84,
    252, 252, 252, 84, 84, 252, 84, 252, 252, 252, 84, 252, 252, 252, 0, 0, 
    0, 20, 20, 20, 32, 32, 32, 44, 44, 44, 56, 56, 56, 68, 68, 68, 80, 80, 80
    , 96, 96, 96, 112, 112, 112, 128, 128, 128, 144, 144, 144, 160, 160, 160,
    180, 180, 180, 200, 200, 200, 224, 224, 224, 252, 252, 252, 0, 0, 252, 
    64, 0, 252, 124, 0, 252, 188, 0, 252, 252, 0, 252, 252, 0, 188, 252, 0, 
    124, 252, 0, 64, 252, 0, 0, 252, 64, 0, 252, 124, 0, 252, 188, 0, 252, 
    252, 0, 188, 252, 0, 124, 252, 0, 64, 252, 0, 0, 252, 0, 0, 252, 64, 0, 
    252, 124, 0, 252, 188, 0, 252, 252, 0, 188, 252, 0, 124, 252, 0, 64, 252,
    124, 124, 252, 156, 124, 252, 188, 124, 252, 220, 124, 252, 252, 124, 
    252, 252, 124, 220, 252, 124, 188, 252, 124, 156, 252, 124, 124, 252, 156
    , 124, 252, 188, 124, 252, 220, 124, 252, 252, 124, 220, 252, 124, 188, 
    252, 124, 156, 252, 124, 124, 252, 124, 124, 252, 156, 124, 252, 188, 124
    , 252, 220, 124, 252, 252, 124, 220, 252, 124, 188, 252, 124, 156, 252, 
    180, 180, 252, 196, 180, 252, 216, 180, 252, 232, 180, 252, 252, 180, 252
    , 252, 180, 232, 252, 180, 216, 252, 180, 196, 252, 180, 180, 252, 196, 
    180, 252, 216, 180, 252, 232, 180, 252, 252, 180, 232, 252, 180, 216, 252
    , 180, 196, 252, 180, 180, 252, 180, 180, 252, 196, 180, 252, 216, 180, 
    252, 232, 180, 252, 252, 180, 232, 252, 180, 216, 252, 180, 196, 252, 0, 
    0, 112, 28, 0, 112, 56, 0, 112, 84, 0, 112, 112, 0, 112, 112, 0, 84, 112,
    0, 56, 112, 0, 28, 112, 0, 0, 112, 28, 0, 112, 56, 0, 112, 84, 0, 112, 
    112, 0, 84, 112, 0, 56, 112, 0, 28, 112, 0, 0, 112, 0, 0, 112, 28, 0, 112
    , 56, 0, 112, 84, 0, 112, 112, 0, 84, 112, 0, 56, 112, 0, 28, 112, 56, 56
    , 112, 68, 56, 112, 84, 56, 112, 96, 56, 112, 112, 56, 112, 112, 56, 96, 
    112, 56, 84, 112, 56, 68, 112, 56, 56, 112, 68, 56, 112, 84, 56, 112, 96,
    56, 112, 112, 56, 96, 112, 56, 84, 112, 56, 68, 112, 56, 56, 112, 56, 56
    , 112, 68, 56, 112, 84, 56, 112, 96, 56, 112, 112, 56, 96, 112, 56, 84, 
    112, 56, 68, 112, 80, 80, 112, 88, 80, 112, 96, 80, 112, 104, 80, 112, 
    112, 80, 112, 112, 80, 104, 112, 80, 96, 112, 80, 88, 112, 80, 80, 112, 
    88, 80, 112, 96, 80, 112, 104, 80, 112, 112, 80, 104, 112, 80, 96, 112, 
    80, 88, 112, 80, 80, 112, 80, 80, 112, 88, 80, 112, 96, 80, 112, 104, 80,
    112, 112, 80, 104, 112, 80, 96, 112, 80, 88, 112, 0, 0, 64, 16, 0, 64, 
    32, 0, 64, 48, 0, 64, 64, 0, 64, 64, 0, 48, 64, 0, 32, 64, 0, 16, 64, 0, 
    0, 64, 16, 0, 64, 32, 0, 64, 48, 0, 64, 64, 0, 48, 64, 0, 32, 64, 0, 16, 
    64, 0, 0, 64, 0, 0, 64, 16, 0, 64, 32, 0, 64, 48, 0, 64, 64, 0, 48, 64, 0
    , 32, 64, 0, 16, 64, 32, 32, 64, 40, 32, 64, 48, 32, 64, 56, 32, 64, 64, 
    32, 64, 64, 32, 56, 64, 32, 48, 64, 32, 40, 64, 32, 32, 64, 40, 32, 64, 
    48, 32, 64, 56, 32, 64, 64, 32, 56, 64, 32, 48, 64, 32, 40, 64, 32, 32, 
    64, 32, 32, 64, 40, 32, 64, 48, 32, 64, 56, 32, 64, 64, 32, 56, 64, 32, 
    48, 64, 32, 40, 64, 44, 44, 64, 48, 44, 64, 52, 44, 64, 60, 44, 64, 64, 
    44, 64, 64, 44, 60, 64, 44, 52, 64, 44, 48, 64, 44, 44, 64, 48, 44, 64, 
    52, 44, 64, 60, 44, 64, 64, 44, 60, 64, 44, 52, 64, 44, 48, 64, 44, 44, 
    64, 44, 44, 64, 48, 44, 64, 52, 44, 64, 60, 44, 64, 64, 44, 60, 64, 44, 
    52, 64, 44, 48, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0];
}

Game.prototype.checkKey = function (key) {
    return this.keys[key];
}

Game.prototype.copyRect = function(x1,y1,width,height, x2,y2) {
    var imgData = this.ctx.getImageData(x1, y1, width, height);
    this.ctx.putImageData(imgData, x2,y2);
}

Game.prototype.calculateTextWidth = function(fontName, text) {
    var font = this.fonts[fontName];
    var hei = 1 + font[0];
    var i = 1;
    var width = 0;
    function getCharWidth(c) {
        return font[hei*(c.charCodeAt(0)-28)+i];
    }
    for (var i = 0; i< text.length; ++i) {
        if (text[i]=="%") {
            switch (text[i+1]) {
            case "F": 
            case "B": i += 4;
                      break;
            case "T": i += 2;
                      break;
            case "1":
            case "2":
            case "3":
            case "4":
            case "5":
            case "6":
            case "7":
            case "8":
            case "%":
                     i+=1;
            }
            continue;
        }
        width += getCharWidth(text[i]);
    }
    return width;
}

Game.prototype.calculateTextHeight = function(fontName) {
    var font = this.fonts[fontName];
    return 1 + font[0];
}

Game.prototype.setView = function(x,y) {
    this.canvas.style.top = "-" + y +"px";
    this.canvas.style.left = "-" + x +"px";
}

function drawGame() {
    this.drawData(0,0,204509);
    //this.drawData(138255);
    //this.drawData(269277);
    //this.drawData(73487);
    //this.drawBar(0,0,320,200,8);
    this.drawLine(10,10,100,100,3);
    this.drawLine(100,10,10,100,3);
    //this.drawBar(110,110,2000,2000,1);
    this.putPixel(105,105,1);
    //this.drawChar("LIGHT14", 70,50,"A",1,3,true);
    this.drawText("LIGHT14", 70, 50, "Haskell rules! %F006Pascal forever! %T0Das ist fantastich!");

}

var game = new Game("canvas", getDefaultPalette(), 800, 600);
//drawGame.bind(game)();
