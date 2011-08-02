function PARAMSTR() {
    // do nothing
}

function WRITE() {
    // do nothing
}

function NOTIMPL() {
    // do nothing
}

function WRITELN() {
    // do nothing
}

function RT_File() {
    // do nothing
}

function ASSIGN() {
    // do nothing
}

function RESET() {
    // do nothing
}

function CLOSE() {
    // do nothing
}

function SEEK() {
    // do nothing
}

function BLOCKREAD() {
    // do nothing
}

function BLOCKWRITE() {
    // do nothing
}

function ERASE() {
    // do nothing
}

function MEMAVAIL() {
    return 424242;
}

function BOOLEAN(x) {
    return x;
}

function WORD(x) {
    return x;
}

function INTEGER(x) {
    return x;
}

function MAXINT() {
    return Math.pow(2,32);
}

function RANDOM() {
    return Math.random();
}

function ABS(x) {
    return Math.abs(x);
}

function PKEYARRAY(x) {
    return x;
}

function ODD(x) {
    return x % 2;
}

function READLN() {
}
 
var IORESULT = 0;

var SEGA000 = 0;

function GETMEM() {
}

function FREEMEM() {
}

function DISPOSE() {
}

function FREESELECTOR() {
}

function ALLOCSELECTOR() {
}

function SETSELECTORBASE() {
}

function SETSELECTORLIMIT() {
}

function ROUND(x) {
    return Math.floor(x);
}

function RUNERROR(x) {
    throw("RUNERROR" + x);
}

function MOVE() {
}

function UPCASE(x) {
    if (typeof(x)=="string") {
        return x.toUpperCase();
    } else {
        throw "Can not uppercase not a string!";
    }
}

var RT_BLACKHOLE_MEM = 0;
var RT_BLACKHOLE_MEMW = 0;
var EXITPROC = 0;
var PORT = 0;
var ERRORADDR = 0;
var EXITCODE = 0;

function HALT(x) {
    throw("HALT");
}

function RANDOMIZE() {
    // do nothing
}

function REWRITE() {
    // do nothing
}

function FILESIZE() {
    // do nothing
}

function ORD(x) {
    x.charCodeAt(0);
}

function CHR(x) {
    return String.fromCodeChar(x);
}

function PCHAR(x) {
    return x;
}

var _tmp$list = null;

