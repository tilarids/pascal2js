function Ptr(a) {
    this.val = a;
}

function Range(from, to) {
    this.from = from;
    this.to = to;
}

function OutVariables(lst) {
    this.lst = lst;
}

Range.prototype.generate = function() {
    
    if (typeof(this.from) == "number" && typeof(this.to) == "number") {
        var result = new Array(this.to - this.from+1);
        for (var i = 0; i<=this.to-this.from; ++i) {
            result[i] = this.to + i;
        }
        return result;
    } else if (typeof(this.from)=="string" && typeof(this.to) == "string" 
               && this.from.length == 1 && this.to.length == 1) {
            var count = this.to.charCodeAt(0) - this.from.charCodeAt(0) + 1;
            var result = new Array(count);
            for (var i = 0; i < count; ++i) {
                result[i] = String.fromCharCode(this.from.charCodeAt(0) + i);
            }
            return result;
    } else {
        throw "Can't generate range: only chars and integers are supported"
    }
}

function Set(args) {
    this.elems = {};
    for (var i = 0; i<args.length; ++i) {
        if (args[i] instanceof Range) {
            var rangeElems = args[i].generate()
            for (var j = 0; j<rangeElems.length; ++j) {
                this.elems[rangeElems[i]] = true;
            }
        } else {
            this.elems[args[i]] = true;
        }
    }
}


Set.prototype.hasKey = function(key) {
    return key in this.elems;
}


//Set.prototype.difference = function(other) {
    //elems = this.elems;
    //otherElems = other.elems;
    //
    //for (var key in otherElems) {
        //if (key in elems) {
            //delete elems[key];
        //}
    //}
    //return new Set(elems);
//}

function RT_replicate(count, val) {
    var ret = new Array(count);
    for (var i = 0; i < count; ++i) {
        ret [i] = val;
    }
    return ret;
}

function RT_range_check(val, left, right) {
    if (typeof(val)!="number") {
        throw "Range check failed: value is not a number";
    } else if (val < left || val > right) {
        throw "Range check failed: number is not in range";
    }
    return val - left;
}

function RT_idiv(a,b) {
    return Math.floor(a / b);
}

function RT_imod(a,b) {
    return a % b;
}

function RT_takeAddr(val) {
    return new Ptr(val);
}

function RT_CHARR2PCHAR(arr) {
    if (! (arr instanceof Array)) {
        throw "Not an array: can't create a PCHAR";
    }
    return new Ptr(arr.join(""));
}

function RT_STR2PCHAR(s) {
    if (! (typeof(s) == "string")) {
        throw "Not a string: can't create a PCHAR";
    }
    return new Ptr(s);
}

function RT_new(a) {
    return new Ptr(a);
}

function RT_SIZEOF(arr) {
    if (!(arr instanceof Array)) {
        throw "Not an array: can't return a size";
    }
    return arr.length;
}

function RT_shl(a,b) {
    return a * Math.pow(2, b);
}

function RT_pcharptr(s, i) {
    if (s instanceof Ptr) {
            //throw "Not a Ptr: can't dereference PChar";
        //}
        if (!(typeof(s.val) == "string")) {
            throw "Not a string: can't get a char";
        }
        if (i >= s.val.length || i < 0) {
            throw "Not in range: can't get a char";
        }
        return s.val[i];
    } else {
        // no checks
        return s[i];
    }
}

function RT_strcharat(s,i) {
    return s[i];
}

function RT_shr(a,b) {
    return Math.floor(a / Math.pow(2, b));
}

function RT_setof() {
    for (var i=0; i < arguments.length; ++i) {
        if (!((typeof(arguments[i])=="number") || 
              ((typeof(arguments[i]) == "string") && arguments[i].length == 1) || 
              (arguments[i] instanceof Range))) {
            throw "Wrong element for the set:" + arguments[i];
        }
    }
    return new Set(arguments);
}

function RT_in_set(key, s) {
    if (! (s instanceof Set)) {
        throw "Not a set: can't check the element";
    }
    return s.hasKey(key);
} 

function RT_mkrange(a,b) {
    return new Range(a,b);
}

function RT_isGenerator(obj) {
    if (obj) {
        return typeof(obj.next) == "function";
    } else {
        return false; // undefined is not a generator
    }
}

function RT_incdecptr(p) {
    // do nothing (strange)
}

function RT_PCHARR2PCHAR(p) {
    if (p instanceof Ptr) {
        return new Ptr(p.val.join(""));
    } else {
        throw "Cannot construct PChar";
    }
}

function RT_CHARR2STRING(p) {
    if (p instanceof Array) {
        return new p.join("");
    } else {
        throw "Cannot construct String";
    }
}

function RT_incchar(s) {
    if (typeof(s)=="string" && s.length == 1) {
        return String.fromCodeChar(s.charCodeAt(0));
    } else {
        throw "Cannot increment non char:" + s;
    }
}

//function RT_comparesets(s1,s2) {
    //if (s1 instanceof Set && s2 instanceof Set) {
        //var diff1 = s1.difference(s2);
        //var diff2 = s2.difference(s1);
        //return (diff1.__count__==0 && diff2.__count__ ==0);
    //} else {
        //throw "Cannot compare non sets:" + s1 + s2
    //}
//}

//alert(RT_replicate(10,3));
//var x = 10;
//alert(RT_range_check(x, 1,9));
//alert(RT_idiv(10,3));
//alert(RT_imod(10,3));
//alert(RT_takeAddr(x).val);
//alert(RT_CHARR2PCHAR(RT_replicate("s",10)).val);
//alert(RT_STR2PCHAR("asd").val);
//alert(RT_new(x).val);
//alert(RT_SIZEOF(RT_replicate("s",10)));
//alert(RT_shl(x,2));
//alert(RT_shr(x,1));
//alert(RT_pcharptr(RT_STR2PCHAR("asd"),2));
//console.log(RT_in_set(RT_setof(1,"3",5,5),"0"));
