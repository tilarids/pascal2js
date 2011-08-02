function f() {
    var z = 5;
    for (var q = 1; q < 20; q++) {
        z = z + q;
        if (z % 2 == 0) yield (z * 3); else yield (z / 5);
    }
}

var g1 = f();
try {
    while (true) {
        print(g1.next());
    }
} catch (ex) {   /* end of sequence */
}

var yi = SOME_ROUTINE();
if (isGenerator(yi)) {
    for (var i in yi) yield i;
}

function x() {
    something1();
    yield 1;
    something2();
    yield 2;
    something3();
}

function x1_1(ctx) {
    something1(ctx.context++);
    return 1;
}
function x1_2(ctx) {
    something2(ctx.context++);
    return 2;
}
function x1_3(ctx) {
    something3(ctx.context++);
    throw new GenerationStop();
}
function x() {
    var ctx = {context: 2}
    return builtin_iterate_parts(ctx, [x1_1, x1_2, x1_3]);
}
