unit unit1;

interface

type s0 = record
        a : Integer;
        b : Integer;
        end;

type s = record
        s0r : s0;
        d : array[1..10] of s1;
        end;

procedure doit;

implementation

type q = record
        l : Integer;
        m : Integer;
        end;

const v1: s = (a: 5; b: 6);
var v2: q;

procedure doit;
begin
        writeln('doit ok!');
end;

begin
        v2.m := 5;
        v2.l := v2.m * 2;
        writeln('Zorroz ftw!',v2.l);
end.
