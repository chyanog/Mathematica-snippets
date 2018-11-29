(* ::Package:: *)

(* 不考虑去重 *)

ClearAll[calc24];

calc24[L : {n1_, n2_, n3_, n4_}] :=
  Cases[
    Tuples[{Tuples[{Plus, Subtract, Times, Divide}, 3], Permutations[L]}] /.
     {{o1_, o2_, o3_}, {a_, b_, c_, d_}} ->
      HoldForm /@ {
        a~o1~b~o2~c~o3~d,
        (a~o1~b)~o2~(c~o3~d),
        (a~o1~(b~o2~c))~o3~d,
        a~o1~(b~o2~(c~o3~d)),
         a~o1~((b~o2~c)~o3~d)
        },
    e_ /; ReleaseHold@e == 24, {2}
    ] // Map[Map[InputForm]] // Quiet;

calc24[{1, 5, 7, 9}]

(* {(1 - 7)*(5 - 9),(5 - 9)*(1 - 7),(7 - 1)*(9 - 5),(9 - 5)*(7 - 1)} *)





(* 考虑去重 *)

ClearAll[calc24, gps];

gps = Groupings[Permutations[{a, b, c, d}], {Plus -> {2, Orderless}, Subtract -> 2, Times -> {2, Orderless}, Divide -> 2}, HoldForm] // DeleteDuplicatesBy[Factor@*ReleaseHold];

calc24[L : {n1_, n2_, n3_, n4_}] := 
  gps /. Thread[{a, b, c, d} -> L] // Pick[#, ReleaseHold[#] /. ComplexInfinity -> 0, 24] & // DeleteDuplicates // Map[Map[InputForm]] // Quiet;


calc24[{1, 5, 7, 9}]
(* {(1 - 7)*(5 - 9)} *)
