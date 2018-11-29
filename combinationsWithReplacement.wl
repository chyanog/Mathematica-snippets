(* ::Package:: *)

ClearAll[combinationsWithReplacement];

combinationsWithReplacement[L_List, k_Integer] := 
  Extract[L, {# - Range[0, k - 1]} & /@ Subsets[Range[Length[L] + k - 1], {k}]];



comb = combinationsWithReplacement[{a, b, c}, 3]

(* {{a, a, a}, {a, a, b}, {a, a, c}, {a, b, b}, {a, b, c}, {a, c, c}, {b, b, b}, {b, b, c}, {b, c, c}, {c, c, c}} *)



Length[comb] == Function[{i, j}, (i + j - 1)!/((i - 1)! j!)][3, 3]

(* True *)



r1 = combinationsWithReplacement[Range[3], 3];
r2 = Table[{a, b, c}, {a, 3}, {b, a, 3}, {c, b, 3}]~Flatten~2;
r3 = Tuples[Range[3], 3] // DeleteDuplicatesBy[Sort];
r1 == r2 == r3

(* True *)
