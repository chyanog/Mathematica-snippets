
ClearAll[primes];
primes[n_Integer?Positive] :=
  Module[{p = Range[1, n, 2]},
   p[[1]] = 2;
   Do[If[p[[(k + 1)/2]] != 0, p[[(k^2 + 1)/2 ;; ;; k]] = 0],
    {k, 3, n^.5, 2}];
   SparseArray[p]["NonzeroValues"]
   ];
   

(r1 = primes[10^7]) // Length // AbsoluteTiming

(* {0.194357, 664579} *)



(r2 = Prime@Range@PrimePi[10^7]) // Length // AbsoluteTiming

(* {1.65236, 664579} *)



r1 == r2

(* True *)