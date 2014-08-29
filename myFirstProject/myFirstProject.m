(* Write a function called fibonacciSequence that returns a list of  
Subscript[F, 0] through Subscript[F, n].  (Note, since we're starting our list 
with n = 0, n + 1 numbers will be returned.  Note: you cannot use the built-in 
function Fibonacci! *)

fibonacciSequence[n_] := 
	Module[{fib={0,1}},
		AppendTo[fib, fib[[#-1]]+fib[[#-2]]] &/@ Range[3,n+1];
		fib[[1;;n+1]]
]

(* Write a function called approximateGoldenRatio that returns a list of the 
first n ratios of sucessive Fibonacci numbers. Since the first ratio is 
undefined, start your list ratios with Subscript[F, 2]/Subscript[F, 1] *)	
approximateGoldenRatio[n_] := 
	Module[{fib=fibonacciSequence[n]},
		 fib[[#+1]]/fib[[#]] &/@ Range[2,n]
]

