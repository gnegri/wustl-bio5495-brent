(* Mathematica Raw Program *)

(* In all of this code, feel free to pass additional parameters, define additional local variables
   in each Module, and define additional functions, as you see fit. The structure given below is 
   meant to help you out but not to shackle you to a particular approach.*)

(* The input format is defined by the function readInput which is defined in the file tools.m *)
basicMM[input_, motifLength_, maxIterations_, accuracy_]:=
	Module[{inputLengths, nCounts, backgroundFreqs, backgroundPWM, motifPseudocounts, lambda, posteriors, motifPWM},
		(*Initialization*)
		inputLengths=Map[Length, input];
		nCounts=Total[Map[BinCounts[#, {Range[5]}] &, input]];
		backgroundFreqs=(nCounts + {1., 1., 1., 1.}) / (Total[nCounts] + 4.);
		motifPseudocounts=backgroundFreqs;
		(*Initialized to one motif expected per input sequence.*)
		lambda=Length[inputLengths] / Total[inputLengths];
		(*You need to write initializePWM*)
		motifPWM=initalizePWM[motifLength];
		backgroundPWM=Table[backgroundFreqs, {motifLength}];
		Do[{motifPWM, backgroundPWM, lambda, posteriors}=
			 updateProbs[input, backgroundFreqs, motifPWM, backgroundPWM, lambda, motifPseudocounts];
		   (*You will also have to write a function to measure the change in the parameters from one iteration
		     to the next. B&E use Euclidean distance*)
		   If[parameterChange[(*Define the arguments*)] < accuracy,
		   	  Return[]],
		   {maxIterations}];
		(*This is the return value. Exponentiate if you used a log representation.*)
		{motifPWM, lambda, posteriors}
	]

(*Write function initializePWM   *)
initalizePWM[motifLength_]:=
	Module[{},
	
	]
	
updateProbs[input_, backgroundFreqs_, motifPWM_, backgroundPWM_, lambda_, motifPseudocounts_]:=
	Module[{motifLength, posteriors, newMotifPWM, newBackgroundPWM, newLambda},
		motifLength=Length[motifPWM];
		posteriors=calculatePosteriors[input, motifPWM, backgroundPWM, lambda];
		newLambda=updateLambda[posteriors];{newMotifPWM, newBackgroundPWM} = updatePWM[motifLength, input, posteriors, motifPseudocounts];
	    {newMotifPWM, newBackgroundPWM, newLambda, posteriors}
	]
 		
(*Write the function calculatePosteriors.*)
calculatePosteriors[input_, motifPWM_, backgroundPWM_, lambda_]:=
	Module[{},
	
	]

(*Write function updateLambda.*)
updateLambda[posteriors_]:=
	Module[{},
	
	]

(*Write the function updatePWMs*)
updatePWM[motifLength_, input_, posteriors_, motifPseudocounts_]:=
	Module[{newMotifPWM, newBackgroundPWM},
		(*Write code here*)
	(*Return updated PWMs for the motif and the background.*)
		{newMotifPWM, newBackgroundPWM}	
	]
	