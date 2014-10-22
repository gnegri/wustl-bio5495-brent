(* decode
INPUT
 - observationSeq = [list] list of observations, e.g., {2,3,4,1,2}
 - states = [list] list of the state names, e.g., {m, h}
 - alphabet = [list] list of the HMM alphabet, e.g., {1, 2, 3, 4}
 - emissionMatrix = [matrix of size numStates x numAlphabet] the emission matrix.  
     Element eij = the probability of state i emitting letter j., e.g., {{0.4, 0.1, 0.1, 0.4}, {0.05, 0.4, 0.5, 0.05}}
 - transitionMatrix = [matrix of size numStates x numStates] the transition matrix.
     Element tij = the probability of state i transitioning to state j, e.g., {{0.99, 0.01}, {0.01, 0.99}}
     
OUTPUT
- stateSeq = [list] sequence of most likely states, e.g., {h,h,m,m,m}.  The length
     of this sequence equal to the length to the input observationSeq.  The ith element 
     of the list is the most likely state for the ith observation in observationSeq.
*)

(* TODO: write the decode function *)
decode[observationSeq_, states_, alphabet_, emissionMatrix_, transitionMatrix_, initialStateProbs_:{.5,.5}] := 
	Module[{numObs = Length[observationSeq], oldDelta, aMatrix, 
		aMatrices = {{{"mm","hm"},{"mh","hh"}}}, lastState, stateSeq}, 
		
		(*delta_0 {1, 2} = b_i(O_t) * P(q_t=S_i) *)
		oldDelta = emissionMatrix[[All,observationSeq[[1]]]]*initialStateProbs;
		
		Do[
			(*transition probabilites * delta_t-1; argument to be maximized*)
			aMatrix = Transpose[transitionMatrix*oldDelta];
			(* store to a list  *)
			AppendTo[aMatrices, aMatrix];
			(*b_i(O_t) * max(aMatrix), rest delta to this for next round*)
			oldDelta = emissionMatrix[[All, observationSeq[[i]]]]*Max @@@ aMatrix,
		{i,2,numObs}];
		
		(* Find last sequence state for backtrace *)
		lastState = Flatten[Position[oldDelta, Max[oldDelta]]][[1]];
		stateSeq = {states[[lastState]]};
		
		(* use previously calculated transition probabilites * delta_t-1*)
		Do[
			lastState = Flatten[Position[aMatrices[[-i, lastState]], Max[aMatrices[[-i, lastState]]]]][[1]];
			PrependTo[stateSeq, states[[lastState]]],
		{i,numObs-1}];
		
		stateSeq
	];


(* calculateAccuracy takes a state sequence genereted from mixed2.fa and calculates 
the number of correctly labeled states.  Note: this function only computes the
accuracy for the mixed2.fa observations.
INPUT
 - stateSeq = [list] list of state sequences, e.g., {h,m,h,m,m}
 
 OUTPUT
 - numCorrectStates = [int] number of correcly labeled states.

*)
	
calculateAccuracy[stateSeq_] := 
	Module[{keyStateSequence, numCorrectStates, percentCorrect},
	
	keyStateSequence = Flatten[Characters[ToLowerCase[Import["mixed2key.fa"]]]];
	numCorrectStates = Count[MapThread[Equal, {stateSeq, keyStateSequence}], True];

	percentCorrect = numCorrectStates/Length[stateSeq]*100 //N
];

(* readHMM takes a HMM text file and outputs the state names, the alphabet
the transition matrix, and emission matrix of the HMM

INPUT
 - file = path to a HMM file. Below is an exmple HMM file:
	2
	ACGT
	m h
	0.5 0.5
	0.4 0.1 0.1 0.4
	0.05 0.4 0.5 0.05
	0.99 0.01
	0.01 0.99

  (This is the same HMM as figure 1 in hmmDecode.nb.)
  
OUTPUT
 - states = [list] list of the state names, e.g., {m, h}
 - alphabet = [list] list of the HMM alphabet, e.g., {1, 2, 3, 4}
 - emissionMatrix = [matrix of size numStates x numAlphabet] the emission matrix.  
     Element eij = the probability of state i emitting letter j., e.g., {{0.4, 0.1, 0.1, 0.4}, {0.05, 0.4, 0.5, 0.05}}
 - transitionMatrix = [matrix of size numStates x numStates] the transition matrix.
     Element tij = the probability of state i transitioning to state j, e.g., {{0.99, 0.01}, {0.01, 0.99}}

*)

readHMM[file_] := 
	Module[{a, numStates, alphabet, numAlphabet, firstStateIndex, lastStateIndex,
		states, firstStateProbIndex, lastStateProbIndex, initialStateProbs, 
		firstEmissionIndex, lastEmissionIndex, emissionList, emissionMatrix,
		firstTransitionIndex, lastTransitionIndex, transitionList, transitionMatrix}, 
		
	a = Import[file, {"Text", "Words"}];
	
	numStates = ToExpression[a[[1]]]; (* Use ToExpression to convert from character to number *)

	alphabet = Characters[a[[2]]];
	numAlphabet = Length[alphabet];

	firstStateIndex = 3;
	lastStateIndex = firstStateIndex + numStates - 1;
	states = a[[firstStateIndex ;; lastStateIndex]];

	firstStateProbIndex = lastStateIndex + 1;
	lastStateProbIndex = firstStateProbIndex + numStates - 1;
	initialStateProbs = ToExpression[a[[firstStateProbIndex ;; lastStateProbIndex]]];

	firstEmissionIndex = lastStateProbIndex + 1;
	lastEmissionIndex = firstEmissionIndex + numStates*numAlphabet - 1;
	emissionList = ToExpression[a[[firstEmissionIndex ;; lastEmissionIndex]]];
	emissionMatrix = Partition[emissionList, numAlphabet];

	firstTransitionIndex = lastEmissionIndex + 1;
	lastTransitionIndex = firstTransitionIndex + numStates*numStates - 1;
	transitionList = ToExpression[a[[firstTransitionIndex ;; lastTransitionIndex]]];
	transitionMatrix = Partition[transitionList, numStates];
	
	{states, alphabet, emissionMatrix, transitionMatrix, initialStateProbs}

];

	
(* readFasta reads a fasta file and outputs the nucleotide sequence converted to numbers
INPUT
- fastaFile = path to fasta file

OUTPUT
- input = [list] list of bases in fasta.  bases are translated form ACGT to 1234.
    e.g., {1,3,2,4,2}
*)
readFasta[fastaFile_]:=
	Module[{input},
		input=Map[Characters, Import[fastaFile]] /. {"A"->1, "C"->2, "G"->3, "T"->4};
		input = Flatten[input]
	];