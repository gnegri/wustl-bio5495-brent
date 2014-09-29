(* Mathematica package *)

(*readInput reads a fasta file, converts nucleotides to numbers, filters out
  any input sequences that are shorter than the motif being sought, and
  adds the reverse complements of the sequences if includeReverseStrand is True.
  When the reverse complements are added, the order of in which the reverse-complement
  sequences appear is also reversed, so in the  value sequences[[-i]] is the 
  reverse complement of sequences[[i]].*)
readInput[fastaFile_, minSeqLength_, includeReverseStrand_:False]:=
	Module[{input},
		input=Map[Characters, Import[fastaFile]] /. {"A"->1, "C"->2, "G"->3, "T"->4};
		input=Select[input, Length[#] >= minSeqLength &];
		If[includeReverseStrand==True,
		   Join[input, Reverse[Map[reverseComplement[#] &, input]]],
		   input]
	]

reverseComplement[sequence_]:=
	Replace[Reverse[sequence], {1->4, 4->1, 2->3, 3->2}, 1]

pwmConsensus[pwm_]:=
	Map[maxPosition, pwm] /. {1->"A", 2->"C", 3->"G", 4->"T"}
	
maxPosition[list_]:=
	Module[{max=-Infinity, position=0},
		Do[If[list[[i]] > max,
			  max=list[[i]];
			  position=i],
		  {i, 1, Length[list]}];
    position
	]