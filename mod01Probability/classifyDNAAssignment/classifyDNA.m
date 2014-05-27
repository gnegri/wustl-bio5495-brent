(* Mathematica package *)

scoreDNAClasses[outputFile_, keyFile_]:=
	Module[{predictions, truth},
		predictions=Get[outputFile];
		truth=Select[Characters[Import[keyFile, "TEXT"]],
  					 Not[StringMatchQ[#, WhitespaceCharacter]] &]
  					 /. {"H" -> 1, "M" -> 0};
  		Round[1 - Total[Abs[predictions - truth]] / Length[truth], 0.01]]

classifyDNA[fastaFile_, outputFile_]:=