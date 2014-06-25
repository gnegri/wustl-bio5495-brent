(* ::Package:: *)

(* Mathematica package *)

(* dice___ code as backend *)
dicePosterior[binCounts_, type1Prior_, type2Prior_, faceProbs1_, faceProbs2_] := 
	Module[{pBgT1, pBgT2}, 

 	pBgT1 = Product[expHelper[faceProbs1[[j]],binCounts[[j]]], {j, 4}];
	pBgT2 = Product[expHelper[faceProbs2[[k]],binCounts[[k]]], {k, 4}];

	(pBgT1*type1Prior)/(pBgT1*type1Prior + pBgT2*type2Prior)
]

expHelper[a_, b_] := If[a==0 && b==0, 1, a^b];
 (*
diceEM[sample_, maxIterations_, accuracy_]:=
	Module[{binCounts, old, new, iter=0, 
		oldType1Prob, oldType2Prob, oldFaceProbs1, oldFaceProbs2, 
		    newType1Prob, newType2Prob, newFaceProbs1, newFaceProbs2},
		    
		(* Initialize the local variables here.*)
		binCounts = BinCounts[sample,{1,5,1}];
		oldFaceProbs1 = {1/6,1/6,1/3,1/3};
		oldFaceProbs2 = {1/4,1/4,1/4,1/4};
		oldType1Prob  = .4;
		oldType2Prob  = .6;
		old = {oldType1Prob,oldType2Prob,oldFaceProbs1,oldFaceProbs2};

		(* Loop here until either maxIterations has been reached or the sum of the absolute values of the changes from one 
		   iteration to the next in all estimated parameters is less than accuracy.
		   On each iteration, call updateProbs, passing in the old values, to set the new values. 
		   Then test whether the termination conditions have been met (Return[] breaks a loop returning its argument).
		   Finally, if termination conditions have not been met, set old values to be the same as the new values.
		 *)
		While[True,
			new = updateProbs[binCounts,oldType1Prob,oldType2Prob,oldFaceProbs1,oldFaceProbs2];
			{newType1Prob, newType2Prob, newFaceProbs1, newFaceProbs2} = new;
			iter++;
			If[iter>maxIterations || Total[Total[Abs[new-old]]] <= accuracy || newType1Prob==0 || newType2Prob==0,
				Return[
					newType2Prob
				]
			];
			{oldType1Prob, oldType2Prob, oldFaceProbs1, oldFaceProbs2} = new;
			old = new;
		]
	]

updateProbs[binCounts_, oldType1Prob_, oldType2Prob_, oldFaceProbs1_, oldFaceProbs2_] :=
	Module[{posteriorType1, faceCounts1, faceCounts2, 
		newType1Prob, newType2Prob, newFaceProbs1, newFaceProbs2},
		
		(*Create list of posterior probabilities of a Type1 die having been rolled on each draw by calling your dicePosteriors,
		  which you should paste in to this file. *)
		newType1Prob  = dicePosterior[binCounts,oldType1Prob,oldType2Prob,oldFaceProbs1,oldFaceProbs2];
		newType2Prob  = 1 - newType1Prob;

		(* Now use the posteriors to calculate EXPECTED counts for each die and each face in the sample. *)
		faceCounts1 = binCounts*posteriorType1;
		faceCounts2 = binCounts*(1-posteriorType1);
	
		(* Finally, use these counts to compute maximum likelihood estimates for the parameters 
			and return these estimates in a list. *)
		newFaceProbs1 = faceCounts1/Total[faceCounts1];
		newFaceProbs2 = faceCounts2/Total[faceCounts2];
		{newType1Prob,newType2Prob,newFaceProbs1,newFaceProbs2}
	]
*)

diceEM[sample_, maxIterations_, accuracy_]:=
	Module[{numFaces, draws, binCounts, old, new, iter=0, 
		oldType1Prob, oldType2Prob, oldFaceProbs1, oldFaceProbs2, 
		    newType1Prob, newType2Prob, newFaceProbs1, newFaceProbs2},
		    
		(* Initialize the local variables here.*)
		numFaces = Max[sample];
		draws = Dimensions[sample][[1]];
		binCounts = Table[BinCounts[sample[[i]],{1,numFaces+1,1}],{i,draws}];
		oldFaceProbs1 = Normalize[RandomInteger[{1,10}, numFaces], Total];
		oldFaceProbs2 = Normalize[RandomInteger[{1,10}, numFaces], Total];
		oldType1Prob  = .4;
		oldType2Prob  = .6;
		old = {oldType1Prob,oldType2Prob,oldFaceProbs1,oldFaceProbs2};

		(* Loop here until either maxIterations has been reached or the sum of the absolute values of the changes from one 
		   iteration to the next in all estimated parameters is less than accuracy.
		   On each iteration, call updateProbs, passing in the old values, to set the new values. 
		   Then test whether the termination conditions have been met (Return[] breaks a loop returning its argument).
		   Finally, if termination conditions have not been met, set old values to be the same as the new values.
		 *)
		While[True,
			new = updateProbs[binCounts,oldType1Prob,oldType2Prob,oldFaceProbs1,oldFaceProbs2];
			{newType1Prob, newType2Prob, newFaceProbs1, newFaceProbs2} = new;
			iter++;
			If[iter>maxIterations || Total[Total[Abs[new-old]]] <= accuracy,
				Return[
					{newType1Prob, newType2Prob, newFaceProbs1, newFaceProbs2, binCounts}
				],
			{oldType1Prob, oldType2Prob, oldFaceProbs1, oldFaceProbs2} = new;
			old = new;
			];
		]
	]

updateProbs[binCounts_, oldType1Prob_, oldType2Prob_, oldFaceProbs1_, oldFaceProbs2_] :=
	Module[{posteriorType1, faceCounts1, faceCounts2, draws, 
		newType1Prob, newType2Prob, newFaceProbs1, newFaceProbs2},
		
		(*Create list of posterior probabilities of a Type1 die having been rolled on each draw by calling your dicePosteriors,
		  which you should paste in to this file. *)
		draws = Dimensions[binCounts][[1]];
		posteriorType1 = Table[dicePosterior[binCounts[[i]],oldType1Prob,oldType2Prob,oldFaceProbs1,oldFaceProbs2],{i,draws}];

		(* Now use the posteriors to calculate EXPECTED counts for each die and each face in the sample. *)
		faceCounts1 = Total[binCounts*posteriorType1];
		faceCounts2 = Total[binCounts*(1-posteriorType1)];
	
		(* Finally, use these counts to compute maximum likelihood estimates for the parameters 
			and return these estimates in a list. *)
		newType1Prob = Total[posteriorType1]/draws;
		newType2Prob  = 1 - newType1Prob;
		newFaceProbs1 = faceCounts1/Total[faceCounts1];
		newFaceProbs2 = faceCounts2/Total[faceCounts2];
		
		{newType1Prob,newType2Prob,newFaceProbs1,newFaceProbs2}
	]

scoreDNAClasses[outputFile_, keyFile_]:=
	Module[{predictions, truth},
		predictions=Get[outputFile];	
		truth=Select[Characters[Import[keyFile, "TEXT"]],
  			Not[StringMatchQ[#, WhitespaceCharacter]] &]
  			 /. {"H" -> 1, "M" -> 0};
  		Round[1-Total[Abs[predictions - truth]] / Length[truth], 0.01]]

classifyDNA[fastaFile_, outputFile_] := 
	Module[{seqs,numSeqs,hVSm={},out=outputFile,j,k,
			type1Prob,type2Prob,faceProbs1,faceProbs2,binCounts},
		seqs = fastaFile;
		numSeqs = Length[seqs];
		seqs = Table[Characters[seqs[[j]]]/.{"A"->1,"T"->2,"C"->3,"G"->4},{j,numSeqs}];
		{type1Prob,type2Prob,faceProbs1,faceProbs2,binCounts} = diceEM[seqs, 100, .01];
		Do[
			AppendTo[hVSm,Round[dicePosterior[binCounts[[k]],.49,.51,{.25,.25,.25,.25},{.45,.31,.1,.14}]]],
		{k,numSeqs}];
		Export[out,hVSm];
	]


(*fastaFile  = "~/Dropbox/School/mblab/wustl-bio5495-brent.git/trunk/mod01Probability/classifyDNAAssignment/mixed.fa";*)
outputFile = "~/Dropbox/School/mblab/wustl-bio5495-brent.git/trunk/mod01Probability/classifyDNAAssignment/out.txt";
keyFile    = "~/Dropbox/School/mblab/wustl-bio5495-brent.git/trunk/mod01Probability/classifyDNAAssignment/answers.txt";
classifyDNA[fastaFile,outputFile]
(*scoreDNAClasses[outputFile,keyFile]*)


fastaFile = Import["~/Dropbox/School/mblab/wustl-bio5495-brent.git/trunk/mod01Probability/classifyDNAAssignment/mixed.fa"];


keyFile2 = Import[keyFile];
keyFileOut = "~/Dropbox/School/mblab/wustl-bio5495-brent.git/trunk/mod01Probability/classifyDNAAssignment/kfout.txt";
keyFile2 = Characters[keyFile2] /.{"H" -> 1, "M" -> 0};
Export[keyFileOut,keyFile2]


scoreDNAClasses2[outputFile_, keyFile_]:=
	Module[{predictions, truth},
		predictions=IntegerDigits[Get[outputFile]];
		truth=Get[keyFile];
  	Round[1-Total[Abs[predictions - truth]] / Length[predictions], 0.01]]
scoreDNAClasses2[outputFile,keyFileOut]
