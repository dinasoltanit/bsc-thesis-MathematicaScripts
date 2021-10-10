

BeginPackage["OrthogonalPolynomials`TrigonometricSemiInteger`",{"OrthogonalPolynomials`Kernel`Common`"}]


aCreateTTRSinPeriodicPi::usage="aCreateTTRSinPeriodicPi[n,weight,noLeg,weLeg,ops] n number of TTR to be constructed, weight \
	weight function, weLeg and noLeg weights nad nodes in Legendre formula, ops there is only on option WorkingPrecision.";
aCreateTTRSinPeriodicPi::mes1="QuadWorkingPrecision is smaller then WorkingPrecision, using QuadWorkingPrecision = WorkingPrecision+10";
aCreateTTRSinPeriodicPi::mes2="QuadWorkingPrecision is smaller then QuadPrecision, using QuadWorkingPrecision = QuadPrecision +10";
aCreateTTRCos::usage="aCreateTTRCos[n,weight,noLeg,weLeg,ops] n number of TTR to be constructed, weight weight function, \
	weLeg and noLeg weights nad nodes in Legendre formula, ops there is only on option WorkingPrecision.";
aCreateTTRCos::mes1="QuadWorkingPrecision is smaller then WorkingPrecision, using QuadWorkingPrecision = WorkingPrecision+10";
aCreateTTRCos::mes2="QuadWorkingPrecision is smaller then QuadPrecision, using QuadWorkingPrecision = QuadPrecision +10";


Options[aCreateTTRSinPeriodicPi]:={WorkingPrecision->$MachinePrecision,QuadWorkingPrecision->$MachinePrecision,
	QuadPrecision->$MachinePrecision-10};
Options[aCreateTTRCos]:={WorkingPrecision->$MachinePrecision,QuadWorkingPrecision->$MachinePrecision,QuadPrecision->$MachinePrecision-10};
Options[aCreateTrigPolynomial]:={WorkingPrecision->$MachinePrecision,Type->Cosine};
Options[aCreateCosPolynomial]:={WorkingPrecision->$MachinePrecision};
Options[aCreateSinPolynomial]:={WorkingPrecision->$MachinePrecision};
Attributes[aCreateSinPolynomial]={ReadProtected};
Attributes[aCreateCosPolynomial]={ReadProtected};
Attributes[aCreateTrigPolynomial]={ReadProtected};
Attributes[aCreateTTRSinPeriodicPi]={ReadProtected};
Attributes[aCreateTTRCos]={ReadProtected};

Begin["`Private`"]

aCreateTTRSinPeriodicPi[n_?IntegerQ,w_,nQuad_?IntegerQ,{Name_?polyQ,polyOps___},ops___Rule]:=
Module[{noLeg,weLeg,wrprec,quadwrprec,quadprec},
	{wrprec,quadwrprec,quadprec}={WorkingPrecision,QuadWorkingPrecision,QuadPrecision}/. {ops} /. Options[aCreateTTRSin];
	If[quadwrprec<wrprec,
		Message[aCreateTTRSinPeriodicPi::mes1];
		quadwrprec=wrprec+10;
	];
	If[quadwrprec<quadprec+10,
		Message[aCreateTTRSinPeriodicPi::mes2];
		quadwrprec=quadprec+10;
	];
	{noLeg,weLeg}=OrthogonalPolynomials`NodesWeights`aGaussianNodesWeights[nQuad,{Name,polyOps},
		WorkingPrecision->quadwrprec,Precision->quadprec];
	Return[aCreateTTRSinPeriodicPi[n,w,noLeg,weLeg,ops]];
];

aCreateTTRCos[n_?IntegerQ,w_,nQuad_?IntegerQ,{Name_?polyQ,polyOps___},ops___Rule]:=
Module[{noLeg,weLeg,wrprec,quadwrprec,quadprec},
	{wrprec,quadwrprec,quadprec}={WorkingPrecision,QuadWorkingPrecision,QuadPrecision}/. {ops} /. Options[aCreateTTRCos];
	If[quadwrprec<wrprec,
		Message[aCreateTTRCos::mes1];
		quadwrprec=wrprec+10;
	];
	If[quadwrprec<quadprec+10,
		Message[aCreateTTRCos::mes2];
		quadwrprec=quadprec+10;
	];
	{noLeg,weLeg}=OrthogonalPolynomials`NodesWeights`aGaussianNodesWeights[nQuad,{Name,polyOps},
		WorkingPrecision->quadwrprec,Precision->quadprec];
	Return[aCreateTTRCos[n,w,noLeg,weLeg,ops]];
];

aCreateTTRSinPeriodicPi[n_?IntegerQ, w_, noLeg1_?VectorQ, weLeg1_?VectorQ, ops___Rule] :=
Module[{x, cosA, sinA, normC = {}, normS = {}, wrprec, we, cos, alpha = {0}, beta = {0}, gama = {0}, delta = {0}, noLeg, weLeg},
	noLeg = Pi noLeg1; weLeg = Pi weLeg1;
      	{wrprec} = {WorkingPrecision} /. {ops} /. Options[aCreateTTRSinPeriodicPi];
        Block[{$MinPrecision = wrprec},
		we = w /@ noLeg;
            	cos = Sin[noLeg];
            	cosA = {Cos[noLeg/2]}; sinA = {Sin[noLeg/2]};
            	AppendTo[normC, weLeg.(we(cosA[[-1]])^2)];
            	AppendTo[normS, weLeg.(we(sinA[[-1]])^2)];
            	
            	pom = LinearSolve[{{normC[[1]], weLeg.(we(cosA[[1]])(sinA[[1]]))},
			{weLeg.(we(cosA[[1]])(sinA[[1]])), normS[[1]]}},
			{2weLeg.(we cos cosA[[1]] cosA[[1]]), 2weLeg.(we cos cosA[[1]] sinA[[1]])}];
            	
            	AppendTo[alpha, pom[[1]]];
            	AppendTo[beta, pom[[2]]];
            	
            	AppendTo[sinA, (2 cos - alpha[[-1]])cosA[[1]] - 
            	beta[[-1]] sinA[[1]]];
            	
            	pom = LinearSolve[{{normC[[1]], weLeg.(we(cosA[[1]])(sinA[[1]]))},
                  	{weLeg.(we(cosA[[1]])(sinA[[1]])), 
			normS[[1]]}}, {2weLeg.(we cos cosA[[1]] sinA[[1]]), 2weLeg.(we cos sinA[[1]] sinA[[1]])}];
            	
            	AppendTo[gama, pom[[1]]];
            	AppendTo[delta, pom[[2]]];
            	
            	AppendTo[cosA, -(2 cos - delta[[-1]])sinA[[1]] + gama[[-1]] cosA[[1]]];
            	
            	AppendTo[normC, weLeg.(we(cosA[[-1]])^2)];
            	AppendTo[normS, weLeg.(we(sinA[[-1]])^2)];
            	
            	For[i = 1, i <= n, ++i,
            		
              		pom = LinearSolve[{{normC[[-2]], weLeg.(we cosA[[-2]]sinA[[-2]])}, 
				{weLeg.(we cosA[[-2]]sinA[[-2]]), normS[[-2]]}}, 
				{2weLeg.(we cos cosA[[-1]] cosA[[-2]]), 2weLeg.(we cos cosA[[-1]] sinA[[-2]])}];
              		AppendTo[alpha, pom[[1]]];
              		AppendTo[beta, pom[[2]]];
              		
              		pom = LinearSolve[{{normC[[-1]], weLeg.(we cosA[[-1]]sinA[[-1]])},
				{weLeg.(we cosA[[-1]]sinA[[-1]]), normS[[-1]]}}, 
				{2weLeg.(we cos cosA[[-1]] cosA[[-1]]), 2weLeg.(we cos cosA[[-1]] sinA[[-1]])}];
              		AppendTo[alpha, pom[[1]]];
              		AppendTo[beta, pom[[2]]];
              		
              		pom = LinearSolve[{{normS[[-2]], weLeg.(we cosA[[-2]]sinA[[-2]])}, 
				{weLeg.(we cosA[[-2]]sinA[[-2]]), normC[[-2]]}}, 
				{2weLeg.(we cos sinA[[-1]] sinA[[-2]]), 2weLeg.(we cos sinA[[-1]] cosA[[-2]])}];
              		AppendTo[gama, pom[[2]]];
              		AppendTo[delta, pom[[1]]];
	             	
        	      	pom = LinearSolve[{{normS[[-1]], weLeg.(we cosA[[-1]]sinA[[-1]])}, 
				{weLeg.(we cosA[[-1]]sinA[[-1]]),normC[[-1]]}}, 
				{2weLeg.(we cos sinA[[-1]] sinA[[-1]]), 2weLeg.(we cos cosA[[-1]] sinA[[-1]])}];
              		AppendTo[gama, pom[[2]]];
              		AppendTo[delta, pom[[1]]];
			
AppendTo[sinA, (2 cos - alpha[[-1]])cosA[[-1]] - beta[[-1]] sinA[[-1]] - alpha[[-2]] cosA[[-2]] - beta[[-2]]sinA[[-2]]];
 			AppendTo[normS, weLeg.(we (sinA[[-1]])^2)];
              		
AppendTo[cosA, -(2 cos - delta[[-1]]) sinA[[-2]] + gama[[-1]] cosA[[-1]] + delta[[-2]] sinA[[-3]] + gama[[-2]]cosA[[-2]]];
             		AppendTo[normC, weLeg.(we (cosA[[-1]])^2)];
		];
	];
        bETA =  Table[{{gama[[2k + 1]], delta[[2k + 1]]}, {-alpha[[2k + 1]], -beta[[2k + 1]]}}, {k, 0, Length[alpha]/2 - 1}];
        aLPHA = Table[{{-gama[[2k]], -delta[[2k]]}, {alpha[[2k]], beta[[2k]]}}, {k, 1, Length[alpha]/2}];
      	For[i = 1, i <= Length[aLPHA], ++i,
        	If[Mod[i, 2] == 0, 
			aLPHA[[i]] = {{0, 1}, {-1, 0}}.aLPHA[[i]]
		, 
			aLPHA[[i]] = {{0, -1}, {1, 0}}.aLPHA[[i]];
		]; 
	];
      	aLPHA = Table[aLPHA[[k]], {k, 1, Length[aLPHA]}];
      	Return[{aLPHA, bETA}];
];

aCreateTTRCos[n_?IntegerQ, w_, noLeg1_?VectorQ, weLeg1_?VectorQ, ops___Rule] :=
Module[{x, cosA, sinA, normC = {}, normS = {}, wrprec, we, cos, alpha = {0}, beta = {0}, gama = {0}, delta = {0},weLeg,noLeg},
      	{wrprec} = {WorkingPrecision} /. {ops} /. Options[aCreateTTRCos];
      	Block[{$MinPrecision = wrprec},
		weLeg = Pi weLeg1; noLeg = Pi(noLeg1);
        	we = w /@ noLeg;
        	cos = Cos[noLeg];
        	cosA = {Cos[noLeg/2]}; sinA = {Sin[noLeg/2]};
        	AppendTo[normC, weLeg.(we(cosA[[-1]])^2)];
        	AppendTo[normS, weLeg.(we(sinA[[-1]])^2)];
        	
        	pom = LinearSolve[{{normC[[1]], weLeg.(we(cosA[[1]])(sinA[[1]]))},
        	      	{weLeg.(we(cosA[[1]])(sinA[[1]])), normS[[1]]}}, {2weLeg.(we cos cosA[[1]] cosA[[1]]), 
			2weLeg.(we cos cosA[[1]] sinA[[1]])}];
        	
        	AppendTo[alpha, pom[[1]]];
        	AppendTo[beta, pom[[2]]];
        	
        	AppendTo[cosA, (2 cos - alpha[[-1]])cosA[[1]] - beta[[-1]] 
        	sinA[[1]]];
        	
        	pom = LinearSolve[{{normC[[1]], weLeg.(we(cosA[[1]])(sinA[[1]]))},
        		{weLeg.(we(cosA[[1]])(sinA[[1]])), normS[[1]]}}, {2weLeg.(we cos cosA[[1]] sinA[[1]]), 
			2weLeg.(we cos sinA[[1]] sinA[[1]])}];
        	
        	AppendTo[gama, pom[[1]]];
        	AppendTo[delta, pom[[2]]];
        	
        	AppendTo[sinA, (2 cos - delta[[-1]])sinA[[1]] - gama[[-1]] cosA[[1]]];
        	
        	
        	AppendTo[normC, weLeg.(we(cosA[[-1]])^2)];
        	AppendTo[normS, weLeg.(we(sinA[[-1]])^2)];
        	
        	
        	For[i = 1, i <= n, ++i,
        		pom = LinearSolve[{{normC[[-2]], weLeg.(we cosA[[-2]]sinA[[-2]])}, 
				{weLeg.(we cosA[[-2]] sinA[[-2]]), normS[[-2]]}}, 
				{2weLeg.(we cos cosA[[-1]] cosA[[-2]]), 2weLeg.(we cos cosA[[-1]] sinA[[-2]])}];
          		AppendTo[alpha, pom[[1]]];
          		AppendTo[beta, pom[[2]]];
          		
          		pom = LinearSolve[{{normC[[-1]], weLeg.(we cosA[[-1]] sinA[[-1]])}, 
				{weLeg.(we cosA[[-1]]sinA[[-1]]), normS[[-1]]}}, {2weLeg.(we cos cosA[[-1]] cosA[[-1]]), 
				2weLeg.(we cos cosA[[-1]] sinA[[-1]])}];
          		AppendTo[alpha, pom[[1]]];
          		AppendTo[beta, pom[[2]]];
 		        
          		
            		pom = LinearSolve[{{normS[[-2]], weLeg.(we cosA[[-2]] sinA[[-2]])}, {weLeg.(we cosA[[-2]]sinA[[-2]]), 
				normC[[-2]]}}, {2weLeg.(we cos sinA[[-1]] sinA[[-2]]),
                           	2weLeg.(we cos sinA[[-1]] cosA[[-2]])}];
          		AppendTo[gama, pom[[2]]];
          		AppendTo[delta, pom[[1]]];
          		
          		pom = LinearSolve[{{normS[[-1]], weLeg.(we cosA[[-1]]sinA[[-1]])}, 
				{weLeg.(we cosA[[-1]]sinA[[-1]]), normC[[-1]]}}, 
				{2weLeg.(we cos sinA[[-1]] sinA[[-1]]), 2weLeg.(we cos cosA[[-1]] sinA[[-1]])}];
          		AppendTo[gama, pom[[2]]];
          		AppendTo[delta, pom[[1]]];
          		
AppendTo[ cosA, (2 cos - alpha[[-1]]) cosA[[-1]] - beta[[-1]] sinA[[-1]] - alpha[[-2]] cosA[[-2]] - beta[[-2]]sinA[[-2]]];
          		AppendTo[normC, weLeg.(we (cosA[[-1]])^2)];
          		
AppendTo[sinA, (2 cos - delta[[-1]])sinA[[-1]] - gama[[-1]] cosA[[-2]] - delta[[-2]] sinA[[-2]] - gama[[-2]]cosA[[-3]]];
          		AppendTo[normS, weLeg.(we (sinA[[-1]])^2)];
          	];
       		
	];
      	bETA = Table[{{alpha[[2k + 1]], beta[[2k + 1]]}, {gama[[2k + 1]], delta[[2k + 1]]}}, {k, 0, Length[alpha]/2 - 1}];
      	aLPHA = Table[{{alpha[[2k]], beta[[2k]]}, {gama[[2k]], delta[[2k]]}}, {k, 1, Length[alpha]/2}];
      	Return[{aLPHA, bETA}];
];

aCreateCosPolynomial[n_,alphaMatrix_,betaMatrix_,x_,ops___]:=
Module[{trigPol,wrprec,k},
	{wrprec} = {WorkingPrecision} /. {ops} /. Options[aCreateCosPolynomial];
  	Block[{$MinPrecision = wrprec},
  	trigPol = {{Cos[x/2], Sin[x/2]}};
  	
    		AppendTo[trigPol, TrigReduce[({{2 Cos[x], 0}, {0, 2 Cos[x]}} - alphaMatrix[[1]]).trigPol[[1]]]];
    		
    		For[k = 2, k <= n, k++,
AppendTo[trigPol, TrigReduce[({{2Cos[x], 0}, {0, 2 Cos[x]}} - alphaMatrix[[k]]).trigPol[[-1]] - betaMatrix[[k]].trigPol[[-2]]]];
		];
    	];
	Return[trigPol];
];

aCreateSinPolynomial[n_,alphaMatrix_,betaMatrix_,x_,ops___]:=
Module[{trigPol,wrprec,k},
    	{wrprec} = {WorkingPrecision} /. {ops} /. Options[aCreateSinPolynomial];
    	Block[{$MinPrecision = wrprec},
		trigPol = {{Cos[x/2], Sin[x/2]}};
        	
        	AppendTo[trigPol, TrigReduce[({{-2Sin[x], 0}, {0, -2 Sin[x]}} - alphaMatrix[[1]]).trigPol[[1]]]];
        
        	For[k = 2, k <= n, k++, 
AppendTo[trigPol, TrigReduce[({{-2Sin[x], 0}, {0, -2 Sin[x]}} - alphaMatrix[[k]]).trigPol[[-1]] - betaMatrix[[k]].trigPol[[-2]]]];
          	];
        ];
    	Return[trigPol];
];

aCreateTrigPolynomial[n_,aLPHA_,bETA_,x_,ops___]:=
Module[{type},
	{type}={Type}/.{ops}/.Options[aCreateTrigPolynomial];
	If[ToString[type] === "Cosine",
		Return[aCreateCosPolynomial[n,aLPHA,bETA,x,ops,Sequence@@Options[aCreateTrigPolynomial]]];
	];
	If[ToString[type] === "Sine",
		Return[aCreateSinPolynomial[n,aLPHA,bETA,x,ops,Sequence@@Options[aCreateTrigPolynomial]]];
	];
];

End[]

EndPackage[]
