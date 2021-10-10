BeginPackage["OrthogonalPolynomials`TTRCoefficients`",{"OrthogonalPolynomials`Kernel`Common`"}]

Unprotect[aStieltjesAlgorithm];
Unprotect[aLanczosAlgorithm];
Unprotect[aChebyshevAlgorithm];
Unprotect[aChebyshevAlgorithmModified];
Unprotect[aThreeTermRecurrence];

aThreeTermRecurrence::usage="General purpose function, which supports all sorts of evaluations \
    for obtaining three-term recurrence coefficients.";
aChebyshevAlgorithm::usage="aChebyshevAlgorithm[momList,ops] first argument of the function \
    is list of moments for which three term recurrence relation coefficients are to be determined. \
    Second aregument are options. Option WorkingPrecision (def. WorkingPrecision->$MachinePrecision) \
    determines number of decimal digits in mantissa which are going to be used in calculation. Option \
    Algorithm (def. Algorithm->Automatic) determines whether numerical or symbolic version of the function is \
    going to be used, if set to Symbolic symbolic version of the function is used. There are more options \
    which can be used with option Algorithm set to Symbolic. Those are Extension, GaussianIntegers and Trig \
    with same meaning they have with functions Factor and Together.";
aChebyshevAlgorithm::nondef="Moment functional associated with moment sequence is not definite.";
aChebyshevAlgorithmModified::bdinp="For the algorithm proper work, you must supply \
    at least noumber of modified moments minus one coefficients of three \
    term recurence relation.";
aChebyshevAlgorithmModified::usage="aChebyshevAlgorithmModified[mModList,coeBeta,coeGamma,ops] \
    function is called with three arguments type list. The first argument is list of \
    modified moments, secound one is list of beta coeffients in three term recurrence \
    relation for polynomials for which modified moments are evaluated. Third argument \
    is list of gamma coefficients in three term recurrence relation. Function returns list. \
    The returned list is consisted of two lists.The first one represent beta coefficients, \
    and the secound one represents gama coefficients. Forth argument are options. \
    Option WorkingPrecision (def. WorkingPrecision->$MachinePrecision) \
    determines number of decimal digits in mantissa which are going to be used in calculation. Option \
    Algorithm (def. Algorithm->Automatic) determines whether numerical or symbolic version of the function is \
    going to be used, if set to Symbolic symbolic version of the function is used. There are more options \
    which can be used with option Algorithm set to Symbolic. Those are Extension, GaussianIntegers and Trig \
    with same meaning they have with functions Factor and Together.";
aChebyshevAlgorithmModified::nondef="Moment functional associated with moment sequence is \
    not definite.";
aLanczosAlgorithm::bdinp="You have to submit lists at the secound and the third place with at \
    least number of elements equal to the first argument.";
aLanczosAlgorithm::usage="aLanczos[Number,nodes,weights,ops] the first argument is number of \
    coefficients to return, second and third arguments are lists consisting of nodes and \
    weights in scalar product, which is defined in discrete case or with which \
    scalar product is approximated. \
    Function returns a list contains at the first position beta coefficients and \
    at the secound gamma coefficients of three term recurence relation. Forth arguments \
    are options. Option WorkingPrecision (def. WorkingPrecision->$MachinePrecision) \
    determines number of decimal digits in mantissa which are going to be used in calculation.";
aStieltjesAlgorithm::bdinp="You have to submit lists at the secound and the third place with at \
    least number of elements equal to the first argument";
aStieltjesAlgorithm::usage="aStieltjes[number,nodes,weights,ops] the first argument is number \
    of coefficients to return, secound and third arguments are lists consist of \
    nodes and weights in scalar product, which is defined in discrete case or with which scalar \
    product is approximated. Function returns a list containing at the first \
    position beta coefficients and at the secound gama coefficients of three term \
    recurence relation. Forth arguments \
    are options. Option WorkingPrecision (def. WorkingPrecision->$MachinePrecision) \
    determines number of decimal digits in mantissa which are going to be used in calculation.";
aStieltjesAlgorithm::bdinp="You have to submit lists at the secound and the third place with at \
    least number of elements equal to the first argument";

(*Options*)
Options[aLanczosAlgorithm]={WorkingPrecision->$MachinePrecision};
Options[aStieltjesAlgorithm]={WorkingPrecision->$MachinePrecision};
Options[aChebyshevAlgorithm]={WorkingPrecision->$MachinePrecision,
    OrthogonalPolynomials`Algorithm->Automatic,Extension->None,GaussianIntegers->False,
    Trig->False};
Options[aChebyshevAlgorithmModified]={WorkingPrecision->$MachinePrecision,
    OrthogonalPolynomials`Algorithm->Automatic,Extension->None,GaussianIntegers->False,
    Trig->False};
Options[aThreeTermRecurrence]={OrthogonalPolynomials`Algorithm->aStieltjes, 
    WorkingPrecision->$MachinePrecision};
(*Options ends*)

Begin["`Private`"]

aChebyshevAlgorithmModified[momList_?VectorQ,coeBeta_?VectorQ,coeGama_?VectorQ,Ops___]:=
    Module[{beta=List[],gama=List[],len,i,j,pom,listBef,prec,alg,listext,gI,trig,ops,opsF},
        len=Length[momList];
        If[Length[coeBeta]<(len-1) || Length[coeGama]<(len-1),
            Message[aChebyshevAlgorithmModified::bdinp];Abort[];];
        {prec,alg}={WorkingPrecision,OrthogonalPolynomials`Algorithm}/.{Ops}
            /.Options[aChebyshevAlgorithmModified];
    If[ToString[alg]==="Symbolic",
        {gI,ext,trig}={GaussianIntegers,Extension,Trig}/.{Ops}
            /.Options[aChebyshevAlgorithm];
        ops=Sequence@@{Extension->ext,Trig->trig};
        opsF=Sequence@@Join[{ops},{GaussianIntegers->gI}];
        list=momList;
        If[list[[1]]==0,Message[aChebyshevAlgorithmModified::nondef];
            Return[{Indeterminate,0}];];
        AppendTo[beta,Factor[Together[coeBeta[[1]]+(list[[2]])/(list[[1]]),ops],opsF]];
        AppendTo[gama,list[[1]]];
        listBef=Table[0,{i,1,len-2}];
        For[i=1,i<(len-1)/2,i++,
            pom=list[[1]];
            For[j=1,j<=len-2i,j++,
                list[[j]]=Together[list[[j+2]]-gama[[-1]]*listBef[[j]]+
                            coeGama[[i+j]]*list[[j]]-(beta[[-1]]-coeBeta[[i+j]])
                    *list[[j+1]],ops];
                listBef[[j]]=list[[j+1]];
            ];
            If[list[[1]]==0,Message[aChebyshevAlgorithmModified::nondef];
                Return[{Append[beta,Indeterminate],Append[gama,0]}];];
            AppendTo[beta,Factor[Together[coeBeta[[i+1]]+list[[2]]/list[[1]]
                -listBef[[1]]/pom,ops],opsF]];
            AppendTo[gama,Factor[Cancel[list[[1]]/pom,ops],opsF]];
            list=Delete[list,{{-1},{-2}}];
            listBef=Delete[listBef,{{1},{-1}}];
        ];
        Return[{beta,gama}];
    ,
        list=SetPrecision[momList,prec];
        Block[{$MinPrecision=prec},
        AppendTo[beta,coeBeta[[1]]+(list[[2]])/(list[[1]])];
        AppendTo[gama,list[[1]]];
        listBef=Table[0,{i,1,len-2}];
        For[i=1,i<(len-1)/2,i++,
            pom=list[[1]];
            For[j=1,j<=len-2i,j++,
                list[[j]]=list[[j+2]]-gama[[-1]]*listBef[[j]]+
                            coeGama[[i+j]]*list[[j]]-(beta[[-1]]-coeBeta[[i+j]])
                    *list[[j+1]];
                listBef[[j]]=list[[j+1]];
            ];
            If[list[[1]]==0,Message[aChebyshevAlgorithmModified::nondef];
                Return[{Append[beta,Indeterminate],Append[gama,0]}];];
            AppendTo[beta,coeBeta[[i+1]]+list[[2]]/list[[1]]
                -listBef[[1]]/pom];
            AppendTo[gama,list[[1]]/pom];
            list=Delete[list,{{-1},{-2}}];
            listBef=Delete[listBef,{{1},{-1}}];
        ];];
        Return[{beta,gama}];
    ];
    ];  
                
aChebyshevAlgorithm[momList_?VectorQ,Ops___]:=
    Module[{beta=List[],gama=List[],len,i,j,pom,listBef,prec,alg,list,ops,opsF,ext,gI,trig},
    {prec,alg}={WorkingPrecision,OrthogonalPolynomials`Algorithm}/.{Ops}
        /.Options[aChebyshevAlgorithm];
    len=Length[momList];
    If[ToString[alg]==="Symbolic",
        {gI,ext,trig}={GaussianIntegers,Extension,Trig}/.{Ops}
            /.Options[aChebyshevAlgorithm];
        ops=Sequence@@{Extension->ext,Trig->trig};
        opsF=Sequence@@Join[{ops},{GaussianIntegers->gI}];
        list = momList;
        If[list[[1]]==0,Message[aChebyshevAlgorithm::nondef];
            Return[{Indeterminate,0}];];
            AppendTo[beta,Factor[(list[[2]])/(list[[1]]),opsF]];
            AppendTo[gama,list[[1]]];   
        listBef=Table[0,{i,1,len-2}];
            For[i=1,i<(len-1)/2,i++,
                pom=list[[1]];
                For[j=1,j<=len-2i,j++, 
                    list[[j]]=Together[(list[[j+2]]-
                        gama[[-1]]*listBef[[j]]-(beta[[-1]]*list[[j+1]])),ops];
                    listBef[[j]]=list[[j+1]];
                ];
        If[list[[1]]==0,Message[aChebyshevAlgorithm::nondef];
            Return[{Append[beta,Indeterminate],AppendTo[gama,0]}];];
            AppendTo[beta,Factor[Together[(list[[2]])/(list[[1]])-(listBef[[1]])/pom,
            ops],opsF]];
            AppendTo[gama,Factor[Cancel[list[[1]]/pom,ops],opsF]];
        list=Delete[list,{{-2},{-1}}];listBef=Delete[listBef,{{1},{-1}}];
            ];
            Return[{beta, gama}];
    ,
        list=SetPrecision[momList,prec];
        Block[{$MinPrecision=prec},
        AppendTo[beta,(list[[2]])/(list[[1]])];
        AppendTo[gama,list[[1]]];
        listBef=Table[0,{i,1,len-2}];
        For[i=1,i<(len-1)/2,i++,
            pom=list[[1]];
            For[j=1,j<=len-2i,j++,
                list[[j]]=list[[j+2]]-gama[[-1]]*listBef[[j]]-
                    beta[[-1]]*list[[j+1]];
                listBef[[j]]=list[[j+1]];
            ];
        If[list[[1]]==0,Message[aChebyshevAlgorithm::nondef];
            Return[{Append[beta,Indeterminate],AppendTo[gama,0]}];];
            AppendTo[beta,(list[[2]])/(list[[1]])
                -(listBef[[1]])/pom];
            AppendTo[gama,list[[1]]/pom];
            list=Delete[list,{{-2},{-1}}];
            listBef=Delete[listBef,{{1},{-1}}];
        ];];
        Return[{beta,gama}];];
    ];  

aLanczosAlgorithm[Number_Integer /; Number > 0, Nodes_?VectorQ, Weights_?VectorQ,Ops___] := 
    Module[{pom0,pom1,i,k,nodes,weights,tk,t,sig,tsig,gam,pi,xlam,tmp,ro,NOfPoints,prec}, 
        If[Or[Length[Nodes] < (Number+1), Length[Weights] < (Number+1)],
            Message[aLanczosAlgorithm::bdinp]; Abort[]];
    {prec}={WorkingPrecision}/.{Ops}/.Options[aLanczosAlgorithm];
    NOfPoints=Min[Length[Nodes],Length[Weights]];
    If[(prec != $MachinePrecision),
        nodes=Take[SetPrecision[Nodes,prec],NOfPoints];
        weights=Take[SetPrecision[Weights,prec],NOfPoints];,
        nodes=Take[Nodes, NOfPoints];weights=Take[Weights, NOfPoints];];
    Block[{$MinPrecision=prec},
        pom0=nodes;pom1=Prepend[Table[0,{i,2,NOfPoints}],weights[[1]]];
        For[i=1,i<NOfPoints,i++,
            pi=weights[[i+1]];gam=1;sig=0;t=0;xlam=nodes[[i+1]];
            For[k=1,k<=i+1,k++,
            ro=pom1[[k]]+pi;
            tmp=gam ro;tsig=sig;
                If[Re[ro]<=0,gam=1;sig=0;,gam=pom1[[k]]/ro;sig=pi/ro;];
            tk=sig(pom0[[k]]-xlam)-gam*t;pom0[[k]]=pom0[[k]]-(tk-t);
            t=tk;
                If[Re[sig]<=0,pi=tsig*pom1[[k]];,pi=t t/sig;];
                tsig=sig;pom1[[k]]=tmp;
            ];
    ];];
        Return[{Take[pom0,Number],Take[pom1,Number]}];
     ];

aStieltjesAlgorithm[Order_Integer/;Order>0,Nodes_?VectorQ,Weights_?VectorQ,Ops___]:= 
    Module[{sum0,sum1,sum2,NOfPoints,pom0,pom1,pom2,alfa=List[],beta=List[],nodes,weights,k,m,
    prec},
        If[Or[Length[Nodes]<(Order),Length[Weights]<(Order)],
        Message[aStieltjesAlgorithm::bdinp];Abort[];];
    {prec}={WorkingPrecision}/.{Ops}/.Options[aStieltjesAlgorithm];
    NOfPoints=Min[Length[Nodes],Length[Weights]];
    If[(prec != $MachinePrecision),
        nodes=Take[SetPrecision[Nodes,prec],NOfPoints];
        weights=Take[SetPrecision[Weights,prec],NOfPoints];,
        nodes=Take[Nodes,NOfPoints];weights=Take[Weights,NOfPoints];];
    Block[{$MinPrecision=prec},
        sum0=Plus @@ weights;sum1=weights.nodes;AppendTo[alfa,sum1/sum0];AppendTo[beta,sum0];
        If[Order==1, Return[{alfa, beta}];];
        pom0=pom1=Table[0,{k,1,NOfPoints}];pom2=Table[1,{k,1,NOfPoints}];
        For[k=1,k<Order,k++,sum1=0;sum2=0;
            For[m=1,m<=NOfPoints,m++, 
                pom0[[m]]=pom1[[m]];pom1[[m]]=pom2[[m]];
            pom2[[m]]=(nodes[[m]]-alfa[[k]])pom1[[m]]
                -beta[[k]]pom0[[m]];
                    pom0[[1]]=weights[[m]]*pom2[[m]]*pom2[[m]];sum1=sum1+pom0[[1]]; 
                    sum2=sum2+pom0[[1]]*nodes[[m]];
        ];
            AppendTo[alfa,sum2/sum1];AppendTo[beta,sum1/sum0];sum0=sum1;
    ];];
        Return[{alfa, beta}];
    ];


(*Operator ThreeTermRecurrence                      *)



aThreeTermRecurrence[mom_?VectorQ,Ops___]:=Module[{},
    Return[aChebyshevAlgorithm[mom,Ops]];];

aThreeTermRecurrence[mom_?VectorQ,alfa_?VectorQ,beta_?VectorQ,Ops___]:=Module[{},
    Return[aChebyshevAlgorithmModified[mom,alfa,beta,Ops]];];

aThreeTermRecurrence[Number_?IntegerQ,nodes_?VectorQ,weights_?VectorQ,Ops___]:=Module[{met,w},
    {met}={OrthogonalPolynomials`Algorithm}/.List[Ops]/.Options[aThreeTermRecurrence];
    If[MemberQ[{aStieltjes,aLanczos},met],
    Return[ToExpression[StringJoin[ToString[met],"Algorithm"]][Number,nodes,weights,Ops]];];
    Message[aThreeTermRecurence::untype];
    Abort[];
    ];

aThreeTermRecurrence[Name_?polyQ,Ops___]:=Module[{},Return[Name["ttr",Ops]];];

(*End of operator ThreeTermRecurrence                   *)
Protect[aStieltjesAlgorithm];
Protect[aLanczosAlgorithm];
Protect[aChebyshevAlgorithm];
Protect[aChebyshevAlgorithmModified];
Protect[aThreeTermRecurrence];

End[]

EndPackage[]
