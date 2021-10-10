


BeginPackage["OrthogonalPolynomials`Discrete`PQ`"]

aPQ::usage="aPQ[n,p,q,x,ops]";

aPQ::supp="Supporting set is set of numbers 1/q^k, k=0,...,infinity.";
aPQ::dist="Distribution function is step function having jumps 1/p^k in points 1/q^k, k=0,...,infinity.";
aPQ::bdpmt="Modulus of parametrs have to be bigger then 1.";

Options[aPQ]={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False,Numerator->0};

Attributes[aPQ]={ReadProtected};

Begin["Private`"]

aPQ[request_,p_,q_]:=Module[{},
	(*If[NumberQ[q] && Abs[q]<=1, Message[aPQ::bdpmt];Abort[];];*)
	(*If[NumberQ[p] && Abs[p]<=1, Message[aPQ::bdpmt];Abort[];];*)
	If[request==="supp",
		Message[aPQ::supp];];
	If[request==="ttr",
		Return[Function[x,If[x!=0,{q^x (p+q-2 p q^x(1+q)+p q^(2x)(p+q))/((p q^(2x-1)-1)(p q^(2x+1)-1)),
			p q^(2x) (q^x-1)^2(p q^(x-1)-1)^2/((p q^(2x-2)-1)(p q^(2x-1)-1)^2(p q^(2x)-1))},
			{q(p-1)/(p q-1),p/(p-1)}]
		]];];
	If[request==="dist",
		Message[aPQ::dist];];
	If[request==="nor",
		Return[Function[x,If[x=!=0,p^(x+1)q^(x(x+1))/(p q^(2 x)-1) Times@@Table[(q^k-1)^2,{k,1,x}]/
			Times@@Table[(p q^k-1)^2,{k,x,2x-1}],p/(p-1)]]
		]];
	If[request==="mom",
		Return[Function[x,p q^x/(p q^x -1)]
		]];
	];/;request[[0]]===String;

aPQ[Order_?IntegerQ/;Order>-1,p_,q_,x_,Ops___]:=Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,ret,list,wrprec,num},
	(*If[NumberQ[q] && Abs[q]<=1, Message[aPQ::bdpmt];Abort[];];*)
	(*If[NumberQ[p] && Abs[p]<=1, Message[aPQ::bdpmt];Abort[];];*)
	If[Order==0,Return[1]];
	{Alfa,Beta}=aPQ["ttr",p,q][0];
	{wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,Numerator}/.{Ops}/.Options[aPQ];
	Block[{$MinPrecision=wrprec},pom=1;
	pom1=x-(Alfa/.k->num);pom2=pom1;If[list,ret={1,pom2};];
	For[i=1,i<Order,i++,
		pom2=Expand[(x-(Alfa/.k->(i+num)))pom1-pom(Beta/.k->(num+i))];
		pom=pom1;
		pom1=pom2;
		If[list,AppendTo[ret,pom2];];
	];];
	If[list,Return[ret];,Return[pom2];];
];

End[]

EndPackage[]
