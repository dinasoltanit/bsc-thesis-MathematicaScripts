

BeginPackage["OrthogonalPolynomials`Discrete`ChebyshevN`"]

aChebyshevN::usage="aChebyshevN[n,M,x,ops] returns value of Chebyshev discrete polynomial \
    of degree n at point x, M is parameter of the family and sholud be positive integer. \
    The forth argument ops is optional and can be used to specify WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision), ReturnList (def. ReturnList->False) \
    to specify if function should return values at point x of all polynomials \
    degree less then or equal to n, or just of polynomial of degree n, Numerator
    (def. Numerator->0) specify if numerator polynomial should be returned of order \
    which is specified in option.";
aChebyshevN::supp="Supporting set is the set of natural numbers {0,...,M}.";
aChebyshevN::dist="Distribution function is step function having jumps 1 \
    at points 0,...,M.";
aChebyshevN::bdpmt="Parameter of the family must be positive integer.";
(*aChebyshevN::acont="Measure does not have absolutly continuous part.";*)
Options[aChebyshevN]={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False,
    Numerator->0};
Attributes[aChebyshevN]={ReadProtected};

Begin["`Private`"]

aChebyshevN[request_,M_]:=Module[{i},
    If[IntegerQ[M] && (M<=0),Message[aChebyshevN::bdpmt];Abort[];];
    If[request==="supp",
        Message[aChebyshevN::supp];
        Return[Function[x,Table[OrthogonalPolynomials`aIndex,
            {OrthogonalPolynomials`aIndex,0,x}]]
        ];
    ];
    If[request==="ttr",
	   Return[Function[x,If[x=!=0,{M/2,1/4 x^2/(4 x^2-1)((M+1)^2-x^2)},{M/2,M+1}]
        ]];
    ];
    If[request==="dist",
        Message[aChebyshevN::dist];
        Return[Function[x,Table[1,{OrthogonalPolynomials`aIndex,0,x}]
        ]];
    ];
    (*If[request==="wth",
        Message[aChebyshevN::acont];
    ];*)
    If[request==="nor",
        Return[Function[x,If[x==0,Sqrt[M+1],Sqrt[M+1] Sqrt[Times@@Table[1/4 OrthogonalPolynomials`aIndex^2/(4 OrthogonalPolynomials`aIndex^2-1)((M+1)^2-OrthogonalPolynomials`aIndex^2),
						{OrthogonalPolynomials`aIndex,1,x}]]]
        ]];
    ];
    If[request==="mom",
        Return[Function[x,Sum[OrthogonalPolynomials`aIndex^x,{OrthogonalPolynomials`aIndex,0,M}]
        ]];
    ];
    If[request==="turanAllowed",
        Return[False];
    ];
    ] /;request[[0]]===String;

aChebyshevM[Order_?IntegerQ/;Order>-1,M_,x_,Ops___]:=
    Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,ret,list,wrprec,num},
    If[IntegerQ[M] && (M<=0),Message[aChebyshevM::bdpmt];Abort[];];
    If[Order==0,Return[1]];
    {Alfa,Beta}=aChebyshevM["ttr",M][k];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.{Ops}/.Options[aChebyshevN];
    Block[{$MinPrecision=wrprec},
    pom1 = x-(Alfa/.k->num);pom2=pom1;If[list,ret={1,pom2};];
    For[i = 1, i < Order, i++,
            pom2 = Expand[(x - (Alfa/.k->(i+num)))pom1 - pom (Beta/.k->(i+num))];
            pom = pom1;
            pom1 = pom2;
        If[list,AppendTo[ret,pom2];];
        ];];
        If[list,Return[ret];,Return[pom2];];
      ]



End[]

EndPackage[]
