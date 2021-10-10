

BeginPackage["OrthogonalPolynomials`Discrete`Charlier`"]

aCharlier::usage="aCharlier[n,a,x,ops] returns value of Charlier polynomial \
    of degree n at point x, a is parameter of the family and sholud be bigger then 0. \
    The forth argument ops is optional and can be used to specify WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision), ReturnList (def. ReturnList->False) \
    to specify if function should return values at point x of all polynomials \
    degree less then or equal to n, or just of polynomial of degree n, Numerator
    (def. Numerator->0) specify if numerator polynomial should be returned of order \
    which is specified in option.";
aCharlier::supp="Supporting set is set of natural numbers including zero.";
aCharlier::dist="Distribution function is step function having jumps Exp[-a] a^x/x! \
    at natural number x including zero.";
aCharlier::bdpmt="Parameter of the family must be bigger then 0.";
(*aCharlier::acont="Measure does not have absolutly continuous part.";*)
Options[aCharlier]={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False,
    Numerator->0};
Attributes[aCharlier]={ReadProtected};

Begin["`Private`"]

aCharlier[request_,a_]:=Module[{i},
    If[NumberQ[a] && (a<=0),Message[aCahrlier::bdpmt];Abort[];];
    If[request==="supp",
        Message[aCharlier::supp];
        Return[Function[x,Table[OrthogonalPolynomials`aIndex,
            {OrthogonalPolynomials`aIndex,0,x}]]
        ];
    ];
    If[request==="ttr",
        Return[Function[x,If[x=!=0,{x+a,a x},{a,1}]
        ]];
    ];
    If[request==="dist",
        Message[aCharlier::dist];
        Return[Function[x,Exp[-a] Sum[a^OrthogonalPolynomials`aIndex/
            OrthogonalPolynomials`aIndex!,{OrthogonalPolynomials`aIndex,0,x}]
        ]];
    ];
    (*If[request==="wth",
        Message[aCharlier::acont];
    ];*)
    If[request==="nor",
        Return[Function[x,Sqrt[a^x x!]
        ]];
    ];
    If[request==="mom",
        Return[Function[x,Sum[OrthogonalPolynomials`aIndex^x a^OrthogonalPolynomials`aIndex/
            OrthogonalPolynomials`aIndex!,{OrthogonalPolynomials`aIndex,0,Infinity}]
        ]];
    ];
    If[request==="turan",
        Return[Function[{x1,x2,n},int=(Append[Append[(2Sqrt[Delete[x2,-1]]-Sqrt[x1])^2,0],0]+
            Prepend[Prepend[2Delete[x2,1]-x1,0],0]);
            Join[{int[[1]],int[[2]]},Take[int,{3,-3}]/2,
                {int[[-2]],int[[-1]]}]]];
    ];  
    If[request==="turanAllowed",
        Return[True];
    ];
    ] /;request[[0]]===String;

aCharlier[Order_?IntegerQ/;Order>-1,a_,x_,Ops___]:=
    Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,ret,list,wrprec,num},
    If[NumberQ[a] && (a<=0),Message[aCahrlier::bdpmt];Abort[];];
    If[Order==0,Return[1]];
    {Alfa,Beta}=aCharlier["ttr",a][k];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.{Ops}/.Options[aCharlier];
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
