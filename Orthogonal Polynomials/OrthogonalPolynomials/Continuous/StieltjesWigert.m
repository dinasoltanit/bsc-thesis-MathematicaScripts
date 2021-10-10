

BeginPackage["OrthogonalPolynomials`Continuous`StieltjesWigert`"]

aStieltjesWigertInt={0,Infinity};
aStieltjesWigert::usage="aStieltjesWigert[n,q,p,x,ops] returns value of StieltjesWigert polynomial \
    of degree n at point x, q and p are parameters of the family and q sholud be positive \
    and p should bigger then or equal to 0 and smaller then 1. \
    The fifth argument ops is optional and can be used to specify WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision), ReturnList (def. ReturnList->False) \
    to specify if function should return values at point x of all polynomials \
    degree less then or equal to n, or just of polynomial of degree n, Numerator \
    (def. Numerator->0) specify if numerator polynomial should be returned of order \
    which is specified in option.";
aStieltjesWigert::bdpmt="Parameters q and p must be bigger then equal to zero and smaller then 1.";
Options[aStieltjesWigert]={WorkingPrecision->$MachinePrecision,
    OrthogonalPolynomials`ReturnList->False,Numerator->0};
Attributes[aStieltjesWigert]={ReadProtected};

Begin["`Private`"]

aStieltjesWigert[request_,q_,p_,left_:Hold[aStieltjesWigertInt[[1]]]]:=
    Module[{a},a=Release[left];
    If[NumberQ[p] && (p<0 || p>=1),Message[aStieltjesWigert::bdpmt];Abort[];];
    If[NumberQ[q] && (q<0 || q>=1),Message[aStieltjesWigert::bdpmt];Abort[];];
    If[request==="sint",
        aStieltjesWigertInt[[1]]=q;
    ];
    If[request==="gint",
        Return[aStieltjesWigertInt];
    ];
    If[request==="ttr",     
        Return[Function[x,If[x=!=0,{-(p+q-(1+q)q^(-x))q^(-x-3/2),
            (1-q^x)(1-p q^(x-1))q^(-4x)},
            {-(a+p-1)/q^(3/2),q^(-1/2)}]]];
    ];  
    If[request==="wth",
        Return[Function[x,Hold[Product[1-p q^"nSW",{"nSW",0,Infinity}]
            Sum[q^("nSW"("nSW"-2)/2)(p/x)^"nSW"/Product[1-q^"kSW",{"kSW",1,"nSW"}],
            {"nSW",0,Infinity}]]Exp[-(Log[x]/(2 Pi Log[1/q]))^2]/Sqrt[2 Pi Log[1/q]]
        ]];
    ];
    If[request==="mom",
        Return[Function[x,If[x=!=0,Hold[Product[1-p q^"kSW",{"kSW",0,x-1}]]q^(-(x+1)^2/2),
            q^(-1/2)]
        ]];
    ];
    If[request==="nor",
        Return[Function[x,If[x=!=0,
        Sqrt[Hold[Product[(1-p q^"kSW")(1-q q^"kSW"),{"kSW",0,x-1}]]q^(-(2x+1)^2/2)],
        q^(-1/2)]
        ]];
    ];
    If[request==="turanAllowed",
        Return[False];
    ];      
    ] /; request[[0]]===String;

aStieltjesWigert[Order_?IntegerQ/;Order>-1,q_,p_,x_,Ops___]:=
Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,wrprec,list,ret,num},
    If[NumberQ[p] && (p<0 || p>=1),Message[aStieltjesWigert::bdpmt];Abort[];];
    If[NumberQ[q] && !Positive[q],Message[aStieltjesWigert::bdpmt];Abort[];];
    If[Order==0,Return[1]];
    {Alfa,Beta}=aStieltjesWigert["ttr",q,p][k];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.{Ops}/.Options[aStieltjesWigert];
    Block[{$MinPrecision=wrprec},
    pom1 = Expand[(x - (Alfa /. k -> num))];pom2=pom1;If[list,ret={1,pom2};];
    For[i = 1, i < Order, i++,
            pom2 = Expand[(x - (Alfa /. k -> (i+num)))pom1 - pom (Beta /. k -> (i+num))];
            pom = pom1;
            pom1 = pom2;
        If[list,AppendTo[ret,pom2];];
        ];];
        If[list,Return[ret];,Return[pom2];];
      ]

Unprotect[Integrate];
Integrate[aStieltjesWigert[n_,q_,p_,x_] aStieltjesWigert[m_,q_,p_,x_] aStieltjesWigert["wth",q_,p_][x_],{x_,0,Infinity}]:=(aStieltjesWigert["nor"q,p][n])^2 KroneckerDelta[n,m];
Integrate[aStieltjesWigert[n_,q_,p_,x_]^2 aStieltjesWigert["wth",q_,p_][x_],{x_,0,Infinity}]:=(aStieltjesWigert["nor"q,p][n])^2;
Protect[Integrate];

End[]

EndPackage[]
