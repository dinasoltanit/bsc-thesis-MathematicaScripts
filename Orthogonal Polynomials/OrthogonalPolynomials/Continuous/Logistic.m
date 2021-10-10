BeginPackage["OrthogonalPolynomials`Continuous`Logistic`"]

aLogisticInt={-Infinity,Infinity};
aLogistic::usage="aLogistic[n,x,ops] returns value of Logistic polynomial of degree n at point x, \
    the third argument ops is optional and can be used to specify WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision), ReturnList (def. ReturnList->False) \
    to specify if function should return values at point x of all polynomials \
    degree less then or equal to n, or just of polynomial of degree n, Numerator \
    (def. Numerator->0) specify if numerator polynomial should be returned of order \
    which is specified in option.";
Options[aLogistic]={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False,
    Numerator->0};
Attributes[aLogistic]={ReadProtected};

Begin["`Private`"]

aLogistic[request_]:=
    Module[{},
    If[request==="sint",
    ];
    If[request==="gint",
        Return[aLogisticInt];
    ];
    If[request==="ttr",     
        Return[Function[x,If[x=!=0,{0,x^4 Pi^2/(4x^2-1)},
            {0,1}]]];
    ];  
    If[request==="wth",
        Return[Function[x,Exp[-x]/(1+Exp[-x])^2]];
    ];
    If[request==="mom",
        Return[Function[k,Integrate[OrthogonalPolynomials`intVariable^k                                                 Exp[-OrthogonalPolynomials`intVariable]/(1+Exp[-OrthogonalPolynomials`intVariable])^2, 
            {OrthogonalPolynomials`intVariable,-Infinity,Infinity}]]];
    ];
    If[request==="nor",
        Return[Function[x,(x!)^2/(2x-1)!! Pi^x/Sqrt[2x+1]]];
    ];  
    If[request==="turan",
        Return[Function[{x1,x2,n},
            If[n==4,
            int={2x2[[1]]-x1[[1]],2x2[[2]]-x1[[2]]};
            int=Join[int,-Reverse[int]];,
            If[OddQ[n],
                int=2Take[x2,(n-1)/2]-Take[x1,(n-1)/2]; 
                int=Join[int,{0},-Reverse[int]];,
            int=2Take[x2,n/2-1]-Take[x1,n/2-1]; 
                AppendTo[int,(2Sqrt[x2[[n/2-1]]]-Sqrt[x1[[n/2-2]]])^2]; 
                int=Join[int,-Reverse[int]];];];int]];
    ];
    If[request==="turanAllowed",
        Return[True];
    ];
    ] /; request[[0]]===String;

aLogistic[Order_?IntegerQ/;Order>-1,x_,Ops___]:=
Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,wrprec,list,ret,num},
    If[Order==0,Return[1]];
    {Alfa,Beta}=aLogistic["ttr"][k];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.{Ops}/.Options[aLogistic];
    Block[{$MinPrecision=wrprec},
    pom1 = x;pom2=pom1;If[list,ret={1,x};];
    For[i = 1, i < Order, i++,
            pom2 = Expand[x pom1 - pom (Beta/.k->(i+num))];
            pom = pom1;
            pom1 = pom2;
        If[list,AppendTo[ret,pom2];];
        ];];
        If[list,Return[ret];,Return[pom2];];
      ]
    
Unprotect[Integrate];
Integrate[aLogistic[n_,x_] aLogistic[m_,x_] aLogistic["wth"][x_],{x_,-Infinity,Infinity}]:=(aLogistic["nor"][n])^2 KroneckerDelta[n,m];
Integrate[aLogistic[n_,x_]^2 aLogistic["wth"][x_],{x_,-Infinity,Infinity}]:=(aLogistic["nor"][n])^2;
Protect[Integrate];

End[]

EndPackage[]
