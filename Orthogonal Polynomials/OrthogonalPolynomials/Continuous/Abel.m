BeginPackage["OrthogonalPolynomials`Continuous`Abel`"]

aAbelInt={-Infinity,Infinity};
aAbel::usage="aAbel[n,x,ops] returns value of Abel polynomial of degree n at point x, \
    the third argument ops is optional and can be used to specify WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision), ReturnList (def. ReturnList->False) \
    to specify if function should return values at point x of all polynomials \
    degree less then or equal to n, or just of polynomial of degree n, Numerator \
    (def. Numerator->0) specify if numerator polynomial should be returned of order \
    which is specified in option.";
Options[aAbel]:={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False,
    Numerator->0};
Attributes[aAbel]={ReadProtected};

Begin["`Private`"]

aAbel[request_]:=
    Module[{int},
    If[request==="sint",
    ];
    If[request==="gint",
        Return[aAbelInt];
    ];
    If[request==="ttr",     
        Return[Function[x,If[x=!=0,{0,x(x+1)/4},{0,1/4}]]];
    ];  
    If[request==="wth",
        Return[Function[x,x/(Exp[Pi x]-Exp[-Pi x])]];
    ];
    If[request==="mom",
        Return[Function[x,(2^(x+2)-1)Abs[BernoulliB[x+2]]/(x+2)]];
    ];
    If[request==="nor",
        Return[Function[x,x! Sqrt[(x+1)]/2^(x+1)]];
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

Unprotect[Integrate];
Integrate[aAbel[n_,x_] aAbel[m_,x_] aAbel["wth"][x_],{x_,-Infinity,Infinity}]:=(aAbel["nor"][n])^2 KroneckerDelta[n,m];
Integrate[aAbel[n_,x_]^2 aAbel["wth"][x_],{x_,-Infinity,Infinity}]:=(aAbel["nor"][n])^2;
Protect[Integrate];

aAbel[Order_?IntegerQ/;Order>-1,x_,Ops___]:=
Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,wrprec,ret,list,num},
    {Alfa,Beta}=aAbel["ttr"][k];
    If[Order==0,Return[1]];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.{Ops}/.Options[aAbel];
    Block[{$MinPrecision=wrprec},
    pom1 = x;pom2=pom1;If[list,ret={1,x};];
    For[i = 1, i < Order, i++,
            pom2 = Expand[x pom1 - pom (Beta /. k -> (i+num))];
            pom = pom1;
            pom1 = pom2;
        If[list,AppendTo[ret,pom2];];
        ];];
        If[list,Return[ret];,Return[pom2];];
      ]
        
End[]

EndPackage[]
