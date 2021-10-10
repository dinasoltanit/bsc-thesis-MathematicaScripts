BeginPackage["OrthogonalPolynomials`Continuous`HermiteG`",{"OrthogonalPolynomials`Kernel`Common`"}]

aHermiteGInt={-Infinity,Infinity};
aHermiteG::usage="aHermiteG[n,mi,x,ops] returns value of generalized \
    Hermite polynomial of degree n at point x, parameter mi is parameter of the \
    family and for the realtions in the package to be valid this parameter should be bigger \
    then -1/2. The forth argument ops is optional and can be used to specify WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision), ReturnList (def. ReturnList->False) \
    to specify if function should return values at point x of all polynomials \
    degree less then or equal to n, or just of polynomial of degree n, Numerator \
    (def. Numerator->0) specify if numerator polynomial should be returned of order \
    which is specified in option.";
aHermiteG::bdpmt="Parameter of the family must be bigger then -1/2.";
Options[aHermiteG]={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False,
    Numerator->0};
Attributes[aHermiteG]={ReadProtected};

Begin["`Private`"]

aHermiteG[request_,mi_]:=
    Module[{int},
    If[NumberQ[mi] && !Positive[2mi+1],Message[aHemiteG::bdpmt];Abort[];];
    If[request==="sint",
    ];
    If[request==="gint",
        Return[aHermiteGInt];
    ];
    If[request==="ttr",     
        Return[Function[x,If[x=!=0,{0,If[Odd[x],(x+2mi)/2,x/2]},
            {0,Gamma[mi+1/2]}]]];
    ];  
    If[request==="wth",
        Return[Function[x,Abs[x]^(2 mi)Exp[-x x]]];
    ];
    If[request==="mom",
        Return[Function[x,(1+(-1)^x)/2 Gamma[(x+1)/2+mi]]];
    ];
    If[request==="nor",
        Return[Function[x,Sqrt[Gamma[Floor[(x+1)/2]+1/2+mi]Gamma[Floor[x/2]+1]]]];
    ];  
    If[request==="turan",
        Return[Function[{x1,x2,n},int=(Append[Append[2Delete[x2,-1]-x1,0],0]+
            Prepend[Prepend[2Delete[x2,1]-x1,0],0]);
            Join[{int[[1]],int[[2]]},
                Take[int,{3,-3}]/2,
                {int[[-2]],int[[-1]]}]]];
    ];
    If[request==="turanAllowed",
        Return[True];
    ];  
    ] /; request[[0]]===String;

Unprotect[Integrate];
Integrate[aHermiteG[n_,mi_,x_] aHermiteG[m_,mi_,x_] aHermiteG["wth",mi_][x_],{x_,-1,1}]:=(aHermiteG["nor",mi][n])^2 KroneckerDelta[n,m];
Integrate[aHermiteG[n_,mi_,x_]^2 aHermiteG["wth",mi_][x_],{x_,-1,1}]:=(aHermiteG["nor",mi][n])^2;
Protect[Integrate];

aHermiteG[Order_?IntegerQ/;Order>-1,mi_,x_,Ops___]:=
Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,wrprec,list,ret,num},
    If[NumberQ[mi] && !Positive[2mi+1],Message[aHemiteG::bdpmt];Abort[];];
    If[Order==0,Return[1];];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.{Ops}/.Options[aHermiteG];
    Block[{$MinPrecision=wrprec},
    {Alfa,Beta}=aHermiteG["ttr",mi][k];
    pom1 = x;pom2=pom1;If[list,ret={1,x};];
    For[i = 1, i < Order, i++,
            pom2 = Expand[x pom1 - pom (Beta/.k->(i+num))];
            pom = pom1;
            pom1 = pom2;
        If[list,AppendTo[ret,pom2];];
        ];];
        If[list,Return[ret];,Return[pom2];];
      ]
    
End[]

EndPackage[]
