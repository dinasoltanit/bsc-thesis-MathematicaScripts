BeginPackage["OrthogonalPolynomials`Continuous`Hermite`"]

aHermiteInt={-Infinity,Infinity};
aHermite::usage="aHermite[n,x,ops] returns value of Hermite polynomial of degree n at \
    point x, the third argument ops is optional and can be used to specify WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision), ReturnList (def. ReturnList->False) \
    to specify if function should return values at point x of all polynomials \
    degree less then or equal to n, or just of polynomial of degree n, Numerator \
    (def. Numerator->0) specify if numerator polynomial should be returned of order \
    which is specified in option.";
Options[aHermite]:={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False,
    Numerator->0};
Attributes[aHermite]={ReadProtected};

Begin["`Private`"]

aHermite[request_]:=
    Module[{int},
    If[request==="sint",
    ];
    If[request==="gint",
        Return[aHermiteInt];
    ];
    If[request==="ttr",     
        Return[Function[x,If[x=!=0,{0,x/2},{0,Sqrt[Pi]}]]];
    ];  
    If[request==="wth",
        Return[Function[x,Exp[-x x]]];
    ];
    If[request==="mom",
        Return[Function[x,(1+(-1)^x)/2 Gamma[(x+1)/2]]];
    ];
    If[request==="nor",
        Return[Function[x,Sqrt[Sqrt[Pi]x!/2^x]]];
    ];  
    If[request==="turan",
        Return[Function[{x1,x2,n},int=(Append[Append[2Delete[x2,-1]-x1,0],0]+
            Prepend[Prepend[2Delete[x2,1]-x1,0],0]);
            Join[{int[[1]],int[[2]]},Take[int,{3,-3}]/2,
                {int[[-2]],int[[-1]]}]]];
    ];
    If[request==="turanAllowed",
        Return[True];
    ];  
    ] /; request[[0]]===String;

Unprotect[Integrate];
Integrate[aHermite[n_,x_] aHermite[m_,x_] aHermite["wth"][x_],{x_,-Infinity,Infinity}]:=(aHermite["nor"][n])^2 KroneckerDelta[n,m];
Integrate[aHermite[n_,x_]^2 aHermite["wth"][x_],{x_,-Infinity,Infinity}]:=(aHermite["nor"][n])^2;
Protect[Integrate];

aHermite[Order_?IntegerQ/;Order>-1,x_,Ops___]:=
Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,ret,list,wrprec,num},
    If[Order==0,Return[1]];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.{Ops}/.Options[aHermite];
    {Alfa,Beta}=aHermite["ttr"][k];
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
