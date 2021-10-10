


BeginPackage["OrthogonalPolynomials`Continuous`LaguerreG`"]

aLaguerreGInt={0,Infinity};
aLaguerreG::usage="aLaguerreG[n,s,x,ops] returns value of generalized Laguerre polynomial \
    of degree n at point x, s is parameter of the family and should be bigger then -1.
    The forth argument ops is optional and can be used to specify WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision), ReturnList (def. ReturnList->False) \
    to specify if function should return values at point x of all polynomials \
    degree less then or equal to n, or just of polynomial of degree n, Numerator \
    (def. Numerator->0) specify if numerator polynomial should be returned of order \
    which is specified in option.";
aLaguerreG::bdpmt="Parameter of the family must be bigger -1.";
Options[aLaguerreG]={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False,
    Numerator->0};
Attributes[aLaguerreG]={ReadProtected};

Begin["`Private`"]

aLaguerreG[Order_?IntegerQ/;Order>-1,s_,x_,Ops___]:=
Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,wrprec,list,num},
    If[NumberQ[s] && !Positive[s+1],Message[aLaguerreG::bdpmt];Abort[];];
    If[Order==0,Return[1]];
    {Alfa,Beta}=aLaguerreG["ttr",s][k];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.{Ops}/.Options[aLaguerreG];
    Block[{$MinPrecision=wrprec},
    pom1 = x-(Alfa/.k->num);pom2=pom1;If[list,ret={1,pom2};];
    For[i = 1, i < Order, i++,
            pom2 = Expand[(x - (Alfa /. k -> (i+num)))pom1 - pom (Beta /. k -> (i+num))];
            pom = pom1;
            pom1 = pom2;
        If[list,AppendTo[ret,pom2]];
        ];];
        If[list,Return[ret];,Return[pom2];];
      ]

aLaguerreG[request_,s_,left_:Hold[aLaguerreGInt[[1]]]]:=
    Module[{a,int},a=Release[left];
    If[NumberQ[s] && !Positive[s+1],Message[aLaguerreG::bdpmt];Abort[];];
    If[request==="sint",
        aLaguerreGInt[[1]]=s;
    ];
    If[request==="gint",
        Return[aLaguerreGInt];
    ];
    If[request==="ttr",     
        Return[Function[x,If[x=!=0,{2 x+1+s+a,x(x+s)},{1+s+a,Gamma[s+1]}]]];
    ];  
    If[request==="wth",
        Return[Function[x,(x-a)^s Exp[-x+a]]];
    ];
    If[request==="mom",
        Return[Function[x,Gamma[x+s+1]]];
    ];
    If[request==="nor",
        Return[Function[x,Sqrt[x! Gamma[x+s+1]]]];
    ];      
    If[request==="turan",
        Return[Function[{x1,x2,n},int=(Append[Append[2Sqrt[Delete[x2,-1]]-Sqrt[x1],0],0]+
            Prepend[Prepend[2Sqrt[Delete[x2,1]]-Sqrt[x1],0],0]);
            (Join[{int[[1]],int[[2]]},Take[int,{3,-3}]/2,
                {int[[-2]],int[[-1]]}])^2]];
    ];
    If[request==="turanAllowed",
        Return[True];
    ];
    ] /; request[[0]]===String;

Unprotect[Integrate];
Integrate[aLaguerreG[n_,s_,x_] aLaguerreG[m_,s_,x_] aLaguerreG["wth",s_][x_],{x_,0,Infinity}]:=(aLaguerreG["nor",s][n])^2 KroneckerDelta[n,m];
Integrate[aLaguerreG[n_,s_,x_]^2 aLaguerreG["wth",s_][x_],{x_,0,Infinity}]:=(aLaguerreG["nor",s][n])^2;
Protect[Integrate];

End[]

EndPackage[]
