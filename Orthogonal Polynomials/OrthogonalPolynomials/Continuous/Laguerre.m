


BeginPackage["OrthogonalPolynomials`Continuous`Laguerre`"]

aLaguerreInt={0,Infinity};
aLaguerre::usage="aLaguerre[n,x,ops] returns value of Laguerre polynomial of degree n at point x, \
    the third argument ops is optional and can be used to specify WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision), ReturnList (def. ReturnList->False) \
    to specify if function should return values at point x of all polynomials \
    degree less then or equal to n, or just of polynomial of degree n, Numerator \
    (def. Numerator->0) specify if numerator polynomial should be returned of order \
    which is specified in option.";
Options[aLaguerre]={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False,
    Numerator->0};
Attributes[aLaguerre]={ReadProtected};

Begin["`Private`"]

aLaguerre[Order_?IntegerQ/;Order>-1,x_,Ops___]:=
Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,ret,list,wrprec,num},
    If[Order==0,Return[1]];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.{Ops}/.Options[aLaguerre];
    {Alfa,Beta}=aLaguerre["ttr"][k];    
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

aLaguerre[request_,left_:Hold[aLaguerreInt[[1]]]]:=
    Module[{a,int},a=Release[left];
    If[request==="sint",
        aLaguerreInt[[1]]=a;
    ];
    If[request==="gint",
        Return[aLaguerreInt];
    ];
    If[request==="ttr",     
        Return[Function[x,If[x=!=0,{2 x+1+a,x x},{1+a,1}]]];
    ];  
    If[request==="wth",
        Return[Function[x,Exp[-x+a]]];
    ];
    If[request==="mom",
        Return[Function[x,Exp[a]Gamma[x+1,a]]];
    ];
    If[request==="nor",
        Return[Function[x,Gamma[x+1]]];
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
Integrate[aLaguerre[n_,x_] aLaguerre[m_,x_] aLaguerre["wth"][x_],{x_,0,Infinity}]:=(aLaguerre["nor"][n])^2 KroneckerDelta[n,m];
Integrate[aLaguerre[n_,x_]^2 aLaguerre["wth"][x_],{x_,0,Infinity}]:=(aLaguerre["nor"][n])^2;
Protect[Integrate];

End[]

EndPackage[]
