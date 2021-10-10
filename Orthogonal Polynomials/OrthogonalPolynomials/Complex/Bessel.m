


BeginPackage["OrthogonalPolynomials`Complex`Bessel`"]

aBessel::usage="aBessel[n,x,ops] returns value of Bessel polynomial of degree n at point x, \
    the third argument ops is optional and can be used to specify WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision), ReturnList (def. ReturnList->False) \
    to specify if function should return values at point x of all polynomials \
    degree less then or equal to n, or just of polynomial of degree n, Numerator \
    (def. Numerator->0) specify if numerator polynomial should be returned of order \
    which is specified in option.";
aBessel::supp="Unit circle.";
Options[aBessel]={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False,
    Numerator->0};
Attributes[aBessel]={ReadProtected};

Begin["`Private`"]

aBessel[request_]:=Module[{},
    If[request=="ttr",
        Return[Function[x,If[x=!=0,{0,-1/(4 x x-1)},{-1,-4 Pi I}]]];
    ];
    If[request=="mom",
        Return[Function[x,2 Pi I (-2)^(x+1)/Gamma[x+2]]];
    ];
    If[request=="wth",
        Return[Function[x,Exp[-2/x]]];
    ];
    If[request=="nor",
        Return[Function[x,2^(-x) Sqrt[Pi]/Gamma[x+1/2]
            Sqrt[4 Pi I (-1)^(x+1)/(2x+1)]]];
    ];
    If[request=="pas",
        Return[Function[n,{aSPoints[n],((#1+1)/#1^2)&}]];
    ];
    If[request=="supp",
        Message[aBessel::supp];
    ];
    If[request==="turanAllowed",
        Return[False];
    ];
    ] /;request[[0]]===String;

aBessel[Order_Integer/;Order>-1,x_,Ops___]:=
    Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,wrprec,list,ret,num},
    If[Order==0,Return[1]];
    {Alfa,Beta}=aBessel["ttr"][k];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.{Ops}/.Options[aBessel];
    Block[{$MinPrecision=wrprec},
    pom1 = x-(Alfa/.k->num);pom2=pom1;If[list,ret={1,pom2};];
    For[i = 1, i < Order, i++,
            pom2 = Expand[(x - (Alfa /. k -> (i+num)))pom1 - pom (Beta /. k -> (i+num))];
            pom = pom1;
            pom1 = pom2;
        If[list,AppendTo[ret,pom2];];
        ];];
        If[list,Return[ret];,Return[pom2];];
      ]

Unprotect[Integrate];
Integrate[aBessel[n_,Exp[I x_]] aBessel[m_,Exp[I x_]] aBessel["wth"][Exp[I x_]] Exp[I x_],{x_,-Pi,Pi}]:=(aBessel["nor"][n])^2 KroneckerDelta[n,m]/I;
Integrate[aBessel[n_,Exp[I x_]]^2 aBessel["wth"][Exp[I x_]] Exp[I x_],{x_,-Pi,Pi}]:=(aBessel["nor"][n])^2/I;
Protect[Integrate];

aSPoints[n_]:=Module[{hn,n1,den2,sro,csn,ro,teta,dteta,k,z={}},
    n1=-1;den2=n+1;hn=Floor[n/2];sro=1/(n+2/3);csn=-1/n;
    teta=ArcCos[csn];dteta=2(Pi-teta)/(n+1);
    For[k=1,k<=hn,k++,
        teta+=(1+.2 Sin[2Pi(k-.5)/hn])dteta;
        ro=(sro+1.15(1-Cos[teta])/den2)/2.15;
        AppendTo[z,ro Exp[I teta]];
    ];
    z=Join[z,Reverse[Conjugate[z]]];
    If[OddQ[n],
        z=Insert[z,-(sro+2.3/den2)/2.15,hn+1];];
    Return[z];
    ];

End[]

EndPackage[]
