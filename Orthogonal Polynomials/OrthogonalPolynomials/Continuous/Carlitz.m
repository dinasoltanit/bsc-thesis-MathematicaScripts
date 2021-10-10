BeginPackage["OrthogonalPolynomials`Continuous`Carlitz`"]

aCarlitzInt={-Infinity,Infinity};
aCarlitz::usage="aCarlitz[n,lambda,x,ops] returns value of \
    Carlitz polynomial of degree n at point x, parameter lambda is parameter of the \
    family and for the realtions in the package to be valid this parameter should be with \
    modulus less then 1. \
    The third argument ops is optional and can be used to specify WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision), ReturnList (def. ReturnList->False) \
    to specify if function should return values at point x of all polynomials \
    degree less then or equal to n, or just of polynomial of degree n, Numerator \
    (def. Numerator->0) specify if numerator polynomial should be returned of order \
    which is specified in option.";
aCarlitz::bdpmt="Parameter of the family must be with modulus less then 1.";    
Options[aCarlitz]={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False,
    Numerator->0};
Attributes[aCarlitz]={ReadProtected};

Begin["`Private`"]

aCarlitz[request_,lambda_]:=
    Module[{},
    If[NumberQ[lambda] && Abs[lambda]>=1,Message[aCarlitz::bdpmt];Abort[];];
    If[request==="sint",
    ];
    If[request==="gint",
        Return[aCarlitzInt];
    ];
    If[request==="ttr",     
        Return[Function[x,If[x=!=0,{0,x^2(x^2-lambda^2)/(4x^2-1)},
            {0,4 ArcTan[Tan[Pi lambda/2]]Csc[Pi lambda]/Pi}]]];
    ];  
    If[request==="wth",
        Return[Function[x,1/(Cosh[Pi x]+Cos[Pi lambda])]];
    ];
    If[request==="mom",
        Return[Function[k,Integrate[OrthogonalPolynomials`intVariable^k/(Cosh[Pi OrthogonalPolynomials`intVariable]+Cos[Pi          lambda]),{OrthogonalPolynomials`intVariable,-Infinity,Infinity}]]];
    ];
    If[request==="nor",
        Return[Function[x,x!/(2x-1)!! Sqrt[2/Sin[Pi lambda] lambda/(2x+1)
            Gamma[1-lambda+x]/Gamma[1-lambda]
            Gamma[1+lambda+x]/Gamma[1+lambda]]]];
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
Integrate[aCarlitz[n_,lambda_,x_] aCarlitz[m_,lambda_,x_] aCarlitz["wth",lambda_][x_],{x_,-Infinity,Infinity}]:=(aCarlitz["nor",lambda][n])^2 KroneckerDelta[n,m];
Integrate[aCarlitz[n_,lambda_,x_]^2 aCarlitz["wth",lambda_][x_],{x_,-Infinity,Infinity}]:=(aCarlitz["nor",lambda][n])^2;
Protect[Integrate];

aCarlitz[Order_?IntegerQ/;Order>-1,lambda_,x_,Ops___]:=
Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,ret,list,wrprec,num},
    If[NumberQ[lambda] && Abs[lambda]>=1,Message[aCarlitz::bdpmt];Abort[];];
    If[Order==0,Return[1]];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.{Ops}/.Options[aCarlitz];
    {Alfa,Beta}=aCarlitz["ttr",lambda][k];
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
    
End[]

EndPackage[]
