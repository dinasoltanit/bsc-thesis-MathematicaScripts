


BeginPackage["OrthogonalPolynomials`Continuous`ChebyshevI`"]

aChebyshevIInt={-1,1};
aChebyshevI::usage="aChebyshevI[n,x,ops] returns value of Chebyshev polynomial of the first \
    kind of degree n at \
    point x, the third argument ops is optional and can be used to specify WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision), ReturnList (def. ReturnList->False) \
    to specify if function should return values at point x of all polynomials \
    degree less then or equal to n, or just of polynomial of degree n, Numerator \
    (def. Numerator->0) specify if numerator polynomial should be returned of order \
    which is specified in option.";
Options[aChebyshevI]={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False,
    Numerator->0};
Attributes[aChebyshevI]={ReadProtected};

Begin["`Private`"]

aChebyshevI[request_,left_:Hold[aChebyshevIInt[[1]]],right_:Hold[aChebyshevIInt[[2]]]]:=
    Module[{a,b,int},a=Release[left];b=Release[right];
    If[request==="sint",
        aChebyshevIInt={a,b};
    ];
    If[request==="gint",
        Return[aChebyshevIInt];
    ];
    If[request==="ttr",     
        Return[Function[x,If[x=!=0,{(b+a)/2,(b-a)^2/4 If[x>1,1/4,1/2]},
            {(b+a)/2,(b-a)Pi/2}]]];
    ];  
    If[request==="wth",
        Return[Function[x,1/Sqrt[1-((2x-(b+a))/(b-a))^2]]];
    ];
    If[request==="mom",
        Return[Function[x,If[x=!=0,(1+(-1)^x)Sqrt[Pi]Gamma[(x+1)/2]/(x Gamma[x/2]),Pi]]];
    ];
    If[request==="nor",
        Return[Function[x,If[x=!=0,Sqrt[(b-a)Pi](b-a)^x/2^(2x),Sqrt[(b-a)Pi/2]]]];
    ];  
    If[request==="turan",
        Return[Function[{x1,x2,n},int=(Append[Append[2Delete[x2,-1]-x1,0],0]+
            Prepend[Prepend[2Delete[x2,1]-x1,0],0]);
            Join[{If[int[[1]]<a,a,int[[1]]],int[[2]]},Take[int,{3,-3}]/2,
                {int[[-2]],If[int[[-1]]>b,b,int[[-1]]]}]]];
    ];
    If[request==="turanAllowed",
        Return[True];
    ];  
    ] /; request[[0]]===String;

Unprotect[Integrate];
Integrate[aChebyshevI[n_,x_] aChebyshevI[m_,x_] aChebyshevI["wth"][x_],{x_,-1,1}]:=(aChebyshevI["nor"][n])^2 KroneckerDelta[n,m];
Integrate[aChebyshevI[n_,x_]^2 aChebyshevI["wth"][x_],{x_,-1,1}]:=(aChebyshevI["nor"][n])^2;
Protect[Integrate];

aChebyshevI[Order_Integer/;Order>-1,x_,Ops___]:=
    Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,wrprec,ret,list,num,int},
    If[Order==0,Return[1]];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.{Ops}/.Options[aChebyshevI];
    {Alfa,Beta}=aChebyshevI["ttr"][k];
(*Print[{Alfa,Beta}];*)
    Block[{$MinPrecision=wrprec},
    pom1 =x;pom2=pom1;If[list,ret={1,x};];
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
