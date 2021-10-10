


BeginPackage["OrthogonalPolynomials`Continuous`Legendre`"]

aLegendreInt={-1,1};
aLegendre::usage="aLegendre[n,x,ops] returns value of Legendre polynomial of degree n at point x, \
    the third argument ops is optional and can be used to specify WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision), ReturnList (def. ReturnList->False) \
    to specify if function should return values at point x of all polynomials \
    degree less then or equal to n, or just of polynomial of degree n, Numerator \
    (def. Numerator->0) specify if numerator polynomial should be returned of order \
    which is specified in option.";
Options[aLegendre]={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False,
    Numerator->0};
Attributes[aLegendre]={ReadProtected};

Begin["`Private`"]

aLegendre[request_,left_:Hold[aLegendreInt[[1]]],right_:Hold[aLegendreInt[[2]]]]:=
    Module[{a,b},a=Release[left];b=Release[right];
    If[request==="sint",
        aLegendreInt={a,b};
    ];
    If[request==="gint",
        Return[aLegendreInt];
    ];
    If[request==="ttr",     
        Return[Function[x,If[x=!=0,{(b+a)/2,(b-a)^2 x x/(4(4 x x-1))},
            {(b+a)/2,(b-a)}]]];
    ];  
    If[request==="wth",
        Return[Function[x,1]];
    ];
    If[request==="mom",
        Return[Function[x,(b^(x+1)-(a)^(x+1))/(x+1)]];
    ];
    If[request==="nor",
        Return[Function[x,Sqrt[(b-a)/(2x+1)](b-a)^x (x!)^2/(2x)!]];
    ];  
    If[request==="turan",
        Return[Function[{x1,x2,n},int=(Append[Append[2Delete[x2,-1]-x1,0],0]+
            Prepend[Prepend[2Delete[x2,1]-x1,0],0]);
            Join[{If[int[[1]]<-1,-1,int[[1]]],int[[2]]},Take[int,{3,-3}]/2,
                {int[[-2]],If[int[[-1]]>1,1,int[[-1]]]}]]];
    ];
    If[request==="turanAllowed",
        Return[True];
    ];
    ] /; request[[0]]===String;

Unprotect[Integrate];
Integrate[aLegendre[n_,x_] aLegendre[m_,x_] aLegendre["wth"][x_],{x_,-1,1}]:=(aLegendre["nor"][n])^2 KroneckerDelta[n,m];
Integrate[aLegendre[n_,x_]^2 aLegendre["wth"][x_],{x_,-1,1}]:=(aLegendre["nor"][n])^2;
Integrate[aLegendre[n_,Cos[x_]],{x_,0,2 Pi}]:=2 Pi (Binomial[n,n/2])^2/Binomial[2n,n] 2^(- n)/;EvenQ[n];
Integrate[(1+x_)^(m_+n_)aLegendre[m_,x_]aLegendre[n_,x_],{x_,-1,1}]:=(2^(m+n+1)((m+n)!)^4)/(((m!)(n!))^2(2m+2n+1)!);
Integrate[(1+x_)^(m_-n_-1)aLegendre[m_,x_]aLegendre[n_,x_],{x_,-1,1}]:=If[m>n,0];
Protect[Integrate];

aLegendre[Order_?IntegerQ/;Order>-1,x_?NumberQ,Ops___]:=
    Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,ret,list,wrprec,num},
    If[Order==0,Return[1]];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.{Ops}/.Options[aLegendre];
    {Alfa,Beta}=aLegendre["ttr"][k];
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
    
End[]

EndPackage[]
