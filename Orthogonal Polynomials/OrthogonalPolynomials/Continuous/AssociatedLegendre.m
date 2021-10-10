


BeginPackage["OrthogonalPolynomials`Continuous`AssociatedLegendre`"]

aAssociatedLegendreInt={-1,1};
aAssociatedLegendre::usage="aAssociatedLegendre[n,ni,x,ops] returns value of
    associated Legendre polynomial of degree n at point x, parameter ni is parameter of the \
    family and for the relations in the package to be valid this parameter should be \
    positive. The third argument ops is optional and can be used to specify WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision), ReturnList (def. ReturnList->False) \
    to specify if function should return values at point x of all polynomials \
    degree less then or equal to n, or just of polynomial of degree n, Numerator \
    (def. Numerator->0) specify if numerator polynomial should be returned of order \
    which is specified in option.";
aAssociatedLegendre::bdpmt="Parameter of the family aAssociatedLegendre must be positive.";
Options[aAssociatedLegendre]:={WorkingPrecision->$MachinePrecision,
    OrthogonalPolynomials`ReturnList->False,Numerator->0};
Attributes[aAssociatedLegendre]={ReadProtected};

Begin["`Private`"]

aAssociatedLegendre[request_,ni_,left_:Hold[aAssociatedLegendreInt[[1]]],
right_:Hold[aAssociatedLegendreInt[[2]]]]:=
    Module[{a,b},a=Release[left];b=Release[right];
    If[NumberQ[ni] && !Positive[ni],Message[aAssociatedLegendre::bdpmt];Abort[];];
    If[request==="sint",
        aAssociatedLegendreInt={a,b};
    ];
    If[request==="gint",
        Return[aAssociatedLegendreInt];
    ];
    If[request==="ttr",     
        Return[Function[x,If[x=!=0,{(b+a)/2,(b-a)^2 (x+ni)^2/(4(4 (x+ni)^2-1))},
            {(b+a)/2,(b-a)/(2 ni-1)}]]];
    ];  
    If[request==="wth",
        Return[Function[x,1/((ni LegendreQ[ni-1,x])^2+(ni Pi LegendreP[ni-1,x]/2)^2)]];
    ];
    If[request==="mom",
        Return[Function[k,Integrate[OrthogonalPolynomials`intVariable^k/((ni LegendreQ[ni-1,OrthogonalPolynomials`intVariable])^2+(ni Pi            LegendreP[ni-1,OrthogonalPolynomials`intVariable]/2)^2),{OrthogonalPolynomials`intVariable,-1,1}]]];
    ];
    If[request==="nor",
        Return[Function[x,Sqrt[(b-a)/(2x+2ni-1)]Gamma[ni+1+x]/Gamma[ni+1]
            Gamma[ni+1/2]/Gamma[ni+x+1/2](b-a)^x/2^(2 x)]];
    ];
    If[request==="turan",
        Return[Function[{x1,x2,n},int=(Append[Append[2Delete[x2,-1]-x1,0],0]+
            Prepend[Prepend[2Delete[x2,1]-x1,0],0]);
            Join[{If[int[[1]]<-1,-1,int[[1]]],int[[2]]},Take[int,{3,-3}]/2,
                {int[[-2]],If[int[[-1]]>1,1,int[[-1]]]}]]];
    ];          
    ] /; request[[0]]===String;

Unprotect[Integrate];
Integrate[aAssociatedLegendre[n_,ni_,x_] aAssociatedLegendre[m_,ni_,x_] aAssociatedLegendre["wth",ni_][x_],{x_,-1,1}]:=(aAssociatedLegendre["nor",ni][n])^2 KroneckerDelta[n,m];
Integrate[aAssociatedLegendre[n_,ni_,x_]^2 aAssociatedLegendre["wth",ni_][x_],{x_,-1,1}]:=(aAssociatedLegendre["nor",ni][n])^2;
Protect[Integrate];

aAssociatedLegendre[Order_?IntegerQ/;Order>-1,ni_,x_,Ops___]:=
    Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,wrprec,cond,list,ret,num},
    If[NumberQ[ni] && !Positive[ni],Message[aAssociatedLegendre::bdpmt];Abort[];];
    If[Order==0,Return[1]];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.List[Ops]/.Options[aAssociatedLegendre];
    Block[{$MinPrecision=wrprec},
    {Alfa,Beta}=aAssociatedLegendre["ttr",ni][k];
    pom1 = Expand[(x - (Alfa/.k->num))];pom2=pom1;If[list,ret={1,pom2};];
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
