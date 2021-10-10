


BeginPackage["OrthogonalPolynomials`Continuous`ChebyshevII`"]

aChebyshevIIInt={-1,1};
aChebyshevII::usage="aChebyshevII[n,x,ops] returns value of Chebyhsev polynomial of the second \
    kind of degree n at point x, \
    the third argument ops is optional and can be used to specify WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision), ReturnList (def. ReturnList->False) \
    to specify if function should return values at point x of all polynomials \
    degree less then or equal to n, or just of polynomial of degree n, Numerator \
    (def. Numerator->0) specify if numerator polynomial should be returned of order \
    which is specified in option.";
Options[aChebyshevII]={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False,
    Numerator->0};
Attributes[aChebyshevII]={ReadProtected};

Begin["`Private`"]

aChebyshevII[request_,left_:Hold[aChebyshevIIInt[[1]]],right_:Hold[aChebyshevIIInt[[2]]]]:=
    Module[{a,b},a=Release[left];b=Release[right];
    If[request==="sint",
        aChebyshevIIInt={a,b};
    ];
    If[request==="gint",
        Return[aChebyshevIIInt];
    ];
    If[request==="ttr",     
        Return[Function[x,If[x=!=0,{(b+a)/2,(b-a)^2/16},
            {(b+a)/2,(b-a)Pi/4}]]];
    ];  
    If[request==="wth",
        Return[Function[x,Sqrt[1-((2x-(b+a))/(b-a))^2]]];
    ];
    If[request==="mom",
        Return[Function[x,(1+(-1)^x)Sqrt[Pi]Gamma[(x+1)/2]/(4 Gamma[2+x/2])]];
    ];
    If[request==="nor",
        Return[Function[x,If[x=!=0,Sqrt[(b-a)Pi/2](b-a)^x/2^(2x+1),Sqrt[(b-a)Pi]/2]]];
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
Integrate[aChebyshevII[n_,x_] aChebsyhevII[m_,x_] aChebyshevII["wth"][x_],{x_,-1,1}]:=(aChebyshevII["nor"][n])^2 KroneckerDelta[n,m];
Integrate[aChebyshevII[n_,x_]^2 aChebyshevII["wth"][x_],{x_,-1,1}]:=(aChebyshevII["nor"][n])^2;
Protect[Integrate];

aChebyshevII[Order_?IntegerQ/;Order>-1,x_,Ops___]:=
    Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,ret,list,cond,wrprec,num},
    If[Order==0,Return[1]];
    {Alfa,Beta}=aChebyshevII["ttr"][k];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.{Ops}/.Options[aChebyshevII];
    Block[{$MinPrecision=wrprec},
    pom1 = x;pom2=pom1;If[list,ret={1,x};];
    For[i = 1, i < Order, i++,
            pom2 = Expand[x pom1 - pom (Beta /. k ->(i+num))];
            pom = pom1;
            pom1 = pom2;
        If[list,AppendTo[ret,pom2];];
        ];];
        If[list,Return[ret];,Return[pom2];];
      ]         

    
End[]

EndPackage[]
