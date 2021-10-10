


BeginPackage["OrthogonalPolynomials`Continuous`Gegenbauer`",{"OrthogonalPolynomials`Kernel'Common`"}]

aGegenbauerInt={-1,1};
aGegenbauer::usage="aGegenbauer[n,lambda,x,ops] returns value of Gegenbauer polynomial \
    of degree n at point x, lambda is parameter of the family and sholud be bigger then -1/2. \
    The forth argument ops is optional and can be used to specify WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision), ReturnList (def. ReturnList->False) \
    to specify if function should return values at point x of all polynomials \
    degree less then or equal to n, or just of polynomial of degree n, Numerator \
    (def. Numerator->0) specify if numerator polynomial should be returned of order \
    which is specified in option.";
aGegenbauer::bdpmt="Parameter of the family must be bigger then -1/2.";
Options[aGegenbauer]={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False,
    Numerator->0};
Attributes[aGegenbauer]={ReadProtected};

Begin["`Private`"]

aGegenbauer[request_,lambda_,left_:Hold[aGegenbauerInt[[1]]],right_:Hold[aGegenbauerInt[[2]]]]:=
    Module[{a,b,int,alfa,c},a=Release[left];b=Release[right];
    If[NumberQ[lambda] && !Positive[lambda+1/2],Message[aGegenbauer::bdpmt];Abort[];];
    If[request==="sint",
        aGegenbauerInt={lambda,left};
    ];
    If[request==="gint",
        Return[aGegenbauerInt];
    ];
    If[request==="ttr",     
        Return[Function[x,If[x=!=0,{(b+a)/2,
            (b-a)^2/16 x/(x+lambda) 
            If[x==1 && lambda==0,2,(x+2 lambda-1)/(x+lambda-1)]},
            {(b+a)/2,(b-a)/2 Sqrt[Pi] Gamma[1/2+lambda]/Gamma[1+lambda]}]]];
    ];  
    If[request==="wth",
        Return[Function[x,(1-((2 x-(a+b))/(b-a))^2)^(lambda-1/2)]];
    ];
    If[request==="mom",
        Return[Function[x,
            (1+(-1)^x)Gamma[(x+1)/2]Gamma[1/2+lambda]/(2Gamma[1+x/2+lambda])]];
    ];
    If[request==="nor",alfa=lambda-1/2;
        Return[Function[x,(b-a)^x/2^x If[Odd[x],c=(x-1)/2;Sqrt[c! Gamma[c+alfa+1]*
            Gamma[c-1/2+2]Gamma[c+alfa-1/2+2]/(Gamma[x+alfa-1/2+1]*
            Gamma[x+alfa-1/2+2])],c=x/2;Sqrt[c! Gamma[c+alfa+1]Gamma[c-1/2+1]*
            Gamma[c+alfa-1/2+1]/(Gamma[x+alfa-1/2+1]Gamma[x+alfa-1/2+2])]]]];
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
Integrate[aGegenbauer[n_,lambda_,x_] aGegenbauer[m_,lambda_,x_] aGegenbauer["wth",lambda_][x_],{x_,-1,1}]:=(aGegenabuer["nor",lambda][n])^2 KroneckerDelta[n,m];
Integrate[aGegenbauer[n_,lambda_,x_]^2 aGegenbauer["wth",lambda_][x_],{x_,-1,1}]:=(aGegenabuer["nor",lambda][n])^2;
Protect[Integrate];

aGegenbauer[Order_?IntegerQ/;Order>-1,lambda_,x_,Ops___]:=
    Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,ret,list,wrprec,num},
    If[NumberQ[lambda] && !Positive[lambda+1/2],Message[aGegenbauer::bdpmt];Abort[];];
    {Alfa,Beta}=aGegenbauer["ttr",lambda][k];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.{Ops}/.Options[aGegenbauer];
    Block[{$MinPrecision=wrprec},
    If[Order==0,Return[1]];
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
