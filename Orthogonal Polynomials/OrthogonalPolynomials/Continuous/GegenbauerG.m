


BeginPackage["OrthogonalPolynomials`Continuous`GegenbauerG`",{"OrthogonalPolynomials`Kernel`Common`"}]

aGegenbauerGInt={-1,1};
aGegenbauerG::usage="aGegenbauerG[n,beta,alfa,x,ops] returns value of generalized Gegenbauer polynomial \
    of degree n at point x, beta and alfa are parameters of the family and they sholud \
    be bigger then -1. \
    The fifth argument ops is optional and can be used to specify WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision), ReturnList (def. ReturnList->False) \
    to specify if function should return values at point x of all polynomials \
    degree less then or equal to n, or just of polynomial of degree n, Numerator \
    (def. Numerator->0) specify if numerator polynomial should be returned of order \
    which is specified in option.";
aGegenbauerG::bdpmt="Parameters of the generalized Gegenbauer polynomials must be bigger then -1.";
Options[aGegenbauerG]={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False,
    Numerator->0};
Attributes[aGegenbauerG]={ReadProtected};

Begin["`Private`"]

aGegenbauerG[request_,beta_,alfa_,left_:Hold[aGegenbauerGInt[[1]]],
    right_:Hold[aGegenbauerGInt[[2]]]]:=Module[{a,b,c,int},a=Release[left];b=Release[right];
    If[NumberQ[alfa] && !Positive[alfa+1],Message[aGegenbauerG::bdpmt];Abort[];];
    If[NumberQ[beta] && !Positive[2 beta+2],Message[aGegenbauerG::bdpmt];Abort[];];
    If[request==="sint",
        aGegenbauerGInt={beta,alfa};
    ];
    If[request==="gint",
        Return[aGegenbauerGInt];
    ];
    If[request==="ttr", 
        Return[Function[x,If[x=!=0,{(b+a)/2,(b-a)^2/4 If[Odd[x],
            ((x+1)/2+beta)((x+1)/2+beta+alfa),(x/2)(x/2+alfa)]
            /((x+alfa+beta)(x+alfa+beta+1))},
            {(b+a)/2,(b-a)/2 Gamma[alfa+1]Gamma[beta+1]/Gamma[2+alfa+beta]}]]];
    ];  
    If[request==="wth",
        Return[Function[x,Abs[(2x-(a+b))/(b-a)]^(2 beta+1)
            (1-((2 x-(a+b))/(b-a))^2)^alfa]];
    ];
    If[request==="mom",
        Return[Function[x,
            (1+(-1)^x)/2 Gamma[1+alfa]Gamma[x/2+beta+1]/Gamma[2+x/2+alfa+beta]]];
    ];
    If[request==="nor",
        Return[Function[x,(b-a)^x/2^x If[Odd[x],c=(x-1)/2;Sqrt[c! Gamma[c+alfa+1]*
            Gamma[c+beta+2]Gamma[c+alfa+beta+2]/(Gamma[x+alfa+beta+1]*
            Gamma[x+alfa+beta+2])],c=x/2;Sqrt[c! Gamma[c+alfa+1]Gamma[c+beta+1]*
            Gamma[c+alfa+beta+1]/(Gamma[x+alfa+beta+1]Gamma[x+alfa+beta+2])]]]];
    ];  
    If[request==="turan",If[Abs[beta/alfa]<8,
        Return[Function[{x1,x2,n},int=(Append[Append[2Delete[x2,-1]-x1,0],0]+
            Prepend[Prepend[2Delete[x2,1]-x1,0],0]);
            Join[{If[int[[1]]<-1,-1,int[[1]]],int[[2]]},Take[int,{3,-3}]/2,
                {int[[-2]],If[int[[-1]]>1,1,int[[-1]]]}]]];,Print["another"];
        Return[Function[{x1,x2,n},
            If[n==4,
            int={2x2[[1]]-x1[[1]],2x2[[2]]-x1[[2]]};
            int=Join[int,-Reverse[int]];,
            If[OddQ[n],
                int=2Take[x2,(n-1)/2]-Take[x1,(n-1)/2]; 
                int=Join[int,{0},-Reverse[int]];,
            int=2Take[x2,n/2-1]-Take[x1,n/2-1]; 
                AppendTo[int,(2Sqrt[x2[[n/2-1]]]-Sqrt[x1[[n/2-2]]])^2]; 
                int=Join[int,-Reverse[int]];];];int]];];
    ];
    If[request==="turanAllowed",
        Return[True];
    ];  
    ] /; request[[0]]===String;

Unprotect[Integrate];
Integrate[aGegenbauerG[n_,beta_,alfa_,x_] aGegenbauerG[m_,beta_,alfa_,x_] aGegenbauerG["wth"][x_,beta_,alfa_],{x_,-1,1}]:=(aGegenbauerG["nor",beta,alfa][n])^2 KroneckerDelta[n,m];
Integrate[aGegenbauerG[n_,beta_,alfa_,x_]^2 aGegenbauerG["wth"][x_,beta_,alfa_],{x_,-1,1}]:=(aGegenbauerG["nor",beta,alfa][n])^2;
Protect[Integrate];

aGegenbauerG[Order_?IntegerQ/;Order>-1,beta_,alfa_,x_,Ops___]:=
    Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,cond,ret,list,wrprec},
    If[NumberQ[alfa] && !Positive[alfa+1],Message[aGegenbauerG::bdpmt];Abort[];];
    If[NumberQ[beta] && !Positive[2 beta+2],Message[aGegenbauerG::bdpmt];Abort[];];
    If[Order==0,Return[1]];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.{Ops}/.Options[aGegenbauerG];
    {Alfa,Beta}=aGegenbauerG["ttr",beta,alfa][k];
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
