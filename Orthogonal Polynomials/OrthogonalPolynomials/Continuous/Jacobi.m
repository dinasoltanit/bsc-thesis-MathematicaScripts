


BeginPackage["OrthogonalPolynomials`Continuous`Jacobi`"]

aJacobiInt={-1,1};
aJacobi::usage="aJacobi[n,alfa,beta,x,ops] returns value of Jacobi polynomial \
    of degree n at point x, beta and alfa are parameters of the family and they sholud \
    be bigger then -1. \
    The fifth argument ops is optional and can be used to specify WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision), ReturnList (def. ReturnList->False) \
    to specify if function should return values at point x of all polynomials \
    degree less then or equal to n, or just of polynomial of degree n, Numerator \
    (def. Numerator->0) specify if numerator polynomial should be returned of order \
    which is specified in option.";
aJacobi::bdpmt="Parameters of the family must be bigger then -1.";
Options[aJacobi]={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False,
    Numerator->0};
Attributes[aJacobi]={ReadProtected};

Begin["`Private`"]

aJacobi[request_,alfa_,beta_,left_:Hold[aJacobiInt[[1]]],right_:Hold[aJacobiInt[[2]]]]:=
    Module[{a,b,int,gamma,delta},a=Release[left];b=Release[right];
    If[NumberQ[alfa] && !Positive[alfa+1],Message[aJacobi::bdpmt];Abort[];];
    If[NumberQ[beta] && !Positive[beta+1],Message[aJacobi::bdpmt];abort[];];
    If[request==="sint",
        aJacobiInt={alfa,beta};
    ];
    If[request==="gint",
        Return[aJacobiInt];
    ];
    If[request==="ttr",     
        Return[Function[x,If[x=!=0,{(b-a)/2((beta^2-alfa^2)/((2 x+alfa+beta)
            (2 x+alfa+beta+2))+(b+a)/(b-a)),(b-a)^2 x(x+alfa)
            (x+beta)/(2x+alfa+beta)^2 If[alfa+beta==-1 && x==1,1/2,
                (x+alfa+beta)/((2x+alfa+beta)^2-1)]},
            {(b-a)/2((beta-alfa)/(alfa+beta+2)+(b+a)/(b-a)),(b-a)
            2^(alfa+beta)/Gamma[2+alfa+beta] Gamma[1+alfa] Gamma[1+beta]}]]];
    ];  
    If[request==="wth",
        Return[Function[x,(-2/(b-a)x+2 b/(b-a))^alfa*(2/(b-a)x-2 a/(b-a))^beta]];
    ];
    If[request==="mom",
        Return[Function[x,gamma=b-a;delta=b+a;
		gamma^(alfa+beta+1)/2^(alfa+beta+x+1)
		If[delta != 0,
		Plus@@Table[Binomial[x,ell] gamma^ell delta^(x-ell) 
		Gamma[ell+1] 
		((-1)^ell Gamma[beta+1] Hypergeometric2F1Regularized[-alfa,ell+1,beta+ell+2,-1] 
		+Gamma[alfa+1] Hypergeometric2F1Regularized[-beta,ell+1,alfa+ell+2,-1]),
		{ell,0,x}]
		,
		gamma^x Gamma[x+1]
		((-1)^x Gamma[beta+1] Hypergeometric2F1Regularized[-alfa,x+1,beta+x+2,-1]
		+Gamma[alfa+1] Hypergeometric2F1Regularized[-beta,x+1,alfa+x+2,-1])
		]
		]];
    ];
    If[request==="nor",
        Return[Function[x,((b-a)/2)^x Sqrt[2^(2 x+alfa+beta)x!(b-a)Gamma[x+alfa+1]
            Gamma[x+beta+1]Gamma[x+alfa+beta+1]/(2 x+alfa+beta+1)]/
            Gamma[2 x+alfa+beta+1]]];
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
Integrate[aJacobi[n_,alpha_,beta_,x_] aJacobi[m_,alpha_,beta_,x_] aJacobi["wth",alpha_,beta_][x_],{x_,-1,1}]:=(aJacobi["nor",alpha,beta][n])^2 KroneckerDelta[n,m];
Integrate[aJacobi[n_,alpha_,beta_,x_]^2 aJacobi["wth",alpha_,beta_][x_],{x_,-1,1}]:=(aJacobi["nor",alpha,beta][n])^2;
Protect[Integrate];

aJacobi[Order_?IntegerQ/;Order>-1,alfa_,beta_,x_,Ops___]:=
    Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,ret,list,wrprec,num},
    If[NumberQ[alfa] && !Positive[alfa+1],Message[aJacobi::bdpmt];Abort[];];
    If[NumberQ[beta] && !Positive[beta+1],Message[aJacobi::bdpmt];abort[];];
    If[Order==0,Return[1]];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.{Ops}/.Options[aJacobi];
    {Alfa,Beta}=aJacobi["ttr",alfa,beta][k];
    Block[{$MinPrecision=wrprec},
    pom1 = Expand[(x - (aJacobi["ttr",alfa,beta][0])[[1]])];pom2=pom1;If[list,ret={1,pom2};];
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
