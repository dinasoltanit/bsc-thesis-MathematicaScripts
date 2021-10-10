


BeginPackage["OrthogonalPolynomials`Complex`GautschiMilovanovic`"]

aGautschiMilovanovic::usage="aGautschiMilovanovic[n,lambda,x] returns value of the \
    Gautschi-Milovanovic polynomial of degree n at the point x, \
    lambda is parametar of the family and can take any real number bigger then -1/2. \
    The third argument ops is optional and can be used to specify \
    WorkingPrecision (def. WorkingPrecision->$MachinePrecision), ReturnList \
    (def. ReturnList->False) to specify if function should return values at point x \
    of all polynomials with degrees less then or equal to n, or just of polynomial \
    with degree n, Numerator (def. Numerator->0) specify if numerator polynomial should be \
    returned of order which is specified in option.";
aGautschiMilovanovic::supp="Upper half of the unit circle.";
aGautschiMilovanovic::bdpmt="Parameter of the GautschiMilovanovic family must be bigger then \
    -1/2.";
Options[aGautschiMilovanovic]={WorkingPrecision->$MachinePrecision,
    OrthogonalPolynomials`ReturnList->False,Numerator->0};
Attributes[aGautschiMilovanovic]={ReadProtected};


Begin["`Private`"]

aGautschiMilovanovic[request_,lambda_]:=
    Module[{},
    If[!Positive[lambda+1/2],Message[aGautschiMilovanovic::bdpmt];Abort[];];
    If[request==="ttr",     
        Return[Function[x,If[x=!=0,
            {I/(lambda+x)Gamma[(x+2)/2]/Gamma[(x+1)/2]
            Gamma[lambda+(x+1)/2]/Gamma[lambda+x/2]-I Gamma[lambda+x/2]If[x>1,
            1/(lambda+x-1)Gamma[(x+1)/2]/Gamma[x/2]
            1/Gamma[lambda+(x-1)/2],
            1/Sqrt[Pi]1/Gamma[lambda+1]],
            (Gamma[lambda+x/2]Gamma[(x+1)/2]/Gamma[x/2])^2If[x>1,(1/(lambda+x-1)
            1/Gamma[lambda+(x-1)/2])^2,
            (1/Gamma[lambda+1])^2]},
            {I/Sqrt[Pi]Gamma[lambda+1/2]/Gamma[lambda+1],Pi}]]];
    ];  
    If[request==="wth",
        Return[Function[x,-I(1-x^2)^(lambda-1/2)/x]];
    ];
    If[request==="mom",
        Return[Function[x,If[x=!=0,
            I Gamma[lambda+1/2]/x Gamma[1+x/2]/Gamma[(x+1)/2+lambda](1-Exp[I x Pi]),
            Pi]]];
    ];
    If[request==="nor",
        Return[Function[x,If[And[x==0,lambda==0],Sqrt[Pi],Gamma[(x+1)/2]Gamma[lambda+x/2]/Gamma[lambda+x]]]];
    ];  
    If[request==="supp",
        Message[aGautschiMilovanovic::supp];
    ];
    If[request==="pas",
        Return[Function[n,{{},(
            (-(2 lambda+1)(n(n+2 lambda-1)-4(n+lambda-1)^2(1/(lambda+n-1)
            Gamma[(n+1)/2]/Gamma[n/2] Gamma[lambda+n/2]/Gamma[lambda+(n-1)/2])^2) #1+
            2(2n+2 lambda-1)(n+lambda-1)(1+2 lambda #1^2)I(1/(lambda+n-1)
            Gamma[(n+1)/2]/Gamma[n/2] Gamma[lambda+n/2]/Gamma[lambda+(n-1)/2]))/
            (2(1-#1^2)(n(n+2 lambda-1)-4(n+lambda-1)^2(1/(lambda+n-1)
            Gamma[(n+1)/2]/Gamma[n/2] Gamma[lambda+n/2]/Gamma[lambda+(n-1)/2])^2-
            2(2 n+2 lambda-1)(n+lambda-1)#1 I(1/(lambda+n-1)
            Gamma[(n+1)/2]/Gamma[n/2] Gamma[lambda+n/2]/Gamma[lambda+(n-1)/2]))))&}]];
    ];
    If[request==="turanAllowed",
        Return[False];
    ];
    ] /; request[[0]]===String;

Unprotect[Integrate];
Integrate[aGautschiMilovanovic[n_,lambda_,Exp[I x_]] aGautschiMilovanovic[m_,lambda_,Exp[I x_]] aGautschiMilovanovic["wth",lambda_][Exp[I x_]],{x_,0,Pi}]:=(aGautschiMilovanovic["nor",lambda][n])^2 KroneckerDelta[n,m];
Integrate[aGautschiMilovanovic[n_,lambda_,Exp[I x_]]^2 aGautschiMilovanovic["wth",lambda_][Exp[I x_]],{x_,0,Pi}]:=(aGautschiMilovanovic["nor",lambda][n])^2;
Protect[Integrate];

aGautschiMilovanovic[Order_?IntegerQ/;Order>-1,lambda_,x_,Ops___]:=
    Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,ret,list,num,wrprec},
    If[!Positive[lambda+1/2],Message[aGautschiMilovanovic::bdpmt];Abort[];];
    If[Order==0,Return[1]];
    {Alfa,Beta}=aGautschiMilovanovic["ttr",lambda][k];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.{Ops}/.Options[aGautschiMilovanovic];
    Block[{$MinPrecision=wrprec},
    pom1 = Expand[(x - (aGautschiMilovanovic["ttr",lambda][num])[[1]])];pom2=pom1;
    If[list,ret={1,pom2};];
    For[i = 1, i < Order, i++,
            pom2 = Expand[(x - (Alfa /. k -> (i+num)))pom1 - pom (Beta /. k -> (i+num))];
            pom = pom1;
            pom1 = pom2;
        If[list,AppendTo[ret,pom2];];
        ];];
        If[list,Return[ret],Return[pom2]];
      ]         

    
End[]

EndPackage[]
