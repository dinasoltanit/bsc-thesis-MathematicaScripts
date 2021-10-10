


BeginPackage["OrthogonalPolynomials`Complex`BesselG`"]

aBesselG::usage="aBesselG[n,alfa,x,ops] returns value of generalized Bessel polynomial of \
    degree n at the point x, alfa is parametar of the family and can take any complex number \
    but negative integers. The third argument ops is optional and can be used to specify \
    WorkingPrecision (def. WorkingPrecision->$MachinePrecision), ReturnList \
    (def. ReturnList->False) to specify if function should return values at point x \
    of all polynomials with degrees less then or equal to n, or just of polynomial \
    with degree n, Numerator (def. Numerator->0) specify if numerator polynomial should be \
    returned of order which is specified in option.";
aBesselG::supp="Unit circle.";
aBesselG::bdpmt="Parameter of the generalized Bessel polynomials can not take negative integer \
    as a value.";
Options[aBesselG]={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False,
    Numerator->0};
Attributes[aBesselG]={ReadProtected};

Begin["`Private`"]

aBesselG[request_,alfa_]:=Module[{},
    If[NumberQ[alfa] && alfa<=0 && IntegerQ[alfa],
        Message[aBesselG::bdpmt];Abort[];];
    If[request=="ttr",
        Return[Function[x,If[x=!=0,{-2alfa/((2x+alfa+2)(2x+alfa)),
            -4x/(2x+alfa)^2 (x+alfa)/((2x+alfa)^2-1)},
            {-2/(alfa+2),-4 Pi I/(alfa+1)}]]];
    ];
    If[request=="mom",
        Return[Function[x,Gamma[alfa+1]/Gamma[alfa+2+x](-2)^(x+1)]];
    ];
    If[request=="wth",
        Return[Function[x,Gamma[1+alfa](-x/2)^alfa
            Exp[-2/x](1-Gamma[alfa,-2/x]/Gamma[alfa])]];
    ];
    If[request=="nor",
        Return[Function[x,2^(-x) Gamma[(alfa+1)/2]/Gamma[x+(alfa+1)/2]
            Gamma[(alfa+2)/2]/Gamma[x+(alfa+2)/2]Gamma[x+alfa+1]/Gamma[alfa+1]
            Sqrt[4 Pi I (-1)^(x+1)/(2x+alfa+1) x!
            Gamma[alfa+1]/Gamma[alfa+1+x]]]];
    ];
    If[request=="pas",
        Return[Function[x,{aSPoints[x,alfa],(((alfa/2+1)#1+1)/#1^2)&}]];
    ];
    If[request=="supp",
        Message[aBesselG::supp];
    ];
    If[request==="turanAllowed",
        Return[False];
    ];
    ] /;request[[0]]===String;

Unprotect[Integrate];
Integrate[aBesselG[n_,alpha_,Exp[I x_]] aBesselG[m_,alpha_,Exp[I x_]] aBesselG["wth",alpha_][Exp[I x_]] Exp[I x_],{x_,-Pi,Pi}]:=(aBesselG["nor",alpha][n])^2 KroneckerDelta[n,m]/I;
Integrate[aBesselG[n_,alpha_,Exp[I x_]]^2 aBesselG["wth",alpha_][Exp[I x_]] Exp[I x_],{x_,-Pi,Pi}]:=(aBesselG["nor",alpha][n])^2/I;
Protect[Integrate];

aBesselG[Order_Integer/;Order>-1,alfa_,x_,Ops___]:=
    Module[{i,k,Alfa,Beta,pom1,pom=1,pom2,list,ret,wrprec,num},
    If[NumberQ[alfa] && alfa<0 && IntegerQ[alfa],
        Message[aBesselG::bdpmt];Abort[];];
    If[Order==0,Return[1]];
    {Alfa,Beta}=aBesselG["ttr",alfa][k];
    {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
        Numerator}/.{Ops}/.Options[aBesselG];
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

aSPoints[n_,alfa_]:=Module[{hn,n1,den2,sro,csn,rf,ro,teta,dteta,k,z={}},
    n1=Ceiling[-1-alfa];den2=n+alfa+1;
    If[den2>=0,
        hn=Floor[n/2];sro=2/(2n+alfa+4/3);
        csn=If[ro=-(alfa+2)/(2n+alfa);Abs[ro]>1,1,ro];
        If[alfa<-4 && n<n1+Ceiling[-alfa/4],
            rf=(n-n1+1)/(Ceiling[-alfa/4]-n1+1);,rf=1;];
        teta=ArcCos[csn];dteta=2(Pi-teta)/(n+1);
        For[k=1,k<=hn,k++,
            teta+=(1+.2 Sin[2Pi(k-.5)/hn])dteta;
            ro=(sro+1.15(1-Cos[teta])/den2)/2.15;
            AppendTo[z,ro rf Exp[I teta]];
        ];
        z=Join[z,Reverse[Conjugate[z]]];
        If[OddQ[n],
            z=Insert[z,-rf(sro+2.3/den2)/2.15,hn+1];];
        Return[z];
    ,
        
    ];
    ];

End[]

EndPackage[]
