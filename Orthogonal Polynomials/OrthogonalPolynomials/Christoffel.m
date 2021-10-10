

BeginPackage["OrthogonalPolynomials`Christoffel`",{"OrthogonalPolynomials`Kernel`Common`"}]

Unprotect[aChristoffelAlgorithm];
Unprotect[aLinear];
Unprotect[aQuadratic];
Unprotect[aQuadraticSymmetric];
Unprotect[aLinearDivisor];
Unprotect[aQuadraticDivisor];
Unprotect[aQuadraticDivisorSymmetric];
Unprotect[aQudraticReal];
Unprotect[aModify];
Unprotect[aFunctionIIKind];
Unprotect[aStartingPoint];
Unprotect[aGaussianKernel];


aChristoffelAlgorithm::usage = "aChristoffelAlgorithm[Order,{mod,moOps},alpha,beta,Ops] returns \
    Order number of modified three term recurrence coefficients for modification \
    specified by argument mod and with parameters of modification given by modOps, for \
    sequence of polynomials given by three term recurrence coefficients alpha and beta. Ops \
    are options. WorkingPrecision (def. WorkingPrecision->$Machineprecision) sets precision \
    used in calculations. Algorithm (def. Algorithm->Automatic) if set to Symbolic \
    performs algorithm in symbolical form, otherwise perform numerical version.";
aChristoffelAlgorithm::bdinp="Lists alpha and beta must have length at least equal to order.";
aChristoffelAlgorithm::bdmod="You have to supply Modification option which is one of the \
    following aLinear, aQuadratic, aQuadraticSymmetric, aLinearDivisor, aQuadraticDivisor, \
    aQuadraticDivisorSymmetric, aQuadraticReal.";
aChristoffelAlgorithm::bdlnr="When modification is made by linear factor, zero of that factor \
    has to be supplied as the second argument of list including key word aLinear.";
aChristoffelAlgorithm::bdquad="When modification is made by quadratic factor with complex zeros \
    that complex zero has to be supplied. Real part is supplied first \ 
    and imaginary part, which must be different from zero, \
    is supplied second after key word aQuadratic.";
aChristoffelAlgorithm::bdqsym="When modification is made by quadratic factor with imaginary zeroes \
    imaginary part of that zero has to be supplied after key word aQuadraticSymmetric."
aChristoffelAlgorithm::bdldiv="When modification is made by linear divisor, then zero of linear \
    divisor and value of integral for modified measure may or may not be supplied after key word \
    aLinearDivisor, respectively.";
aChristoffelAlgorithm::bdqdiv="When modification is made by quadratic divisor with imaginary \
    zeroes, after key word aQuadraticDivisor, real part and imaginary part, which must be different \
    from zero, of the zero must be supplied. Also real and imaginary part of the \
    integral of modified measure may or may not be supplied.";
aChristoffelAlgorithm::bdqsdiv="When modification is made by quadratic factor with imaginary zero \
    imaginary part of that zero and imaginary part of integral for the modified measure have \
    to be supplied after key word aQuadraticDivisorSymmetric.";
aChristoffelAlgorithm::bdqrl="When modification is made by quadratic factor value of zero \
    of the factor has to be supplied.";
    
aModify::usage="aModify[Order,{mod,modOps},{name,polyOps},ops] returns Order number of modified \
    sequences of three term recurrence coefficients for modification given by mod for \
    class of orthogonal polynomials specified by name with parameters given by polyOps. \
    Forth parameter ops are options.";
aFunctionIIKind::bdinp="Coefficients of three term recurrence relation must be lists of lengths \   
    at least 5*IterationDepth+StartingPoint.";
aFunctionIIKind::usage="aFunctionIIKind[Order,stp,z,alfa,beta,ops]";
aFunctionIIKind::idexc="IterationDepth exiceded.";
aFunctionIIKind::stpts="You should supply secound parametar to be grather or equal to first \
    parametar. Taking equal and proceeding.";
aStartingPoint::untype="You have to supply type one of the following aHermite, aLaguerre, \
    aLaguerreG, aJacobi, aGegenabuer,aLegendre,aChebushevI,aChebyshevII or leave it empty.";
aStartingPoint::usage="aStartingPoint[Order,z,type:Automatic,ops]";
aGaussianKernel::usage="aGaussianKernel[Order,stp,z,alfa,beta,ops]";

(*Options*)
Options[aChristoffelAlgorithm] = {Precision->$MachinePrecision-10,Type->{Automatic},
    OrthogonalPolynomials`IterationDepth->5,WorkingPrecision->$MachinePrecision,
    OrthogonalPolynomials`Algorithm->Automatic,GaussianIntegers->False,Extension->None,
    Trig->False,OrthogonalPolynomials`Start->Automatic};
Options[aModify]={Precision->$MachinePrecision-10,Type->{Automatic},
    OrthogonalPolynomials`IterationDepth->5,WorkingPrecision->$MachinePrecision,
    OrthogonalPolynomials`Algorithm->Automatic,GaussianIntegers->False,Extension->None,
    Trig->False,OrthogonalPolynomials`Start->Automatic};
Options[aFunctionIIKind]={Precision->$MachinePrecision-10, 
     OrthogonalPolynomials`IterationDepth->5,WorkingPrecision->$MachinePrecision};
Options[aStartingPoint]={Precision->6};
Options[aGaussianKernel]={Precision->$MachinePrecision-10, 
     OrthogonalPolynomials`IterationDepth->5,WorkingPrecision->$MachinePrecision};
(*Options ends*)


(*Attributes    *)
Attributes[aModify]={ReadProtected};
Attributes[aChristoffelAlgorithm]={ReadProtected};
Attributes[aFunctionIIKind]={ReadProtected};
Attributes[aGaussianKernel]={ReadProtected};
Attributes[aStartingPoint]={ReadProtected};
(*Attributes end*)


Begin["`Private`"]

aCAM[momList_?VectorQ,coeBeta_?VectorQ,coeGama_?VectorQ,Ops___]:=
    Module[{beta=List[],gama=List[],len,i,j,pom,listBef,wrprec,cond},
        len=Length[momList];
        If[Length[coeBeta]<(len-1) || Length[coeGama]<(len-1),
            Message[aChristoffelAlgorithm::bdinp];Abort[];];
        {wrprec}={WorkingPrecision}/.{Ops}/.{WorkingPrecision->$MachinePrecision};
        list=momList;
        Block[{$MinPrecision=wrprec},
        AppendTo[beta,coeBeta[[1]]+(list[[2]])/(list[[1]])];
        AppendTo[gama,list[[1]]];
        listBef=Table[0,{i,1,len-2}];
        For[i=1,i<(len-1)/2,i++,
            pom=list[[1]];
            For[j=1,j<=len-2i,j++,
                list[[j]]=list[[j+2]]-gama[[-1]]*listBef[[j]]+
                            coeGama[[i+j]]*list[[j]]-(beta[[-1]]-coeBeta[[i+j]])
                    *list[[j+1]];
                listBef[[j]]=list[[j+1]];
            ];
            AppendTo[beta,coeBeta[[i+1]]+list[[2]]/list[[1]]
                -listBef[[1]]/pom];
            AppendTo[gama,list[[1]]/pom];
            list=Delete[list,{{-1},{-2}}];
            listBef=Delete[listBef,{{1},{-1}}];
        ];];
        Return[{beta,gama}];
    ];  

aChristoffelAlgorithm[Order_?IntegerQ/;Order>1,{mod_,modOps__},ALFA_?VectorQ,BETA_?VectorQ,
    Ops___Rule]:= 
    Module[{alfa=List[],beta=List[],mod,x,y,i,pom0,pom1,pom2,pom3,pom4,pom5,pom6,pom7,hr,hi,
    wrprec,aLFA,bETA,alg,stp,r,eps},
    If[Length[ALFA]<Order || Length[BETA]<Order,Message[aChristoffelAlgorithm::bdinp];
        Abort[];];
        {wrprec,alg,type}={WorkingPrecision,OrthogonalPolynomials`Algorithm,
        Type}/.{Ops}/.Options[aChristoffelAlgorithm];
    If[ToString[alg]==="Symbolic",aLFA=ALFA;bETA=BETA;
        {gI,ext,trig}={GaussianIntegers,Extension,Trig}/.{Ops}
            /.Options[aChristoffelAlgorithm];
        ops=Sequence@@{Extension->ext,Trig->trig};
        opsF=Sequence@@Join[{ops},{GaussianIntegers->gI}];
    ,  
	Block[{$MinPrecision=wrprec}, 
        	aLFA=N[ALFA,wrprec];bETA=N[BETA,wrprec];
	];];
        If[mod==aLinear,x=modOps;
        If[Length[{x}]!=1,
            Message[aChristoffelAlgorithm::bdlnr];Abort[];];
        If[ToString[alg]==="Symbolic",
                pom0=0;
                For[i=1,i<=Order,i++,
                    pom1=Together[aLFA[[i]]-pom0-x,ops];
                AppendTo[beta,Factor[Cancel[pom1*pom0,ops],opsF]]; 
                    pom0=Cancel[bETA[[i+1]]/pom1,ops];
                AppendTo[alfa,Factor[Together[x+pom1+pom0,ops],opsF]];
                ];
                beta[[1]]=Factor[Cancel[bETA[[1]]Together[(aLFA[[1]]-x),ops],ops],opsF];
            Return[{alfa,beta}];
        ,
        Block[{$MinPrecision=wrprec},
            pom0=0;
                For[i=1,i<=Order,i++,
                    pom1=aLFA[[i]]-pom0-x;
                AppendTo[beta,pom1*pom0]; 
                    pom0=bETA[[i+1]]/pom1;
                AppendTo[alfa,x+pom1+pom0];
                ];
                beta[[1]]=bETA[[1]](aLFA[[1]]-x);
            Return[{alfa,beta}];];
        ];
        ];
        If[mod==aQuadratic,
        If[Length[{modOps}]!=2,
            Message[aChristoffelAlgorithm::bdquad];Abort[];];
        {x,y}={modOps};
        If[ToString[alg]==="Symbolic",
                pom0=Together[x-aLFA[[1]],ops];pom1=y;pom2=0;
                For[i=1,i<=Order,i++,
                    pom3=Together[pom0*pom0+pom1*pom1,ops];
                pom4=Cancel[-bETA[[i+1]]*pom0/pom3,ops];
                    pom5=Cancel[bETA[[i+1]]*pom1/pom3,ops];
                pom0=Together[x+pom4-aLFA[[i+1]],ops];
                    pom1=Together[y+pom5,ops];
                    AppendTo[alfa,
                Factor[Together[x+pom1*pom4/pom5-pom0*pom5/pom1,ops],opsF]];
                    AppendTo[beta,
            Factor[Cancel[pom1*pom2*(Together[1+(pom4/pom5)^2,ops]),ops],opsF]];
                pom2=pom5;
                ];
                beta[[1]]=Cancel[bETA[[1]]*
                (Together[bETA[[2]]+(aLFA[[1]]-x)^2+y*y,ops]),ops];
                Return[{alfa,beta}];
        ,
        Block[{$MinPrecision=wrprec},
            pom0=x-aLFA[[1]];pom1=y;pom2=0;
                For[i=1,i<=Order,i++,
                    pom3=pom0*pom0+pom1*pom1;pom4=-bETA[[i+1]]*pom0/pom3;
                    pom5=bETA[[i+1]]*pom1/pom3;pom0=x+pom4-aLFA[[i+1]];
                    pom1=y+pom5;
                    AppendTo[alfa,x+pom1*pom4/pom5-pom0*pom5/pom1];
                    AppendTo[beta,pom1*pom2*(1+(pom4/pom5)^2)];pom2=pom5;
                ];
                beta[[1]]=bETA[[1]]*(bETA[[2]]+(aLFA[[1]]-x)^2+y*y);];
                Return[{alfa,beta}];
        ];  
        ];
        If[mod==aQuadraticSymmetric,y=modOps;
        If[Length[{y}]!=1,
            Message[aChristoffelAlgorithm::bdqsym];Abort[];];
        If[ToString[alg]==="Symbolic",
                pom0=y;pom1=0;alfa=Table[0,{i,1,Order}];
                For[i=1,i<=Order,i++,
                    pom2=Cancel[bETA[[i+1]]/pom0,ops];pom0=Together[y+pom2,ops]; 
                    AppendTo[beta,Factor[Cancel[pom0*pom1,ops],opsF]];pom1=pom2;
                ];
                beta[[1]]=Cancel[bETA[[1]]*(Together[bETA[[2]]+y*y,ops]),ops];
                Return[{alfa,beta}];
        ,
        Block[{$MinPrecision=wrprec},
            pom0=y;pom1=0;alfa=Table[0,{i,1,Order}];
                For[i=1,i<=Order,i++,
                    pom2=bETA[[i+1]]/pom0;pom0=y+pom2; 
                    AppendTo[beta,pom0*pom1];pom1=pom2;
                ];
                beta[[1]]=bETA[[1]]*(bETA[[2]]+y*y);];
                Return[{alfa,beta}];
        ];
        ];
        If[mod==aLinearDivisor,
        If[ToString[alg]==="Symbolic",
            If[Length[{modOps}]!=2,
                Message[aChristoffelAlgorithm::bdldiv];Abort[];];
            {x,hr}={modOps};hr=-hr;
            AppendTo[alfa,Together[x-bETA[[1]]/hr,ops]];
            AppendTo[beta,-hr];pom0=Cancel[-bETA[[1]]/hr,ops];
                For[i=2,i<=Order,i++,
                    pom1=Together[aLFA[[i-1]]-x-pom0,ops];
                AppendTo[beta,Factor[Cancel[ExpandNumerator[
                    ExpandDenominator[pom0*pom1]],ops],opsF]];
                pom0=Cancel[bETA[[i]]/pom1,ops];
                AppendTo[alfa,Factor[Simplify[Together[pom0+pom1+x,ops]],opsF]];
                ];
                Return[{alfa, beta}];
        ,
        If[Length[{modOps}]==2,{x,hr}={modOps};hr=-hr;
        Block[{$MinPrecision=wrprec},
            AppendTo[alfa,x-bETA[[1]]/hr];AppendTo[beta,-hr];pom0=-bETA[[1]]/hr;
                For[i=2,i<=Order,i++,
                    pom1=aLFA[[i-1]]-x-pom0;AppendTo[beta,pom0*pom1]; 
                    pom0=bETA[[i]]/pom1;AppendTo[alfa,pom0+pom1+x];
                ];];
                Return[{alfa, beta}];];
        If[Length[{modOps}]==1,x=modOps;
            {stp}={OrthogonalPolynomials`Start}/.{Ops}/.Options[aChristoffelAlgorithm];
            If[ToString[stp]==="Automatic",
                stp=aStartingPoint[2*Order-1,x,type,Ops,
                    Sequence@@Options[aChristoffelAlgorithm]];];
            r=aFunctionIIKind[2*Order-1,stp,x,aLFA,bETA,Ops,
                Sequence@@Options[aChristoffelAlgorithm]];
            Return[aCAM[-r,aLFA,bETA,Ops]];];
        Message[aChristoffelAlgorithm::bdldiv];Abort[];
        ];
        ];
        If[mod==aQuadraticDivisor,
            If[ToString[alg]==="Symbolic",
            If[Length[{modOps}]!=4,
                Message[aChristoffelAlgorihtm::bdqdiv];Abort[];];
            {x,y,hr,hi}={modOps};hr=-hr;hi=-hi;
            pom0=Together[hr*hr+hi*hi,ops];
            pom1=Together[-bETA[[1]]*hi/pom0-y,ops];
            pom2=Together[aLFA[[1]]-x+bETA[[1]]*hr/pom0,ops];
            AppendTo[alfa,Together[x+hr*y/hi,ops]];
                AppendTo[beta,Cancel[-hi/y,ops]];
            AppendTo[alfa,Together[x-bETA[[1]]*hi*pom2/(pom0*pom1)+hr*pom1/hi,ops]];
                AppendTo[beta,Cancel[y*pom1*(Together[1+(hr/hi)^2,ops]),ops]];
                If[Order==2,Return[{alfa, beta}];];
                pom3=Cancel[bETA[[2]]/(Together[pom1*pom1+pom2*pom2,ops]),ops];
            pom4=Together[aLFA[[2]]-x-pom3*pom2,ops];pom5=Together[pom3*pom1-y,ops];
            AppendTo[alfa,Together[x+pom2*pom5/pom1+pom3*pom1*pom4/pom5,ops]];
                AppendTo[beta,Cancel[-bETA[[1]]*hi*pom5
                (Together[1+(pom2/pom1)^2,ops])/pom0,ops]];
                If[Order==3,Return[{alfa, beta}];];
                For[i=3,i<Order,i++,
                    pom0=Cancel[bETA[[i]]/(Together[pom4*pom4+pom5*pom5,ops]),ops];
                pom6=Together[aLFA[[i]]-x-pom0*pom4,ops];
                    pom7=Together[pom0*pom5-y,ops];
                AppendTo[alfa,
                Factor[Together[x+pom4*pom7/pom5+pom0*pom5*pom6/pom7,ops],opsF]];
                    AppendTo[beta,
            Factor[Cancel[pom3*pom1*pom7(Together[1+(pom4/pom5)^2,ops]),ops],opsF]];
                    pom1=pom5;pom2=pom4;pom4=pom6;pom5=pom7;pom3=pom0;
                ];
            Return[{alfa,beta}];
        ,
        If[Length[{modOps}]==4,
        Block[{$MinPrecision=wrprec},
            {x,y,hr,hi}={modOps};hr=-hr;hi=-hi;
            pom0=hr*hr+hi*hi;
            pom1=-bETA[[1]]*hi/pom0-y;pom2=aLFA[[1]]-x+bETA[[1]]*hr/pom0;
            AppendTo[alfa,x+hr*y/hi];
                AppendTo[beta,-hi/y];AppendTo[alfa,x-bETA[[1]]*hi*pom2/(pom0*pom1)+hr*pom1/hi];
                AppendTo[beta,y*pom1*(1+(hr/hi)^2)];
                If[Order==2,Return[{alfa, beta}];];
                pom3=bETA[[2]]/(pom1*pom1+pom2*pom2);
            pom4=aLFA[[2]]-x-pom3*pom2;pom5=pom3*pom1-y;
            AppendTo[alfa,x+pom2*pom5/pom1+pom3*pom1*pom4/pom5];
                AppendTo[beta,-bETA[[1]]*hi*pom5(1+(pom2/pom1)^2)/pom0];
                If[Order==3,Return[{alfa, beta}];];
                For[i=3,i<Order,i++,
                    pom0=bETA[[i]]/(pom4*pom4+pom5*pom5);pom6=aLFA[[i]]-x-pom0*pom4;
                    pom7=pom0*pom5-y;AppendTo[alfa,x+pom4*pom7/pom5+pom0*pom5*pom6/pom7];
                    AppendTo[beta,pom3*pom1*pom7(1+(pom4/pom5)^2)];
                    pom1=pom5;pom2=pom4;pom4=pom6;pom5=pom7;pom3=pom0;
                ];];
            Return[{alfa,beta}];
        ];
        If[Length[{modOps}]==2,
            {x,y}={modOps};y=Abs[y];
            {stp}={OrthogonalPolynomials`Start}/.{Ops}/.Options[aChristoffelAlgorithm];
            If[ToString[stp]==="Automatic",
                stp=aStartingPoint[2*Order-1,x+I y,type,Ops,
                    Sequence@@Options[aChristoffelAlgorithm]];];
            r=aFunctionIIKind[2*Order-1,stp,x+I y,aLFA,bETA,Ops,
                Sequence@@Options[aChristoffelAlgorithm]];
            Return[aCAM[-Im[r]/y,aLFA,bETA,Ops]];
        ];
        Message[aChristoffelAlgorithm::bdqdiv];Abort[];
        ];
        ];
        If[mod==aQuadraticDivisorSymmetric,
        If[Length[{modOps}]!=2,
            Message[aChristoffelAlgorithm::bdqsdiv];Abort[];];
        {y,hi}={modOps};hi=-hi;
            alfa=Table[0,{i,1,Order}];
        If[ToString[alg]==="Symbolic",
            pom0=Together[-bETA[[1]]/hi-y,ops];pom1=Together[bETA[[2]]/pom0-y,ops]; 
                AppendTo[beta,Cancel[-hi/y,ops]];AppendTo[beta,Cancel[y*pom0,ops]];
            If[Order==2,Return[{alfa, beta}];];
                AppendTo[beta,Cancel[-bETA[[1]]pom1/hi,ops]];
                If[Order==3,Return[{alfa,beta}];];
                For[i=3,i<Order,i++,
                    pom2=Together[bETA[[i]]/pom1-y,op];
                AppendTo[beta,Factor[Cancel[bETA[[i-1]]*pom2/pom0,ops],opsF]]; 
                    pom0=pom1;pom1=pom2;
                ];
                Return[{alfa, beta}];
        ,
        Block[{$MinPrecision=wrprec},
            pom0=-bETA[[1]]/hi-y;pom1=bETA[[2]]/pom0-y; 
                AppendTo[beta,-hi/y];AppendTo[beta,y*pom0];
            If[Order==2,Return[{alfa, beta}];];
                AppendTo[beta,-bETA[[1]]pom1/hi];
                If[Order==3,Return[{alfa,beta}];];
                For[i=3,i<Order,i++,
                    pom2=bETA[[i]]/pom1-y;AppendTo[beta,bETA[[i-1]]*pom2/pom0]; 
                    pom0=pom1;pom1=pom2;
                ];];
                Return[{alfa, beta}];
        ];
        ];
        If[mod==aQuadraticReal,x=modOps;
        If[Length[{x}]!=1,
            Message[aChristoffelAlgorithm::bdqrl];Abort[];];
        If[ToString[alg]==="Symbolic",
                pom0=0;pom1=1;pom2=0;pom6=0;
                For[i=1,i<=Order,i++,
                    pom3=Together[aLFA[[i]]-x-pom0,ops];pom4=pom2;pom2=pom1;
                If[pom2!=0,pom5=Cancel[pom3*pom3/pom2];,
                pom5=Cancel[pom4*bETA[[i]],ops];];
                AppendTo[beta,Factor[Cancel[pom6*
                    (Together[pom5+bETA[[i+1]],ops]),ops],opsF]];
                    pom6=Cancel[bETA[[i+1]]/(Together[pom5+bETA[[i+1]],ops]),ops];
                pom1=Cancel[pom5/(Together[pom5+bETA[[i+1]],ops]),ops]; 
                    pom0=Cancel[pom6*(Together[pom3+aLFA[[i+1]]-x,ops]),ops]; 
                    AppendTo[alfa,Factor[Together[pom3+pom0+x,ops],opsF]];
                ];
                beta[[1]]=Cancel[bETA[[1]]*(Together[bETA[[2]]+(x-aLFA[[1]])^2,ops]),ops];
                Return[{alfa,beta}];
        ,
        Block[{$MinPrecision=wrprec},eps=10^(-wrprec);
            pom0=0;pom1=1;pom2=0;pom6=0;
                For[i=1,i<=Order,i++,
                    pom3=aLFA[[i]]-x-pom0;pom4=pom2;pom2=pom1;
                If[Abs[pom2]>eps,pom5=pom3*pom3/pom2;,
                pom5=pom4*bETA[[i]];];AppendTo[beta,pom6*(pom5+bETA[[i+1]])];
                    pom6=bETA[[i+1]]/(pom5+bETA[[i+1]]);pom1=pom5/(pom5+bETA[[i+1]]); 
                    pom0=pom6*(pom3+aLFA[[i+1]]-x); 
                    AppendTo[alfa,pom3+pom0+x];
                ];
                beta[[1]]=bETA[[1]]*(bETA[[2]]+(x-aLFA[[1]])^2);];
                Return[{alfa,beta}];
        ];
        ];
    Message[aChristoffelAlgorithm::bdmod];Abort[];
    ];

aStartingPoint[Order_?IntegerQ/;Order>0,z_?NumberQ,type_,Ops___]:=
     Module[{prec,x,y,pom1,pom2,pom3,pom4,alfa=0}, 
    {prec}={Precision}/.{Ops}/.Options[aStartingPoint];
        x=Re[z];y=Im[z];
        If[MemberQ[{"aJacobi","aGegenbauer","aLegendre",
        "aChebyshevI","aChebyshevII"},ToString[type[[1]]]], 
            If[x<-1,pom1=(2. Pi+ArcTan[y/(x-1.)]+ArcTan[y/(x+1.)])/2.;, 
            If[x==-1,pom1=(3. Pi/2-ArcTan[y/2.])/2.;, 
                If[x==1,pom1=(Pi/2.+ArcTan[y/2.])/2.;, 
                If[x>1,pom1=(ArcTan[y/(x-1.)]+ArcTan[y/(x+1.)])/2.;, 
                pom1=(Pi+ArcTan[y/(x-1.)]+ArcTan[y/(x+1.)])/2.;];];];];
            pom2=x*x;pom3=y*y;
            pom4=((pom2-pom3-1)^2+4. pom2*pom3)^(1/4);
            pom4=Sqrt[(x+pom4*Cos[pom1])^2+(y+pom4*Sin[pom1])^2];
        If[pom1=Floor[Order+1.+.5 prec/Log[pom4]]+1;pom1 > 3 Order,
            Return[3 Order];,
                Return[pom1];];];
        If[ToString[type[[1]]]==="aHermite", 
        If[pom1=Floor[2*(Sqrt[Order/2+.5]+.25*prec/Abs[y])^2]+1;pom1 > 3 Order,
            Return[3 Order];,
                Return[pom1]];;];
    If[MemberQ[{"aLaguerre","aLaguerreG"},ToString[type[[1]]]],
        If[Length[type]==2,alfa=type[[2]];,alfa=0];
        pom1=Pi/2.;
            If[y<0,pom1=1.5 Pi;];
            If[x != 0,pom1=ArcTan[y/x];
            If[y<=0 || x<=0,pom1=pom1+Pi;
                If[x>=0,pom1=pom1+Pi;];];];
        If[pom1=Floor[(Sqrt[Order+1.+.5*alfa+.5]+prec/(4*(x*x+y*y)^(.25)*
            Cos[.5(pom1-Pi)]))^2-.5(alfa+1)]+1;pom1 > 3 Order,
            Return[3 Order];,
                Return[pom1];];];
        Return[Ceiling[3 Order/2]];
     ];

aFunctionIIKind[Order_?IntegerQ/;Order>0,Stp_?IntegerQ/;Stp>0,z_?NumberQ,alfa_?VectorQ,
beta_?VectorQ,Ops___]:= 
    Module[{iD,prec,r,rold,countID=0,pom1,stp=Stp,i,wrprec,eps,cond},
    {prec,iD,wrprec}={Precision,OrthogonalPolynomials`IterationDepth,WorkingPrecision} /.{Ops}
        /.Options[aFunctionIIKind];
        If[Length[alfa]<(stp+5 iD) || Length[beta]<(stp+5 iD),
            Message[aFunctionIIKind::bdinp];Abort[];];
        If[stp<Order,Message[aFunctionIIKind::stpts];stp=Order;];
    Block[{$MinPrecision=wrprec},
        eps=10^-prec;r=Table[0,{i,1,Order+1}];
Label[20];
        If[countID==iD,Message[aFunctionIIKind::idexc];Abort[]];
        rold=r;pom1=0;
        For[i=stp+5 countID,i>Order+1,i--,pom1=beta[[i]]/(z-alfa[[i]]-pom1);];
    r[[Order+1]]=pom1;
        For[i=Order,i>=1,i--,r[[i]]=beta[[i]]/(z-alfa[[i]]-r[[i+1]]);];
    countID++;
        For[i=1,i<=Order+1,i++,If[Abs[r[[i]]-rold[[i]]]>eps*Abs[rold[[i]]],Goto[20];];];
    For[i=2,i<=Order+1,i++,
        r[[i]]=r[[i]]r[[i-1]];];];
        Return[r];
    ];

aGaussianKernel[Order_?IntegerQ/;Order>0,stp_?IntegerQ/;stp>0,z_?NumberQ,alfa_?VectorQ,
beta_?VectorQ,Ops___]:= 
    Module[{pom0,pom1,pom2,r,i,wrprec,alfa,beta},
    {wrprec}={WorkingPrecision}/.{Ops}/.Options[aGaussianKernel];
    r=aFunctionIIKind[Order,stp,z,Alfa,Beta,Ops,Sequence@@Options[aGaussianKernel]];
        pom0=0;pom1=1;
    Block[{$MinPrecision=wrprec},
    For[i=1,i<=Order,i++,pom2=pom0;pom0=pom1;
            pom1=(z-Alfa[[i]])pom0-Beta[[i]]pom2;r[[i+1]]=r[[i+1]]/pom1;
    ];];
        Return[r];
    ];

aModify[Order_?IntegerQ/;Order>1,{mod_,modOps__},alfa_?VectorQ,beta_?VectorQ,Ops___Rule]:=
    Module[{},
    Return[aChristoffelAlgorithm[Order,{mod,modOps},alfa,beta,Ops,
        Sequence@@Options[aModify]]];
    ];

aModify[Order_?IntegerQ/;Order>1,{mod_,modOps__},{Name_?polyQ,polyOps___},Ops___Rule]:=
    Module[{alfa,beta,k,stp,r,x,y},
    If[mod==aLinearDivisor && Length[{modOps}]==1,
        {iD,wrprec,stp}={OrthogonalPolynomials`IterationDepth,
            WorkingPrecision,OrthogonalPolynomials`Start}/.{Ops}/.Options[aModify];
        type={Name,polyOps};
        If[ToString[stp]==="Automatic",
            stp=aStartingPoint[2*Order-1,modOps,type,Ops,
                Sequence@@Options[aModify]];];
        alfa=Transpose[Table[SetPrecision[Name["ttr",polyOps][k],wrprec],{k,0,stp+5 iD}]];
        beta=alfa[[2]];alfa=alfa[[1]];
        r=aFunctionIIKind[2*Order-1,stp,modOps,alfa,beta,Ops,
            Sequence@@Options[aModify]];
        Return[aCAM[-r,alfa,beta,Ops,
            Sequence@@Options[aModify]]];];
    If[mod==aQuadraticDivisor && Length[{modOps}]==2,
        {iD,wrprec,stp}={OrthogonalPolynomials`IterationDepth,
            WorkingPrecision,OrthogonalPolynomials`Start}/.{Ops}/.Options[aModify];
        {x,y}={modOps};y=Abs[y];
        type={Name,polyOps};
        If[ToString[stp]==="Automatic",
            stp=aStartingPoint[2*Order-1,x+I y,type,Ops,
                Sequence@@Options[aModify]];];
        alfa=Transpose[Table[SetPrecision[Name["ttr",polyOps][k],wrprec],{k,0,stp+5 iD}]];
        beta=alfa[[2]];alfa=alfa[[1]];
        r=aFunctionIIKind[2*Order-1,stp,x+I y,alfa,beta,Ops,
            Sequence@@Options[aModify]];
        Return[aCAM[-Im[r]/y,alfa,beta,Ops,
            Sequence@@Options[aModify]]];];
    alfa=Transpose[Table[Name["ttr",polyOps][k],{k,0,Order}]];
    beta=alfa[[2]];alfa=alfa[[1]];
    Return[aChristoffelAlgorithm[Order,{mod,modOps},alfa,beta,Ops,
        Sequence@@Options[aModify]]];
    ];

Protect[aChristoffelAlgorithm];
Protect[aLinear];
Protect[aQuadratic];
Protect[aQuadraticSymmetric];
Protect[aLinearDivisor];
Protect[aQuadraticDivisor];
Protect[aQuadraticDivisorSymmetric];
Protect[aQudraticReal];
Protect[Type];
Protect[aModify];
Protect[aStartingPoint];
Protect[aFunctionIIKind];
Protect[aGaussianKernel];

End[]

EndPackage[]
