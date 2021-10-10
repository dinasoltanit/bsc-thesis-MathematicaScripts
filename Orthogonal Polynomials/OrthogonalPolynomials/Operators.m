BeginPackage["OrthogonalPolynomials`Operators`",{"OrthogonalPolynomials`Kernel`Common`"}]

aSetInterval::usage="aSetInterval[name,left,right] sets interval of orthogonality for class \
    specified by name.";
aGetInterval::usage="aGetInterval[name,polyOps] returns interval of orthogonality \
    for class of orthogonal polynomials specified by name and polyOps are parameters \
    of the class if there is any.";
aNorm::usage="aNorm[name,polyOps] returns pure function which represents the norm for the class \
    of orthogonal polynomials specified by name and polyOps are parameters of the class \
    if there is any.";
aWeight::usage="aWeight[name,polyOps] returns pure function which represents the weight function \
    for the class of orthogonal polynomials specified by name and polyOps are parameters of the \
    class if there is any.";
aMoments::usage="aMoments[name,polyOps] returns pure function which represents moments for the \
    class of orthogonal polynomials specified by name and polyOps are parameters if there is \
    any.";
aSupport::usage="aSupport[name,polyOps] returns supporting set for the class of orthogonal \
    polynomials specified by name and polyOps are parameters of the class if there is any.";
aDistribution::usage="aDistribution[name,polyOps] returns pure function which is ditribution \
    function for class of orthogonal polynomials specified by name and polyOps are parameters \
    of the class if there is any.";
aKernel::usage="aKernel[order,{name,polyOps},k,ops] returns pure function represents kernel polynomial \
    of degree order for class of polynomials specified by name and polyOps which are \
    parameters of the class if there is any, with parameter k, ops are options which can be used \
    with this function. WorkingPrecision (def. WorkingPrecision->$MachinePrecision) number \
    of decimal digits used in calculation. Numerator (def. Numerator->0) if set to some \
    positive integer gives kernel polynomial for numerator polynomials of order to which \
    option is set of the class specified by name.";
aNumerator::usage="aNumerator[order,{name,polyOps},num,ops] returns pure function represents \
    numerator polynomial of class specified by name and polyOps of degree order and of order \
    num, ops are options. WorkingPrecision (def. WorkingPrecision->$MachinePrecision) \
    number of decimal digits used in calculation.";
aChangeInterval::usage="aChangeInterval[nodes,weights,{{leftOld, rightOld}, \
    {leftNew,rightNew}},ops] returns shifted nodes and weights from interval (leftOld, rightOld)\
    to interval (leftNew,rightNew).ops are options. WorkingPrecision (def. WorkingPrecision->$MachinePrecision) \
    reprents number of decimal digits in mantissa to be used in calculation.";
aMakePolynomial::bdinp="Lists of alpha and beta coefficients must have length grater than or \
    equal to degree of polynomial which zeroes are to be returned.";
aMakePolynomial::usage="aMakePolynomial[n,alpha,beta,x,ops] first argument is degree of polynomial \
    to be returned, alpha and beta are three term recurrence coefficients of respected \
    orthogonal polynomials, x is variable of polynomial, ops are options. WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision) represents number of decimal digits in matissa \
    to be used in computations. ReturnList (def. ReturnList->False) whether to return list \
    of values of all orthogonal polynomials of degree 0 to n at point x. Numerator (def. \
    Numerator->0) order of numerator polynomial to be returned.";

(*  Options         *)
Options[aKernel]={WorkingPrecision->$MachinePrecision,Numerator->0};
Options[aNumerator]={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False};
Options[aMakePolynomial]={WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`ReturnList->False, 
      Numerator->0};
Options[aChangeInterval]={WorkingPrecision->$MachinePrecision};
(*  Options end     *)

(*  Attributes      *)
Atributes[aSetInterval]={ReadProtected};
Atributes[aGetInterval]={ReadProtected};
Atributes[aMoments]={ReadProtected};
Atributes[aWeight]={ReadProtected};
Atributes[aNorm]={ReadProtected};
Atributes[aDistribution]={ReadProtected};
Atributes[aSupport]={ReadProtected};
Atributes[aKernel]={ReadProtected};
Atributes[aNumerator]={ReadProtected};
Atributes[aMakePolynomial]={ReadProtected};

Begin["`Private`"]

aSetInterval[Name_?polyQ,left_,right_]:=Module[{},
    Return[Name["sint",left,right]];];

aGetInterval[Name_?polyQ,polyOps___]:=Module[{},
    Return[Name["gint",polyOps]];];

aMoments[Name_?polyQ,Ops___]:=Module[{},
    Return[Name["mom",Ops]];];

aWeight[Name_?polyQ,Ops___]:=Module[{},
    Return[Name["wth",Ops]];];

aNorm[Name_?polyQ,Ops___]:=Module[{},
    Return[Name["nor",Ops]];];

aDistribution[Name_?polyQ,Ops___]:=Module[{},
    Return[Name["dist",Ops]];];

aSupport[Name_?polyQ,Ops___]:=Module[{pom},
    Return[Name["supp",Ops]];];

aKernel[Order_?IntegerQ/;Order>0,{Name_?polyQ,polyOps___},k_,Ops___]:=Module[{},
    Return[Function[x,seq=Sequence@@Options[aKernel];
        a=Name[Order,polyOps,x,OrthogonalPolynomials`ReturnList->True,Ops,seq];
        b=Name[Order,polyOps,k,OrthogonalPolynomials`ReturnList->True,Ops,seq];
        beta=Table[(Name["ttr",polyOps][i])[[2]],{i,0,Order}];
        beta=Table[Times@@Take[beta,i],{i,1,Order+1}];
        ((a/beta).b)beta[[-1]]/b[[-1]] ]];];

aNumerator[Order_?IntegerQ/;Order>0,{Name_?polyQ,polyOps___},num_?IntegerQ/;num>-1,Ops___]:=
    Module[{},
    Return[Function[x,Name[Order,polyOps,x,Numerator->num
        ,Ops,Sequence@@Options[aNumerator]]]];];

aChangeInterval[nodes_?VectorQ,weights_?VectorQ,{{leftOld_,rightOld_},{leftNew_,rightNew_}},
    Ops___]:=Module[{i,t,t2},
    {wrprec}={WorkingPrecision}/.{Ops}/.Options[aChangeInterval];
    Block[{$MinPrecision=wrprec},
    If[leftNew==-Infinity && rightNew==Infinity,
        Return[{nodes/(1-nodes*nodes),weights*(nodes*nodes+1)/(1-nodes*nodes)^2}];];
    If[leftNew==-Infinity,
        Return[{rightNew-(rightOld+nodes)/(rightOld-nodes),
            (rightOld-leftOld)*weights/(nodes-rightOld)^2}];];
    If[rightNew==Infinity,
        Return[{leftNew+(leftOld+nodes)/(leftOld-nodes),
            (rightOld-leftOld)*weights/(nodes-leftOld)^2}];];
    t=rightNew-leftNew;t2=rightNew+leftNew;];
    Return[{(t*nodes+t2)/(rightOld-leftOld),t*weights/(rightOld-leftOld)}];];

aMakePolynomial[n_,al_,be_,x_,Ops___]:=Module[{wrprec,list,pom0,pom1,pom2,num,i,ret},
        pom0=0;pom1=1;If[n==0,Return[pom1]];
        {wrprec,list,num}={WorkingPrecision,OrthogonalPolynomials`ReturnList,
                Numerator}/.{Ops}/.Options[aMakePolynomial];
        If[Length[al]<n+num || Length[be]<n+num,Message[aMakePolynomial::bdinp];Abort[];];
        Block[{$MinPrecision=wrprec},
    If[list,ret={1};];
        For[i=1+num,i<=n+num,i++,
            pom2=Expand[(x-al[[i]])pom1-be[[i]]pom0];
            pom0=pom1;pom1=pom2;If[list,AppendTo[ret, pom2];];];
        If[list, Return[ret];, Return[pom2];];];];

End[]

EndPackage[]
