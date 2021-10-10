BeginPackage["OrthogonalPolynomials`NodesWeights`",{"OrthogonalPolynomials`Kernel`Common`"}]


Unprotect[aQR];
Unprotect[aPasquini];
Unprotect[aZero];
Unprotect[aGaussianNodesWeights];
Unprotect[aGaussianWeights];
Unprotect[aLobattoNodesWeights];
Unprotect[aRadauNodesWeights];
Unprotect[aLaurie];
Unprotect[aDevideConquer];
Unprotect[aKronrodNodesWeights];
Unprotect[aTuranNodes];
Unprotect[aTuranNodesWeights];
Unprotect[aSigmaNodes];
Unprotect[aSigmaWeights];
Unprotect[aSigmaNodesWeights];
Unprotect[aAntiGaussianNodesWeights];
Unprotect[aNodesWeights];
Unprotect[aFejerNodesWeights];
Unprotect[aInterpolatoryWeights];
Unprotect[aCheckSigma];
Unprotect[aCheckTuran];

aZero::usage="aZero[order,{name,polyOps},ops] returns zeroes of orthogonal polynomial \
    belonging to class specified by name of degree order, ops are options. WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision) represents number of decimal digits used in calculations. \
    Precision (def. Precision->$MachinePrecision-10) represents number of exact decimal digits in result. \
    IterationDepth (def. IterationDepth->30) represents number of iterations in iterative process, which when \
    achieved aborts calculation. Algorithm (def. Algorithm->aQR) defines algorithm which is \
    used in calculations, aQR can be applied to every polynomial class, but for classes in \
    super class complex aPasquini should be used if degree of polynomial is bigger then 100.";
aZero::prec="Can not achieve Precision bigger then WorkingPrecision.";
aQR::bdinp="Lists of alpha and beta coefficients must have length grater then or equal to \
    degree of polynomial which zeroes are to be calculated.";
aQR::err="IterationDepth excided.";
aQR::prec="Can not achieve Precision bigger then WorkingPrecision.";
aQR::usage="aQR[order,alpha,beta,ops] returns zeroes of polynomial of degree order for class of \
    orthogonal polynomials which satisfy three term recurrence relation with coefficients \
    alpha and beta using QR algorithm, ops are options. \
    WorkingPrecision (def. WorkingPrecision->$MachinePrecision) \
    sets number of decimal digits in mantissa with which calculation runs. Precision \
    (def. Precision->$MachinePrecision-10) represents number of exact decimal digits in result to be \
    returned. IterationDepth (def. IterationDepth->30) represents number of iterations in iteration \
    process in calculation, if this value is excided function aborts.";
aPasquini::err="IterationDepth excided.";
aPasquini::usage="aPasquini[order,stp,fun,ops] returns zeroes of polynomial of degre order \
    with starting values stp and with coefficients in second order differential equation \
    with second and first derivative which quotient is in fun, ops are options. \
    WorkingPrecision (def. WorkingPrecision->$MachinePrecision) sets the number of decimal \
    in mantissa to be used in calculation. Precision (def. Precision->$MachinePrecision-10) \
    number of exact decimal digits to be returned in result. IterationDepth \
    (def. IterationDepth->30) number of iterations in the iterative process of calculation, \
    if this number is excided function aborts.";

aRadauNodesWeights::bdinp="Secound and third arguments have to be of lengths at least first \
    argument and first argument plus one, respectively.";
aRadauNodesWeights::usage="aRadauNodesWeights[Order,alpha,beta,end,Ops] returns Gauss-Radau nodes \
    and weights for class of polynomials with three term recurrence coefficients alpha and beta \
    of degree Order and with fixed point end, ops are options which are the same as for \
    GaussianNodesWeights.";

aLobattoNodesWeights::bdinp="Secound and third arguments have to be lists of length at least \
    first argument plus one, respectively.";
aLobattoNodesWeights::usage="aLobattoNodesWeights[Order,alpha,beta,left,right,Ops] returns \
    Gauss-Lobatto nodes and weights for polynomials which satisfy three term recurrence \
    relation with coefficients alpha and beta of degree order and with fixed points \
    left and right, ops are options which are the same as for aGaussianNodesWeights.";

aGaussianNodesWeights::usage = "aGaussianNodesWeights[Order,alpha,beta,Ops] returns Gaussian nodes \
    and weights of degree Order, for measure specified by alpha, beta three term recurrence \
    coefficients. Forth parameter ops represents options. Option IterationDepth (def. IterationDepth->30) \
    represents maximal number of iterations to be used to achieve given precision in result. \
    Option Precision (def. Precision->$MachinePrecision-10) represents precision in returned result. \
    Option WorkingPrecision (def. WorkingPrecision->$MachinePrecision) represents number of \
    decimal digits in mantissa used in calculations. Option Accuracy (def. Accuracy->$MachinePrecision) \
    represents accuracy of returned result. Option ModifiedAlgorithm (def. ModifiedAlgorithm->True) \
    determines whether modified algorithm for construction of weights is used or weights are \
    constructed using QR algorithm. Option Algorithm (def. Algorithm->aQR) determines whether \
    Pasquini's or QR algorithm going to be used for construction of nodes.";
aGaussianNodesWeights::err="IterationDepth excided.";
aGaussianNodesWeights::wrprec="WorkingPrecision may be to small. If function do not \
    succeed try with WorkingPrecision=Precision+10.";
aGaussianNodesWeights::prec="Can not achieve Precision bigger than WorkingPrecision. Exiting.";
aGaussianNodesWeights::bdinp="Can not construct Gaussian rule of n points with less then
    n three term recurrence coefficients.";
aGaussianNodesWeights::werr="Weights are constructed probably with error and `1` decimal \
    digits are exact. If you want to achieve requested precision try with higher WorkingPrecision.";

aGaussianWeights::werr="Weights are constructed probably with error and `1` decimal \
    digits are exact. If you want to achieve requested precision you should try with nodes \
    given with bigger precision and increased WorkingPrecision.";
aGaussianWeights::usage="aGaussianWeights[order,alpha,beta,nodes,ops] returns weights in \
    Gaussian quadrature rule for orthogonal polynomials which satisfy three term recurrence \
    relation with coefficients alpha and beta and with nodes nodes, ops are options. \
    There is only one option WorkingPrecision, which sets number of decimal digits used in \
    computations.";
aGaussianWeights::neqlen="First argument of the function has to be equal with length of the forth \
    argument of the function.";

aLaurie::usage="aLaurie[Order,alpha,beta,Ops] first argument is number of Gaussian nodes in \
    Kronrod quadrature rule to be constructed, second argument are alpha coefficients, third \
    argument are beta coefficients of three term recurrence relation, length of the lists \
    have to be Floor[3 Order/2]+1 and Ceiling[3 Order/2]+1, respectively. Forth argument are \
    options. WorkingPrecision (def. WorkingPrecision->$MachinePecision) represents number \
    of decimal digits in mantissa to be used in calculations. Symbolic (def. Symbolic->False) \
    defines whether to calculate symbolic three term recurrence coefficients or numeric. \
    Function returns list consisting of two lists, first \
    holding alpha coefficients, and second holding beta coefficients of three term \
    recurrence relation.";
aLaurie::bdinp="Secound argument has to be vector of Length Floor[3 Order/2]+1 and third \
    argument has to be vector of Lenth Ceiling[3 Order/2]+1.";

aDevideConquer::usage="aDevideConquer[Order,alpha,beta,nG,wFG,wLG,ops] first argument is number \
    of Gaussain nodes to be contained in Kronrod qudrature rule, second is list of alpha \
    coefficients, third is list of beta coefficients, second and third arguments have to be \
    of length Floor[3 Order/2]+1 and Ceilling[3 Order/2]+1, respectivelly, forth argument \
    are Gaussian nodes of length Order, fifth argument is list of first components of \
    eigenvectors and sixth argument is list of last components of eigenvectors of symmetric \
    three diagonal matrix with alfa coefficinets on the diagonal and beta coefficients on the \
    side diagonal, seventh argument are options. WorkingPrecision (def. WorkingPrecision-> \
    $MachinePrecision) respresents number of decimal digits in matissa to be used in calculation. \
    IterationDepth (def. IterationDepth->30) number of iterations to be used in calculation. \
    Precision (def. Precision->$MachinePrecision-10) number of exact decimal digits to be \
    returned in result. Accuracy (def. Accuracy->$MachinePrecision-9) defines exponential \
    neighborhood of number zero, all nodes which are in that neighborhood are returned \
    without monitoring precision.";
aDevideConquer::iDexc="Iteration depth excided.";
aDevideConquer::bdinp="Secound argument has to be vector of Length Floor[3 Order/2]+1 \
    and third argument has to be vector of lenth Ceiling[3 Order/2]+1.";
aDevideConquer::cmpl="You are trying to evaluate Gauss-Kronrod rule for which respecting \
    Kronrod-Jacobi matrix is not positive definite so you can not use Devide&Conquer \
    algorithm. You should try with Laurie`s algorithm.";

aKronrodNodesWeights::prec="Can not achive precision bigger than working precision.";
aKronrodNodesWeights::bdinp="Secound argument has to vector of length Floor[3 Order/2]+1 \
    and third argument has to be vector of lenth Ceiling[3 Order/2]+1.";
aKronrodNodesWeights::usage="aKronrodNodesWeights[Order,alpha,beta,Ops] first argument is \
    order of Gauss-Kronrod rule to be computed and used to construct Gauss-Kronrod \
    quadrature rule of order 2 order+1. Secound and third arguments are lists of three \
    term recurence relation coefficients for which Gauss-Kronrod rule will be computed. \
    Forth argument is \
    optional. Function has three options WorkingPrecision (def. WorkingPrecision-> \
    $MachinePrecision) indicating noumber of digits used in computation, Precision \
    (def. Precision->$MachinePrecision-10) indicating precision to which nodes and weights \
    are computed in the sense of precision offen used with Givens rotation. \
    IterationDepth (def. IterationDepth->30) used to control divergent behavior of \
    QR algorithm. If function successes it returns list of two lists first one with nodes \ 
    and secound with weights of Gauss-Kronrod quadrature rule.";

aTuranNodes::usage="aTuranNodes[Order,{name,polyOps},s,ops] returns nodes of Gauss-Turan type quadrature \
    rule, for the class specified by name with parameters polyOps with degree s. \
    Last parameter ops represents possible options. Option WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision) defines number of decimal digits in mantissa used \
    in calculation. Option Precision (def. Precision->$MachinePrecision-10) represents \
    number of exact decimal digits in mantissa to be returned in result. Option Accuracy \
    (def. Accuracy->$MachinePrecision-9) represents number of decimal positions in the result, \
    which if are zero, result is treated as zero. Option ReturnList (def. ReturnList->False) \
    if set function returns nodes for all Gauss-Turan quadrature rule with k<Order, if it is \
    not set than function returns just nodes for Gauss-Turan quadrature rule of length Order. \
    Option IterationDepth (def. IterationDepth->100) defines maximal number of iterations \
    which are going to be used to estimate Gauss-Turan nodes. Option InternalPrecision \
    (def. InternalPrecision->6) defines precision of results generated during calculation, \
    it is important to note that this is precision in which Gauss-Turan nodes with k<Order will be \
    returned. Option InternalAccuracy (def. InternalAccuracy->7) similar to InternalPrecision \
    represents accuracy with which internal results are calculated. Option AlgorithmSigma \
    (def. AlgorithmSigma->Homotopy) determines algorithm which is going to be used in construction, \
    if this value is IncreaseDegree construction is performed by increasing degree over Gauss-Turan \
    quadrature rule, if this value is IncreaseS construction is performed over sigma-orthogonal polynomials. \
    Set this value to IncreaseDegree to have faster construction.";
aTuranNodes::iDexc="IterationDepth excided.";
aTuranNodes::ualg="Unknow value for the option AlgorithmSigma. Possible values for the option \
    AlgorithmSigma are IncreaseDegree, IncreaseS and Homotopy";
aTuranNodes::usalg="Can not use value IncreaseDegree of option Algorithm with this calling format.;"
aTuranNodes::incDgr="Can not construct Gauss-Turan quadrature rule using Increasing Degree method.";
aTuranNodesWeights::usage="aTuranNodesWeights[Order,alpha,beta,s,ops] returns nodes and weights of \
    Gauss-Turan type quadrature rule, for class specified by name with parameters polyOps with degree s. \
    Last parameter ops represents possible options. Option WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision) defines number of decimal digits in mantissa used \
    in calculation. Option Precision (def. Precision->$MachinePrecision-10) represents \
    number of exact decimal digits in mantissa to be returned in result. Option Accuracy \
    (def. Accuracy->$MachinePrecision-9) represents number of decimal positions in the result \
    which if are zero result is treated as zero. Option ReturnList (def. ReturnList->False) \
    if set function returns nodes for all Gauss-Turan quadrature rule with k<Order, if it is \
    not set than function returns just nodes for Gauss-Turan quadrature rule of length Order. \
    Option IterationDepth (def. IterationDepth->100) defines maximal number of iterations \
    which are going to be used to estimate Gauss-Turan nodes. Option InternalPrecision \
    (def. InternalPrecision->6) defines precision of results generated during calculation, \
    it is important to note that this is precision in which Gauss-Turan nodes with k<Order will be \
    returned. Option InternalAccuracy (def. InternalAccuracy->7) similar to InternalPrecision \
    represents accuracy with which internal results are calculated. Option AlgorithmSigma \
    (def. AlgorithmSigma->Homotopy) determines algorithm which is going to be used in construction, \
    if this value is IncreaseDegree construction is performed by increasing degree over Gauss-Turan \
    quadrature rule, if this value is IncreaseS construction is performed over sigma-orthogonal polynomials. \
    Set this value to IncreaseDegree to have faster construction.";
aTuranNodesWeights::err="IterationDepth excided.";
aTuranNodesWeights::prec="Can not achieve precision bigger than working precision.";
aTuranNodesWeights::intPrec="Can not achieve intermediate results with bigger precision then \
    working precision.";

aAntiGaussianNodesWeights::usage="aAntiGaussianNodesWeights[n,{name,polyOps},ops] returns nodes and weights in \
	anti-Gaussian quadrature rule. Quadrature rule is constructed with 2n+1 nodes for the\
	measure characterized by name and polyOps. Parameter ops specifies WorkingPrecison \
	and Precision of the underlying QR algorithm used for the construction."

aSigmaWeights::usage="aSigmaWeights[Order,{name,polyOps},sigma,zero,ops] returns weights of \
    sigma quadrature rule whose nodes are zeros of degree Order, for class of polynomials specified with name \
    with parameters polyOps, for sigma quadrature rule specified with list sigma. There is only one \
    option specified with ops. Option WorkingPrecision (def. WorkingPrecision->$MachinePrecision) \
    represents number of decimal digits in mantissa to be used in calculation.";
aSigmaNodes::usage="aSigmaNodes[Order,{name,polyOps},sigma,ops] returns nodes of \
    sigma type quadrature rule, for the class specified by name with parameter polyOps \
    with degree s. Last parameter ops represents possible options. Option WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision) defines number of decimal digits in mantissa used \
    in calculation. Option Precision (def. Precision->$MachinePrecision-10) represents \
    number of exact decimal digits in mantissa to be returned in result. Option Accuracy \
    (def. Accuracy->$MachinePrecision-9) represents number of decimal positions in the result \
    which if are zero result is treated as zero. Option ReturnList (def. ReturnList->False) \
    if set function returns nodes for all sigma quadrature rules with degrees less than Order, if it is \
    not set than function returns just nodes for sigma quadrature rule of length Order. \
    Option IterationDepth (def. IterationDepth->100) defines maximal number of iterations \
    which are going to be used to estimate sigma nodes. Option InternalPrecision \
    (def. InternalPrecision->6) defines precision of results generated during calculation, \
    it is important to note that this is precision in which sigma nodes with k<Order will be \
    returned. Option InternalAccuracy (def. InternalAccuracy->7) similarly to InternalPrecision \
    represents accuracy with which internal results are calculated. Option AlgorithmSigma \
    (def. AlgorithmSigma->Homotopy) determines algorithm which is going to be used in construction, \
    if this value is IncreaseDegree construction is performed by increasing degree over Gauss-Turan \
    quadrature rule, if this value is IncreaseS construction is performed over sigma-orthogonal polynomials. \
    Set this value to IncreaseDegree to have faster construction.";
aSigmaNodes::ualg="Unknown value for the option AlgorithSigma. Possible values of option \
    AlgorithmSigma are Homotopy, IncreaseDegree and IncreaseS."
aSigmaNodes::usalg="Can not use value IncreaseDegree for option AlgorithmSigma with this calling format.";
aSigmaNodesWeights::usage="aSigmaNodesWeights[Order,{name,polyOps},sigma,ops] returns nodes and weights of \
    sigma type quadrature rule, for the class specified by name with parameter polyOps \
    with degree s. Last parameter ops represents possible options. Option WorkingPrecision \
    (def. WorkingPrecision->$MachinePrecision) defines number of decimal digits in mantissa used \
    in calculation. Option Precision (def. Precision->$MachinePrecision-10) represents \
    number of exact decimal digits in mantissa to be returned in result. Option Accuracy \
    (def. Accuracy->$MachinePrecision-9) represents number of decimal positions in the result \
    which if are zero result is treated as zero. Option ReturnList (def. ReturnList->False) \
    if set function returns nodes for all sigma quadrature rules with degrees less than Order, if it is \
    not set than function returns just nodes for sigma quadrature rule of length Order. \
    Option IterationDepth (def. IterationDepth->100) defines maximal number of iterations \
    which are going to be used to estimate sigma nodes. Option InternalPrecision \
    (def. InternalPrecision->6) defines precision of results generated during calculation, \
    it is important to note that this is precision in which sigma nodes with k<n will be \
    returned. Option InternalAccuracy (def. InternalAccuracy->7) similar to InternalPrecision \
    represents accuracy with which internal results are calculated. Option AlgorithmSigma \
    (def. AlgorithmSigma->Homotopy) determines algorithm which is going to be used in construction, \
    if this value is IncreaseDegree construction is performed by increasing degree over Gauss-Turan \
    quadrature rule, if this value is IncreaseS construction is performed over sigma-orthogonal polynomials. \
    Set this value to IncreaseDegree to have faster construction.";
aSigmaNodesWeights::iDexc="IterationDepth excided.";
aSigmaNodesWeights::neg="None of the elements of the list sigma, which is forth argument \
    in the call of aSigmaNodesWeights, must not be negative.";
aSigmaNodesWeights::prec="Can not achive precision bigger than working precision.";
aSigmaNodesWeights::bdinp="Lists of the three term recurrence coefficients which are \
    second and third argument in the call of aSigmaNodesWeights must have length \
    at least equal to Floor[(2(s+1)n-1)/2]+1, where s is maximum in the list which is forth \
    argument of the function call and n is the first argument.";
aSigmaNodesWeights::nelen="First argument has to equal to length of list which defines powers \
    in modified measure.";
aSigmaNodesWeights::intPrec="Can not achive intermediate results with bigger precision then \
    working precision.";

aNodesWeights::untype="Type of operator aNodesWeights must be one of the following \
    aGaussian, aKronrod, aRadau, aLobatto,aTuran,aSigma.";
aNodesWeights::usage="aNodesWeights[Order,{Name,polyOps},type,Ops] return nodes and weights \
    constructed for class name with parameters polyOps, of type type and of degree Order \
    with options Ops. There are many different types which can be used: aGaussian, aRadau, \
    aLobatto, aKronrod, aTuran and aSigma they specify construction of Gaussian, Radau, Lobatto, \
    Kronrod, Turan and Sigma quadrature rule, respectively. Options can be different with different \
    quadrature rule. For options with specific quadrature rule see options for specific \
    quadrature rule.";

aFejerNodesWeights::usage="aFejerNodesWeights[n,ops] first argument in function call \
    is number of nodes and weights in Fejer quadrature rule desired. The only Option \
    is WorkingPrecision (def. WorkingPrecision->$MachinePrecision). If function successes \
    it returns list consisting of two lists where first one represents nodes for Fejer \
    quadrature rule and secound one represents weights of Fejers qudrature rule.";

aInterpolatoryWeights::usage="aInterpolatoryWeights[{name,polyops},no,sigma,ops] ...";

aCheckSigma::usage="aCheckSigma[no,we,sigma,delta,{name,polyops},ops] ...";
aCheckTuran::usage="aCheckTuran[no,we,s,delta,{name,polyops},ops] ...";

(*Options*)
Options[aZero]={OrthogonalPolynomials`IterationDepth->30,Precision->$MachinePrecision-10, 
    WorkingPrecision->$MachinePrecision,OrthogonalPolynomials`Algorithm->aQR};
Options[aPasquini]={OrthogonalPolynomials`IterationDepth->30,Precision->$MachinePrecision-10,
    WorkingPrecision->$MachinePrecision};
Options[aQR]={OrthogonalPolynomials`IterationDepth->30,Precision->$MachinePrecision-10, 
    WorkingPrecision->$MachinePrecision};

Options[aGaussianNodesWeights] = {OrthogonalPolynomials`IterationDepth -> 30,
    Precision -> $MachinePrecision - 10,WorkingPrecision -> $MachinePrecision,
    Accuracy->$MachinePrecision,ModifiedAlgorithm->True,OrthogonalPolynomials`Algorithm->aQR};
Options[aGaussianWeights] = {WorkingPrecision -> $MachinePrecision,
    Precision->$MachinePrecision-10,ReturnLast->False};
Options[aRadauNodesWeights] = {OrthogonalPolynomials`IterationDepth -> 30,
    Accuracy->$MachinePrecision,OrthogonalPolynomials`Algorithm->aQR,ModifiedAlgorithm->True,
    Precision->$MachinePrecision-10,WorkingPrecision->$MachinePrecision};
Options[aLobattoNodesWeights] = {OrthogonalPolynomials`IterationDepth -> 30,
    Accuracy->$MachinePrecision,OrthogonalPolynomials`Algorithm->aQR,ModifiedAlgorithm->True,
    Precision->$MachinePrecision-10,WorkingPrecision->$MachinePrecision};

Options[aLaurie] = {WorkingPrecision -> $MachinePrecision,OrthogonalPolynomials`Symbolic->False};
Options[aDevideConquer]={WorkingPrecision->$MachinePrecision,Precision->$MachinePrecision-10,
    Accuracy->$MachinePrecision-9,OrthogonalPolynomials`IterationDepth->30};
Options[aKronrodNodesWeights] = {WorkingPrecision -> $MachinePrecision,
    OrthogonalPolynomials`AlgorithmKronrod->aLaurie,Precision->$MachinePrecision-10,
    OrthogonalPolynomials`IterationDepth->30,OrthogonalPolynomials`Symbolic->False,
    Accuracy->$MachinePrecision-9};

Options[aTuranNodes]={WorkingPrecision->$MachinePrecision,Precision->$MachinePrecision-10,
    Accuracy->$MachinePrecision-9,OrthogonalPolynomials`IterationDepth->100,
    InternalPrecision->6,InternalAccuracy->7,OrthogonalPolynomials`ReturnList->False,
    OrthogonalPolynomials`Start->{2,3},
    OrthogonalPolynomials`AlgorithmSigma->OrthogonalPolynomials`Homotopy,
    OrthogonalPolynomials`GaussianPoints->500,
    OrthogonalPolynomials`StartFraction->1/10,OrthogonalPolynomials`IterationDepth->20};
Options[aTuranNodesWeights]={WorkingPrecision->$MachinePrecision,
    Precision->$MachinePrecision-10,OrthogonalPolynomials`IterationDepth->100,
    Accuracy->$MachinePrecision-9,OrthogonalPolynomials`ReturnList->False,
    InternalPrecision->6,InternalAccuracy->7,
    OrthogonalPolynomials`AlgorithmSigma->OrthogonalPolynomials`Homotopy,
    OrthogonalPolynomials`GaussianPoints->50,OrthogonalPolynomials`IterationDepth->20,
    OrthogonalPolynomials`StartFraction->1/10};

Options[aSigmaNodes]={WorkingPrecision->$MachinePrecision,
    Precision->$MachinePrecision-10,OrthogonalPolynomials`IterationDepth->100,
    Accuracy->$MachinePrecision-9,OrthogonalPolynomials`ReturnList->False,
    InternalPrecision->6,InternalAccuracy->7,
    OrthogonalPolynomials`Start->{2,3},
    OrthogonalPolynomials`AlgorithmSigma->OrthogonalPolynomials`Homotopy,
    OrthogonalPolynomials`StartFraction->1/10,OrthogonalPolynomials`GaussianPoints->500,
    OrthogonalPolynomials`IterationDepth->20};
Options[aSigmaWeights]={WorkingPrecision->$MachinePrecision};
Options[aSigmaNodesWeights]={WorkingPrecision->$MachinePrecision,
    Precision->$MachinePrecision-10,OrthogonalPolynomials`IterationDepth->100,
    Accuracy->$MachinePrecision-9,OrthogonalPolynomials`ReturnList->False,
    InternalPrecision->6,InternalAccuracy->7,
    OrthogonalPolynomials`AlgorithmSigma->OrthogonalPolynomials`Homotopy,
    OrthogonalPolynomials`GaussianPoints->500,OrthogonalPolynomials`IterationDepth->20,
    OrthogonalPolynomials`StartFraction->1/10};

Options[aAntiGaussianNodesWeights]={WorkingPrecision->$MachinePrecision,Precision->$MachinePrecision-10};

Options[aNodesWeights]={Precision->$MachinePrecision-10,WorkingPrecision->$MachinePrecision,
    Accuracy->$MachinePrecision-9,OrthogonalPolynomials`IterationDepth->30,
    OrthogonalPolynomials`ReturnList->False,InternalPrecision->6,InternalAccuracy->7,
    InternalWorkingPrecision->$MachinePrecision};

Options[aFejerNodesWeights]={WorkingPrecision->$MachinePrecision};

Options[aInterpolatoryWeights] = {WorkingPrecision -> $MachinePrecision, 
    Precision -> $MachinePrecision - 10};

Options[aCheckSigma]={WorkingPrecision->$MachinePrecision};
Options[aCheckTuran]={WorkingPrecision->$MachinePrecision};

(*End options           *)
(*Attributes            *)
Attributes[aQR]={ReadProtected};
Attributes[aPasquini]={ReadProtected};
Attributes[aZero]={ReadProtected};
Attributes[aGaussianNodesWeights]={ReadProtected};
Attributes[aGaussianWeights]={ReadProtected};
Attributes[aRadauNodesWeights]={ReadProtected};
Attributes[aLobattoNodesWeights]={ReadProtected};
Attributes[aLaurie]={ReadProtected};
Attributes[aDevideConquer]={ReadProtected};
Attributes[aKronrodNodesWeights]={ReadProtected};
Attributes[aTuranNodes]={ReadProtected};
Attributes[aTuranNodesWeights]={ReadProtected};
Attributes[aSigmaNodes]={ReadProtected};
Attributes[aSigmaWeights]={ReadProtected};
Attributes[aSigmaNodesWeights]={ReadProtected};
Attributes[aAntiGaussianNodesWeights]={ReadProtected};
Attributes[aNodesWeights]={ReadProtected};
Attributes[aFejerNodesWeights]={ReadProtected};
Attributes[aInterpolatoryWeights]={ReadProtected};
Attributes[aCheckSigma]={ReadProtected};
Attributes[aCheckTuran]={ReadProtected};
(*End Attributes            *)
Begin["`Private`"]

aPasquini[n_Integer,st_?VectorQ,Fun_Function,Ops___]:=
    Module[{zs,dzs,f,ha,jac,err,i,j,it,iD,ad,as,k,h,eps,wrprec,prec,x,der,fun}, 
    jac=Table[Table[0,{i,1,n}],{j,1,n}];dzs=Table[1,{i,1,n}];f=dzs;it=1;
    {iD,wrprec,prec}={OrthogonalPolynomials`IterationDepth,WorkingPrecision,Precision}/.{Ops}
        /.Options[aPasquini];
    eps=10^-prec;zs=SetPrecision[st,wrprec];
    Block[{$MinPrecision=wrprec},fun=Fun[x];der=D[Fun[x],x];
        While[Or@@(((#1[[1]]>(eps #1[[2]]))&)/@Transpose[{Abs/@dzs,Abs/@zs}]),
            For[h=1,h<=n,h++,ad=0;as=0;
                For[k=n,k>=h+1,k--,jac[[h,k]]=1/(zs[[h]]-zs[[k]]); 
                        jac[[k,h]]=-jac[[h,k]];ad+=jac[[h, k]];];
                For[k=1,k<=h-1,k++,as+=jac[[h, k]];];
                f[[h]]=ad+as+(fun/.(x->zs[[h]]));];
            For[h=1,h<=n,h++,jac[[h,h]]=0;
                For[k=n,k>=h+1,k--,jac[[h,k]]=-jac[[h,k]]*jac[[h,k]]; 
                        jac[[k,h]]=jac[[h,k]];jac[[h,h]]+=-jac[[h,k]];];
                    For[k=1,k<=h-1,k++,jac[[h,h]]+=-jac[[h,k]];];
                jac[[h,h]]+=-(der/.(x->zs[[h]]));];
            dzs=LinearSolve[jac,f];zs=zs+dzs;it++;
            If[it>iD,Message[aPasquini::err];Abort[];];];];
     Return[Sort[zs,(If[Re[#1]<Re[#2],True,If[Re[#1]==Re[#2],Im[#1]<Im[#2],False]])&]];];

aQR[Order_Integer/;Order>0,alfa_?VectorQ,beta_?VectorQ,Ops___]:= 
    Module[{nodes,pom,n=Order,i,iD,j,k,l,m,prec,wrprec,eps,s,c,p,g,r,f,b},
    {iD,prec,wrprec}={OrthogonalPolynomials`IterationDepth,Precision,WorkingPrecision}/.{Ops}/.
        Options[aQR];
    If[Length[alfa]<n || Length[beta]<n,Message[aQR::bdinp];Abort[];];
        If[wrprec < prec, Message[aQR::prec]; Abort[];];
        If[n == 1, Return[{N[alfa[[1]], wrprec], N[beta[[1]], wrprec]}];];
    Block[{$MinPrecision=wrprec},
    nodes=SetPrecision[Take[alfa,n],wrprec];
    pom=SetPrecision[Join[Sqrt[Take[beta,{2,n}]],{0}],wrprec];eps=10^-prec;  
    For[l=1,l<=n,l++,j=0;
Label[10];
        For[m=l,m<n,m++, 
            If[Abs[pom[[m]]]<=eps*(Abs[nodes[[m]]]+Abs[nodes[[m + 1]]]),Break[];];];
            If[m>l,If[j==iD,Break[];];
                j++;p=nodes[[l]];g=(nodes[[l+1]]-p)/(2*pom[[l]]);r=Sqrt[g*g+1]; 
                g=nodes[[m]]-p+pom[[l]]/(g+If[Re[g]>=0,r,-r]);s=1;c=1;p=0;
                For[i=m-1,i>=l,i--,f=s*pom[[i]];b=c*pom[[i]];
                    If[Abs[f]<Abs[g],s=f/g;r=Sqrt[s*s+1];pom[[i+1]]=g*r;c=1/r;s=s*c;,
            c=g/f;r=Sqrt[c*c+1];pom[[i+1]]=f*r;s=1/r;c=c*s;];
            g=nodes[[i+1]]-p;r=(nodes[[i]]-g)*s+2*c*b;p=s*r;
            nodes[[i+1]]=g+p;g=c*r-b;];
                nodes[[l]]=nodes[[l]]-p;pom[[l]]=g;pom[[m]]=0;Goto[10];];];];
        If[j==iD,Message[aQR::err];Abort[];]; 
        Return[Sort[nodes,(If[Re[#1]<Re[#2],True,If[Re[#1]==Re[#2],Im[#1]<Im[#2],False]]) &]];];

aZero[Order_Integer/;Order>0,stv_?VectorQ,fun_Function,Ops___]:=
    Module[{},
    Return[aPasquini[Order,stv,fun,Ops,Sequence@@Options[aZero]]];];

aZero[Order_Integer/;Order>0,alfa_?VectorQ,beta_?VectorQ,Ops___]:=
    Module[{},
    Return[aQR[Order,alfa,beta,Ops,Sequence@@Options[aZero]]];];

aZero[Order_Integer/;Order>0,{Name_?polyQ,polyOps___},Ops___]:=
    Module[{a,b,alfa,beta,wrprec,prec},
    {alg,wrprec,prec}={OrthogonalPolynomials`Algorithm,WorkingPrecision,Precision}/.{Ops}
        /.Options[aZero];
    If[wrprec < prec, Message[aZero::prec]; Abort[];];
    {a,b}=Name["ttr",polyOps][0];
    alfa={a};beta={b};
    a=Name["ttr",polyOps][k];
    alfa=Join[alfa,Table[(a/.k->i)[[1]],{i,1,Order}]];
    beta=Join[beta,Table[(a/.k->i)[[2]],{i,1,Order}]];
    If[alg==aQR,
        Return[aZero[Order,alfa,beta,Ops]];];
    If[alg==aPasquini,
        b=Name["pas",polyOps][Order];
        b[[2]]=Evaluate[b[[2]]];
        If[Length[b[[1]]]==0,
            b[[1]]=SetPrecision[aZero[Order,alfa,beta,OrthogonalPolynomials`Algorithm->aQR,Ops],wrprec];];
        Return[aZero[Order,b[[1]],b[[2]],Ops]];];];

aGaussianNodesWeights[Order_Integer/;Order>0,alfa_?VectorQ,beta_?VectorQ,Ops___] := 
    Module[{nodes,weights,pom,n=Order,i,iD,j,k,l,m,prec,wrprec,eps,s,c,p,g,r,f,b,mod,zero},
    {iD,prec,wrprec,mod}={OrthogonalPolynomials`IterationDepth,Precision,WorkingPrecision,
        ModifiedAlgorithm}/.{Ops}/.Options[aGaussianNodesWeights];
        If[Length[alfa]<n || Length[beta]<n,Message[aGaussianNodesWeights::bdinp];Abort[];];
        If[wrprec < prec, Message[aGaussianNodesWeights::prec]; Abort[];];
        If[n == 1, Return[{{N[alfa[[1]], wrprec]},{N[beta[[1]], wrprec]}}];];
        nodes=SetPrecision[Take[alfa,n],wrprec];
    pom=SetPrecision[Join[Sqrt[Take[beta,{2,n}]],{0}],wrprec];eps=10^-prec;

    If[!mod,
    If[wrprec < prec + 10, Message[aGaussianNodesWeights::wrprec];];
    Block[{$MinPrecision=wrprec},
    weights=Join[{1},Table[0,{k,2,n}]];
    For[l=1,l<=n,l++,j=0;
Label[10];
    For[m=l,m<n,m++, 
            If[Abs[pom[[m]]]<=eps*(Abs[nodes[[m]]]+Abs[nodes[[m + 1]]]),Break[];];];
            If[m>l,If[j==iD,Break[];];
                j++;p=nodes[[l]];g=(nodes[[l+1]]-p)/(2*pom[[l]]);r=Sqrt[g*g+1]; 
                g=nodes[[m]]-p+pom[[l]]/(g+If[Re[g]>=0,r,-r]);s=1;c=1;p=0;
                For[i=m-1,i>=l,i--,f=s*pom[[i]];b=c*pom[[i]];
                    If[Abs[f]<Abs[g],s=f/g;r=Sqrt[s*s+1];pom[[i+1]]=g*r;c=1/r;s=s*c;,
            c=g/f;r=Sqrt[c*c+1];pom[[i+1]]=f*r;s=1/r;c=c*s;];
                    f=weights[[i+1]];g=nodes[[i+1]]-p;r=(nodes[[i]]-g)*s+2*c*b;p=s*r;
                    nodes[[i+1]]=g+p;g=c*r-b;weights[[i+1]]=s*weights[[i]]+c*f;
                    weights[[i]]=c*weights[[i]]-s*f;];
                nodes[[l]]=nodes[[l]]-p;pom[[l]]=g;pom[[m]]=0;
                Goto[10];];];
    
        If[j==iD,Message[aGaussianNodesWeights::err];Abort[];, 
            p=SetPrecision[beta[[1]],wrprec];weights=(p*#1^2 &) /@ weights;
        Return[Transpose[Sort[Transpose[{nodes,weights}],
            (If[Re[#1[[1]]]<Re[#2[[1]]],True,If[Re[#1[[1]]]==Re[#2[[1]]],
            Im[#1[[1]]]<Im[#2[[1]]],False]])&]]];];];,
    nodes=aZero[Order,alfa,beta,
		Ops,Sequence@@Options[aGaussianNodesWeights]];
    weights=aGaussianWeights[Order,alfa,beta,nodes,Ops,
        Sequence@@Options[aGaussianNodesWeights]];
    Return[Transpose[Sort[Transpose[{nodes,weights}],
        (If[Re[#1[[1]]]<Re[#2[[1]]],True,If[Re[#1[[1]]]==Re[#2[[1]]],
        Im[#1[[1]]]<Im[#2[[1]]],False]])&]]];];];

aGaussianNodesWeights[Order_Integer/;Order>0,{Name_?polyQ,polyOps___},Ops___]:=
    Module[{nodes,a,b,alfa,beta,i,k,mod,alg},
    {mod,alg}={ModifiedAlgorithm,OrthogonalPolynomials`Algorithm}/.{Ops}/.Options[aGaussianNodesWeights];
    {a,b}=Name["ttr",polyOps][0];
    alfa={a};beta={b};
    a=Name["ttr",polyOps][k];
    alfa=Join[alfa,Table[(a/.k->i)[[1]],{i,1,Order}]];
    beta=Join[beta,Table[(a/.k->i)[[2]],{i,1,Order}]];

    If[!mod && ToString[alg]==="aQR",
        Return[aGaussianNodesWeights[Order,alfa,beta,Ops]];];
    nodes=aZero[Order,{Name,polyOps},Ops,Sequence@@Options[aGaussianNodesWeights]];
    weights=aGaussianWeights[Order,alfa,beta,nodes,Ops,
        Sequence@@Options[aGaussianNodesWeights]];
    Return[Transpose[Sort[Transpose[{nodes,weights}],
        (If[Re[#1[[1]]]<Re[#2[[1]]],True,If[Re[#1[[1]]]==Re[#2[[1]]],
            Im[#1[[1]]]<Im[#2[[1]]],False]])&]]];];


aGaussianWeights[Order_?IntegerQ/;Order>0,alfa_?VectorQ,beta_?VectorQ,Nodes_?VectorQ,Ops___]:= 
    Module[{wrprec,pom,weights,f,i,n=Order,g,eps,zero,werr,prec,last},
    
    If[n!=Length[Nodes],Message[aGaussianWeights::neqlen];Abort[];];
    {wrprec,prec,last}={WorkingPrecision,Precision,
        ReturnLast}/.{Ops}/.Options[aGaussianWeights];
        pom=Table[0,{i,1,n}];nodes=SetPrecision[Nodes,wrprec];zero=10^(-prec+1);
    eps=10^-prec;
        Block[{$MinPrecision=wrprec},
        f=Table[SetPrecision[1,wrprec],{i,1,n}]/Sqrt[beta[[1]]];weights=f^2;
        For[i=1,i<n,i++, 
            g=((nodes-alfa[[i]])f-Sqrt[beta[[i]]]pom)/Sqrt[beta[[i + 1]]];
            pom=f;f=g;weights +=g^2;];
        werr=((nodes-alfa[[n]])f-Sqrt[beta[[n]]]pom)/(Sqrt[beta[[n]]]pom);
        For[i=1,i<=n,i++,If[Abs[nodes[[i]]]<zero,(*Break[];Print["manje"];*)
        werr[[i]]=((nodes[[i]]-alfa[[n]])f[[i]]-Sqrt[beta[[n]]]pom[[i]]);];];
        (*If[i<=n,werr[[i]]=((nodes[[i]]-alfa[[n]])f[[i]]-Sqrt[beta[[n]]]pom[[i]]);];*)
        werr=Max[Abs[werr]];
        If[werr>10 eps,Message[aGaussianNodesWeights::werr,Round[-Log[10, werr]]];];];
    If[last,Return[{1/Sqrt[weights beta[[1]]],g/Sqrt[weights]}],Return[1/weights];];];

aRadauNodesWeights[Number_Integer/;Number>0,Alfa_?VectorQ,Beta_?VectorQ,End_?NumberQ,Ops___]:= 
    Module[{alfa,beta,pom0=0,pom1=1,p,i,prec,end},
        If[Length[Alfa]<Number || Length[Beta]<=Number,
        Message[aRadauNodesWeights::bdinp];Abort[];];
    {prec}={WorkingPrecision}/.{Ops}/.Options[aRadauNodesWeights];
    Block[{$MinPrecision=prec},
    end=SetPrecision[End,prec];alfa=Take[Alfa,Number];beta=Take[Beta,Number+1];
        For[i=1,i<=Number,i++,
            p=pom0;pom0=pom1;pom1=(end-alfa[[i]])pom0-beta[[i]]p;];
        AppendTo[alfa,end-beta[[Number+1]]pom0/pom1];
        Return[aGaussianNodesWeights[Number+1,alfa,beta,Ops,
        Sequence@@Options[aRadauNodesWeights]]];];];

aLobattoNodesWeights[Number_Integer/;Number>0,Alfa_?VectorQ,Beta_?VectorQ,Left_?NumberQ,
    Right_?NumberQ,Ops___]:= 
    Module[{pom0L=0,pom0R=0,pom1L=1,pom1R=1,p1L,p1R,i,alfa,beta,right,left,prec},
        If[Length[Alfa]<Number+1 || Length[Beta]<Number+1,
            Message[aLobattoNodesWeights::bdinp];Abort[];];
    {prec}={WorkingPrecision}/.{Ops}/.Options[aLobattoNodesWeights];
    Block[{$MinPrecision=prec},
    left=SetPrecision[Left,prec];right=SetPrecision[Right,prec];
        alfa=Take[Alfa,Number+1];beta=Take[Beta,Number+1];
        For[i=1,i<=(Number+1),i++,
            p1L=pom0L;pom0L=pom1L;p1R=pom0R;pom0R=pom1R; 
            pom1L=(left-alfa[[i]])pom0L-beta[[i]]p1L; 
            pom1R=(right-alfa[[i]])pom0R-beta[[i]]p1R;];
        p1L=pom1L pom0R-pom1R pom0L;
        AppendTo[alfa,(left pom1L pom0R-right pom1R pom0L)/p1L]; 
        AppendTo[beta,(right-left)pom1L pom1R/p1L];
        Return[aGaussianNodesWeights[Number+2,alfa,beta,Ops,
        Sequence@@Options[aLobattoNodesWeights]]];];];

aLaurie[Order_Integer/;Order>1,Alfa_?VectorQ,Beta_?VectorQ,Ops___]:= 
    Module[{n=Order,i,s,t,alfa,beta,u,j,k,l,m,prec,alg},
    If[Length[Alfa]<(Floor[3 n/2]+1) || Length[Beta]<(Ceiling[3 n/2]+1),
            Message[aLaurie::bdinp];Abort[];];
        {prec,alg}={WorkingPrecision,OrthogonalPolynomials`Symbolic}/.{Ops}/.Options[aLaurie];
    If[!alg,
        alfa=SetPrecision[Take[Alfa,Floor[3 n/2]+1],prec];
        beta=SetPrecision[Take[Beta,Ceiling[3 n/2]+1],prec];,
        alfa=Take[Alfa,Floor[3 n/2]+1];beta=Take[Beta,Ceiling[3 n/2]+1];];
        s=Table[0,{i,1,Floor[n/2]+2}];t=s;t[[2]]=beta[[n+2]];
    Block[{$MinPrecision=prec},
            For[m=0,m<=(n-2),m++,u=0;
            For[k=Floor[(m+1)/2],k>=0,k--,l=m-k;
            u=Cancel[Factor[Together[u+Cancel[Factor[Together[(alfa[[k+n+2]]-
                alfa[[l+1]])t[[k+2]]+beta[[n+2+k]]s[[k+1]]-
                beta[[l+1]]s[[k+2]]]]]]]];s[[k+2]]=u;];{s,t}={t,s};];
        s=RotateRight[s];
        For[m=n-1,m<=(2n-3),m++,u=0;
            For[k=m+1-n,k<=Floor[(m-1)/2],k++,l=m-k;j=n-1-l;
            u=Cancel[Factor[Together[u-Cancel[Factor[Together[(alfa[[n+k+2]]-
                alfa[[l+1]])t[[j+2]]+beta[[k+n+2]]s[[j+2]]-
                beta[[l+1]]s[[j+3]]]]]]]];s[[j+2]]=u;];
            If[EvenQ[m],k=m/2;
            AppendTo[alfa,Cancel[Factor[Together[alfa[[k+1]]+(s[[j+2]]-
                beta[[k+n+2]]s[[j+3]])/t[[j+3]]]]]];,
                k=(m+1)/2;AppendTo[beta,Cancel[s[[j+2]]/s[[j+3]]]];];{s,t}={t,s};];
        AppendTo[alfa,Cancel[Factor[Together[alfa[[n]]-beta[[2n+1]]s[[2]]/t[[2]]]]]];
        Return[{alfa,beta}];];];

aDevideConquer[Order_?IntegerQ/;Order>1,Alfa_?VectorQ,Beta_?VectorQ,nG_?VectorQ,wFG_?VectorQ,
    wLG_?VectorQ,Ops___]:=
    Module[{eps,prec,wrprec,iD,zero,n,w,alfa,beta,lambda,x0,sigma,delta,pom1,pom2,res={},i,
    j,f,wF,c,wGinK,wKinK},
    If[Length[Alfa]<(Floor[3 Order/2]+1) || Length[Beta]<(Ceiling[3 Order/2]+1),
            Message[aDevideConquer::bdinp];Abort[];];
    {wrprec,prec,iD,zero}={WorkingPrecision,Precision,OrthogonalPolynomials`IterationDepth,
        Accuracy}/.{Ops}/.Options[aDevideConquer];
    If[wrprec<prec,Message[aKronrodNodesWeights::prec];Abort[];];
    eps=10^-prec;
    Block[{$MinPrecision=wrprec},alfa=Alfa;beta=Beta;zero=10^-zero;
        If[OddQ[Order],
        {n,w}=aGaussianNodesWeights[(Order-1)/2,Take[alfa,{Order+2,(3Order+1)/2}],
            Prepend[Take[beta,{Order+3,(3Order+1)/2}],1],Ops,
            Sequence@@Options[aDevideConquer]];
        AppendTo[alfa,(Plus@@nG)-2(Plus@@Take[alfa,{Order+2,(3Order+1)/2}])
            -(w.Table[Times @@ (n[[i]]-nG),{i,1,(Order-1)/2}])/
            (Times @@ (Take[beta,{Order+3,(3Order+1)/2+1}]))];];
    {n,w}=aGaussianNodesWeights[Floor[(Order+1)/2],Take[alfa,{Order+2,
        Ceiling[3Order/2]+1}],Prepend[Take[beta,{Order+3,Ceiling[3Order/2]+1}],1]
        ,Ops,Sequence@@Options[aDevideConquer]];
    wF=Table[lambda=Delete[nG,k];Table[Times @@((n[[i]]-lambda)/(nG[[k]]-lambda)),
           {i,1,Ceiling[Order/2]}].w,{k,1,Order}];c=wLG^2 beta[[Order+1]]+wF beta[[Order+2]];
        gama=alfa[[Order+1]];
        
    If[Or@@((#1<0)&/@c),Message[aDevideConquer::cmpl];Abort[];];

        (*Zero finder*)
        For[i=1,i<Order,i++,x0=(nG[[i]]+nG[[i+1]])/2;k=0;
Label[10];
            k++;sigma=3x0-(gama+nG[[i]]+nG[[i+1]])+Plus @@Table[c[[j]]/(nG[[j]]-x0)
            (nG[[j]]-nG[[i]])/(nG[[j]]-x0)(nG[[j]]-nG[[i+1]])/(nG[[j]]-x0),{j,1,i-1}]
            +Plus @@Table[c[[j]]/(nG[[j]]-x0)(nG[[j]]-nG[[i]])/(nG[[j]]-x0)
            (nG[[j]]-nG[[i+1]])/(nG[[j]]-x0),{j,i+2,Order}];
        pom1=sigma/((nG[[i]]-x0)(x0-nG[[i+1]]));f=x0-gama+Plus @@(c/(nG-x0));
            pom2=(1+Plus @@(c/(nG-x0)^2))-(1/(nG[[i]]-x0)+1/(nG[[i+1]]-x0))f;
            delta=2 f/pom2/(1+Sqrt[1+2 pom1/pom2 2f/pom2]);x0=x0-delta;
            If[(Abs[delta]>(eps Abs[x0])) && (Abs[x0]>zero) && k<iD,Goto[10]];
            If[k==iD,Message[aDevideConquer::iDexc];Abort[];];
            AppendTo[res, x0];];

        If[nG[[1]]<gama,x0=(gama+nG[[1]])/2-Sqrt[((gama-nG[[1]])/2)^2+(Plus @@c)];,
            x0=nG[[1]]+(Plus @@c)/((nG[[1]]-gama)/2+Sqrt[((gama-nG[[1]])/2)^2+(Plus @@c)]);];
        k=0;
Label[20];
        k++;pom1=-(1+Plus @@Table[(Sqrt[c[[i]]]/(x0-nG[[i]]))^2(nG[[1]]-nG[[i]])/(x0-nG[[i]]),
        {i,2,Order}])/(x0-nG[[1]]);f=x0-gama+Plus @@(c/(nG - x0));
        pom2=(1+Plus @@(c/(nG - x0)^2))+f/(x0-nG[[1]]); 
        delta=2 f/pom2/(1+Sqrt[1+2 pom1/pom2 2f/pom2]);x0=x0-delta;
        If[(Abs[delta]>(eps Abs[x0])) && k<iD, Goto[20]];
        If[k==iD,Message[aDevideConquer::iDexc];Abort[]];
        PrependTo[res, x0];

        If[nG[[Order]]>gama,x0=((gama+nG[[Order]])/2+Sqrt[((gama-nG[[Order]])/2)^2+(Plus @@c)]);,
            x0=nG[[Order]]-(Plus @@c)/((-nG[[Order]]+gama)/2+Sqrt[((gama-nG[[Order]])/2)^2+
            (Plus @@c)]);];
        k=0;
Label[30];
        k++;pom1=-(1+Plus @@Table[(Sqrt[c[[i]]]/(x0-nG[[i]]))^2(nG[[Order]]-nG[[i]])/
        (x0-nG[[i]]),{i,2,Order}])/(x0-nG[[Order]]);f=x0-gama+Plus @@(c/(nG-x0));
    pom2=(1+Plus @@(c/(nG-x0)^2))+f/(x0-nG[[Order]]);
        delta=2 f/pom2/(1+Sqrt[1 + 2 pom1/pom2 2f/pom2]);x0=x0-delta; 
        If[(Abs[delta]>(eps Abs[x0])) && k<iD,Goto[30]];
        If[k==iD,Message[aDevideConquer::iDexc];Abort[]];
        AppendTo[res, x0];
        (*Zero finder end*)
      
      
        (*Construction*)
        wGinK=beta[[1]]beta[[Order+2]]wFG^2*wF/c;c=Sqrt[c];
        pom1=Table[pom2=-c/(res[[i]]-nG);pom2/Sqrt[pom2.pom2+1],{i,1,Order+1}];sigma=wFG*(wLG/c);
        wKinK=beta[[1]]beta[[Order+1]](Table[pom1[[i]].sigma,{i,1,Order+1}])^2;
        Return[Transpose[Sort[Transpose[{Join[nG,res],Join[wGinK,wKinK]}]]]];];
        (*Construction end*)
   ];

aKronrodNodesWeights[Order_Integer/;Order>1,Alfa_?VectorQ,Beta_?VectorQ,Ops___]:=
    Module[{prec,alfa,beta,wrprec,n,wf,wl},
        If[Length[Alfa]<(Floor[3 Order/2]+1) || Length[Beta]<(Ceiling[3 Order/2]+1), 
        Message[aKronrodNodesWeights::bdinp];Abort[];];
        {wrprec,prec,alg}={WorkingPrecision,Precision,OrthogonalPolynomials`AlgorithmKronrod}/.{Ops}
        /.Options[aKronrodNodesWeights];
    If[wrprec<prec,Message[aKronrodNodesWeights::prec];Abort[];];
    alfa=Take[Alfa,Floor[3 Order/2]+1];beta=Take[Beta,Ceiling[3 Order/2]+1];
    If[ToString[alg]==="aLaurie",    
            {alfa,beta}=aLaurie[Order,alfa,beta,Ops,
            Sequence@@Options[aKronrodNodesWeights]];
            Return[aGaussianNodesWeights[2 Order+1,alfa,beta,Ops,
                Sequence@@Options[aKronrodNodesWeights]]];];
    If[ToString[alg]==="aDevideConquer",
        n=aZero[Order,alfa,beta,Ops,Sequence@@Options[aKronrodNodesWeights]];
        {wf,wl}=aGaussianWeights[Order,alfa,beta,n,ReturnLast->True,Ops,
            Sequence@@Options[aKronrodNodesWeights]];
        Return[{{n,beta[[1]]wf^2},aDevideConquer[Order,alfa,beta,n,wf,wl,Ops,
            Sequence@@Options[aKronrodNodesWeights]]}];];];

(*construction over increasing s                            *)
aTuranNodes[Order_?IntegerQ/;Order>0,Alfa_?VectorQ,Beta_?VectorQ,s_?IntegerQ/;s>=0,Ops___Rule]:= 
    Module[{n=Order,tau,tau1,w,a,b,forExtr,i,j,deltaTau,prec,eps,iter,nG,wG,pom0,pom1,pom2,pom3,
    pom4,pom5,pom6,zero,l,k,list,ncap,slob,jac,iD,sl,node,intPrec,intAcc,alg},
    {alg}={OrthogonalPolynomials`AlgorithmSigma}/.{Ops}/.Options[aTuranNodes];
    If[ToString[alg]==="IncreaseS",
    If[Length[Alfa]<Floor[(2(s+1)n-1)/2]+1 || Length[Beta]<Floor[(2(s+1)n-1)/2]+1, 
            Message[aTuranNodesWeights::bdinp];Abort[];];
        {wrprec,prec,iD,acc,list,intPrec,intAcc}={WorkingPrecision,Precision, 
                OrthogonalPolynomials`IterationDepth,Accuracy,OrthogonalPolynomials`ReturnList,
        	InternalPrecision,InternalAccuracy}/.{Ops}/.Options[aTuranNodes];
        If[wrprec<prec,Message[aSigmaNodesWeights::prec];Abort[];];
        If[wrprec<intPrec,Message[aSigmaNodesWeights::intPrec];Abort[];];
        If[s==0,Return[aZero[Order,Alfa,Beta,WorkingPrecision->wrprec,Precision->prec]];];
        eps=10^-intPrec;slob=Table[0,{i,1,n}];zero=10^-intAcc;jac=Table[Table[0,{i,1,n}],{j,1,n}];
        tau=aZero[n,Alfa,Beta,Precision->intPrec,WorkingPrecision->wrprec];
        Block[{$MinPrecision=wrprec},
        (*QR step           *)
        pom0=0;pom1=1;pom2=0;pom6=0;a={};b={};
        For[i=1,i<=n,i++,pom3=Alfa[[i]]-tau[[1]]-pom0;pom4=pom2;pom2=pom1;
            If[Abs[pom2]>$MinNumber,pom5=pom3*pom3/pom2;,pom5=pom4*Beta[[i+1]];]; 
            AppendTo[b,pom6*(pom5+Beta[[i+1]])];pom6=Beta[[i+1]]/(pom5+Beta[[i+1]]); 
            pom1=pom5/(pom5+Beta[[i+1]]);pom0=pom6*(pom3+Alfa[[i+1]]-tau[[1]]); 
            AppendTo[a,pom3+pom0+tau[[1]]];];
        (*QR step end           *)
        tau1=aZero[n,a,b,Precision->intPrec,WorkingPrecision->wrprec];
        forExtr={tau1,tau1};deltaTau=tau1-tau;pom0=Table[0,{i,1,n}];
        For[i=1,i<=s,i++,(*Print["i:=",i];*)
            ncap=Floor[(2(i+1)n-1)/2]+1;(*Print["tau:=",forExtr[[-1]]];Print["ncap:=", ncap];*)
            If[i<s,
                    {nG,wG}=aGaussianNodesWeights[ncap,Alfa,Beta,Precision->intPrec, 
                        WorkingPrecision->wrprec,Ops];,
            {nG,wG}=aGaussianNodesWeights[ncap,Alfa,Beta,Precision->prec, 
                        WorkingPrecision->wrprec,Ops];];
            For[j=1,j<=n,j++,
                    If[OddQ[i],pom0[[j]] +=1;,pom0[[n+1-j]] +=1;]; 
                    If[i==s && j==n,eps=10^-prec;zero=10^-acc;];iter=0;
            While[Or@@(((If[#1[[2]]>zero,#1[[1]]>(eps #1[[2]]),False])&)/@ 
                        Transpose[{Abs/@deltaTau,Abs/@tau1}]),
        (*Newton*)
        jac *=0;slob *=0;
                For[k=1,k<=ncap,k++, 
                    sl=wG[[k]]*(Times @@ ((nG[[k]]-tau1)^(2 pom0+1)));
                    node=Table[(nG[[k]])^(l-1),{l,1,n}];slob +=sl node;
                        For[l=1,l<=n,l++, 
                        jac[[l]] +=-(2 pom0+1)sl/(nG[[k]]-tau1) node[[l]]];];
                (*Newton end*)
        deltaTau=LinearSolve[jac,slob];tau=tau1;tau1=tau1-deltaTau;
                If[iter==iD,Print[AppendTo[forExtr,tau1]];Print["j:=",j];
                    Print["iter:=",iter];Message[aSigmaNodesWeights::iDexc];Abort[];];
                iter++;];
                tau1=Sort[tau1];If[list,AppendTo[forExtr,tau1];,forExtr={forExtr[[-1]],tau1};];
                (*Lagrange*)
        tau1=2forExtr[[-1]]-forExtr[[-2]];
                (*Lagrange*)
        deltaTau=tau1-tau;];];
        tau1=forExtr[[-1]];
        If[list,Return[forExtr];,Return[tau1];];];];
      If[ToString[alg]==="Homotopy",
	Return[aSigmaNodes[Order,Alfa,Beta,Table[s,{i,1,Order}],Ops,
		Sequence@@Options[aTuranNodes]]];
      ];
      If[ToString[alg]==="IncreaseDegree",
	Message[aTuranNodes::usalg];
	Abort[];
      ];
      Message[aTuranNodes::ualg];
      Abort[];
];
(*construction over increasing s                            *)

(*construction over increasing degree helper                *)
aTuranNodes[Order_?IntegerQ/;Order>0,{Name_?polyQ,polyOps___},s_?IntegerQ/;s>0,
    forExtr1_?VectorQ,forExtr2_?VectorQ,Ops___Rule] :=
    Module[{jac,slob,eps,tau,i,j,k,ncap,n,w,pom1,deltaTau,forExtr,poly,norm,intPrec,
        intAcc,wrprec,prec,acc,iD,zero,iter,kGauss,list},
    {wrprec,prec,acc,iD,intPrec,intAcc,list}={WorkingPrecision,Precision,
        Accuracy,OrthogonalPolynomials`IterationDepth,InternalPrecision,
        InternalAccuracy,OrthogonalPolynomials`ReturnList}/.{Ops}/.Options[aTuranNodes];
    If[wrprec<prec,Message[aTuranNodesWeights::prec];Abort[];];
    If[wrprec<intPrec,Message[aTuranNodesWeights::intPrec];Abort[];];
    If[Not[Name["turanAllowed",polyOps]],Message[aTuranNodes::incDgr];Abort[];];
        eps=10^-intPrec;zero=10^-intAcc;forExtr={forExtr1, forExtr2};
        
        Block[{$MinPrecision=wrprec},
        norm=N[Table[Name["nor",polyOps][i],{i,0,Order-1}],wrprec];
    (*Print["tau iz Sigma:=",forExtr];*)
        kGauss=Quotient[Length[forExtr2],8]+1;tau=forExtr[[-1]];
    If[Order>=8 kGauss,ncap=Floor[(16(s+1)kGauss-1)/2]+1;
	{n,w}=aGaussianNodesWeights[ncap,
        	{Name,polyOps},WorkingPrecision->wrprec,Precision->intPrec,Ops];,
        ncap=Floor[(2(s+1)Order-1)/2]+1;
	{n,w}=aGaussianNodesWeights[ncap,
        	{Name,polyOps},WorkingPrecision->wrprec,Precision->prec,Ops];];
        For[i=Length[forExtr2]+1,i<=Order,i++,(*Print["i:=",i]; *)
            If[i==Order,eps=10^-prec;zero=10^-acc;];
        If[i-8 kGauss>0,kGauss++;
            If[Order>=8 kGauss,ncap=Floor[(16(s+1)kGauss-1)/2]+1;
                {n,w}=aGaussianNodesWeights[ncap,{Name,polyOps},
                    WorkingPrecision->wrprec,Precision->intPrec,Ops];,
                ncap=Floor[(2(s+1)Order-1)/2]+1;
                {n,w}=aGaussianNodesWeights[ncap,{Name,polyOps},
					WorkingPrecision->wrprec,Precision->prec,Ops];];
        (*Print["ncap:=",ncap];*)];
    	deltaTau=Table[1,{j,1,i}];iter=0;        
        tau=(Name["turan",polyOps][forExtr[[-2]],forExtr[[-1]],i]);
        (*Print["tauStart:=",tau];*)
        While[Or @@ (((If[#1[[2]]>zero,#1[[1]]>(eps #1[[2]]),False])&) /@ 
        Transpose[{Abs /@ deltaTau, Abs /@ tau}]),
            jac=Table[0,{j,1,i},{k,1,i}];slob=Table[0,{j,1,i}];
            For[j=1,j<=ncap,j++,
                poly=Name[i-1,polyOps,n[[j]],WorkingPrecision->wrprec, 
                        OrthogonalPolynomials`ReturnList->True]/Take[norm,i];
                For[k=1,k<=i,k++,
                    pom1=w[[j]] poly[[k]](Times @@ (n[[j]]-tau))^(2s+1);
                    jac[[k]] +=pom1/(n[[j]]-tau);slob[[k]] +=pom1;];];
            jac=(2s+1)jac;deltaTau=LinearSolve[jac,slob];tau=tau+deltaTau;iter++;
            If[iter==iD,Message[aTuranNodes::iDexc];AppendTo[forExtr,tau];
                Return[forExtr];];];
        tau=Sort[tau];(*Print["tau:=",tau];Print["iter:=",iter];*)AppendTo[forExtr,tau];];
    If[list,Return[forExtr];,Return[tau];];];];
(*construction over increasing degree                           *)

(*construction for bigger precision                         *)
aaTuranNodes[Order_Integer/;Order>0,{Name_?polyQ,polyOps___},s_?IntegerQ/;s>0,tau_?VectorQ,
    Ops___Rule]:=Module[{i},(*Print["hello3Tur"];*)
    Return[aSigmaNodes[Order,{Name,polyOps},Table[s,{i,1,Order}],tau,Ops,
        Sequence@@Options[aTuranNodes]]];];
(*construction fot bigger precision                         *)

(*construction of Turan nodes                               *)
aTuranNodes[Order_?IntegerQ/;Order>0,{Name_?polyQ,polyOps___},s_?IntegerQ/;s>0,Ops___Rule] :=
    Module[{forExtr,intPrec,intAcc,wrprec,alfa,beta,a,k,i,start,alg},(*Print["hello4Tur"];*)
        {start,wrprec,intPrec,intAcc,alg}={OrthogonalPolynomials`Start,WorkingPrecision,
        InternalPrecision,InternalAccuracy,OrthogonalPolynomials`AlgorithmSigma}/.{Ops}/.Options[aTuranNodes];
    a=Name["ttr",polyOps];
   If[ToString[alg]==="IncreaseDegree",
    If[Not[Name["turanAllowed",polyOps]],Message[aTuranNodes::incDgr];Abort[];];
    {alfa,beta}=Transpose[Name["ttr",polyOps]/@Range[0,Floor[(2 start[[2]](s+1)-1)/2]+1]];
    forExtr={aTuranNodes[start[[1]],alfa,beta,s,WorkingPrecision->wrprec,Precision->intPrec,
        Accuracy->intAcc,OrthogonalPolynomials`AlgorithmSigma->OrthogonalPolynomials`IncreaseS,
        OrthogonalPolynomials`ReturnList->False,Ops]};
        AppendTo[forExtr,aTuranNodes[start[[2]],alfa,beta,s,WorkingPrecision->wrprec,
        Precision->intPrec,Accuracy->intAcc,
	OrthogonalPolynomials`AlgorithmSigma->OrthogonalPolynomials`IncreaseS,
        OrthogonalPolynomials`ReturnList->False,Ops]];
(*Print[forExtr];*)
    Return[aTuranNodes[Order,{Name,polyOps},s,forExtr[[1]],forExtr[[2]],Ops]];
   ];
(*Print["non increase degree"];*)
    If[ToString[alg]==="IncreaseS",
    	{alfa,beta}=Transpose[Name["ttr",polyOps]/@Range[0,Floor[(2*Order*(s+1)-1)/2]+1]];
    	Return[aTuranNodes[Order,alfa,beta,s,Ops]];
    ];
    If[ToString[alg]==="Homotopy",
    	Return[aSigmaNodes[Order,{Name,polyOps},Table[s,{i,1,Order}],Ops,
		Sequence@@Options[aTuranNodes]]];
    ];
    Message[aTuranNodes::ualg];
    Abort[];
    ];
(*construction of Turan nodes                               *)

(*construction of Turan nodes & weights                         *)
aTuranNodesWeights[Order_?IntegerQ/;Order>0,Alfa_?VectorQ,Beta_?VectorQ,s_?IntegerQ/;s>0,
    Ops___Rule]:=Module[{n},
    Return[{n=aTuranNodes[Order,Alfa,Beta,s,Ops],
        aSigmaWeights[Order,Alfa,Beta,Table[s,{i,1,Order}],n,Ops,
        Sequence@@Options[aTuranNodesWeights]]}];];
(*construction of Turan nodes & weights                         *)

(*construction of Turan nodes & weights                         *)
aTuranNodesWeights[Order_?IntegerQ/;Order>0,{Name_?polyQ,polyOps___},s_Integer/;s>0,Ops___Rule]:=
    Module[{alfa,beta,nodes,w,i},
    nodes=aTuranNodes[Order,{Name,polyOps},s,Ops,Sequence@@Options[aTuranNodesWeights]];
    w=Name["ttr",polyOps];
    {alfa,beta}=Transpose[w/@Range[0,Floor[(2(s+1)Order-1)/2]+1]];
    Return[{nodes,aSigmaWeights[Order,alfa,beta,Table[s,{i,1,Order}],nodes,Ops,
        Sequence@@Options[aTuranNodesWeights]]}];];
(*construction of Turan nodes & weights                         *)

(*construction with increasing precision                        *)
aaSigmaNodes[Order_?IntegerQ/;Order>0,{Name_?polyQ,polyOps___},sigma_?VectorQ,Tau_?VectorQ,
    Ops___Rule]:=Module[{jac,slob,eps,tau,i,j,k,ncap,n,w,pom1,deltaTau,forExtr,poly,norm,
    wrprec,prec,acc,iD,zero,iter},
    If[Order!=Length[sigma],Message[aSigmaNodesWeights::nelen];Abort[];];
    If[Or@@((#1<0)/@sigma),Message[aSigmaNodesWeights::neg];Abort[];];
        {wrprec,prec,acc,iD}={WorkingPrecision,Precision,
    Accuracy,OrthogonalPolynomials`IterationDepth}/.{Ops}/.Options[aSigmaNodes];    
    If[wrprec<prec,Message[aSigmaNodesWeights::prec];Abort[];];
        eps=10^-prec;zero=10^-acc;tau=SetPrecision[Tau,wrprec];
        Block[{$MinPrecision=wrprec},
        norm=N[Table[Name["nor",polyOps][i],{i,0,Order-1}],wrprec];
    ncap=Floor[(2(Plus@@(sigma+1))-1)/2]+1; 
    {n,w}=aGaussianNodesWeights[ncap,{Name,polyOps},WorkingPrecision->wrprec,
        Precision->prec,Ops];
        deltaTau=Table[1,{j,1,Order}];iter=0;
    (*Print["iD:=",iD];*)
        While[Or @@ (((If[#1[[2]]>zero,#1[[1]]>(eps #1[[2]]),False])&) /@ 
            Transpose[{Abs /@ deltaTau, Abs /@ tau}]),
            jac=Table[0,{j,1,Order},{k,1,Order}];slob=Table[0,{j,1,Order}];
            For[j=1,j<=ncap,j++,
                poly=Name[Order-1,polyOps,n[[j]],WorkingPrecision->wrprec, 
                        OrthogonalPolynomials`ReturnList->True]/Take[norm,Order];
                For[k=1,k<=Order,k++,
                    pom1=w[[j]] poly[[k]](Times @@ ((n[[j]]-tau)^(2 sigma+1)));
                    jac[[k]] +=(2 sigma+1)pom1/(n[[j]]-tau);slob[[k]] +=pom1;];];
            deltaTau=LinearSolve[jac,slob];tau=tau+deltaTau;iter++;
        (*Print["deltaTau:=",deltaTau];Print["slob:=",slob];Print["tau:=",tau];*)
            If[iter==iD,Message[aTuranNodes::iDexc];Abort[];];];
        tau=Sort[tau];(*Print["tau:=",tau];Print["iter:=",iter];*)
        Return[tau];];];
(*construction with increasing precision                        *)

(*construction of sigma nodes with depresing of s helper        *)
aSigmaNodes[Order_?IntegerQ/;Order>0,Alfa_?VectorQ,Beta_?VectorQ,sigma_?VectorQ,Tau1_?VectorQ,
    Ops___Rule]:=Module[{n=Order,tau,tau1,forExtr,i,j,deltaTau,prec,eps,iter,s,nG,wG,zero,l, 
    k,list,ncap,slob,jac,iD,sl,node,intPrec,intAcc},(*Print["hello1Sigma"];*)
        If[Order!=Length[sigma],Message[aSigmaNodesWeights::nelen];Abort[];];
        If[Or@@(((#1<0)&)/@sigma),Message[aSigmaNodesWeights::neg];Abort[];];
        s=Max[sigma];tau1=Tau1;forExtr={tau1};tau=tau1;
        If[Length[Alfa]<Floor[(2(s+1)n-1)/2]+1 || Length[Beta]<Floor[(2(s+1)n-1)/2]+1, 
            Message[aSigmaNodesWeights::bdinp];Abort[];];
        {wrprec,prec,iD,acc,list,intPrec,intAcc}={WorkingPrecision,Precision,
        OrthogonalPolynomials`IterationDepth,Accuracy,OrthogonalPolynomials`ReturnList,
        InternalPrecision,InternalAccuracy}/.{Ops}/.Options[aSigmaNodes];
        If[wrprec<prec,Message[aSigmaNodesWeights::prec];Abort[];];
        If[And@@(((#1==0)&)/@sigma),Return[aZero[Order,Alfa,Beta,WorkingPrecision->wrprec, 
                Precision->prec]];];
        slob=Table[0,{i,1,n}];jac=Table[Table[0,{i,1,n}],{j,1,n}]; 
        deltaTau=Table[1,{i,1,n}];ncap=Floor[(2(s+1)n-1)/2]+1;
        {nG,wG}=aGaussianNodesWeights[ncap,Alfa,Beta,WorkingPrecision->wrprec,Precision->prec,,Ops];
        Block[{$MinPrecision=wrprec},
        s=Table[s,{i,1,n}];
        For[i=n,i>=1,i--,(*Print["i:=",i];*)eps=10^-intPrec;zero=10^-intAcc;
            While[s[[i]]>sigma[[i]],iter=0;s[[i]] -=1; 
                    If[s[[i]]==sigma[[i]],eps=10^-prec;zero=10^-acc;];            
                While[Or@@(((If[#1[[2]]>zero,#1[[1]]>(eps #1[[2]]),False])&)/@ 
                        Transpose[{Abs/@deltaTau,Abs/@tau1}]),
                    (*Newton*)
                    jac *=0;slob *=0;
                        For[k=1,k<=ncap,k++, 
                        sl=wG[[k]]*(Times @@ ((nG[[k]]-tau1)^(2s+1)));
                        node=Table[nG[[k]]^(l-1),{l,1,n}];slob +=sl node;
                            For[l=1,l<=n,l++, 
                            jac[[l]] +=-(2s+1)sl/(nG[[k]]-tau1) node[[l]];];];
                    (*Newton end*)
                    deltaTau=LinearSolve[jac,slob];tau=tau1;tau1=tau1-deltaTau;
                    If[iter==iD,Print[forExtr];Message[aSigmaNodesWeights::iDexc];Abort[];];
            iter++;];
                tau=Sort[tau];tau1=Sort[tau1];
                If[list,AppendTo[forExtr,tau1];,forExtr={forExtr[[-1]],tau1};];
                (*Lagrange*)
                tau1=2forExtr[[-1]]-forExtr[[-2]];
                (*Lagrange*)
                deltaTau=tau1-tau;];];
        tau1=forExtr[[-1]];
        (*end sigma construction*)
        If[list,Return[{forExtr,tau1}];,Return[tau1]];];];
(*construction of sigma nodes with depresing of s                   *)

(*construction of sigma nodes                               *)
aSigmaNodes[Order_?IntegerQ/;Order>0,Alfa_?VectorQ,Beta_?VectorQ,sigma_?VectorQ,Ops___Rule]:=
    Module[{wrprec,prec,acc,n,nG,wG,tau,sigmaW,sigmaWW,delta,nodes,deltaTau,
          eps,zero,pom,slob,jac,iterW,i,j,iDW,list,startDelta},
    {alg}={OrthogonalPolynomials`AlgorithmSigma}/.{Ops}/.Options[aSigmaNodes];
    If[ToString[alg]==="Homotopy",
	{wrprec,prec,acc,iDW,list,startDelta}={WorkingPrecision,Precision,Accuracy,
        	OrthogonalPolynomials`IterationDepth,OrthogonalPolynomials`ReturnList,
		OrthogonalPolynomials`StartFraction}/.{Ops}/.Options[aSigmaNodes];

   	Block[{$MinPrecision=wrprec},
        	eps=10^(-prec);zero=10^(-acc);
        	{nG,wG}=aGaussianNodesWeights[Min[Length[Alfa],Length[Beta]],Alfa,Beta,Ops,
                	Sequence@@Options[aSigmaNodes]];
        	tau=aZero[Order,Alfa,Beta,Ops,Sequence@@Options[aSigmaNodes]];
        	sigmaW=Table[0,{i,1,Order}];nodes={{sigmaW,tau}};
		jac=Table[0,{i,1,Order},{j,1,Order}];slob=Table[0,{i,1,Order}];
        	While[sigmaW != sigma,
                	delta=startDelta;sigmaWW=sigmaW+delta sigma;
                	tau=nodes[[-1]][[2]];deltaTau=Table[Infinity,{i,1,Order}];
                	iterW=0;
                	While[Or @@ (((If[#1[[2]]>zero,#1[[1]]>(eps #1[[2]]),False])&)/@
                        	Transpose[{Abs /@ deltaTau, Abs /@ tau}]),
                        	For[i=1,i<=Order,++i,
                                	pom=Table[(Times@@(((Abs/@(nG[[k]]-tau))^(2 sigmaWW+2)))/
                                        	(nG[[k]]-tau[[i]])),{k,1,Length[nG]}];
                                	slob[[i]]=wG.pom;
                                	jac[[i]]=-(2 sigmaWW+2)Table[wG.(pom/
                                        	Table[(nG[[j]]-tau[[k]]),{j,1,Length[nG]}]),{k,1,Order}];
                                	jac[[i,i]]+=wG.(pom/Table[(nG[[j]]-tau[[i]]),{j,1,Length[nG]}]);
                        	];
                        	deltaTau=-LinearSolve[jac,slob];
                        	tau+=deltaTau;++iterW;
(*Print["iterW,sigmaWW,deltaTau:=",iterW,sigmaWW,deltaTau];*)
				If[iterW>iDW,
                                	delta=delta/2;sigmaWW=sigmaW+delta sigma;
                                	tau=nodes[[-1]][[2]];deltaTau=Table[Infinity,{i,1,Order}];
                                	iterW=0;
                        	];
                	];
                	sigmaW=sigmaWW;AppendTo[nodes,{sigmaW,tau}];
        	];
   	];
   	If[list,Return[nodes];,Return[nodes[[-1]][[2]]];];
   ];
   If[ToString[alg]==="IncreaseS",
	tau=aTuranNodes[Order,Alfa,Beta,Max[sigma],Ops,Sequence@@Options[aSigmaNodes]];
	Return[aSigmaNodes[Order,Alfa,Beta,sigma,tau,Ops,Sequence@@Options[aSigmaNodes]]];
   ];
   If[ToString[alg]==="IncreaseDegree",
	Message[aSigmaNodes::usalg];
	Abort[];
   ];
   Message[aSigmaNodes::ualg];
   Abort[];
];
(*construction of sigma nodes                               *)

(*construction of sigma nodes                               *)
aSigmaNodes[Order_?IntegerQ/;Order>0,{Name_?polyQ,polyOps___},sigma_?VectorQ,Ops___Rule]:=
    Module[{tau,alfa,beta,s,alg,gP},s=Max[sigma];
    {alg}={OrthogonalPolynomials`AlgorithmSigma}/.{Ops}/.Options[aSigmaNodes];
    If[ToString[alg]==="IncreaseDegree" || ToString[alg]==="IncreaseS",
        tau=aTuranNodes[Order,{Name,polyOps},s,Ops,Sequence@@Options[aSigmaNodes]];
        {alfa,beta}=Transpose[(Name["ttr",polyOps]/@Range[0,Floor[(2(s+1)Order-1)/2]+1])];
        Return[aSigmaNodes[Order,alfa,beta,sigma,tau,Ops]];
    ];
    If[ToString[alg]==="Homotopy",
        {gP}={OrthogonalPolynomials`GaussianPoints}/.{Ops}/.Options[aSigmaNodes];
        {alfa,beta}=Transpose[Table[Name["ttr",polyOps][k],{k,0,gP}]];
        Return[aSigmaNodes[Order,alfa,beta,sigma,Ops]];
    ];
    Message[aSigmaNodes::ualg];
    Abort[];
];
(*construction of sigma nodes                               *)

aSigmaWeights[n_Integer/;n>0,alfa_?VectorQ,beta_?VectorQ,sigma_?VectorQ,zero_?VectorQ,Ops___]:= 
    Module[{wrprec,cond,s,w,ncap,indSigma,cmom,ak,u,z,pom,pom1,j,i,ret,no,k,alf},
        {wrprec}={WorkingPrecision}/.{Ops}/.Options[aSigmaWeights];
        Block[{$MinPrecision=wrprec},
    s=Max[sigma];ncap=Floor[(2(s+1)n-1)/2]+1;
        {no,w}=aGaussianNodesWeights[ncap,alfa,beta,Ops,Sequence @@ Options[aSigmaWeights]]; 
        ret=Table[Table[0,{i,1,2s+1}],{j,1,n}];
        For[indSigma=1,indSigma<=n,indSigma++,
        z=Delete[zero,indSigma];cmom=Table[0,{i,1,2sigma[[indSigma]]+1}]; 
        ak=Table[Table[0,{i,1,2sigma[[indSigma]]+1}],{j,1,2sigma[[indSigma]]+1}];u=cmom;
        For[j=1,j<=ncap,j++,
        pom=w[[j]](Times @@ (((no[[j]]-z)/(zero[[indSigma]]-z))^(2Delete[sigma,indSigma]+1)));
            pom1=no[[j]]-zero[[indSigma]];
        cmom +=Table[pom1^(k-1),{k,1,2sigma[[indSigma]]+1}]pom;];
            pom=1/(z-zero[[indSigma]]);pom1=1;
            For[j=1,j<=2sigma[[indSigma]],j++,
                    u[[j]] +=(2Delete[sigma,indSigma]+1).(pom1=pom1*pom;pom1);];
            ak[[1,1]]=1;
            For[k=1,k<=2sigma[[indSigma]],k++,ak[[k+1,k+1]]=ak[[k,k]];];
            For[j=1,j<=2sigma[[indSigma]],j++,pom=0;
                    For[l=1,l<=j,l++,pom=pom+u[[l]] ak[[l,j]];];
                For[k=1,k<=(2sigma[[indSigma]]+1)-j,k++,ak[[k,k+j]] = -pom/j;];];
                ret[[indSigma]]=Join[LinearSolve[ak,cmom],Table[0,{j,2sigma[[indSigma]]+1,2s}]];
            For[j=1,j<=(2sigma[[indSigma]]+1),j++,ret[[indSigma,j]]=ret[[indSigma,j]]/(j-1)!;];];];
        Return[Transpose[ret]];];

aSigmaWeights[n_Integer/;n>0,{Name_?polyQ,polyOps___},sigma_?VectorQ,zero_?VectorQ,Ops___Rule]:=
    Module[{alfa,beta},
    {alfa,beta}=Transpose[Name["ttr",polyOps]/@Range[0,Floor[(2(Max[sigma]+1)n-1)/2]+1]];
    Return[aSigmaWeights[n,alfa,beta,sigma,zero,Ops]];];

aSigmaNodesWeights[Order_Integer/;Order>0,alfa_?VectorQ,beta_?VectorQ,sigma_?VectorQ,Ops___]:=
     Module[{n},
    Return[{n=aSigmaNodes[Order,alfa,beta,Max[sigma],Ops,
        Sequence@@Options[aSigmaNodesWeights]],
        aSigmaWeights[Order,alfa,beta,sigma,n,Ops,Sequence@@Options[aSigmaNodesWeights]]}];];

aSigmaNodesWeights[Order_Integer/;Order>0,{Name_?polyQ,polyOps___},sigma_?VectorQ,Ops___]:=
    Module[{s,alfa,beta},
    s=Max[sigma];
    {alfa,beta}=Transpose[Name["ttr",polyOps]/@Range[0,Floor[(2(s+1)Order-1)/2]+1]];
    Return[{s=aSigmaNodes[Order,{Name,polyOps},sigma,Ops,Sequence@@Options[aSigmaNodesWeights]],
        aSigmaWeights[Order,alfa,beta,sigma,s,Ops,Sequence@@Options[aSigmaNodesWeights]]}];];       
        
aFejerNodesWeights[Order_?IntegerQ/;Order>0,Ops___]:= 
    Module[{n=Order,nodes=List[],weights=List[],k,m,c0,c1,c2, 
    sum,t,wrprec},
        {wrprec}={WorkingPrecision}/.List[Ops]/.Options[aFejerNodesWeights];
    Block[{$MinPrecision=wrprec},
        nodes=Table[N[Cos[(2k-1)Pi/(2n)],wrprec],{k,1,n/2}];nodes=Join[-nodes,Reverse[nodes]];
        If[OddQ[n],nodes=Insert[nodes,0,(n + 1)/2];];
        For[k=1,k<=(n+1)/2,k++,c1=N[1,wrprec];c0=2nodes[[k]]nodes[[k]]-1;t=2c0;sum=c0/3;
            For[m=2,m<=n/2,m++,c2=c1;c1=c0;c0=t c1-c2;sum += c0/(4 m m-1);];
            weights=Insert[weights,2(1-2sum)/n,{{k},{k}}];];
        If[OddQ[n],weights=Delete[weights,(n + 1)/2];];];
        Return[{nodes, weights}];];

(*Anti Gaussian					*)
aAntiGaussianNodesWeights[Order_?IntegerQ/;Order>0,alG_?VectorQ,beG_?VectorQ,ops___]:=
	Module[{al,be,alE,beE,no,we,wrprec},
		{wrprec}={WorkingPrecision}/.{ops}/.Options[aAntiGaussianNodesWeights];
		If[Order+2>Length[alG] || Order+2>Length[beG],
			Message[aAntiGaussianNodesWeights::error];
			Return[{{},{}}];
		];
		Block[{$MinPrecision=wrprec},
			alE=Reverse[Take[alG,Order]];
			beE=Join[{beG[[Order+2]]},Delete[Reverse[Take[beG,Order]],-1]];
			al=Join[Take[alG,Order+1],alE];
			be=Join[Take[beG,Order+1],beE];
			no=aZero[2 Order+1,al,be,ops,Sequence@@Options[aAntiGaussian]];
			we=aInterpolatoryWeights[{alG,beG},no,Table[0,{i,1,Length[no]}],
				ops,Sequence@@Options[aInterpolatoryRule]];
		];
		Return[{no,we[[1]]}];
	];
aAntiGaussianNodesWeights[Order_?IntegerQ/;Order>0,{aName_,polyOps___},ops___]:=
	Module[{al,be,k},
		{al,be}=Transpose[Table[aName["ttr",polyOps][k],{k,0,Order+2}]];
		Return[aAntiGaussianNodesWeights[Order,al,be,ops]];
	];
(*Anti Gaussian					*)

(*Operator aNodesWeights                    *)

aNodesWeights[Order_?IntegerQ/;Order>0,alfa_?VectorQ,beta_?VectorQ,type_,Ops___]:=
    Module[{},
    If[qrQ[type],
    Return[ToExpression[StringJoin[ToString[type],"NodesWeights"]][Order,alfa,beta,Ops,
        Sequence@@Options[aNodesWeights]]];,
    Message[aNodesWeights::untype];Abort[];];];

aNodesWeights[Order_?IntegerQ/;Order>0,{Name_?polyQ,polyOps___},type_,Ops___]:=
    Module[{a,b,k,alfa,beta,i,s,ops={Ops},extr=0,alg,nodes,weights},
    If[qrQ[type],
    {a,b}=Name["ttr",polyOps][0];alfa={a};beta={b};a=Name["ttr",polyOps][k];
    If[type==aGaussian,
        Return[aGaussianNodesWeights[Order,{Name,polyOps},Ops,
            Sequence@@Options[aNodesWeights]]];];
    If[type==aKronrod,
        {alg}={OrthogonalPolynomials`Algorithm}/.{Ops}/.Options[aKronrodNodesWeights];
        alfa=Join[alfa,Table[(a/.k->i)[[1]],{i,1,Floor[3 Order/2]+1}]];
        beta=Join[beta,Table[(a/.k->i)[[2]],{i,1,Ceiling[3 Order/2]+1}]];
        If[Or@@(beta<0) || MemberQ[beta,_Complex] || MemberQ[alfa,_Complex],
            If[alg==aDevideConquer,Message[aNodesWeights::negKDC];Abort[];];];];
    If[type==aRadau,
        alfa=Join[alfa,Table[(a/.k->i)[[1]],{i,1,Order}]];
        beta=Join[beta,Table[(a/.k->i)[[2]],{i,1,Order}]];];    
    If[type==aLobatto,
        alfa=Join[alfa,Table[(a/.k->i)[[1]],{i,1,Order}]];
        beta=Join[beta,Table[(a/.k->i)[[2]],{i,1,Order}]];];
    If[type==aTuran,
        s=ops[[1]];
        alfa=Join[alfa,Table[(a/.k->i)[[1]],{i,1,Floor[(2(s+1)Order-1)/2]+1}]];
        beta=Join[beta,Table[(a/.k->i)[[2]],{i,1,Floor[(2(s+1)Order-1)/2]+1}]];];
    If[type==aSigma,
        s=ops[[1]];s=Max[s];
        alfa=Join[alfa,Table[(a/.k->i)[[1]],{i,1,Floor[(2(s+1)Order-1)/2]+1}]];
        beta=Join[beta,Table[(a/.k->i)[[2]],{i,1,Floor[(2(s+1)Order-1)/2]+1}]];];
    If[type==aAntiGaussian,
        alfa=Join[alfa,Table[(a/.k->i)[[1]],{i,0,Order}]];
        beta=Join[beta,Table[(a/.k->i)[[2]],{i,0,Order}]];];
    Return[aNodesWeights[Order,alfa,beta,type,Ops]];,
    Message[aNodesWeights::untype];Abort[];];]; 

aNodesWeights[Order_?IntegerQ/;Order>0,{aFejer,left_:-1,right_:1},Ops___]:=
    Module[{n,w,pom,pom1},
    {n,w}=aFejerNodesWeights[Order,Ops,Sequence@@Options[aNodesWeights]];
    If[left==-Infinity && right==Infinity,pom=n^2;
        Return[{n/(1-pom),w*(pom+1)/(1-pom)^2}];];
    If[left==-Infinity,Return[{right-(1-n)/(1+n),2 w/(n+1)^2}];];
    If[right==Infinity,Return[{left+(1+n)/(1-n),2 w/(n-1)^2}];];
    pom1=right-left;pom=right+left;Return[{(pom1 n+pom)/2,pom1 w/2}];];

(*end operator aNodesWeights                        *)

(*Interpolatory Weights					*)
aInterpolatoryWeights[intNodes_?VectorQ, intWeights_?VectorQ, nodes_?VectorQ, sigma_?VectorQ, ops___Rule] := 
  Module[{wrprec, mat, slob, pom, we, pom1, j, i, start, s},
   {wrprec} = {WorkingPrecision} /. {ops} /. Options[aInterpolatoryWeights];
   Block[{$MinPrecision = wrprec},
    mat =
     Table[
      Flatten[Table[
        Table[(Times @@ (Table[(k - ell + 1), {ell, 1, 
               j}])) (nodes[[i]])^(k - j), {j, 0, 2 sigma[[i]]}], {i, 
         1, Length[nodes]}]],
      {k, 0, (Plus @@ (2 sigma + 1)) - 1}];
    slob = Table[intWeights.(intNodes^k), {k, 0, (Plus @@ (2 sigma + 1)) - 1}];
    pom = LinearSolve[mat, slob];
    s = Max[sigma];
    we = {};
    start = 1;
    For[j = 1, j <= Length[nodes], ++j,
     AppendTo[we, 
      Join[Take[pom, {start, start + 2 sigma[[j]]}], 
       Table[0, {i, 2 sigma[[j]] + 2, 2 s + 1}]]];
     start += 2 sigma[[j]] + 1;
     ];
    ];
   Return[Transpose[we]];
   ];
aInterpolatoryWeights[intNodes_?VectorQ, intWeights_?VectorQ, nodes_?VectorQ, s_?IntegerQ /; s > -1, ops___Rule] := 
  Module[{},
   Return[aInterpolatoryWeights[intNodes, intWeights, nodes, Table[s, {i, 1, Length[nodes]}], ops]];
   ];
aInterpolatoryWeights[{al_?VectorQ,be_?VectorQ},nodes_?VectorQ,sigma_?VectorQ,ops___Rule]:=
  Module[{no,we},
   {no,we}=aGaussianNodesWeights[Min[Length[al],Length[be]],al,be,ops,Sequence@@Options[aInterpolatoryWeights]];
   Return[aInterpolatoryWeights[no,we,nodes,sigma,ops]];
   ];
aInterpolatoryWeights[{al_?VectorQ,be_?VectorQ},nodes_?VectorQ,s_?IntegerQ/;s>-1,ops___Rule]:=
  Module[{},
   Return[aInterpolatoryRule[{al,be},nodes,Table[s,{i,1,Length[nodes]}],ops]];
   ];
aInterpolatoryWeights[Order_?IntegerQ /; Order > 0, {aName_, polyOps___}, nodes_?VectorQ, sigma_?VectorQ, ops___Rule] := 
  Module[{no, we},
   {no, we} = aGaussianNodesWeights[Order, {aName, polyOps},ops, Sequence @@ Options[aInterpolatoryWeights]];
   Return[aInterpolatoryWeights[no, we, nodes, sigma, ops]];
   ];
aInterpolatoryWeights[Order_?IntegerQ /; Order > 0, {aName_?polyQ, polyOps___}, nodes_?VectorQ, s_?IntegerQ /; s > -1, ops___Rule] := 
  Module[{},
   Return[aInterpolatoryWeights[Order, {aName, polyOps}, nodes, Table[s, {i, 1, Length[nodes]}], ops]];
   ];
aInterpolatoryWeights[{aName_?polyQ, polyOps___}, nodes_?VectorQ, sigma_?VectorQ, ops___Rule] := 
  Module[{},
   Return[aInterpolatoryWeights[Ceiling[((2 (Plus @@ sigma) + 1) Length[nodes] - 1)/2], {aName, polyOps}, nodes, sigma, ops]];
   ];
aInterpolatoryWeights[{aName_?polyQ, polyOps___}, nodes_?VectorQ, s_?IntegerQ /; s > -1, ops___Rule] := 
  Module[{},
   Return[aInterpolatoryWeights[{aName, polyOps}, nodes, Table[s, {i, 1, Length[nodes]}], ops]];
   ];
(*end interpolatory weights				*)

(*check rules						*)
aCheckSigma[no_?VectorQ,we_?MatrixQ,sigma_?VectorQ,delta_?IntegerQ,{aName_?polyQ,polyOps___},ops___Rule]:=
  Module[{ADP,deg,mom,n,s,wrprec,pom},
   {wrprec}={WorkingPrecision}/.{ops}/.Options[aCheckSigma];
   n=Length[no];
   ADP=Plus@@(2 sigma+1)+n-1;
   s=Max[sigma];
   Block[{$MinPrecision=wrprec},
    mom=Table[aName["mom",polyOps][k],{k,0,ADP+delta}];
    pom=Table[Plus@@Table[(we[[i]]).((Times@@Table[(k-j+1),{j,1,i-1}])no^(k-i+1)),{i,1,2s+1}],
		{k,0,ADP+delta}];
    Return[pom-mom];
   ];
  ];
aCheckTuran[no_?VectorQ,we_?MatrixQ,s_?IntegerQ/;s>-1,delta_?IntegerQ,{aName_?polyQ,polyOps___},ops___Rule]:=
  Module[{},
   Return[aCheckSigma[no,we,Table[s,{i,1,Length[no]}],delta,{aName,polyOps},
		ops,Sequence@@Options[aCheckTuran]]];
  ];
(*end check rules					*)

Protect[aQR];
Protect[aPasquini];
Protect[aZero];
Protect[aGaussianNodesWeights];
Protect[aGaussianWeights];
Protect[aLobattoNodesWeights];
Protect[aRadauNodesWeights];
Protect[aLaurie];
Protect[aDevideConquer];
Protect[aKronrodNodesWeights];
Protect[aTuranNodes];
Protect[aTuranNodesWeights];
Protect[aSigmaNodes];
Protect[aSigmaWeights];
Protect[aSigmaNodesWeights];
Protect[aAntiGaussianNodesWeights];
Protect[aNodesWeights];
Protect[aFejerNodesWeights];
Protect[aInterpolatoryWeights];
Protect[aCheckSigma];
Protect[aCheckTuran];

End[]

EndPackage[]
