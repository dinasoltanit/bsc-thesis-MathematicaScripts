

BeginPackage["OrthogonalPolynomials`"]
Algorithm;Symbolic;ReturnList;IterationDepth;aIndex;Start;intVariable;Homotopy;IncreaseDegree;
IncreaseS;GaussianPoints;StartFraction;AlgorithmSigma;AlgorithmKronrod;
Protect[Algorithm];
Protect[Symbolic];
Protect[ReturnList];
Protect[IterationDepth];
Protect[aIndex];
Protect[Start];
Protect[intVariable];
Protect[Homotopy];
Protect[IncreaseDegree];
Protect[IncreaseS];
Protect[GaussianPoints];
Protect[StartFraction];
Protect[AlgorithmSigma];
Protect[AlgorithmKronrod];
OrthogonalPolynomials::usage="Please cite every usage of this package by the following two papers: \

1. A.S. Cvetkovic, G.V. Milovanovic, The Mathematica Package ''OrthogonalPolynomials'', Facta Univ. Ser. Math. Inform. 19 (2004), 17-36.\

2. G.V. Milovanovic, A.S. Cvetkovic, Special classes of orthogonal polynomials and corresponding quadratures of Gaussian type, Math. Balkanica 26 (2012), 169-184.";

Message[OrthogonalPolynomials::usage]; 
EndPackage[]




DeclarePackage["OrthogonalPolynomials`Kernel`Common`",{"Odd","polyQ","qrQ","cpolyQ",
    "aSupportedPolynomials"}]




(*Continuous polys*)
DeclarePackage["OrthogonalPolynomials`Continuous`Jacobi`",{"aJacobi","aJacobiInt"}]
DeclarePackage["OrthogonalPolynomials`Continuous`Gegenbauer`",{"aGegenbauer","aGegenbauerInt"}]
DeclarePackage["OrthogonalPolynomials`Continuous`GegenbauerG`",{"aGegenbauerG","aGegenbauerGInt"}]
DeclarePackage["OrthogonalPolynomials`Continuous`Legendre`",{"aLegendre","aLegendreInt"}]
DeclarePackage["OrthogonalPolynomials`Continuous`ChebyshevI`",{"aChebyshevI","aChebyshevIInt"}]
DeclarePackage["OrthogonalPolynomials`Continuous`ChebyshevII`",{"aChebyshevII","aChebyshevIIInt"}]
DeclarePackage["OrthogonalPolynomials`Continuous`Laguerre`",{"aLaguerre","aLaguerreInt"}]
DeclarePackage["OrthogonalPolynomials`Continuous`LaguerreG`",{"aLaguerreG","aLaguerreGInt"}]
DeclarePackage["OrthogonalPolynomials`Continuous`Hermite`",{"aHermite","aHermiteInt"}]
DeclarePackage["OrthogonalPolynomials`Continuous`HermiteG`",{"aHermiteG","aHermiteGInt"}]
DeclarePackage["OrthogonalPolynomials`Continuous`Abel`",{"aAbel","aAbelInt"}]
DeclarePackage["OrthogonalPolynomials`Continuous`Lindelof`",{"aLindelof","aLindelofInt"}]
DeclarePackage["OrthogonalPolynomials`Continuous`Carlitz`",{"aCarlitz","aCarlitzInt"}]
DeclarePackage["OrthogonalPolynomials`Continuous`Logistic`",{"aLogistic","aLogisticInt"}]
(*DeclarePackage["OrthogonalPolynomials`Continuous`Pollaczek`",{"aPollaczek","aPollaczekInt"}]*)
(*DeclarePackage["OrthogonalPolynomials`Continuous`PollaczekII`",{"aPollaczekII","aPollaczekIIInt"}]*)
(*DeclarePackage["OrthogonalPolynomials`Continuous`PollaczekIII`",{"aPollaczekIII","aPollaczekIIIInt"}]*)
DeclarePackage["OrthogonalPolynomials`Continuous`AssociatedLegendre`",{"aAssociatedLegendre",
    "aAssociatedLegendreInt"}]
(*DeclarePackage["OrthogonalPolynomials`Continuous`Wilson`",{"aWilson","aWilsonInt"}]*)
(*DeclarePackage["OrthogonalPolynomials`Continuous`DualHan`",{"aDualHan","aDualHanInt"}]*)
(*DeclarePackage["OrthogonalPolynomials`Continuous`Han`",{"aHan","aHanInt"}]*)
(*DeclarePackage["OrthogonalPolynomials`Continuous`MeixnerII`",{"aMeixnerII","aMeixnerIIBeta0",
    "aMeixnerIIInt"}]*)
DeclarePackage["OrthogonalPolynomials`Continuous`StieltjesWigert`",{"aStieltjesWigert",
    "aStieltjesWigertInt"}]


(*Discrete polys*)
DeclarePackage["OrthogonalPolynomials`Discrete`Charlier`",{"aCharlier"}]
DeclarePackage["OrthogonalPolynomials`Discrete`ChebyshevM`",{"aChebyshevM"}]
(*DeclarePackage["OrthogonalPolynomials`Discrete`TricomiCarlitz`",{"aTricomiCarlitz"}]*)
(*DeclarePackage["OrthogonalPolynomials`Discrete`Lommel`",{"aLommel"}]*)
(*DeclarePackage["OrthogonalPolynomials`Discrete`MeixnerI`",{"aMeixnerI"}]*)
(*DeclarePackage["OrthogonalPolynomials`Discrete`EProblem`",{"aEProblem"}]*)
(*DeclarePackage["OrthogonalPolynomials`Discrete`PQ`",{"aPQ"}]*)


(*Complex polys*)
DeclarePackage["OrthogonalPolynomials`Complex`BesselG`",{"aBesselG"}]
DeclarePackage["OrthogonalPolynomials`Complex`Bessel`",{"aBessel"}]
DeclarePackage["OrthogonalPolynomials`Complex`GautschiMilovanovic`",{"aGautschiMilovanovic"}]


DeclarePackage["OrthogonalPolynomials`NodesWeights`",{
    "aGaussianNodesWeights","aLobattoNodesWights","aRadauNodesWeights",
    "aKronrodNodesWeights","aTuranNodesWeights","aAntiGaussianNodesWeights",
	"aSigmaNodesWeights","aFejerNodesWeights","aInterpolatoryWeights",
    "aNodesWeights","aGaussianWeights","ReturnLast",
    "aTuranNodes","aPasquini","aQR","aZero","aSigmaNodes","aSigmaWeights",
	"aCheckSigma","aCheckTuran",
    "aLaurie","aDevideConquer",
    "aGaussian","aLobatto","aRadau","aKronrod","aSigma","aTuran","aFejer","aAntiGaussian",  
    "InternalPrecision","InternalAccuracy","ModifiedAlgorithm"}]

DeclarePackage["OrthogonalPolynomials`TTRCoefficients`",{"aThreeTermRecurrence",
    "aChebyshevAlgorithmModified","aChebyshevAlgorithm","aStieltjesAlgorithm",
    "aLanczosAlgorithm","aStieltjes","aLanczos"}]

DeclarePackage["OrthogonalPolynomials`Christoffel`",{"aChristoffelAlgorithm",
    "aLinear","aQuadratic","aQuadraticSymmetric","aLinearDivisor","aQuadraticDivisor",
    "aQuadraticDivisorSymmetric","aQuadraticReal","aStartingPoint", 
    "aFunctionIIKind","aGaussianKernel","aModify"}]

DeclarePackage["OrthogonalPolynomials`Operators`",{"aMoments","aMakePolynomial",
    "aWeight","aSetInterval","aGetInterval","aNorm","aKernel",
    "aNumerator","aDistribution","aSupport","aChangeInterval"}]

DeclarePackage["OrthogonalPolynomials`TrigonometricSemiInteger`",{"aCreateTTRCos","aCreateTTRSinPeriodicPi",
	"aCreateTrigPolynomial","aCreateCosPolynomial","aCreateSinPolynomial"}]


Null
