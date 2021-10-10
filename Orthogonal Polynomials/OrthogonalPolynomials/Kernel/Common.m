

BeginPackage["OrthogonalPolynomials`Kernel`Common`"]

Odd::usage="Odd[k] returns True iff k is odd number, False otherwise.";
polyQ::usage="polyQ[name] returns True iff name is name of supported polynomial class.";
qrQ::usage="qrQ[quadType] returns True iff quadType is supported quadrature rule.";
aSupportedPolynomials::usage="aSupportedPolynomials[type] returns names of polynomial classes \
    belonging to supper class given by type. Variable type is string with values Continuous, \
    Discrete or Complex.";
aSupportedPolynomials::cont:="Supported polynomials with the real support are:
    aJacobi,aLegendre,aGegenbauer,aGegenbauerG,aChebyshevI,aChebyshevII,aLaguerre,
    aLaguerreG,aHermite,aHermiteG,aStieltjesWigert,aAssociatedLegendre,
    aLindelof,aLogistic,aAbel,aCarlitz";
aSupportedPolynomials::disc="Supported polynomilas with real and discrete support are:
    aCharlier";
aSupportedPolynomials::cmpl="Supported polynomials with the complex support are:
    aBesselG,aBessel,aGautschiMilovanovic.";
Attributes[Odd]={ReadProtected};
Attributes[polyQ]={ReadProtected};
Attributes[qrQ]={ReadProtected};
Attributes[aSupportedPolynomial]={ReadProtected};


Begin["`Private`"]

Odd[k_?IntegerQ]:=Module[{},
    Return[If[OddQ[k],True,False]];
    ];

polyQ[Name_]:=Module[{},
    Return[MemberQ[{"aJacobi","aLegendre","aGegenbauer","aGegenbauerG","aChebyshevI",
            "aChebyshevII","aLaguerre","aLaguerreG","aHermite","aHermiteG","aCharlier",
        "aStieltjesWigert","aMeixnerI","aMeixnerII","aBesselG","aAssociatedLegendre",
        "aPollaczek","aPollaczekII","aPollaczekIII","aLommel","aTricomiCarlitz",
        "aAbel","aLindelof","aCarlitz","aLogistic","aBessel","aWilson","aDualHan",
        "aHan","aGautschiMilovanovic","aEProblem","aPQ"},
            SymbolName[Name]]];
    ];

qrQ[Name_]:=Module[{},
    Return[MemberQ[{"aGaussian","aKronrod","aRadau","aLobatto","aTuran",
        "aSigma"},SymbolName[Name]]];
    ];

cpolyQ[Name_]:=Module[{},
    Return[MemberQ[{"aGautschiMilovanovic","aBessel","aBesselG"},SymbolName[Name]]];
    ];

aSupportedPolynomials[type_String]:=Module[{},
    If[type=="Continuous",
        Message[aSupportedPolynomials::cont];Return[];];
    If[type=="Discrete",
        Message[aSupportedPolynomials::disc];Return[];];
    If[type=="Complex",
        Message[aSupportedPolynomials::cmpl];Return[];];
    ];
    

End[]

EndPackage[]
