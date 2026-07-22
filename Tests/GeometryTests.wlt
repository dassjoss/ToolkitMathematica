(* ::Package:: *)

(* Test Suite para el Módulo Geometry.wl *)

(* NOTA: El test "Geometry-Hodge-Scalar" se omite intencionalmente.
   La logica actual de HodgeDual retorna Return[expr] sin multiplicar
   por eps cuando p==0 (caso escalar, sin indices del manifold).
   Se reactivara cuando se corrija esa rama en Geometry.wl. *)

VerificationTest[
    (* 1. Setup Robusto con Métrica *)Module[{},
        Quiet[
            If[NameQ["M4"],
                UndefManifold[M4]
            ];
            If[NameQ["g"],
                UndefMetric[g]
            ];
            If[NameQ["eps"],
                UndefTensor[eps]
            ];
        ];
        DefManifold[M4, 4, {a, b, c, d, e, f, mu, nu, rho, sigma}];
(* Definimos una métrica para que las contracciones del Dual funcionen.
    
    
    Nota: se removio SymbolPrintAs -> "g" porque no es una opcion valida
    
    
    de DefMetric/DefCovD en xAct (causaba OptionValue::nodef). *)
        DefMetric[-1, g[-a, -b], CD, {",", "∇"}];
        And[ManifoldQ[M4], IntegerQ[DimOfManifold[M4]]]
    ]
    ,
    True
    ,
    TestID -> "Geometry-Setup-Manifold"
]

VerificationTest[
    (* 2. Definición de Levi-Civita como Densidad *)Module[{res},
        Quiet[
            If[NameQ["eps"],
                UndefTensor[eps]
            ]
        ];
(* Definimos el Levi-Civita. res debe ser la asociación del registro o el símbolo 
    
    
    *)
        res = DefineLeviCivita[M4, eps, "Density"];
        (* Verificamos si tiene el peso correcto en xAct *)
        WeightOfTensor[eps] === 1
    ]
    ,
    True
    ,
    TestID -> "Geometry-DefDensity-Weight"
]

VerificationTest[
    (* 3. Formateo Visual de Densidad (OverTilde) *)Module[
        {boxes}
        ,
        (* Generamos las cajas visuales del tensor *)
        boxes = ToBoxes[eps[-a, -b, -c, -d]];
(* Verificamos que contenga un OverTildeBox (Regla de Oro: Boxes, no Strings) 
    
    
    *)
        !FreeQ[boxes, OverscriptBox]
    ]
    ,
    True
    ,
    TestID -> "Geometry-Visual-Tilde"
]

VerificationTest[
    (* 4. Contracción de Epsilons (EpsContract) *)Module[
        {expr, res}
        ,
        (* Identidad: eps_{abcd} eps^{abcd} = 4! = 24 *)
(* Nota: Forzamos ToCanonical para que xAct procese la GDelta resultante 
    
    
    *)
        expr = eps[-a, -b, -c, -d] eps[a, b, c, d];
        res = ToCanonical[ContractMetric[EpsContract[expr]]];
        res
    ]
    ,
    -24
    ,
    TestID -> "Geometry-EpsContract-Full"
]

VerificationTest[
    (* 6. Dual de Hodge de un Vector *)Module[{dual, VVec},
        Quiet[
            If[NameQ["VVec"],
                UndefTensor[VVec]
            ]
        ];
        xAct`xTensor`DefTensor[VVec[-a], M4];
        dual = System`HodgeDual[VVec[-a], M4];
        !FreeQ[dual, eps[-mu, -nu, -rho, -a]] && !FreeQ[dual, a]
    ]
    ,
    True
    ,
    TestID -> "Geometry-Hodge-Vector-Indices"
]

VerificationTest[
    (* 7. Consistencia del Registro *)(* Verificamos que el manifold M4 esté mapeado al símbolo eps en el registry global 
        
        
        *)$LeviCivitaRegistry[M4] === eps, True, TestID -> "Geometry-Registry-Check"
    
]

VerificationTest[
    (* 8. Dual de Hodge de un Escalar (p=0) *)
    Module[{phi, dual},
        Quiet[
            If[NameQ["phi"], UndefTensor[phi]];
        ];
        xAct`xTensor`DefTensor[phi[], M4];
        dual = System`HodgeDual[phi[], M4];
        (* En 4D: *phi = phi * eps_{abcd} *)
        !FreeQ[dual, eps] && !FreeQ[dual, phi[]]
    ],
    True,
    TestID -> "Geometry-Hodge-Scalar"
]

VerificationTest[
    (* 9. Dual de Hodge de una 2-Forma: debe tener 2 índices libres *)
    Module[{F, dual},
        Quiet[
            If[NameQ["F"], UndefTensor[F]];
        ];
        xAct`xTensor`DefTensor[F[-a, -b], M4, Antisymmetric[{1, 2}]];
        dual = System`HodgeDual[F[-a, -b], M4];
        !FreeQ[dual, mu] && !FreeQ[dual, nu] && FreeQ[dual, rho]
    ],
    True,
    TestID -> "Geometry-Hodge-2Form-Indices"
]

VerificationTest[
    (* 10. Dual de Hodge de una 3-Forma: debe tener 1 índice libre *)
    Module[{A, dual},
        Quiet[
            If[NameQ["A"], UndefTensor[A]];
        ];
        xAct`xTensor`DefTensor[A[-a, -b, -c], M4, Antisymmetric[{1, 2, 3}]];
        dual = System`HodgeDual[A[-a, -b, -c], M4];
        !FreeQ[dual, mu] && FreeQ[dual, nu]
    ],
    True,
    TestID -> "Geometry-Hodge-3Form-Indices"
]

VerificationTest[
    (* 11. Identidad del Doble Dual para Vector (p=1, n=4, Lorentziano -> **v = +v) *)
    Module[{VVec, dual2},
        Quiet[
            If[NameQ["VVec"], UndefTensor[VVec]];
        ];
        xAct`xTensor`DefTensor[VVec[-a], M4];
        dual2 = System`HodgeDual[System`HodgeDual[VVec[-a], M4], M4];
        MatchQ[ToCanonical[ContractMetric[dual2]], VVec[_]]
    ],
    True,
    TestID -> "Geometry-Hodge-DoubleDual-Vector"
]

VerificationTest[
    (* 12. Identidad del Doble Dual para 2-Forma (p=2, n=4, Lorentziano -> **F = -F) *)
    Module[{F, dual2},
        Quiet[
            If[NameQ["F"], UndefTensor[F]];
        ];
        xAct`xTensor`DefTensor[F[-a, -b], M4, Antisymmetric[{1, 2}]];
        dual2 = System`HodgeDual[System`HodgeDual[F[-a, -b], M4], M4];
        MatchQ[ToCanonical[ContractMetric[dual2]], -F[_, _]]
    ],
    True,
    TestID -> "Geometry-Hodge-DoubleDual-2Form"
]

VerificationTest[
    (* 13. Contracción Parcial de Epsilons: debe producir GDelta *)
    Module[{expr, res},
        expr = eps[-a, -b, -c, -d] eps[a, b, -e, -f];
        res = TensorToolkit`EpsContract[eps[-a, -b, -c, -d] eps[a, b, -e, -f]];
        res = ToCanonical[res];
        !FreeQ[res, g]
    ],
    True,
    TestID -> "Geometry-EpsContract-Partial"
]

VerificationTest[
    (* 14. EpsContract NO debe contraer epsilons de distintos manifolds *)
    Module[{M3, eps3, expr, res},
        Quiet[
            If[NameQ["M3"], UndefManifold[M3]];
            If[NameQ["eps3"], UndefTensor[eps3]];
        ];
        DefManifold[M3, 3, {i, j, k, l, m}];
        DefineLeviCivita[M3, eps3, "Density"];
        expr = eps[-a, -b, -c, -d] eps3[-i, -j, -k];
        res = EpsContract[expr];
        res === expr
    ],
    True,
    TestID -> "Geometry-EpsContract-DifferentManifolds"
]

VerificationTest[
    (* 15. Registro Global con Múltiples Manifolds simultáneos *)
    Module[{M3, eps3},
        Quiet[
            If[NameQ["M3"], UndefManifold[M3]];
            If[NameQ["eps3"], UndefTensor[eps3]];
        ];
        DefManifold[M3, 3, {u, v, w, x, y}];
        DefineLeviCivita[M3, eps3, "Density"];
        $LeviCivitaRegistry[M4] === eps && $LeviCivitaRegistry[M3] === eps3
    ],
    True,
    TestID -> "Geometry-Registry-MultipleManifolds"
]

VerificationTest[
    (* 16. Formato Visual de tipo "Tensor": NO debe tener tilde (OverscriptBox) *)
    Module[{epsTensor, boxes},
        Quiet[
            If[NameQ["epsTensor"], UndefTensor[epsTensor]];
        ];
        DefineLeviCivita[M4, epsTensor, "Tensor"];
        boxes = ToBoxes[epsTensor[-a, -b, -c, -d]];
        FreeQ[boxes, OverscriptBox]
    ],
    True,
    TestID -> "Geometry-Visual-Tensor-NoTilde"
]

VerificationTest[
    (* 17. Peso de Levi-Civita tipo "Tensor" debe ser 0 (no densidad) *)
    Module[{epsTensor2},
        Quiet[
            If[NameQ["epsTensor2"], UndefTensor[epsTensor2]];
        ];
        DefineLeviCivita[M4, epsTensor2, "Tensor"];
        xAct`xTensor`WeightOfTensor[epsTensor2] === 0
    ],
    True,
    TestID -> "Geometry-DefTensor-Weight"
]

VerificationTest[
    (* 18. HodgeDual sin métrica definida debe retornar expr sin modificar *)
    Module[{MTest, epsTest, vec, res},
        Quiet[
            If[NameQ["MTest"], UndefManifold[MTest]];
            If[NameQ["epsTest"], UndefTensor[epsTest]];
            If[NameQ["vec"], UndefTensor[vec]];
        ];
        DefManifold[MTest, 4, {p, q, r, s, t}];
        DefineLeviCivita[MTest, epsTest, "Density"];
        xAct`xTensor`DefTensor[vec[-p], MTest];
        (* MTest no tiene DefMetric, por tanto no hay métrica *)
        res = Quiet[System`HodgeDual[vec[-p], MTest]];
        res === vec[-p]
    ],
    True,
    TestID -> "Geometry-Hodge-NoMetric"
]
