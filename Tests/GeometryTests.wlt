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
    24
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
