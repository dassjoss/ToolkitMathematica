BeginTestSection["Core Logic Suite"]

(* 1. Test de Registro *)

VerificationTest[
    TensorToolkit`$IndexRegistry = <||>;
    TensorToolkit`DefineTheoryIndices[muTest, "μ", "M4", {0, 3}];
    TensorToolkit`$IndexRegistry["muTest", "Manifold"]
    ,
    "M4"
    ,
    TestID -> "Core-Registry-Access"
]

(* 2. Test de Deducción *)

VerificationTest[
    Block[{getIdxSym},
        getIdxSym[Times[-1, s_Symbol]] := s;
        getIdxSym[s_Symbol] := s;
        getIdxSym[_] := Null;
        Map[getIdxSym, {alpha, -alpha}]
    ]
    ,
    {alpha, alpha}
    ,
    TestID -> "Core-Index-Extraction-Logic"
]

(* 3. Test de Integración: DefTensorF *)

VerificationTest[
(* Usamos Quiet para que los mensajes informativos de xAct no rompan el test 
    
    
    
    
    
    
    
    
    
    
    
    *)Quiet[
        If[NameQ["MTest"],
            xAct`xTensor`UndefManifold[MTest]
        ];
        xAct`xTensor`DefManifold[MTest, 4, {aT, bT, cT}];
        TensorToolkit`DefTensorF[TTensor[aT, bT], MTest];
    ];
    (* Verificamos si TTensor tiene reglas de formato asociadas *)
    (* Tu Debug mostró que efectivamente se crean en FormatValues *)
    Length[FormatValues[TTensor]] > 0
    ,
    True
    ,
    TestID -> "Core-DefTensorF-AutoFormatting"
]

EndTestSection[]
