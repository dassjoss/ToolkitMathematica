GreekIndex::usage = "GreekIndex[idx] traduce un nombre de indice (string o simbolo) a su representacion griega/latina Unicode.";

SetDisplayName::usage = "SetDisplayName[simbolo, \"etiqueta\"] define el alias visual de un tensor/constante (cosmetico, no afecta el calculo).";

DisplayNameOf::usage = "DisplayNameOf[simbolo] devuelve la etiqueta visual asignada, o el nombre real si no se asigno ninguna.";

IndexLabel::usage = "IndexLabel[idx] generaliza GreekIndex para aceptar tambien pares {n,base} (componentes numericas de xCoba) y simbolos con signo.";

IsDownIndex::usage = "IsDownIndex[idx] detecta si un indice (abstracto o componentizado) es covariante (abajo).";

SetTensorFormatting::usage = "SetTensorFormatting[tensor] define Format[tensor[inds___]] con sub/superindices visuales automaticos.";

CleanVisual::usage = "CleanVisual[expr] limpia la expresion de envoltorios escalares internos (como xAct`xTensor`Scalar).";

CheckEinsteinNotation::usage = "CheckEinsteinNotation[expr] verifica que no haya indices repetidos mas de 2 veces en productos.";

Begin["`Private`"]

GreekIndex[idx_] :=
  Module[{name = ToString[idx]},
    Lookup[<|"mu" -> μ, "nu" -> ν, "rho" -> ρ, "sigma" -> σ, "si" -> 
      \[Sigma], "tau" -> \[Tau], "alpha" -> \[Alpha], "beta" -> \[Beta], "gamma"
       -> \[Gamma], "delta" -> \[Delta], "lambda" -> \[Lambda], "la" -> \[Lambda],
       "kappa" -> \[Kappa], "ka" -> \[Kappa], "epsilon" -> \[CurlyEpsilon],
       "phi" -> \[Phi], "psi" -> \[Psi], "chi" -> \[Chi], "eta" -> \[Eta], 
      "zeta" -> \[Zeta], "xi" -> \[Xi], "II" -> "I", "JJ" -> "J", "KK" -> "K",
       "LL" -> "L"|>, name, name]
  ];

$TensorDisplayName = <||>;

SetDisplayName[sym_Symbol, label_String] :=
  $TensorDisplayName[sym] = label;

DisplayNameOf[sym_Symbol] :=
  Module[{label},
    label = Lookup[$TensorDisplayName, sym, ToString[sym]];
    If[StringQ[label],
      label = Lookup[$GreekStringMap, label, label]
    ];
    If[StringQ[label] && StringMatchQ[label, "\\[*]"],
      label = ToExpression[label]
    ];
    label
  ];

$GreekStringMap = <|"\[Mu]" -> \[Mu], "\[Nu]" -> \[Nu], "\[Rho]" -> \[Rho],
   "\[Sigma]" -> \[Sigma], "\[Tau]" -> \[Tau], "\[Alpha]" -> \[Alpha], 
  "β" -> β, "γ" -> γ, "δ" -> δ, "\[Lambda]" -> \[Lambda], "\[Kappa]" ->
   \[Kappa], "\[CurlyEpsilon]" -> \[CurlyEpsilon], "\[Phi]" -> \[Phi], 
  "\[Psi]" -> \[Psi], "\[Chi]" -> \[Chi], "\[Eta]" -> \[Eta], "\[Zeta]"
   -> \[Zeta], "\[Xi]" -> \[Xi], "\[CapitalGamma]" -> \[CapitalGamma], 
  "ω" -> ω, "ϵ" -> ϵ|>;

IndexLabel[idx_Symbol] :=
  Module[{entry, mfd, vis},
    entry = Lookup[TensorToolkit`$IndexRegistry, SymbolName[idx], <||>
      ];
    mfd = Lookup[entry, "Manifold", Null];
    vis = Lookup[entry, "Visual", Null];
    If[vis === Null,
      vis = GreekIndex[idx]
    ];
    If[StringQ[vis],
      vis = Lookup[$GreekStringMap, vis, vis]
    ];
    If[StringQ[vis] && StringMatchQ[vis, "\\[*]"],
      vis = ToExpression[vis]
    ];
    If[Head[vis] === Symbol,
      vis = SymbolName[vis]
    ];
    If[vis === "I",
      vis = "\\[CapitalIota]"
    ];
    If[ToString[mfd] === "Internal",
      StyleBox[vis, Bold, FontSlant -> "Plain"]
      ,
      vis
    ]
  ];

IndexLabel[{n_, _}] :=
  ToString[n];

IndexLabel[Times[-1, s_Symbol]] :=
  IndexLabel[s];

IsDownIndex[Times[-1, _]] :=
  True;

IsDownIndex[{_, Times[-1, _]}] :=
  True;

IsDownIndex[_] :=
  False;

SetTensorFormatting[t_Symbol] :=
  With[{tt = t},
    Format[tt[inds___]] :=
      Module[{label, nInds, blocks, k, idx, isDown},
        nInds = Length[{inds}];
        label = ToString[DisplayNameOf[tt]];
        If[nInds == 0,
          Return[RawBoxes[label]]
        ];
        blocks =
          Table[
            idx = {inds}[[k]];
            isDown = IsDownIndex[idx];
            SubsuperscriptBox[
              If[k == 1,
                label
                ,
                "\\[InvisibleSpace]"
              ]
              ,
              If[isDown,
                IndexLabel[idx]
                ,
                " "
              ]
              ,
              If[!isDown,
                IndexLabel[idx]
                ,
                " "
              ]
            ]
            ,
            {k, 1, nInds}
          ];
        RawBoxes[TagBox[RowBox[blocks], "Tensor"]]
      ];
  ];

SetDisplayName[xAct`xTensor`GDelta, "\\[Delta]"];

TensorToolkit`SetTensorFormatting[xAct`xTensor`GDelta];

CheckEinsteinNotation[expr_] :=
  Module[{checkTerm, badIndices = {}},
    checkTerm[term_] :=
      Module[{inds, counts, repeats},
        inds = Cases[term, idx_Symbol, Infinity];
        inds = Select[inds, !MatchQ[#, A | B | TensorC | X | Y | TensorG
          ]&];
        counts = Tally[inds];
        repeats = Select[counts, #[[2]] >= 3&];
        If[Length[repeats] > 0,
          badIndices = Join[badIndices, repeats[[All, 1]]];
        ];
      ];
    If[Head[expr] === Plus,
      checkTerm /@ List @@ expr
      ,
      checkTerm[expr]
    ];
    If[Length[badIndices] > 0,
      Print[Style["[!] Error de Notacion: indices repetidos 3 o mas veces: "
         <> ToString[DeleteDuplicates[badIndices]], Red]];
      "Error"
      ,
      "OK"
    ]
  ];

TensorToolkit`CleanVisual[expr_] :=
  Module[{cleaned = expr /. {xAct`xTensor`Scalar[x_] :> x, xAct`xCoba`Scalar[
    x_] :> x}},
    If[Head[cleaned] === TensorToolkit`CleanVisual,
      expr
      ,
      cleaned
    ]
  ];

(* Post-procesador visual global para extraer signos negativos de los tensores y eliminar sus parentesis innecesarios *)

TensorToolkit`CleanBoxes[boxes_] :=
  If[FreeQ[boxes, TagBox[_, "Tensor", ___]],
    boxes
    ,
    Module[{step1},
      step1 = boxes //. {RowBox[{"(", b : FormBox[TagBox[_, "Tensor",
         ___], _], ")"}] :> b, RowBox[{"(", b : TagBox[_, "Tensor", ___], ")"
        }] :> b, RowBox[{"(", RowBox[{"-", b : FormBox[TagBox[_, "Tensor", ___
        ], _]}], ")"}] :> RowBox[{"-", b}], RowBox[{"(", RowBox[{"-", b : TagBox[
        _, "Tensor", ___]}], ")"}] :> RowBox[{"-", b}]};
      step1 //. {RowBox[{pre___, "+", RowBox[{mid1___, RowBox[{"-", t_
        }], mid2___}], post___}] :> RowBox[{pre, "-", RowBox[{mid1, t, mid2}],
         post}], RowBox[{pre___, "+", RowBox[{"-", t_}], post___}] :> RowBox[
        {pre, "-", t, post}]}
    ]
  ];

Unprotect[Plus];

MakeBoxes[Plus[args___], StandardForm] /; (!TrueQ[$inCleanPlusForm]) :=
  Block[{$inCleanPlusForm = True},
    TensorToolkit`CleanBoxes[MakeBoxes[Plus[args], StandardForm]]
  ];

MakeBoxes[Plus[args___], TraditionalForm] /; (!TrueQ[$inCleanPlusForm
  ]) :=
  Block[{$inCleanPlusForm = True},
    TensorToolkit`CleanBoxes[MakeBoxes[Plus[args], TraditionalForm]]
  ];

Protect[Plus];

Unprotect[Times];

MakeBoxes[Times[args___], StandardForm] /; (!TrueQ[$inCleanPlusForm]) :=
  Block[{$inCleanPlusForm = True},
    TensorToolkit`CleanBoxes[MakeBoxes[Times[args], StandardForm]]
  ];

MakeBoxes[Times[args___], TraditionalForm] /; (!TrueQ[$inCleanPlusForm
  ]) :=
  Block[{$inCleanPlusForm = True},
    TensorToolkit`CleanBoxes[MakeBoxes[Times[args], TraditionalForm]]
      
  ];

Protect[Times];

End[]
