(* ::Package:: *)

BeginPackage["TensorToolkit`", {"xAct`xTensor`"}];

(* Declaraciones de funciones (Exportadas) *)

GreekIndex::usage = "GreekIndex[idx] traduce un nombre de indice (string o simbolo) a su representacion griega/latina Unicode.";

SetDisplayName::usage = "SetDisplayName[simbolo, \"etiqueta\"] define el alias visual de un tensor/constante (cosmetico, no afecta el calculo).";

DisplayNameOf::usage = "DisplayNameOf[simbolo] devuelve la etiqueta visual asignada, o el nombre real si no se asigno ninguna.";

IndexLabel::usage = "IndexLabel[idx] generaliza GreekIndex para aceptar tambien pares {n,base} (componentes numericas de xCoba) y simbolos con signo.";

IsDownIndex::usage = "IsDownIndex[idx] detecta si un indice (abstracto o componentizado) es covariante (abajo).";

IsInternalIndexQ::usage = "IsInternalIndexQ[idx] verifica si el indice pertenece al manifold Internal o es de Lorentz.";

SetTensorFormatting::usage = "SetTensorFormatting[tensor] define Format[tensor[inds___]] con sub/superindices visuales automaticos.";

CleanVisual::usage = "CleanVisual[expr] limpia la expresion de envoltorios escalares internos (como xAct`xTensor`Scalar).";

CheckEinsteinNotation::usage = "CheckEinsteinNotation[expr] verifica que no haya indices repetidos mas de 2 veces en productos.";

ToLatexExport::usage = "ToLatexExport[expr] convierte la expresion a su representacion en LaTeX (version preliminar).";

(* Símbolos compartidos con Core.wl *)

$IndexRegistry::usage = "Registro global de indices compartido.";

$GreekStringMap::usage = "Mapa de conversion de glifos griegos.";

Begin["`Private`"]

(* --- INICIO DE TU LÓGICA ORIGINAL (SIN CAMBIOS) --- *)

TensorToolkit`$GreekStringMap = <|"mu" -> \[Mu], "nu" -> \[Nu], "rho"
   -> \[Rho], "sigma" -> \[Sigma], "si" -> \[Sigma], "tau" -> \[Tau], "alpha"
   -> \[Alpha], "beta" -> \[Beta], "gamma" -> \[Gamma], "delta" -> \[Delta],
   "lambda" -> \[Lambda], "la" -> \[Lambda], "kappa" -> \[Kappa], "ka" 
  -> \[Kappa], "epsilon" -> \[CurlyEpsilon], "phi" -> \[Phi], "psi" -> 
  \[Psi], "chi" -> \[Chi], "eta" -> \[Eta], "zeta" -> \[Zeta], "xi" -> 
  \[Xi], "\[Mu]" -> \[Mu], "\[Nu]" -> \[Nu], "\[Rho]" -> \[Rho], "\[Sigma]"
   -> \[Sigma], "\[Tau]" -> \[Tau], "\[Alpha]" -> \[Alpha], "β" -> β, "γ"
   -> γ, "δ" -> δ, "\[Lambda]" -> \[Lambda], "\[Kappa]" -> \[Kappa], "\[CurlyEpsilon]"
   -> \[CurlyEpsilon], "\[Phi]" -> \[Phi], "\[Psi]" -> \[Psi], "\[Chi]"
   -> \[Chi], "\[Eta]" -> \[Eta], "\[Zeta]" -> \[Zeta], "\[Xi]" -> \[Xi],
   "\[CapitalGamma]" -> \[CapitalGamma], "ω" -> ω, "ϵ" -> ϵ, "II" -> "I",
   "JJ" -> "J", "KK" -> "K", "LL" -> "L"|>;

GreekIndex[idx_] :=
  Lookup[TensorToolkit`$GreekStringMap, ToString[idx], ToString[idx]];

$TensorDisplayName = <||>;

SetDisplayName[sym_Symbol, label_String] :=
  $TensorDisplayName[sym] = label;

DisplayNameOf[sym_Symbol] :=
  Module[{label},
    label = Lookup[$TensorDisplayName, sym, ToString[sym]];
    If[StringQ[label],
      label = Lookup[TensorToolkit`$GreekStringMap, label, label]
    ];
    If[StringQ[label] && StringMatchQ[label, "\\[*]"],
      label = ToExpression[label]
    ];
    label
  ];

IsInternalIndexQ[idx_Symbol] :=
  Module[{entry, mfd, name = SymbolName[idx]},
    entry = Lookup[TensorToolkit`$IndexRegistry, name, <||>];
    mfd = Lookup[entry, "Manifold", Null];
    ToString[mfd] === "Internal" || MemberQ[{"II", "JJ", "KK", "LL"},
       name]
  ];

IsInternalIndexQ[{_, base_Symbol}] :=
  IsInternalIndexQ[base];

IsInternalIndexQ[Times[-1, s_]] :=
  IsInternalIndexQ[s];

IsInternalIndexQ[_] :=
  False;

IndexLabel[idx_Symbol] :=
  Module[{entry, vis, name = SymbolName[idx]},
    entry = Lookup[TensorToolkit`$IndexRegistry, name, <||>];
    vis = Lookup[entry, "Visual", Null];
    If[vis === Null,
      vis = GreekIndex[idx]
    ];
    If[StringQ[vis],
      vis = Lookup[TensorToolkit`$GreekStringMap, vis, vis]
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
    ToString[vis]
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
    Format[tt[inds___], StandardForm] :=
      Module[{label, nInds, groups, blocks, k, grp, isDown, base, labels
        },
        nInds = Length[{inds}];
        label = ToString[DisplayNameOf[tt]];
        If[nInds == 0,
          Return[RawBoxes[label]]
        ];
        groups = Split[{inds}, IsDownIndex[#1] == IsDownIndex[#2]&];
        blocks =
          Table[
            grp = groups[[k]];
            isDown = IsDownIndex[grp[[1]]];
            labels =
              Table[
                If[IsInternalIndexQ[idxObj],
                  StyleBox[IndexLabel[idxObj], Bold, FontSlant -> "Plain"
                    ]
                  ,
                  IndexLabel[idxObj]
                ]
                ,
                {idxObj, grp}
              ];
            base =
              If[k == 1,
                label
                ,
                " "
              ];
            If[isDown,
              SubsuperscriptBox[base, RowBox[labels], " "]
              ,
              SubsuperscriptBox[base, " ", RowBox[labels]]
            ]
            ,
            {k, 1, Length[groups]}
          ];
        RawBoxes[TagBox[RowBox[blocks], "Tensor"]]
      ];
    Format[tt[inds___], TraditionalForm] :=
      Module[{label, nInds, groups, blocks, k, grp, isDown, base, labels
        },
        nInds = Length[{inds}];
        label = ToString[DisplayNameOf[tt]];
        If[nInds == 0,
          Return[RawBoxes[label]]
        ];
        groups = Split[{inds}, IsDownIndex[#1] == IsDownIndex[#2]&];
        blocks =
          Table[
            grp = groups[[k]];
            isDown = IsDownIndex[grp[[1]]];
            labels =
              Table[
                If[IsInternalIndexQ[idxObj],
                  StyleBox[IndexLabel[idxObj], Bold, FontSlant -> "Plain"
                    ]
                  ,
                  IndexLabel[idxObj]
                ]
                ,
                {idxObj, grp}
              ];
            base =
              If[k == 1,
                label
                ,
                " "
              ];
            If[isDown,
              SubsuperscriptBox[base, RowBox[labels], " "]
              ,
              SubsuperscriptBox[base, " ", RowBox[labels]]
            ]
            ,
            {k, 1, Length[groups]}
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
        inds = Select[inds, KeyExistsQ[TensorToolkit`$IndexRegistry, 
          SymbolName[#]]&];
        counts = Tally[inds];
        repeats = Select[counts, #[[2]] >= 3&];
        If[Length[repeats] > 0,
          badIndices = Join[badIndices, repeats[[All, 1]]];
        ];
      ];
    If[Head[expr] === Plus,
      checkTerm /@ (List @@ expr)
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

ToLatexExport[expr_] :=
  ToString[TeXForm[expr]];

(* --- FIN DE TU LÓGICA ORIGINAL --- *)

End[]

EndPackage[]
