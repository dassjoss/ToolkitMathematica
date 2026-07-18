DefineLeviCivita::usage = "DefineLeviCivita[mfd, eps, tipo] define un simbolo de Levi-Civita. tipo='Tensor' o 'Density'.";
SetLeviCivitaFormatting::usage = "SetLeviCivitaFormatting[eps, tipo] establece el formato visual (con o sin tilde).";
EpsContract::usage = "EpsContract[expr] contrae productos de simbolos Levi-Civita devolviendo la Delta Generalizada GDelta.";

Begin["`Private`"]

$LeviCivitaRegistry = <||>;

SetAttributes[DefineLeviCivita, HoldAll];

DefineLeviCivita[mfd_Symbol, eps_Symbol, tipo_String] :=
  Module[{dim, inds, metrics, met},
    dim = xAct`xTensor`DimOfManifold[mfd];
    inds = (-1) * Take[xAct`xTensor`IndicesOfVBundle[xAct`xTensor`Tangent[mfd]][[1]], dim];
    If[tipo === "Tensor",
      metrics = xAct`xTensor`MetricsOfVBundle[xAct`xTensor`Tangent[mfd]];
      If[Length[metrics] > 0,
        met = First[metrics];
        eps = xAct`xTensor`epsilon[met];
        SetLeviCivitaFormatting[eps, tipo];
        $LeviCivitaRegistry[mfd] = eps;
        ,
        xAct`xTensor`DefTensor[eps @@ inds, mfd, Antisymmetric[Range[dim]]];
        SetLeviCivitaFormatting[eps, tipo];
        $LeviCivitaRegistry[mfd] = eps;
      ];
      ,
      xAct`xTensor`DefTensor[eps @@ inds, mfd, Antisymmetric[Range[dim]], xAct`xTensor`WeightOfTensor -> 1];
      SetLeviCivitaFormatting[eps, tipo];
      $LeviCivitaRegistry[mfd] = eps;
    ];
  ];

SetLeviCivitaFormatting[eps_Symbol, tipo_String] :=
  Module[{},
    If[tipo === "Density",
      TensorToolkit`SetDisplayName[eps, "\\[Epsilon]"];
      With[{tt = eps},
        Format[tt[inds___]] :=
          Module[{ups, downs, lblBox, upsBox, downsBox},
            lblBox = ToBoxes[OverTilde[ToExpression["\\[Epsilon]"]]];
            ups = Select[{inds}, !TensorToolkit`IsDownIndex[#]&];
            downs = Select[{inds}, TensorToolkit`IsDownIndex[#]&];
            
            upsBox = If[Length[ups] > 0, RowBox[Riffle[TensorToolkit`IndexLabel /@ ups, "\[ThinSpace]"]], ""];
            downsBox = If[Length[downs] > 0, RowBox[Riffle[TensorToolkit`IndexLabel /@ downs, "\[ThinSpace]"]], ""];
            
            Which[
              ups === {} && downs === {}, DisplayForm[lblBox],
              downs === {}, DisplayForm[SuperscriptBox[lblBox, upsBox]],
              ups === {}, DisplayForm[SubscriptBox[lblBox, downsBox]],
              True, DisplayForm[SubsuperscriptBox[lblBox, downsBox, RowBox[{"\[ThinSpace]", upsBox}]]]
            ]
          ];
      ];
      ,
      TensorToolkit`SetDisplayName[eps, "\\[Epsilon]"];
      TensorToolkit`SetTensorFormatting[eps];
    ];
  ];

EpsContract[expr_] :=
  expr /. eps1_Symbol[i1__] * eps2_Symbol[i2__] /; (StringMatchQ[SymbolName[eps1], "*eps*"] || StringMatchQ[SymbolName[eps1], "*Eps*"] || StringMatchQ[SymbolName[eps1], "*epsilon*"]) && eps1 === eps2 :> xAct`xTensor`GDelta @@ Join[{i1}, {i2}];

(* --- Implementacion Matematica del Dual de Hodge --- *)

Quiet[Remove["TensorToolkit`HodgeDual"]];
Off[HodgeDual::shdw];
Unprotect[System`HodgeDual];
ClearAll[System`HodgeDual];

System`HodgeDual[expr_, mfd_] :=
  Module[{eps, dim, freeInds, p, allMfdInds, newInds, epsInds, getSym, allFree, dummies, hodgeExpr},
    eps = Lookup[$LeviCivitaRegistry, mfd, Null];
    If[eps === Null, Return[expr]];
    dim = xAct`xTensor`DimOfManifold[mfd];
    getSym[Times[-1, s_]] := s; getSym[s_] := s; getSym[_] := Null;
    
    (* Extraccion infalible: Busca cualquier argumento que pertenezca a mfd usando VBundleOfIndex *)
    allFree = {};
    Cases[{expr}, head_[inds___] :> 
      Module[{mfdInds},
        mfdInds = Select[{inds}, (
          Module[{sym = getSym[#]},
            sym =!= Null && Quiet[xAct`xTensor`BaseOfVBundle[Quiet[xAct`xTensor`VBundleOfIndex[sym]]]] === mfd
          ]
        )&];
        If[Length[mfdInds] > Length[allFree], allFree = mfdInds];
      ],
      Infinity
    ];
    
    freeInds = allFree;
    p = Length[freeInds]; 
    If[p == 0, Return[expr]]; 
    
    allMfdInds = xAct`xTensor`IndicesOfVBundle[xAct`xTensor`Tangent[mfd]][[1]];
    dummies = {};
    Cases[{expr}, head_[inds___] :> 
      (dummies = Join[dummies, Select[{inds}, (
          Module[{sym = getSym[#]},
            sym =!= Null && Quiet[xAct`xTensor`BaseOfVBundle[Quiet[xAct`xTensor`VBundleOfIndex[sym]]]] === mfd
          ]
      )&]]),
      Infinity
    ];
    dummies = Tally[Map[getSym, dummies]];
    dummies = Select[dummies, #[[2]] > 1 &][[All, 1]];
    
    (* Seleccionamos los indices nuevos desde el final de la lista para priorizar letras griegas (rho, sigma) y evitar a, b *)
    newInds = Reverse[Take[Reverse[Select[allMfdInds, !MemberQ[Map[getSym, freeInds], #] && !MemberQ[dummies, #]&]], dim - p]];
    
    epsInds = Join[Map[Minus, freeInds], Map[Minus, newInds]];
    
    (* Multiplicacion con epsilon y factor dinamico 1/p! *)
    hodgeExpr = (1 / Factorial[p]) * expr * (eps @@ epsInds);
    
    (* Ejecutar contraccion real de indices automaticamente y limpiar visualmente *)
    hodgeExpr = xAct`xTensor`ToCanonical[xAct`xTensor`ContractMetric[hodgeExpr]];
    TensorToolkit`CleanVisual[TensorToolkit`SmartContract[hodgeExpr]]
  ];

(* Correccion visual: Usar \\[Star] en lugar de unicode de estrella *)
Format[HoldPattern[System`HodgeDual[expr_, mfd_]]] := RawBoxes[RowBox[{"\[FivePointedStar]", ToBoxes[expr]}]];

Protect[System`HodgeDual];

End[]
