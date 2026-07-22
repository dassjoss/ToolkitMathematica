(* ::Package:: *)

BeginPackage["TensorToolkit`", {"xAct`xTensor`"}];

DefineLeviCivita::usage = "DefineLeviCivita[mfd, eps, tipo] define un simbolo de Levi-Civita. tipo='Tensor' o 'Density'.";

SetLeviCivitaFormatting::usage = "SetLeviCivitaFormatting[eps, tipo] establece el formato visual (con o sin tilde).";

EpsContract::usage = "EpsContract[expr] contrae productos de simbolos Levi-Civita devolviendo la Delta Generalizada GDelta.";

$LeviCivitaRegistry::usage = "$LeviCivitaRegistry es el diccionario global mfd->eps registrado por DefineLeviCivita.";

Begin["`Private`"]

$LeviCivitaRegistry = <||>;

SetAttributes[DefineLeviCivita, HoldAll];

DefineLeviCivita[mfd_Symbol, eps_Symbol, tipo_String] :=
  Module[{dim, inds, metrics, met},
    If[KeyExistsQ[$LeviCivitaRegistry, mfd],
        $LeviCivitaRegistry = KeyDrop[$LeviCivitaRegistry, mfd]
    ];
    dim = xAct`xTensor`DimOfManifold[mfd];
    inds = (-1) * Take[xAct`xTensor`IndicesOfVBundle[xAct`xTensor`Tangent[
      mfd]][[1]], dim];
    If[tipo === "Tensor",
      metrics = xAct`xTensor`MetricsOfVBundle[xAct`xTensor`Tangent[mfd
        ]];
      If[Length[metrics] > 0,
        met = First[metrics];
        eps = xAct`xTensor`epsilon[met];
        SetLeviCivitaFormatting[eps, tipo];
        $LeviCivitaRegistry[mfd] = eps;
        If[!MemberQ[xAct`xTensor`$Tensors, eps],
          xAct`xTensor`DefTensor[eps @@ inds, mfd, Antisymmetric[Range[dim]]]
        ];
        SetLeviCivitaFormatting[eps, tipo];
        $LeviCivitaRegistry[mfd] = eps;
      ];
      ,
      If[!MemberQ[xAct`xTensor`$Tensors, eps],
        xAct`xTensor`DefTensor[eps @@ inds, mfd, Antisymmetric[Range[dim]], xAct`xTensor`WeightOfTensor -> 1]
      ];
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
            lblBox = OverscriptBox["\\[Epsilon]", "~"];
              
            ups = Select[{inds}, !TensorToolkit`IsDownIndex[#]&];
            downs = Select[{inds}, TensorToolkit`IsDownIndex[#]&];
            upsBox =
              If[Length[ups] > 0,
                RowBox[Riffle[TensorToolkit`IndexLabel /@ ups, "\[ThinSpace]"
                  ]]
                ,
                ""
              ];
            downsBox =
              If[Length[downs] > 0,
                RowBox[Riffle[TensorToolkit`IndexLabel /@ downs, "\[ThinSpace]"
                  ]]
                ,
                ""
              ];
            Which[
              ups === {} && downs === {},
                DisplayForm[lblBox]
              ,
              downs === {},
                DisplayForm[SuperscriptBox[lblBox, upsBox]]
              ,
              ups === {},
                DisplayForm[SubscriptBox[lblBox, downsBox]]
              ,
              True,
                DisplayForm[SubsuperscriptBox[lblBox, downsBox, RowBox[
                  {" ", upsBox}]]]
            ]
          ];
      ];
      ,
      TensorToolkit`SetDisplayName[eps, "\\[Epsilon]"];
      TensorToolkit`SetTensorFormatting[eps];
    ];
  ];

EpsContract[expr_] :=
  Module[{res, getSym, getManifold, getMetric, signDet},
    getSym[Times[-1, s_Symbol]] := s;
    getSym[s_Symbol] := s;
    getSym[_] := Null;
    
    getManifold[idx_] := Module[{sym = getSym[idx]},
      If[sym === Null, Return[Null]];
      Quiet[xAct`xTensor`BaseOfVBundle[
        Quiet[xAct`xTensor`VBundleOfIndex[sym]]]]
    ];
    
    getMetric[mfd_Symbol] := Module[{metrics},
      metrics = xAct`xTensor`MetricsOfVBundle[xAct`xTensor`Tangent[mfd]];
      If[Length[metrics] > 0, First[metrics], Null]
    ];
    
    res = expr /. eps1_Symbol[i1__] * eps2_Symbol[i2__] /; 
      (StringMatchQ[SymbolName[eps1], "*eps*"] || 
       StringMatchQ[SymbolName[eps1], "*Eps*"] || 
       StringMatchQ[SymbolName[eps1], "*epsilon*"]) && eps1 === eps2 :> 
      Module[{n = Length[{i1}], mfd, met, signDet},
        mfd = getManifold[{i1}[[1]]];
        met = If[mfd =!= Null, getMetric[mfd], Null];
        signDet = If[met =!= Null, 
          xAct`xTensor`SignDetOfMetric[met], 
          1
        ];
        If[Sort[Map[getSym, {i1}]] === Sort[Map[getSym, {i2}]],
          signDet * Factorial[n],
          If[signDet == -1 && n >= 2,
            xAct`xTensor`Gdelta @@ ReplacePart[Join[{i1}, {i2}], {1 -> Join[{i1}, {i2}][[2]], 2 -> Join[{i1}, {i2}][[1]]}],
            signDet * (xAct`xTensor`Gdelta @@ Join[{i1}, {i2}])
          ]
        ]
      ];
    res
  ];

(* --- Implementacion Matematica del Dual de Hodge --- *)

Quiet[Remove["TensorToolkit`HodgeDual"]];

Off[HodgeDual::shdw];

Unprotect[System`HodgeDual];

ClearAll[System`HodgeDual];

System`HodgeDual::noinds = "Se necesitan al menos `1` índices disponibles en `3`, pero solo hay `2`.";
System`HodgeDual::toohigh = "La forma tiene `1` índices, pero la dimensión de `3` es solo `2`.";
System`HodgeDual::nometric = "No se encontró métrica en el manifold `1`. El dual de Hodge requiere métrica.";

System`HodgeDual[expr_, mfd_] :=
  Module[{eps, dim, freeInds, p, allMfdInds, newInds, epsInds, getSym,
     allFree, dummies, hodgeExpr, metrics, met, availableInds, unusedInds, contractInds, metricFactors},
    eps = Lookup[$LeviCivitaRegistry, mfd, Null];
    If[eps === Null,
      Return[expr]
    ];
    dim = xAct`xTensor`DimOfManifold[mfd];
    metrics = xAct`xTensor`MetricsOfVBundle[xAct`xTensor`Tangent[mfd]];
    If[Length[metrics] == 0,
      Message[System`HodgeDual::nometric, mfd];
      Return[expr]
    ];
    met = First[metrics];
    getSym[Times[-1, s_Symbol]] := s;
    getSym[s_Symbol] := s;
    getSym[_] := Null;
    freeInds = Select[List @@ xAct`xTensor`FindFreeIndices[expr], 
      Quiet[xAct`xTensor`BaseOfVBundle[Quiet[xAct`xTensor`VBundleOfIndex[getSym[#]]]]] === mfd &];
    dummies = Select[List @@ xAct`xTensor`FindDummyIndices[expr], 
      Quiet[xAct`xTensor`BaseOfVBundle[Quiet[xAct`xTensor`VBundleOfIndex[getSym[#]]]]] === mfd &];
    p = Length[freeInds];
    allMfdInds = xAct`xTensor`IndicesOfVBundle[xAct`xTensor`Tangent[mfd]][[1]];
    
    If[p == 0,
      hodgeExpr = expr * (eps @@ Map[Minus, Take[allMfdInds, dim]]);
      hodgeExpr = xAct`xTensor`ContractMetric[hodgeExpr];
      hodgeExpr = xAct`xTensor`ToCanonical[hodgeExpr];
      Return[hodgeExpr]
    ];
    
    If[p > dim,
      Message[System`HodgeDual::toohigh, p, dim, mfd];
      Return[expr]
    ];
    
    
    Catch[
      availableInds = Select[allMfdInds, !MemberQ[Map[getSym, freeInds], #] && !MemberQ[dummies, #]&];
      If[Length[availableInds] < dim,
          Message[System`HodgeDual::noinds, dim, Length[availableInds], mfd];
          Return[expr]
      ];
      unusedInds = Reverse[Take[Reverse[availableInds], dim]];
      newInds = Take[unusedInds, dim - p];
      contractInds = Drop[unusedInds, dim - p];

      epsInds = Join[Map[Minus, contractInds], Map[Minus, newInds]];
      
      metricFactors = Times @@ Table[
        met[getSym[freeInds[[i]]], contractInds[[i]]]
        ,
        {i, 1, p}
      ];

      hodgeExpr = (1 / Factorial[p]) * expr * metricFactors * (eps @@ epsInds);
      hodgeExpr = xAct`xTensor`ContractMetric[hodgeExpr];
      hodgeExpr = TensorToolkit`EpsContract[hodgeExpr];
      hodgeExpr = xAct`xTensor`ToCanonical[hodgeExpr];
      Return[hodgeExpr]
    ]
      
  ];

(* Correccion visual: Usar \\[Star] en lugar de unicode de estrella *)

Format[HoldPattern[System`HodgeDual[expr_, mfd_]]] :=
  RawBoxes[RowBox[{"\[FivePointedStar]", ToBoxes[expr]}]];

Protect[System`HodgeDual];

End[]

EndPackage[];
