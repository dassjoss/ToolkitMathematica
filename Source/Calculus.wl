DefineCovariantDerivative::usage = "DefineCovariantDerivative[covD, pd, gamma, omega] registra un operador de derivada covariante.";
ExpandDerivative::usage = "ExpandDerivative[expr, covD, dummyRules] expande covD introduciendo las conexiones registradas, usando los indices de dummyRules (ej. <|M -> c, Internal -> JJ|>).";
DefineOrthogonality::usage = "DefineOrthogonality[rule] registra una regla de ortogonalidad (ej. A*B :> 0).";
SmartContract::usage = "SmartContract[expr] aplica ToCanonical, ContractMetric y las reglas de ortogonalidad registradas.";

Begin["`Private`"]

$DerivativeRegistry = <||>;

DefineCovariantDerivative[covD_Symbol, pd_Symbol, rules_Association] :=
  (
    $DerivativeRegistry[covD] = <|"PD" -> pd|>;
    Do[
      Module[{key = r[[1]], val = r[[2]]},
        If[ListQ[val],
          $DerivativeRegistry[covD][key] = <|"Conn" -> val[[1]], "Sign" -> val[[2]]|>
          ,
          $DerivativeRegistry[covD][key] =
            <|
              "Conn" -> val
              ,
              "Sign" ->
                If[ToString[key] === "Internal",
                  -1
                  ,
                  1
                ]
            |>
        ]
      ]
      ,
      {r, Normal[rules]}
    ];
  );

DefineCovariantDerivative[covD_Symbol, pd_Symbol, rules : (_Rule...)] :=
  DefineCovariantDerivative[covD, pd, Association[rules]];

ExpandDerivative[expr_, covD_Symbol, dummies_Association] :=
  Module[{pd, gamma, omega, applyCovD},
    If[!KeyExistsQ[$DerivativeRegistry, covD],
      Return[expr]
    ];
    pd = $DerivativeRegistry[covD]["PD"];
    gamma = $DerivativeRegistry[covD]["Gamma"];
    omega = $DerivativeRegistry[covD]["Omega"];
    applyCovD[dIdx_, t_Symbol[inds___]] :=
      Module[{res, idx, idxSym, mfd, isDown, dum, connEntry, conn, sign},
        res = pd[dIdx][t[inds]];
        Do[
          idx = {inds}[[k]];
          isDown = TensorToolkit`IsDownIndex[idx];
          idxSym =
            If[isDown,
              idx[[2]]
              ,
              idx
            ];
          mfd = Lookup[Lookup[TensorToolkit`$IndexRegistry, SymbolName[idxSym], <||>], "Manifold", Null];
          If[mfd === Null,
            mfd = xAct`xTensor`ManifoldOf[idxSym]
          ];
          If[mfd =!= Null && KeyExistsQ[dummies, mfd],
            dum = dummies[mfd];
            connEntry = Lookup[$DerivativeRegistry[covD], mfd, Null];
              
            If[connEntry =!= Null,
              conn =
                If[AssociationQ[connEntry],
                  connEntry["Conn"]
                  ,
                  connEntry
                ];
              sign =
                If[AssociationQ[connEntry],
                  connEntry["Sign"]
                  ,
                  1
                ];
              If[isDown,
                res = res - sign * conn[dIdx, idx, dum] * ReplacePart[
                  t[inds], k -> Times[-1, dum]]
                ,
                res = res + sign * conn[dIdx, Times[-1, dum], idx] * 
                  ReplacePart[t[inds], k -> dum]
              ]
            ]
          ];
          ,
          {k, Length[{inds}]}
        ];
        res
      ];
    Module[{expanded},
      expanded = expr //. covD[dIdx_][A_ * B__] :> B * covD[dIdx][A] 
        + A * covD[dIdx][Times[B]];
      expanded /. covD[dIdx_][t_Symbol[inds___]] :> applyCovD[dIdx, t[inds]]
    ]
  ];

$OrthogonalityRules = {};

DefineOrthogonality[rule_] :=
  AppendTo[$OrthogonalityRules, rule];

SmartContract[expr_] :=
  Module[{res},
    res = xAct`xTensor`ToCanonical[expr];
    res = xAct`xTensor`ContractMetric[res];
    res = res /. $OrthogonalityRules;
    res = TensorToolkit`CleanVisual[res];
    res
  ];

End[]
