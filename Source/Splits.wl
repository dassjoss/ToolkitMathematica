TensorCollect::usage = "TensorCollect[expr, form] agrupa tensores usando algebra de xAct.";
SumHold::usage = "SumHold[expr, iteradores...] envoltura de Sum que se ve oculta en pantalla (solo se ve expr) hasta activarla con ActivateSplit.";
ActivateSplit::usage = "ActivateSplit[expr] reemplaza recursivamente todo SumHold anidado por Sum real y lo evalua.";
DefIndexFamily::usage = "DefIndexFamily[clave, rango, base] registra una familia de indices splittable (ej. \"Greek\", Range[0,3], ch).";
DefSplit::usage = "DefSplit[familia, valoresFijos, simboloNuevo] crea un split concreto: fija valoresFijos, simboloNuevo recorre el complemento. Valida colision con indices abstractos conocidos.";
ApplySplit::usage = "ApplySplit[tensor[indices], {{posicion,valor},...}, split] fija uno o varios slots de un tensor segun un split, preservando signo covariante/contravariante.";
SumOverSplit::usage = "SumOverSplit[expr, split] envuelve expr en SumHold sumando sobre el rango complementario de un unico split.";
SumOverSplitMulti::usage = "SumOverSplitMulti[expr, {splits...}] como SumOverSplit pero sumando sobre varios splits a la vez.";
ReplaceIndexGlobally::usage = "ReplaceIndexGlobally[expr, nombreIndice, valor, base] reemplaza TODAS las apariciones de un indice por nombre en toda la expresion, preservando signo.";
AutoSplit::usage = "AutoSplit[expr, {{indice,split},...}] divide uno o mas indices sobre una expresion completa, distribuyendo correctamente sobre Plus y Times (linealidad garantizada), generando las 2^n combinaciones automaticamente.";
DefSplit::collides = "El simbolo nuevo `1` colisiona con un nombre reservado. Registralo primero con RegisterReservedIndices, o elige otro nombre.";
RegisterReservedIndices::usage = "RegisterReservedIndices[{simbolos...}] declara que nombres estan reservados como indices abstractos de ALGUN manifold de la teoria actual, para que DefSplit los rechace como nombre nuevo de split.";

Begin["`Private`"]

TensorCollect[expr_, form_] :=
  Collect[xAct`xTensor`ToCanonical[expr], form];

Format[SumHold[expr_, ___]] :=
  DisplayForm[expr];

ActivateSplit[expr_] :=
  expr /. SumHold[e_, iters___] :> Sum[e, iters];

$IndexFamilies = <||>;

$ReservedIndexNames = {};

DefIndexFamily[key_String, range_List, basis_] :=
  $IndexFamilies[key] = <|"Range" -> range, "Basis" -> basis|>;

RegisterReservedIndices[names_List] :=
  $ReservedIndexNames = Union[$ReservedIndexNames, names];

DefSplit::badfam = "La familia `1` no existe. Definila con DefIndexFamily.";

DefSplit::badfixed = "El valor fijo `1` no pertenece al rango `2` de la familia `3`.";

DefSplit[familyKey_String, fixedValues_List, newSymbol_Symbol] :=
  Module[{fam, fullRange, complement},
    If[!KeyExistsQ[$IndexFamilies, familyKey],
      Message[DefSplit::badfam, familyKey];
      Return[$Failed]
    ];
    If[MemberQ[$ReservedIndexNames, newSymbol],
      Message[DefSplit::collides, newSymbol];
      Return[$Failed]
    ];
    fam = $IndexFamilies[familyKey];
    fullRange = fam["Range"];
    If[!SubsetQ[fullRange, fixedValues],
      Message[DefSplit::badfixed, fixedValues, fullRange, familyKey];
      Return[$Failed]
    ];
    complement = Complement[fullRange, fixedValues];
    Print["[Split] Familia \"", familyKey, "\": fijo = ", fixedValues, "  |  ", newSymbol, " = ", complement];
    <|"Family" -> familyKey, "Fixed" -> fixedValues, "NewSymbol" -> newSymbol, "ComplementRange" -> complement, "Basis" -> fam["Basis"]|>
  ];

ApplySplit[head_Symbol[indices___], slotChoices : {{_Integer, _}...}, split_Association] :=
  Module[{idxList = {indices}},
    Do[
      Module[{pos = sc[[1]], val = sc[[2]], orig, isDown},
        orig = idxList[[pos]];
        isDown = MatchQ[orig, Times[-1, _]];
        idxList[[pos]] =
          If[isDown,
            {val, -split["Basis"]}
            ,
            {val, split["Basis"]}
          ];
      ]
      ,
      {sc, slotChoices}
    ];
    head @@ idxList
  ];

SumOverSplit[expr_, split_Association] :=
  SumHold[expr, {split["NewSymbol"], split["ComplementRange"]}];

SumOverSplitMulti[expr_, splits : {__Association}] :=
  SumHold[expr, Sequence @@ ({#["NewSymbol"], #["ComplementRange"]}& /@ splits)];

ReplaceIndexGlobally[expr_, idxName_Symbol, val_, basis_] :=
  With[{iName = idxName},
    expr /. {Times[-1, iName] :> {val, -basis}, iName :> {val, basis}}
  ];

AutoSplitCore[expr_, indexSplits : {{_Symbol, _Association}...}] :=
  Module[{combos},
    combos = Tuples[{"Fixed", "Complement"}, Length[indexSplits]];
    Total[
      Table[
        Module[{current = expr, sumVars = {}},
          Do[
            Module[{idxName = indexSplits[[k, 1]], split = indexSplits[[k, 2]], choice = combo[[k]]},
              If[choice === "Fixed",
                current = ReplaceIndexGlobally[current, idxName, split["Fixed"][[1]], split["Basis"]]
                ,
                current = ReplaceIndexGlobally[current, idxName, split["NewSymbol"], split["Basis"]];
                AppendTo[sumVars, {split["NewSymbol"], split["ComplementRange"]}]
              ]
            ]
            ,
            {k, Length[indexSplits]}
          ];
          If[sumVars === {},
            current
            ,
            SumHold[current, Sequence @@ sumVars]
          ]
        ]
        ,
        {combo, combos}
      ]
    ]
  ];

GetNextAvailableIndex[expr_, currentSym_Symbol] :=
  Module[{candidates, symName},
    symName = ToString[currentSym];
    candidates =
      If[StringMatchQ[symName, "i" | "j" | "k" | "l" | "m" | "n"],
        {i, j, k, l, m, n}
        ,
        If[StringMatchQ[symName, "a" | "b" | "c" | "d" | "e" | "f"],
          {a, b, c, d, e, f}
          ,
          {currentSym}
        ]
      ];
    SelectFirst[candidates, FreeQ[expr, #]&, Unique[symName]]
  ];

AutoSplit[expr_, indexSplits : {{_Symbol, _Association}...}] :=
  Module[{indices, safeSplits},
    indices = indexSplits[[All, 1]];
    If[FreeQ[expr, Alternatives @@ indices],
      Return[expr]
    ];
    safeSplits =
      Table[
        Module[{idxName = indexSplits[[k, 1]], split = indexSplits[[k, 2]], newSym = indexSplits[[k, 2]]["NewSymbol"]},
          If[!FreeQ[expr, newSym],
            split["NewSymbol"] = GetNextAvailableIndex[expr, newSym];
          ];
          {idxName, split}
        ]
        ,
        {k, Length[indexSplits]}
      ];
    If[Head[expr] === Plus,
      Return[AutoSplit[#, safeSplits]& /@ expr]
    ];
    If[Head[expr] === Times,
      Module[{parts, hasNot, has},
        parts = List @@ expr;
        hasNot = Select[parts, FreeQ[#, Alternatives @@ indices]&];
        has = Select[parts, !FreeQ[#, Alternatives @@ indices]&];
        If[hasNot === {},
          Return[AutoSplitCore[expr, safeSplits]]
          ,
          Return[Times @@ hasNot * AutoSplit[Times @@ has, safeSplits]]
        ]
      ]
    ];
    If[Head[expr] === SumHold,
      Return[SumHold[AutoSplit[expr[[1]], safeSplits], Sequence @@ Rest[List @@ expr]]]
    ];
    AutoSplitCore[expr, safeSplits]
  ];

End[]
