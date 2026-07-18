DefineTheoryIndices::usage = "DefineTheoryIndices[simbolo, visual, manifold, rango] registra un indice en el registro global para deducir automaticamente sus propiedades.";
FastTensor::usage = "FastTensor[texpr, opts] define un tensor deduciendo automaticamente sus manifolds a partir de los indices registrados en DefineTheoryIndices.";
DefTensorF::usage = "DefTensorF[texpr, mfd, opts] como DefTensor nativo, pero aplica SetTensorFormatting automaticamente al Head definido. NO asigna alias -- usar SetDisplayName aparte en el notebook.";
$IndexRegistry::usage = "$IndexRegistry es el diccionario global de indices registrados.";

FastTensor::noreg = "Los siguientes indices no estan registrados en DefineTheoryIndices: `1`. El tensor no fue definido.";

Begin["`Private`"]

$IndexRegistry = <||>;

DefineTheoryIndices[idxSym_Symbol, vis_String, mfd_, rng_List] :=
  ($IndexRegistry[SymbolName[idxSym]] = <|"Visual" -> vis, "Manifold" -> mfd, "Range" -> rng|>;);

SetAttributes[FastTensor, HoldFirst];

FastTensor[texpr_, opts___] :=
  Module[{inds, mfds, sym, head, getIdxSym},
    head = Head[Unevaluated[texpr]];
    sym = Extract[Unevaluated[texpr], 0, Unevaluated];
    getIdxSym[Times[-1, s_Symbol]] := s;
    getIdxSym[s_Symbol] := s;
    getIdxSym[_] := Null;
    inds = Map[getIdxSym, List @@ Unevaluated[texpr]];
    mfds = Map[If[Head[#] === Symbol, Lookup[Lookup[$IndexRegistry, SymbolName[#], <||>], "Manifold", Null], Null]&, inds];
    If[MemberQ[mfds, Null],
      Module[{faltantes},
        faltantes = Pick[inds, mfds, Null];
        Message[FastTensor::noreg, faltantes];
      ];
      Return[$Failed]
    ];
    mfds = Flatten[mfds];
    DefTensorF[texpr, mfds, opts];
  ];

SetAttributes[DefTensorF, HoldFirst];

DefTensorF[texpr_, mfd_, opts___] :=
  (
    With[{t = Unevaluated[texpr]},
      xAct`xTensor`DefTensor[t, mfd, opts]
    ];
    TensorToolkit`SetTensorFormatting[Head[Unevaluated[texpr]]];
  );

End[]
