(* ================================================================== *)
(* CleanBoxes_Variants_Test.wl                                        *)
(* Prueba de 3 variantes de reglas adicionales para CleanBoxes        *)
(* SIN MODIFICAR Visual.wl                                            *)
(* ================================================================== *)

(* ================================================================== *)
(* REGLAS ORIGINALES (las 2 que ya existen en Visual.wl)               *)
(* ================================================================== *)
originalRules = {
  RowBox[{"(", b:FormBox[TagBox[_, "Tensor", ___], _], ")"}] :> b,
  RowBox[{"(", b:TagBox[_, "Tensor", ___], ")"}] :> b
};

(* ================================================================== *)
(* VARIANTE CB-A: Extrae signo, deja orden del Plus intacto            *)
(*   Agrega patrones para ("(", RowBox[{"-", tensor}], ")")            *)
(*   Solo quita el parentesis, el "-" queda donde estaba.              *)
(* ================================================================== *)
rulesA = Join[originalRules, {
  RowBox[{"(", RowBox[{"-", b:FormBox[TagBox[_, "Tensor", ___], _]}], ")"}] :> RowBox[{"-", b}],
  RowBox[{"(", RowBox[{"-", b:TagBox[_, "Tensor", ___]}], ")"}] :> RowBox[{"-", b}]
}];

CleanBoxesA[boxes_] := boxes //. rulesA;

(* ================================================================== *)
(* VARIANTE CB-B: Post-proceso a nivel del RowBox completo de Plus     *)
(*   Busca {prev___, "+", RowBox[{factors_con_neg_tensor}], rest___}   *)
(*   y cambia el "+" por "-" quitando el signo interior.               *)
(* ================================================================== *)

(* CB-B usa las reglas originales + reglas de CB-A como base *)
rulesB = Join[originalRules, {
  RowBox[{"(", RowBox[{"-", b:FormBox[TagBox[_, "Tensor", ___], _]}], ")"}] :> RowBox[{"-", b}],
  RowBox[{"(", RowBox[{"-", b:TagBox[_, "Tensor", ___]}], ")"}] :> RowBox[{"-", b}]
}];

(* CB-B simplificado: usa solo las reglas locales y una regla global
   que transforma el patron completo a nivel del infijo del Plus *)
CleanBoxesBsimple[boxes_] := Module[{step1},
  step1 = boxes //. rulesB;
  (* Patron: dentro de un RowBox de Plus (infijo con +/-),
     buscar un termino producto donde el tensor con signo extraido
     (que ahora es RowBox[{"-", tensor}]) aparece. Lo movemos al frente.
     NOTA: MakeBoxes de Plus a veces anida los terminos, asi que buscamos 
     el signo y lo extraemos cambiando el operador anterior *)
  step1 //. {
    RowBox[{pre___, "+", RowBox[{mid1___, RowBox[{"-", t_}], mid2___}], post___}] :>
      RowBox[{pre, "-", RowBox[{mid1, t, mid2}], post}],
    RowBox[{pre___, "+", RowBox[{"-", t_}], post___}] :>
      RowBox[{pre, "-", t, post}]
  }
];

(* ================================================================== *)
(* VARIANTE CB-C: Igual que CB-A pero con InvisibleSpace               *)
(* ================================================================== *)
rulesC = Join[originalRules, {
  RowBox[{"(", RowBox[{"-", b:FormBox[TagBox[_, "Tensor", ___], _]}], ")"}] :> RowBox[{"-\\[InvisibleSpace]", b}],
  RowBox[{"(", RowBox[{"-", b:TagBox[_, "Tensor", ___]}], ")"}] :> RowBox[{"-\\[InvisibleSpace]", b}]
}];

CleanBoxesC[boxes_] := boxes //. rulesC;

(* ================================================================== *)
(* EJECUCION DE TESTS CON CAJAS SINTETICAS                           *)
(* Simulamos la salida exacta de WolfBook (que incluye el            *)
(* parentesis alrededor del signo negativo dentro del Plus)          *)
(* ================================================================== *)

(* Estas son las boxes exactas que WolfBook recibe y renderiza mal:
   e^I_ρ * (-Γ_μν^ρ) envuelto en parentesis. *)
syntheticRawBoxes = RowBox[{
  RowBox[{TagBox[SubscriptBox["\\[PartialD]", "\\[Mu]"], DisplayForm], 
  "(", TagBox[RowBox[{SubsuperscriptBox["e", " ", "\\[CapitalIota]"], SubsuperscriptBox["\\[InvisibleSpace]", "\\[Nu]", " "]}], "Tensor"], ")"}],
  "-", 
  RowBox[{TagBox[RowBox[{SubsuperscriptBox["e", " ", "J"], SubsuperscriptBox["\\[InvisibleSpace]", "\\[Nu]", " "]}], "Tensor"], " ", TagBox[RowBox[{SubsuperscriptBox["\\[Omega]", "\\[Mu]", " "], SubsuperscriptBox["\\[InvisibleSpace]", "J", " "], SubsuperscriptBox["\\[InvisibleSpace]", " ", "\\[CapitalIota]"]}], "Tensor"]}],
  "+", 
  RowBox[{TagBox[RowBox[{SubsuperscriptBox["e", " ", "\\[CapitalIota]"], SubsuperscriptBox["\\[InvisibleSpace]", "\\[Rho]", " "]}], "Tensor"], " ", RowBox[{"(", RowBox[{"-", TagBox[RowBox[{SubsuperscriptBox["\\[CapitalGamma]", "\\[Mu]", " "], SubsuperscriptBox["\\[InvisibleSpace]", "\\[Nu]", " "], SubsuperscriptBox["\\[InvisibleSpace]", " ", "\\[Rho]"]}], "Tensor"]}], ")"}]}]
}];

Print["************************************************************"];
Print["  TEST 0: Reglas originales (sin cambios)                    "];
Print["************************************************************"];
cleanedOrig = syntheticRawBoxes //. originalRules;
Print["StandardForm con reglas originales:"];
Print[RawBoxes[cleanedOrig]];
Print[];

Print["************************************************************"];
Print["  TEST CB-A: Extrae signo, orden intacto                    "];
Print["************************************************************"];
cleanedA = CleanBoxesA[syntheticRawBoxes];
Print["StandardForm CB-A:"];
Print[RawBoxes[cleanedA]];
Print[];

Print["************************************************************"];
Print["  TEST CB-B: Reordenamiento a nivel de Plus                 "];
Print["************************************************************"];
cleanedB = CleanBoxesBsimple[syntheticRawBoxes];
Print["StandardForm CB-B:"];
Print[RawBoxes[cleanedB]];
Print[];

Print["************************************************************"];
Print["  TEST CB-C: InvisibleSpace                                 "];
Print["************************************************************"];
cleanedC = CleanBoxesC[syntheticRawBoxes];
Print["StandardForm CB-C:"];
Print[RawBoxes[cleanedC]];
Print[];

Print["************************************************************"];
Print["  GRID COMPARATIVO FINAL                                    "];
Print["************************************************************"];
Grid[{
  {"Variante", "StandardForm render"},
  {"Original (sin CB extra)", RawBoxes[cleanedOrig]},
  {"CB-A (signo local)", RawBoxes[cleanedA]},
  {"CB-B (reorden Plus)", RawBoxes[cleanedB]},
  {"CB-C (InvisibleSpace)", RawBoxes[cleanedC]}
}, Alignment -> Left, Spacings -> {2, 1.5}, Frame -> All]
