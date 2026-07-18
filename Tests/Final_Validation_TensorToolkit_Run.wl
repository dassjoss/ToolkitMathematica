Quiet[<<xAct`xTensor`];
$toolkitPath = FileNameJoin[{$HomeDirectory, "ArchivoPortable", "02_Academico", "Semestre", "scripts", "TensorToolkit_Loader.wl"}];
Get[$toolkitPath];

DefManifold[M, 4, {a, b, c, d, mu, nu, rho, sigma}];
DefManifold[Internal, 4, {II, JJ, KK, LL, MM, NN, PP, QQ}];

DefineTheoryIndices[mu, "\\[Mu]", M, Range[0, 3]];
DefineTheoryIndices[nu, "\\[Nu]", M, Range[0, 3]];
DefineTheoryIndices[rho, "\\[Rho]", M, Range[0, 3]];
DefineTheoryIndices[sigma, "\\[Sigma]", M, Range[0, 3]];

(* Definicion de tensores para Block 1 y Block 2 *)
DefTensor[e[-mu, II], {M, Internal}];
DefTensorF[Vb[-mu], M]; SetDisplayName[Vb, "e"];
DefTensorF[GammaAff[-a, -b, c], M]; SetDisplayName[GammaAff, "\\[CapitalGamma]"];
DefTensorF[omegaSpin[-a, -II, JJ], {M, Internal}]; SetDisplayName[omegaSpin, "\\[Omega]"];

(* Ortogonalidad *)
DefineOrthogonality[e[-mu, II] * e[mu, JJ] :> GDelta[II, JJ]];
DefineOrthogonality[e[-mu, II] * e[nu, -II] :> GDelta[-mu, nu]];

(* Bloque 1 - SmartContract *)
exprH = e[-mu, II] * e[mu, JJ] * e[-nu, -II] * Vb[nu];
Print["*** TEST 1: SmartContract[exprH] ***"];
Print["Input: ", InputForm[exprH]];
result1 = SmartContract[exprH];
Print["Result boxes (should not have extra parenthesis):"];
Print[ToBoxes[result1, StandardForm]];
Print[];

(* Bloque 2 - ExpandDerivative *)
DefCovD[CD, M];
Format[CD[idx_]] := DisplayForm[SubscriptBox["\\[PartialD]", IndexLabel[idx]]];
DefineCovariantDerivative[D, CD, <| M -> {GammaAff, 1}, Internal -> {omegaSpin, -1} |>];
exprExpanded = ExpandDerivative[D[-mu][Vb[II, -nu]], D, <|M -> rho, Internal -> JJ|>];
Print["*** TEST 2: ExpandDerivative ***"];
Print["Result boxes (should use minus sign correctly and NO parenthesis):"];
Print[ToBoxes[exprExpanded, StandardForm]];
