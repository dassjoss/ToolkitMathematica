Quiet[<<xAct`xTensor`];
$toolkitPath = FileNameJoin[{$HomeDirectory, "ArchivoPortable", "02_Academico", "Semestre", "scripts", "TensorToolkit_Loader.wl"}];
Get[$toolkitPath];

DefManifold[M, 4, {a, b, c, d, mu, nu, rho, sigma}];
DefManifold[Internal, 4, {II, JJ, KK, LL, MM, NN, PP, QQ}];

DefineTheoryIndices[mu, "\\[Mu]", M, Range[0, 3]];
DefineTheoryIndices[nu, "\\[Nu]", M, Range[0, 3]];
DefineTheoryIndices[rho, "\\[Rho]", M, Range[0, 3]];
DefineTheoryIndices[sigma, "\\[Sigma]", M, Range[0, 3]];

DefTensor[e[-mu], M];
DefTensor[GammaAff[-mu, -nu, rho], M];
DefTensor[omega[-mu, -II, JJ], M];

(* Registramos los nombres visuales (Lateados) para TODOS los tensores *)
SetDisplayName[e, "e"];
SetDisplayName[GammaAff, "\\[CapitalGamma]"];
SetDisplayName[omega, "\\[Omega]"];

(* Activamos el formato visual nativo del Toolkit (Lego Vertical + Parche de Signos) *)
TensorToolkit`SetTensorFormatting[e];
TensorToolkit`SetTensorFormatting[GammaAff];
TensorToolkit`SetTensorFormatting[omega];

(* Replicamos la salida de la derivada del usuario usando D *)
Format[CovDOp[idx_][expr_]] := DisplayForm[
  RowBox[{SubscriptBox["D", IndexLabel[idx]], "(", ToBoxes[expr, TraditionalForm], ")"}]
];

(* Construimos la derivada simulada *)
fakeDeriv = CovDOp[mu][e[II, -nu]] - omega[mu, J, II] * e[J, -nu] + e[II, rho] * (-GammaAff[mu, -nu, rho]);

Print["Salida Nativa (Los parentesis indeseados ahora deben estar removidos automaticamente por Visual.wl):"];
TraditionalForm[fakeDeriv]
