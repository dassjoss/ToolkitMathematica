Quiet[<<xAct`xTensor`];
$toolkitPath = FileNameJoin[{$HomeDirectory, "ArchivoPortable", "02_Academico", "Semestre", "scripts", "TensorToolkit_Loader.wl"}];
Get[$toolkitPath];

DefManifold[M, 4, {a, b, c, d, mu, nu, rho, sigma, alpha, beta, gamma}];
DefineTheoryIndices[mu, "\\[Mu]", M, Range[0, 3]];
DefineTheoryIndices[nu, "\\[Nu]", M, Range[0, 3]];
DefineTheoryIndices[rho, "\\[Rho]", M, Range[0, 3]];
DefineTheoryIndices[sigma, "\\[Sigma]", M, Range[0, 3]];
DefineTheoryIndices[alpha, "\\[Alpha]", M, Range[0, 3]];
DefineTheoryIndices[beta, "\\[Beta]", M, Range[0, 3]];

DefMetric[-1, metric[-a, -b], CDMet, {";", "\\nabla"}];
SetDisplayName[metric, "g"];
TensorToolkit`SetTensorFormatting[metric];

Print["*** BLOQUE 3 y 4: Levi-Civita y HodgeDual ***"];
DefineLeviCivita[M, epsTensor, "Tensor"];
FastTensor[Fform[-mu, -nu]]; SetDisplayName[Fform, "F"];

expr4 = HoldForm[System`HodgeDual[Fform[-mu, -nu], M]];
Print["Output A (Estado Inicial - inerte): "];
Print[ToBoxes[expr4, StandardForm]];
Print["Output B (Estado Final - evaluado y contraido): "];
result4 = ReleaseHold[expr4];
Print[ToBoxes[result4, StandardForm]];

Print["*** BLOQUE 5: Linter de Einstein ***"];
FastTensor[vecA[mu]]; SetDisplayName[vecA, "A"];
FastTensor[vecB[-mu]]; SetDisplayName[vecB, "B"];
FastTensor[vecC[mu]]; SetDisplayName[vecC, "C"];

expr5 = vecA[mu] * vecB[-mu] * vecC[mu];
Print["Output (Linter de expresion con 3 indices mu): "];
CheckEinsteinNotation[expr5];
