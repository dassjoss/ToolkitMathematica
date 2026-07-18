DefineLeviCivita[M, epsTensor, "Tensor"];
FastTensor[Fform[-mu, -nu]]; SetDisplayName[Fform, "F"];

expr4 = HoldForm[System`HodgeDual[Fform[-mu, -nu], M]];
Print["Output A (Estado Inicial - inerte): "];
expr4
Print["Output B (Estado Final - evaluado y contraido): "];
ReleaseHold[expr4]
FastTensor[vecA[mu]]; SetDisplayName[vecA, "A"];
FastTensor[vecB[-mu]]; SetDisplayName[vecB, "B"];
FastTensor[vecC[mu]]; SetDisplayName[vecC, "C"];

expr5 = vecA[mu] * vecB[-mu] * vecC[mu];
Print["Output (Linter de expresion con 3 indices mu): "];
CheckEinsteinNotation[expr5];
