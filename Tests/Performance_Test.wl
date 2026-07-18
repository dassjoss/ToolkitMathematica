Quiet[<<xAct`xTensor`];
$toolkitPath = FileNameJoin[{$HomeDirectory, "ArchivoPortable", "02_Academico", "Semestre", "scripts", "TensorToolkit_Loader.wl"}];
Get[$toolkitPath];

Print["*** TEST DE REGRESION EN ARITMETICA ORDINARIA ***"];
expr1 = 2 * x;
expr2 = 3 * Pi * y^2;
expr3 = Sum[i^2, {i, 1, 5}];
expr4 = (a + b) * (c - d);

Print["expr1 (2*x):"];
Print[ToBoxes[expr1, StandardForm]];

Print["expr2 (3*Pi*y^2):"];
Print[ToBoxes[expr2, StandardForm]];

Print["expr3 (Sum[i^2]):"];
Print[ToBoxes[expr3, StandardForm]];

Print["expr4 ((a+b)*(c-d)):"];
Print[ToBoxes[expr4, StandardForm]];

Print["\n*** TEST DE RENDIMIENTO ***"];
t1 = AbsoluteTiming[Do[RandomReal[] * RandomReal[], {100000}];];
Print["Tiempo Do loop (evaluacion interna sin render): ", t1];

bigSum = Sum[q[i,j] * p[j,k] + r[i,k], {i, 1, 20}, {j, 1, 20}, {k, 1, 20}];
t2 = AbsoluteTiming[boxes = ToBoxes[bigSum, StandardForm];];
Print["Tiempo de MakeBoxes para una suma gigantesca (sin tensores): ", t2[[1]], " segundos."];
