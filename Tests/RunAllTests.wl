(* ::Package:: *)

(* Master Test Runner - TensorToolkit *)

Print["\n[🚀] Iniciando Suite Completa de Tests..."];

(* 1. Preparar Entorno *)

basePath = ParentDirectory[DirectoryName[$InputFileName]];

SetDirectory[basePath];

Print["[📂] Directorio Raíz: ", basePath];

(* 2. Cargar Módulos del Toolkit *)

Print["[📦] Cargando Módulos..."];

Quiet[
    Get["Source/Visual.wl"];
    Get["Source/Core.wl"];
    Needs["xAct`xTensor`"];
];

(* 3. Localizar todos los archivos .wlt *)

testFiles = FileNames["*.wlt", {"Tests"}, Infinity];

Print["[🔍] Tests encontrados: ", Length[testFiles]];

Do[Print["    - ", FileNameTake[f]], {f, testFiles}];

(* 4. Ejecución Global *)

Print["\n[🧪] Ejecutando Pruebas..."];

globalReport = TestReport[testFiles];

(* 5. Reporte Final de Calidad *)

Print["\n" <> StringRepeat["=", 40]];

Print["📊 RESUMEN EJECUTIVO"];

Print[StringRepeat["=", 40]];

Print["✅ Tests Exitosos: ", globalReport["TestsSucceededCount"]];

Print["❌ Tests Fallidos: ", globalReport["TestsFailedCount"]];

Print["⏱ Tiempo Total:  ", Round[globalReport["TimeUsed"], 0.01], " s"
    ];

Print[StringRepeat["-", 40]];

If[globalReport["AllTestsSucceeded"],
    Print["🌟 RESULTADO: TODO VERDE. El Toolkit es estable."]
    ,
    Print["⚠️  RESULTADO: SE DETECTARON ERRORES."];
    Print["\nDetalle de fallos:"];
    Scan[
        If[#["Outcome"] === "Failure" || #["Outcome"] === "MessagesFailure",
            
            Print["   [X] ", #["TestID"], " en ", FileNameTake[#["TestFileName"
                ]]]
        ]&
        ,
        Values[globalReport["TestResults"]]
    ];
];

Print[StringRepeat["=", 40] <> "\n"];
