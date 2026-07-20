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
    Get["Source/Geometry.wl"];
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
        Module[{tr = #},
            If[tr["Outcome"] === "Failure" || tr["Outcome"] === "MessagesFailure"
                 || tr["Outcome"] === "Error",
                Print["\n" <> StringRepeat["-", 40]];
                Print["   [X] TestID:        ", tr["TestID"]];
                Print["       Archivo:       ", FileNameTake[tr["TestFileName"
                    ]]];
                Print["       Outcome:       ", tr["Outcome"]];
                Print["       Esperado:      ", tr["ExpectedOutput"]]
                    ;
                Print["       Obtenido:      ", tr["ActualOutput"]];
                If[tr["ActualMessages"] =!= {},
                    Print["       ⚠ Mensajes generados:"];
                    Scan[Print["           - ", ToString[#, InputForm
                        ]]&, tr["ActualMessages"]];
                ];
                If[tr["ExpectedMessages"] =!= {} && tr["ExpectedMessages"
                    ] =!= tr["ActualMessages"],
                    Print["       Mensajes esperados: ", tr["ExpectedMessages"
                        ]];
                ];
            ];
        ]&
        ,
        Values[globalReport["TestResults"]]
    ];
    Print["\n" <> StringRepeat["-", 40]];
];

Print[StringRepeat["=", 40] <> "\n"];
