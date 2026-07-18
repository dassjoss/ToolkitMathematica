(* ============================================================
   TensorToolkit_Loader.wl -> init.wl
   Motor generico de formato visual + sistema de splits para xAct.
   Cargador Maestro de Modulos.
   
   NO contiene nada especifico de ninguna teoria (sin manifolds,
   sin metricas, sin tensores concretos, sin alias). Eso vive en
   cada notebook .wb que cargue este paquete.
   ============================================================ *)

BeginPackage["TensorToolkit`"]

$IndexRegistry::usage = "$IndexRegistry es el diccionario global de indices registrados.";

Off[General::shdw];
Off[xAct`xTensor`TensorQ::shdw];

$toolkitModulesDir = FileNameJoin[{DirectoryName[$InputFileName], "..", "Source"}];

Get[FileNameJoin[{$toolkitModulesDir, "Core.wl"}]];
Get[FileNameJoin[{$toolkitModulesDir, "Visual.wl"}]];
Get[FileNameJoin[{$toolkitModulesDir, "Calculus.wl"}]];
Get[FileNameJoin[{$toolkitModulesDir, "Geometry.wl"}]];
Get[FileNameJoin[{$toolkitModulesDir, "Splits.wl"}]];

On[General::shdw];
On[xAct`xTensor`TensorQ::shdw];

EndPackage[]

Print["[TensorToolkit] Modulos cargados exitosamente (Core, Visual, Calculus, Geometry, Splits)."];
