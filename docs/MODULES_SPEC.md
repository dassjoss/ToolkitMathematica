# Especificación de Módulos

## Core.wl
- `DefineTheoryIndices`: Registra índices en `$IndexRegistry`.
- `FastTensor[expr]`: Crea un tensor deduciendo automáticamente su manifold.

## Visual.wl
- `GreekIndex[idx]`: Traduce símbolos a Unicode griego.
- `IndexLabel[idx]`: Maneja la estética de índices con signo y numéricos.
- `SetTensorFormatting[tensor]`: Aplica el formato de sub/superíndices.
- `ToLatexExport[expr]`: (PENDIENTE) Función para convertir expresiones de xAct a LaTeX de alta calidad.

## Calculus.wl
- `SmartContract`: Contracción métrica y aplicación de reglas de ortogonalidad.
- `TensorCollect`: Simplificación de polinomios tensoriales.

## Splits.wl
- `ApplySplit`: Fija un valor (ej. 0) en un slot específico de un tensor.
- `SumHold`: Mantiene las sumas colapsadas visualmente hasta su activación.