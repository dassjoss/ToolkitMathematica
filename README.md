# TensorToolkit (Modular Version)

## Objetivo
Motor de alta velocidad para Wolfram Language y xAct. Permite definir tensores, realizar cálculos de relatividad general y exportar resultados con calidad editorial (LaTeX) de forma automatizada.

## Arquitectura de Directorios (Estándar Paclet)
- `Source/`: Lógica pura y funciones del motor.
  - `Core.wl`: Registro de índices y creación rápida de tensores (`FastTensor`).
  - `Visual.wl`: Formateo de cajas y exportación a LaTeX.
  - `Calculus.wl`: Derivadas covariantes, contracciones y álgebra.
  - `Splits.wl`: Sistema de ruptura de índices (3+1 ADM).
- `Kernel/init.wl`: Cargador oficial del paquete.
- `Tests/`: Suites de validación de boxes y de identidades físicas.
- `Resources/`: Bases de datos (registry.json, etc.).
- `docs/`: Reglas de oro y convenciones (VER CONVENTIONS.md).

## 🛠 Control de Calidad e Integridad
Para garantizar la estabilidad del motor (especialmente el renderizado visual), el proyecto utiliza un sistema de **Golden Snapshot Testing**.

### Validación Visual (`GoldenVisual.wlt`)
Debido a la complejidad de las estructuras `Boxes` de Mathematica, se utiliza un "Test de Oro" que compara el `InputForm` del renderizado actual contra una huella digital validada.

**Para ejecutar los tests:**
Desde una celda de Mathematica:
```wolfram
SetDirectory["Ruta/A/ToolkitModules"];
Get["Source/Visual.wl"];
TestReport["Tests/GoldenVisual.wlt"]