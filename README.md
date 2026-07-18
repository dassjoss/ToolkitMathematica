# TensorToolkit (Modular Version)

## Objetivo
Motor de alta velocidad para Wolfram Language y xAct. Permite definir tensores, realizar cálculos de relatividad general y exportar resultados con calidad editorial (LaTeX) de forma automatizada.

## Arquitectura de Directorios (Estándar Paclet)
- `Source/`: Lógica pura y funciones del motor (Encapsulados como Paquetes).
  - `Core.wl`: Registro de índices y creación rápida de tensores (`FastTensor`).
  - `Visual.wl`: Motor de renderizado y escalonado de índices.
  - `Calculus.wl`: Derivadas covariantes y álgebra.
  - `Splits.wl`: Sistema de ruptura de índices (3+1 ADM).
- `Kernel/init.wl`: Cargador oficial del paquete.
- `Tests/`: Suites de validación automatizada (.wlt).
- `docs/`: Reglas de oro y convenciones.

## 🛠 Control de Calidad e Integridad
El proyecto cuenta con un sistema de **Integración Continua** manual para asegurar que ningún cambio en la lógica rompa la estética o los cálculos.

### Ejecución de la Suite Completa
Para validar todo el toolkit (Visual y Core) de una sola vez, ejecuta en Mathematica:
```wolfram
SetDirectory["/ruta/a/ToolkitModules"];
Get["Tests/RunAllTests.wl"]