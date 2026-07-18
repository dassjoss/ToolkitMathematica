# Convenciones y Reglas de Oro (Anti-Bug)

Para evitar alucinaciones y errores de Kernel, toda modificación debe seguir estas reglas:

## 1. Gestión de Índices
- **Prohibición:** NUNCA usar `I` o `J` como índices (son reservados por Wolfram). 
- **Solución:** Usar siempre `II, JJ, KK, LL` para índices internos.
- **Reserva:** Los índices `a, b, c, d, mu, nu, rho, sigma` están reservados para manifolds.

## 2. Definiciones de xAct
- **Carga Limpia:** No usar `Quiet` al cargar xAct. No usar `If[!NameQ]` para definir manifolds (Bug #2).
- **Creación de Tensores:** NUNCA usar `DefTensor` directamente. Usar siempre el wrapper `DefTensorF` (Bug #6).

## 3. Visualización (Visual.wl)
- **Cajas, no Texto:** El output debe ser `Boxes` (`TagBox`, `SubsuperscriptBox`). Prohibido devolver `Strings` planos o LaTeX crudo en el output del notebook.
- **Separación de Índices:** Usar `Row[]` para índices múltiples, NUNCA `StringJoin` (Bug #10).
- **No Intrusión:** Prohibido usar `Unprotect[Plus]` o `Unprotect[Times]`. Usar `Format` específico por tensor.

## 4. xCoba y Splits
- **Abstracción:** No todo se componentiza. Mantener índices de Lorentz abstractos mientras los griegos se dividen (ADM).
- **Linealidad:** `AutoSplit` debe distribuir sobre sumas y no duplicar constantes como $\lambda$.