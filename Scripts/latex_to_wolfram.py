import sys
import json
import os
import re
import subprocess

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
REGISTRY_PATH = os.path.join(SCRIPT_DIR, 'registry.json')

def load_registry():
    if os.path.exists(REGISTRY_PATH):
        try:
            with open(REGISTRY_PATH, 'r') as f:
                return json.load(f)
        except: pass
    return {"tensors": {}, "operators": {}, "constants": {}}

def get_clipboard_linux():
    try:
        result = subprocess.run(['wl-paste', '-t', 'text', '--no-newline'],
                                capture_output=True, text=True, check=True)
        return result.stdout.strip()
    except:
        return ""

def parse_indices(latex_part):
    r"""
    Extrae indices inteligentemente.
    Convierte \mu -> mu, y mantiene mu como mu (sin separarlo en m, u).
    """
    latex_part = latex_part.strip().strip('{}')
    protected_names = [
        "mu", "nu", "rho", "sigma", "si", "tau", "alpha", "beta",
        "gamma", "delta", "lambda", "la", "kappa", "ka", "epsilon",
        "phi", "psi", "chi", "eta", "zeta", "xi"
    ]
    pattern = r'\\[a-zA-Z]+|' + '|'.join(protected_names) + r'|[a-zA-Z]'
    tokens = re.findall(pattern, latex_part)
    return [t.replace('\\', '').strip() for t in tokens]


def process_operators(text, operators):
    """
    Reemplaza \nabla / \partial (y cualquier operador definido en el registro)
    por su forma Wolfram, soportando ENCADENAMIENTO correcto:
    \nabla_{\mu} \nabla^{\mu} \phi  ->  CD[-mu][CD[mu][phi]]

    Se implementa como un mini parser recursivo en vez de un re.sub plano:
    así el "target" de un operador nunca puede tragarse literalmente el
    siguiente operador (la causa del bug original), y en su lugar el
    siguiente operador se procesa como el operando anidado.
    """
    if not operators:
        return text

    op_names = sorted(operators.keys(), key=len, reverse=True)
    op_alt = '|'.join(re.escape(o) for o in op_names)

    single_op_re = re.compile(
        r'\\(' + op_alt + r')(?:([_^])(\{.*?\}|[a-zA-Z\\]+))?\s*'
    )
    # Un "target" crudo: nombre + bloques de indices opcionales.
    target_re = re.compile(r'[a-zA-Z\\]+(?:[_^](?:\{.*?\}|[a-zA-Z\\]+))*')
    scan_re = re.compile(r'\\(?:' + op_alt + r')')

    def parse_target(pos):
        m = target_re.match(text, pos)
        if not m or not m.group(0):
            return "", pos
        return m.group(0).replace('\\', ''), m.end()

    def parse_from(pos):
        m = single_op_re.match(text, pos)
        if not m:
            return parse_target(pos)

        opname = m.group(1)
        sign_char = m.group(2)
        idx_content = m.group(3)
        wolf_op = operators[opname]
        sign = "-" if sign_char == "_" else ""

        if idx_content:
            idxs = parse_indices(idx_content)
            formatted_idxs = ", ".join(f"{sign}{i}" for i in idxs)
        else:
            formatted_idxs = ""

        newpos = m.end()
        inner_str, newpos = parse_from(newpos)  # recursion: puede ser otro operador o el target final

        if inner_str:
            return f"{wolf_op}[{formatted_idxs}][{inner_str}]", newpos
        return f"{wolf_op}[{formatted_idxs}]", newpos

    result = []
    pos = 0
    while pos < len(text):
        m = scan_re.search(text, pos)
        if not m:
            result.append(text[pos:])
            break
        result.append(text[pos:m.start()])
        replaced, newpos = parse_from(m.start())
        result.append(replaced)
        pos = newpos
    return "".join(result)


def translate(text):
    if not text: return ""
    registry = load_registry()
    tensors = registry.get("tensors", {}).keys()
    operators = registry.get("operators", {})
    constants = registry.get("constants", {})

    # 1. Limpieza basica de ruido LaTeX
    text = text.strip().replace('$', '')
    text = re.sub(r'\\(,|;|!|quad|qquad|left|right)', '', text)

    # 2. Fracciones: \frac{a}{b} -> ((a)/(b))
    while r'\frac' in text:
        text = re.sub(r'\\frac\{((?:[^{}]|\{[^{}]*\})*)\}\{((?:[^{}]|\{[^{}]*\})*)\}', r'((\1)/(\2))', text)

    # 3. Raices: \sqrt{x} -> Sqrt[x]
    while r'\sqrt' in text:
        text = re.sub(r'\\sqrt\{((?:[^{}]|\{[^{}]*\})*)\}', r'Sqrt[\1]', text)

    # 4. Quitar decoradores visuales: \mathcal{R} -> R, \mathbf{g} -> g
    text = re.sub(r'\\math[a-z]+\{([a-zA-Z\\]+)\}', r'\1', text)

    # 5. Constantes y Mapeos Globales (SE MUEVE AQUI, antes de tensores,
    #    para que "G_grav" no sea interpretado como el tensor "G" con
    #    indices sueltos "g","r","a","v" cuando "G" ya esta registrado
    #    como tensor). Se usan limites de palabra (\b) para evitar
    #    coincidencias parciales dentro de otros nombres.
    text = text.replace(r'\pi', 'Pi').replace(r'\infty', 'Infinity')
    for tex_const, wolf_const in constants.items():
        text = re.sub(r'\\' + re.escape(tex_const) + r'\b', wolf_const, text)
        text = re.sub(r'\b' + re.escape(tex_const) + r'\b', wolf_const, text)

    # 6. Procesar Operadores (\nabla, \partial) - version recursiva (ver process_operators)
    text = process_operators(text, operators)

    # 7. Procesar Tensores (g_{\mu\nu}, R^{\alpha}_{\beta\mu\nu})
    tensor_pattern = r'([a-zA-Z\\]+)(?:([_^])(\{.*?\}|[a-zA-Z\\]+))+(?:([_^])(\{.*?\}|[a-zA-Z\\]+))*'

    def tensor_replacer(m):
        raw_name = m.group(1).replace('\\', '')
        if raw_name in tensors:
            wolf_name = registry["tensors"][raw_name].get("wolfram", raw_name)
            all_blocks = re.findall(r'([_^])(\{.*?\}|[a-zA-Z\\]+)', m.group(0))
            all_formatted_indices = []
            for sign_char, content in all_blocks:
                sign = "-" if sign_char == "_" else ""
                idxs = parse_indices(content)
                all_formatted_indices.extend([f"{sign}{i}" for i in idxs])
            return f"{wolf_name}[{', '.join(all_formatted_indices)}]"
        return m.group(0)

    text = re.sub(tensor_pattern, tensor_replacer, text)

    # 8. Limpieza final de barras invertidas sueltas
    text = re.sub(r'\\([a-zA-Z]+)', r'\1', text)

    # 9. "=" de ecuacion -> "==" (igualdad simbolica de Wolfram).
    #    En Wolfram, "=" es asignacion inmediata (Set) y falla si el LHS
    #    no es un simbolo simple (Set::write, Tag Plus is Protected, etc.)
    #    cuando el LHS es una expresion tensorial compuesta como en la
    #    ecuacion de Einstein. No convertimos si ya es "==" (idempotente).
    text = re.sub(r'(?<!=)=(?!=)', '==', text)

    text = re.sub(r'\s+', ' ', text).strip()

    return text

if __name__ == "__main__":
    if len(sys.argv) > 1:
        input_text = " ".join(sys.argv[1:])
    else:
        input_text = get_clipboard_linux()

    print(translate(input_text))
