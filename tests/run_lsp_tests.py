from __future__ import annotations

import importlib.util
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
LSP_PATH = ROOT / "scripts" / "i_lsp.py"
TEST_DIR = ROOT / "build" / "i_lsp_tests"


def load_lsp():
    spec = importlib.util.spec_from_file_location("i_lsp", LSP_PATH)
    if spec is None or spec.loader is None:
        raise RuntimeError("failed to load i_lsp.py")
    module = importlib.util.module_from_spec(spec)
    sys.modules["i_lsp"] = module
    spec.loader.exec_module(module)
    return module


def decoded_semantic_tokens(lsp, data: list[int]) -> list[tuple[int, int, int, str]]:
    out: list[tuple[int, int, int, str]] = []
    line = 0
    start = 0
    for i in range(0, len(data), 5):
        delta_line, delta_start, length, token_type, _mods = data[i : i + 5]
        line += delta_line
        start = delta_start if delta_line else start + delta_start
        out.append((line, start, length, lsp.SEMANTIC_TOKEN_TYPES[token_type]))
    return out


def decoded_semantic_tokens_with_modifiers(lsp, data: list[int]) -> list[tuple[int, int, int, str, set[str]]]:
    out: list[tuple[int, int, int, str, set[str]]] = []
    line = 0
    start = 0
    for i in range(0, len(data), 5):
        delta_line, delta_start, length, token_type, mods = data[i : i + 5]
        line += delta_line
        start = delta_start if delta_line else start + delta_start
        modifier_names = {
            name
            for index, name in enumerate(lsp.SEMANTIC_TOKEN_MODIFIERS)
            if mods & (1 << index)
        }
        out.append((line, start, length, lsp.SEMANTIC_TOKEN_TYPES[token_type], modifier_names))
    return out


def has_semantic_token(
    tokens: list[tuple[int, int, int, str, set[str]]],
    line: int,
    start: int,
    length: int,
    kind: str,
    *modifiers: str,
) -> bool:
    required = set(modifiers)
    return any(
        token_line == line
        and token_start == start
        and token_length == length
        and token_kind == kind
        and required.issubset(token_modifiers)
        for token_line, token_start, token_length, token_kind, token_modifiers in tokens
    )


def main() -> int:
    lsp = load_lsp()
    TEST_DIR.mkdir(parents=True, exist_ok=True)
    module = TEST_DIR / "shared.i"
    duplicate_module = TEST_DIR / "duplicate.i"
    value_duplicate_module = TEST_DIR / "value_duplicate.i"
    c_header_import_module = TEST_DIR / "c_header_import.i"
    import_completion_module_dir = TEST_DIR / "modules"
    import_completion_module_dir.mkdir(parents=True, exist_ok=True)
    import_completion_nested_module = import_completion_module_dir / "nested.i"
    cinclude_nested_header = import_completion_module_dir / "nested.h"
    cinclude_header = TEST_DIR / "vendor.h"
    cinclude_completion = TEST_DIR / "cinclude_completion.i"
    import_completion = TEST_DIR / "import_completion.i"
    compiler_diag_app = TEST_DIR / "compiler_diag.i"
    compiler_dirty_app = TEST_DIR / "compiler_dirty.i"
    completion_context_app = TEST_DIR / "completion_context.i"
    app = TEST_DIR / "app.i"
    module_dot_path = f"{TEST_DIR.as_posix()}/./shared.i"
    module.write_text(
        """
Payload:struct = {
    value:i32 @ "editor,serialize";
    values:[4]i32;
}

CallbackBase:alias = *proc(payload:*Payload, amount:i32)->i32;
Callback:alias = CallbackBase;
Vec3:alias = [3]f32;
Mat3:alias = [3]Vec3;

Handler:struct = {
    cb:Callback;
}

global_payload:Payload = {};
global_cb:Callback = payload_add;

Array:struct<T> = {
    length:u64;
    data:*T;
}

Array<T>reserve:proc<T>(length:u64)->Array<T> = {
    out:Array<T> = {};
    out.length = length;
    return out;
}

Vec:struct<T> = {
    length:u64;
    data:*T;
}

Vec<T>reserve:proc<T>(length:u64)->Vec<T> = {
    out:Vec<T> = {};
    out.length = length;
    return out;
}

json_read:proc<i32>(out:*i32)->b32 = { external; }

json_read:proc<Array<T>>(out:*Array<T>)->b32 = { external; }

json_read:proc<Vec<T>>(out:*Vec<T>)->b32 = { external; }

Other:struct = {
    value:i32;
}

BadFields:struct = {
    value:i32;
    value:f32;
}

Kind:enum = {
    None,
    Ready,
}

BadKind:enum = {
    One,
    One,
}

payload_add:proc(p:*Payload, amount:i32)->i32 = {
    return p[0].value + amount;
}

consume_payload:proc(payload:Payload)->i32 = {
    return payload.value;
}

mix3:proc(a:i32, b:i32, c:i32)->i32 = {
    return a + b + c;
}

text_amount:proc(text:*const char, amount:i32)->i32 = {
    return amount;
}

bad_params:proc(value:i32, value:i32)->i32 = {
    return value;
}

bad_param_local:proc(value:i32)->i32 = {
    value:i32 = 1;
    return value;
}
""".strip()
        + "\n",
        encoding="utf-8",
        newline="\n",
    )
    duplicate_module.write_text(
        """
payload_add:proc()->i32 = {
    return 0;
}

global_payload:Payload = {};
""".strip()
        + "\n",
        encoding="utf-8",
        newline="\n",
    )
    value_duplicate_module.write_text(
        """
shared_value:proc()->i32 = {
    return 1;
}
""".strip()
        + "\n",
        encoding="utf-8",
        newline="\n",
    )
    c_header_import_module.write_text(
        """
import "stdio.h"
""".strip()
        + "\n",
        encoding="utf-8",
        newline="\n",
    )
    import_completion_nested_module.write_text("Nested:struct = { external; }\n", encoding="utf-8", newline="\n")
    cinclude_header.write_text("#pragma once\n#define VENDOR_H 1\n", encoding="utf-8", newline="\n")
    cinclude_nested_header.write_text("#pragma once\n#define NESTED_H 1\n", encoding="utf-8", newline="\n")
    import_completion.write_text(
        """
import "sh
import "modules/"
""".strip()
        + "\n",
        encoding="utf-8",
        newline="\n",
    )
    cinclude_completion.write_text(
        """
cinclude "vendor.h"
cinclude "ven
cinclude "modules/"
""".strip()
        + "\n",
        encoding="utf-8",
        newline="\n",
    )
    compiler_diag_app.write_text(
        """
main:proc()->i32 = {
    value:i32 = missing_symbol;
    return value;
}
""".strip()
        + "\n",
        encoding="utf-8",
        newline="\n",
    )
    compiler_dirty_app.write_text(
        """
main:proc()->i32 = {
    return 0;
}
""".strip()
        + "\n",
        encoding="utf-8",
        newline="\n",
    )
    completion_context_app.write_text(
        f"""
import "{module.as_posix()}"

main:proc()->i32 = {{
    payload:Payload = {{ .value = 1, . }};
    k:Kind = K;
    dot_kind:Kind = Kind.;
    value:i32 = 1;
    score:i32 = v;
    payload_ptr:*Payload = p;
    payload_type_field:i32 = Payload.;
    payload_reflect_field:*const char = Payload<>.;
    kind_reflect_field:u64 = Kind<>.;
    call_struct_field:i32 = consume_payload({{ . }});
    payload_array:Array<Payload> = Array<Payload>r;
    payload_array_empty:Array<Payload> = Array<Payload>;
    read_payloads:b32 = json_read<Array<Payload>>(
    read_value:b32 = json_read<i32>(
    return 0;
}}
""".strip()
        + "\n",
        encoding="utf-8",
        newline="\n",
    )
    app.write_text(
        f"""
import "{module.as_posix()}"
import "{module_dot_path}"
import "{duplicate_module.as_posix()}"
import "{value_duplicate_module.as_posix()}"
import "{c_header_import_module.as_posix()}"

shared_value:i32 = 2;

helper_shadow:proc()->i32 = {{
    total:i32 = 100;
    global_payload:Other = {{}};
    global_payload.value = total;
    global_payload.values[0] = total;
    return total;
}}

main:proc()->i32 = {{
    p:Payload = {{}};
    o:Other = {{}};
    cb:Callback = payload_add;
    handler:Handler = {{}};
    k:Kind = Kind_Ready;
    values:Array<i32> = Array<i32>reserve(4);
    payload_values:Array<Payload> = Array<Payload>reserve(2);
    payload_vec:Vec<Payload> = Vec<Payload>reserve(2);
    read_payloads:b32 = json_read<Array<Payload>>(payload_values.&);
    read_payload_vec:b32 = json_read<Vec<Payload>>(payload_vec.&);
    read_value:b32 = json_read<i32>(total.&);
    basis:Mat3 = {{}};
    basis_element:f32 = basis[0][0];
    payload_ptr:*Payload = p.&;
    total:i32 = 0;
    bad_decl:i32 = p;
    p.value = 4;
    p.value = p;
    total = p;
    payload_ptr = p;
    payload_ptr[0].value = 5;
    payload_values.data[0].value = 6;
    global_payload.value = 7;
    o.value = 8;
    p.missing = 9;
    payload_ptr[0].missing = 10;
    payload_values.data[0].missing = 11;
    values.data[0].missing = 12;
    total = payload_add(p.&, 3);
    total = cb(p.&, 4);
    total = payload_add(p, 3);
    total = cb(p.&, p);
    total = handler.cb(p, 4);
    total = global_cb(p.&, p);
    total = payload_add(p.&);
    total = cb(p.&, 4, 5);
    total = mix3(payload_add(p.&, 1), 2, 3);
    total = text_amount("a,b", 5);
    total = handler.cb(p.&, 4);
    total = handler.cb(p.&);
    total = global_cb(p.&, 4);
    total = global_cb(p.&);
    dup:i32 = 1;
    dup:i32 = 2;
    while (total < 10) {{
        total += 1;
        continue;
    }}
    switch (total) {{
        case 1:
            break;
        default:
            continue;
    }}
    break;
    continue;
    return total;
}}
""".strip()
        + "\n",
        encoding="utf-8",
        newline="\n",
    )

    workspace = lsp.Workspace()
    doc = workspace.open_path(app)
    import_completion_doc = workspace.open_path(import_completion)
    cinclude_completion_doc = workspace.open_path(cinclude_completion)
    completion_context_doc = workspace.open_path(completion_context_app)

    class CaptureServer(lsp.LspServer):
        def __init__(self) -> None:
            super().__init__()
            self.sent: list[dict] = []
            self.diagnostic_debounce_seconds = 3600.0
            self.workspace_symbol_debounce_seconds = 3600.0

        def send(self, payload: dict) -> None:
            self.sent.append(payload)

    fallback_doc = lsp.Document(
        "untitled:i-fallback",
        None,
        "",
        [],
        {},
        {},
        [],
        [],
        [],
        [lsp.Diagnostic(0, 0, "python fallback diagnostic")],
    )

    class CompilerTruthServer(CaptureServer):
        def compiler_diagnostics(self, doc: lsp.Document) -> tuple[bool, list[lsp.Diagnostic]]:
            return True, []

    compiler_truth_server = CompilerTruthServer()
    compiler_truth_server.notify_diagnostics(fallback_doc)
    compiler_truth_publish = compiler_truth_server.sent[-1].get("params", {}) if compiler_truth_server.sent else {}
    if compiler_truth_publish.get("diagnostics") != []:
        print("lsp: compiler-backed diagnostics should suppress Python semantic fallback diagnostics")
        print(compiler_truth_publish)
        return 1

    class CompilerFallbackServer(CaptureServer):
        def compiler_diagnostics(self, doc: lsp.Document) -> tuple[bool, list[lsp.Diagnostic]]:
            return False, []

    compiler_fallback_server = CompilerFallbackServer()
    compiler_fallback_server.notify_diagnostics(fallback_doc)
    compiler_fallback_publish = compiler_fallback_server.sent[-1].get("params", {}) if compiler_fallback_server.sent else {}
    fallback_diag = next(
        (
            diag
            for diag in compiler_fallback_publish.get("diagnostics", [])
            if "python fallback diagnostic" in diag.get("message", "")
        ),
        None,
    )
    if fallback_diag is None or fallback_diag.get("source") != "i-lsp":
        print("lsp: Python diagnostics should publish when compiler diagnostics are unavailable")
        print(compiler_fallback_publish)
        return 1

    structured_doc = lsp.Document(
        "untitled:i-structured-symbols",
        None,
        "",
        [],
        {},
        {},
        [],
        [],
        [],
        [],
    )
    structured_field = lsp.compiler_symbol_to_lsp_symbol(
        structured_doc,
        {
            "kind": "field",
            "owner": "Payload",
            "name": "value",
            "file": "",
            "line": 1,
            "column": 1,
            "detail": "Payload.value: WrongType",
            "type": "i32",
        },
    )
    structured_generic_field = lsp.compiler_symbol_to_lsp_symbol(
        structured_doc,
        {
            "kind": "field",
            "owner": "Crate",
            "name": "item",
            "file": "",
            "line": 1,
            "column": 1,
            "detail": "Crate.item: Item",
            "type": "Item",
            "type_param": "Item",
        },
    )
    structured_variable = lsp.compiler_symbol_to_lsp_symbol(
        structured_doc,
        {
            "kind": "variable",
            "name": "value",
            "file": "",
            "line": 1,
            "column": 1,
            "detail": "value: WrongType",
            "type": "i32",
        },
    )
    structured_alias = lsp.compiler_symbol_to_lsp_symbol(
        structured_doc,
        {
            "kind": "alias",
            "name": "Callback",
            "file": "",
            "line": 1,
            "column": 1,
            "detail": "Callback:alias = WrongType;",
            "target_type": "*proc(wrong:*Payload)->void",
            "params": [{"name": "x", "type": "i32"}],
            "return_type": "i32",
            "variadic": False,
        },
    )
    structured_generic_proc = lsp.compiler_symbol_to_lsp_symbol(
        structured_doc,
        {
            "kind": "proc",
            "name": "Crate<T>make",
            "file": "",
            "line": 1,
            "column": 1,
            "detail": "Crate<T>make:proc<T>(wrong:Crate<T>)->T",
            "params": [{"name": "box", "type": "Crate<Item>"}],
            "return_type": "Item",
            "type_param": "Item",
        },
    )
    structured_proc = lsp.compiler_symbol_to_lsp_symbol(
        structured_doc,
        {
            "kind": "proc",
            "name": "make_value",
            "file": "",
            "line": 1,
            "column": 1,
            "detail": "make_value:proc(wrong:*Payload)->WrongReturn",
            "params": [{"name": "value", "type": "i32"}],
            "return_type": "i32",
        },
    )
    structured_enum_member = lsp.compiler_symbol_to_lsp_symbol(
        structured_doc,
        {
            "kind": "enumMember",
            "name": "Misleading_Name",
            "file": "",
            "line": 1,
            "column": 1,
            "detail": "Wrong.Owner: enum member",
            "owner": "Kind",
            "item": "Ready",
        },
    )
    if (
        not isinstance(structured_field, lsp.FieldSymbol)
        or structured_field.type_name != "i32"
        or structured_field.type_param != ""
        or not isinstance(structured_generic_field, lsp.FieldSymbol)
        or structured_generic_field.type_param != "Item"
        or lsp.field_with_owner_type(structured_generic_field, "Crate<i32>").type_name != "i32"
        or not isinstance(structured_variable, lsp.VariableSymbol)
        or structured_variable.type_name != "i32"
        or not isinstance(structured_alias, lsp.Symbol)
        or structured_alias.target_type != "*proc(wrong:*Payload)->void"
        or structured_alias.params != ("x:i32",)
        or structured_alias.return_type != "i32"
        or not isinstance(structured_generic_proc, lsp.Symbol)
        or structured_generic_proc.type_param != "Item"
        or lsp.proc_parameter_labels_for_symbol(structured_generic_proc, "Crate<i32>make") != ["box:Crate<i32>"]
        or lsp.proc_return_type_for_symbol(structured_generic_proc, "Crate<i32>make") != "i32"
        or lsp.proc_signature_label_for_symbol(structured_generic_proc, "Crate<i32>make") != "Crate<i32>make:proc(box:Crate<i32>)->i32"
        or not isinstance(structured_proc, lsp.Symbol)
        or structured_proc.params != ("value:i32",)
        or structured_proc.return_type != "i32"
        or not isinstance(structured_enum_member, lsp.Symbol)
        or lsp.enum_member_parts(structured_enum_member) != ("Kind", "Ready")
    ):
        print("lsp: compiler symbol ingestion should prefer structured metadata over display detail")
        print(structured_field, structured_variable, structured_alias, structured_generic_proc, structured_proc, structured_enum_member)
        return 1
    structured_workspace = lsp.Workspace()
    structured_workspace.documents[structured_doc.uri] = lsp.Document(
        structured_doc.uri,
        None,
        "",
        [structured_alias, structured_proc],
        {},
        {"value": lsp.VariableSymbol("value", "i32", structured_doc.uri, 1, 1, "value: i32")},
        [],
        [],
        [],
        [],
    )
    structured_workspace.reindex()
    if lsp.proc_signature_detail_for_type(structured_workspace, "Callback") != "*proc(x:i32)->i32":
        print("lsp: alias proc signature resolution should prefer compiler-backed params/return metadata")
        print(lsp.proc_signature_detail_for_type(structured_workspace, "Callback"))
        return 1
    if lsp.proc_parameter_labels_for_type(structured_workspace, "Callback") != ["x:i32"]:
        print("lsp: alias proc parameters should prefer compiler-backed params metadata")
        print(lsp.proc_parameter_labels_for_type(structured_workspace, "Callback"))
        return 1
    structured_alias_hover = lsp.hover_markdown_for_symbol(structured_workspace, structured_alias)
    if "resolves to `*proc(x:i32)->i32`" not in structured_alias_hover:
        print("lsp: alias hover should prefer compiler-backed proc params/return metadata")
        print(structured_alias_hover)
        return 1
    if lsp.callable_parameter_labels_for_name(structured_workspace, structured_doc, "make_value", 1) != ["value:i32"]:
        print("lsp: callable parameters should prefer compiler-backed proc metadata")
        print(lsp.callable_parameter_labels_for_name(structured_workspace, structured_doc, "make_value", 1))
        return 1
    if lsp.callable_return_type_for_name(structured_workspace, structured_doc, "make_value", 1) != "i32":
        print("lsp: callable return type should prefer compiler-backed proc metadata")
        print(lsp.callable_return_type_for_name(structured_workspace, structured_doc, "make_value", 1))
        return 1
    if lsp.infer_simple_expr_type(structured_workspace, structured_doc, "make_value(value)", 1) != "i32":
        print("lsp: call expression inference should prefer compiler-backed proc return metadata")
        print(lsp.infer_simple_expr_type(structured_workspace, structured_doc, "make_value(value)", 1))
        return 1
    structured_proc_hover = lsp.hover_markdown_for_symbol(structured_workspace, structured_proc)
    if "`make_value:proc(value:i32)->i32`" not in structured_proc_hover or "WrongReturn" in structured_proc_hover:
        print("lsp: proc hover should prefer compiler-backed proc params/return metadata")
        print(structured_proc_hover)
        return 1
    structured_proc_completion = lsp.completion_to_lsp(structured_workspace, structured_proc)
    if (
        structured_proc_completion.get("detail") != "make_value:proc(value:i32)->i32"
        or structured_proc_completion.get("insertTextFormat") != 2
        or structured_proc_completion.get("insertText") != "make_value(${1:value})$0"
    ):
        print("lsp: proc completion detail should prefer compiler-backed proc params/return metadata")
        print(structured_proc_completion)
        return 1
    enum_usage_uri = lsp.path_to_uri(TEST_DIR / "structured_enum_usage.i")
    enum_usage_text = (
        "Mode:enum = {\n"
        "    Ready,\n"
        "}\n"
        "\n"
        "main:proc()->i32 = {\n"
        "    Mode_Ready;\n"
        "    Mode.Ready;\n"
        "    return 0;\n"
        "}\n"
    )
    enum_usage_lines = enum_usage_text.splitlines()
    enum_usage_doc = lsp.Document(
        enum_usage_uri,
        None,
        enum_usage_text,
        [
            lsp.Symbol("Mode", "enum", enum_usage_uri, 0, 0, "Mode:enum = {", len("Mode")),
            lsp.Symbol(
                name="Wrong_Ready",
                kind="enumMember",
                uri=enum_usage_uri,
                line=1,
                col=4,
                detail="Wrong.Ready: enum member",
                source_len=len("Ready"),
                enum_owner="Mode",
                enum_item="Ready",
            ),
        ],
        {},
        {},
        [],
        [],
        [],
        [],
    )
    enum_usage_workspace = lsp.Workspace()
    enum_usage_workspace.documents[enum_usage_uri] = enum_usage_doc
    enum_usage_workspace.reindex()
    enum_usage_member = enum_usage_workspace.find_enum_member_usage("Mode_Ready")
    enum_usage_line = next(i for i, line in enumerate(enum_usage_lines) if "Mode_Ready" in line)
    enum_usage_col = enum_usage_lines[enum_usage_line].index("Mode_Ready")
    enum_dot_usage_line = next(i for i, line in enumerate(enum_usage_lines) if "Mode.Ready" in line)
    enum_dot_usage_col = enum_usage_lines[enum_dot_usage_line].index("Ready")
    enum_usage_tokens = decoded_semantic_tokens(lsp, lsp.semantic_tokens_for_doc(enum_usage_workspace, enum_usage_doc))
    enum_usage_server = lsp.LspServer()
    enum_usage_server.workspace = enum_usage_workspace
    enum_usage_resolved = enum_usage_server.symbol_at_request(
        {
            "textDocument": {"uri": enum_usage_uri},
            "position": {"line": enum_usage_line, "character": enum_usage_col},
        }
    )
    enum_usage_refs = enum_usage_workspace.enum_member_references(enum_usage_member) if enum_usage_member else []
    enum_usage_rename = enum_usage_workspace.enum_member_rename_edits(enum_usage_member, "Done") if enum_usage_member else {}
    enum_usage_rename_edits = [
        (uri, edit["range"]["start"]["line"], edit["range"]["start"]["character"], edit["newText"])
        for uri, edits in enum_usage_rename.get("changes", {}).items()
        for edit in edits
    ]
    if (
        not isinstance(enum_usage_member, lsp.Symbol)
        or enum_usage_member.name != "Wrong_Ready"
        or enum_usage_resolved != enum_usage_member
        or (enum_usage_line, enum_usage_col, len("Mode_Ready"), "enumMember") not in enum_usage_tokens
        or (enum_dot_usage_line, enum_dot_usage_col, len("Ready"), "enumMember") not in enum_usage_tokens
        or len(enum_usage_refs) != 2
        or not any(ref.get("range", {}).get("start", {}).get("line") == 1 for ref in enum_usage_refs)
        or not any(ref.get("range", {}).get("start", {}).get("line") == enum_usage_line for ref in enum_usage_refs)
        or (enum_usage_uri, 1, 4, "Done") not in enum_usage_rename_edits
        or (enum_usage_uri, enum_usage_line, enum_usage_col, "Mode_Done") not in enum_usage_rename_edits
    ):
        print("lsp: enum usage lookup/references/rename should prefer compiler-backed owner/item metadata")
        print(enum_usage_member, enum_usage_resolved, enum_usage_tokens, enum_usage_refs, enum_usage_rename)
        return 1

    compiler_symbol_server = CaptureServer()
    compiler_symbol_uri = lsp.path_to_uri(TEST_DIR / "compiler_symbol_preference.i")
    real_run_compiler_symbols = lsp.run_compiler_symbols
    try:
        def fake_run_compiler_symbols(doc: lsp.Document, include_imports: bool = False):
            return True, [
                lsp.Symbol("CompilerOnly", "struct", doc.uri, 0, 0, "CompilerOnly:struct = {", len("CompilerOnly")),
                lsp.Symbol("Mode", "enum", doc.uri, 2, 0, "Mode:enum = {", len("Mode")),
                lsp.Symbol(
                    name="Wrong_Ready",
                    kind="enumMember",
                    uri=doc.uri,
                    line=3,
                    col=4,
                    detail="Wrong.Ready: enum member",
                    source_len=len("Ready"),
                    enum_owner="Mode",
                    enum_item="Ready",
                ),
                lsp.Symbol(
                    "compiler_take",
                    "proc",
                    doc.uri,
                    2,
                    0,
                    "compiler_take:proc(wrong:*Payload)->void",
                    len("compiler_take"),
                    ("value:i32",),
                    "i32",
                ),
                lsp.Symbol("main", "proc", doc.uri, 4, 0, "main:proc()->i32", len("main"), (), "i32"),
            ], [
                lsp.VariableSymbol("compiler_global", "i32", doc.uri, 1, 0, "compiler_global: i32", "global"),
                lsp.VariableSymbol("compiler_param", "i32", doc.uri, 4, 10, "compiler_param: i32", "parameter", "main"),
                lsp.VariableSymbol("compiler_local", "i32", doc.uri, 6, 4, "compiler_local: i32", "variable", "main"),
                lsp.VariableSymbol("other_scope_local", "i32", doc.uri, 6, 4, "other_scope_local: i32", "variable", "other_proc"),
            ], [
                lsp.FieldSymbol("CompilerOnly", "compiler_field", "i32", "editor", doc.uri, 1, 4, "CompilerOnly.compiler_field: i32")
            ]

        lsp.run_compiler_symbols = fake_run_compiler_symbols
        compiler_symbol_text = (
            "PythonOnly:struct = {\n"
            "    python_field:i32;\n"
            "}\n"
            "\n"
            "Mode:enum = {\n"
            "    Ready,\n"
            "}\n"
            "\n"
            "main:proc()->i32 = {\n"
            "    CompilerOnly;\n"
            "    python_local:i32 = 0;\n"
            "    compiler_local = 1;\n"
            "    compiler_global = 1;\n"
            "    compiler_take(compiler);\n"
            "    return 0;\n"
            "}\n"
        )
        compiler_symbol_doc = compiler_symbol_server.workspace.upsert(compiler_symbol_uri, compiler_symbol_text)
        compiler_symbol_lines = compiler_symbol_text.splitlines()
        compiler_type_line = next(i for i, line in enumerate(compiler_symbol_lines) if "CompilerOnly;" in line)
        compiler_type_col = compiler_symbol_lines[compiler_type_line].index("CompilerOnly")
        compiler_global_line = next(i for i, line in enumerate(compiler_symbol_lines) if "compiler_global" in line)
        compiler_global_col = compiler_symbol_lines[compiler_global_line].index("compiler_global")
        compiler_call_line = next(i for i, line in enumerate(compiler_symbol_lines) if "compiler_take(" in line)
        compiler_call_col = compiler_symbol_lines[compiler_call_line].rindex("compiler")
        compiler_completion_items = compiler_symbol_server.workspace.completion_symbols_for_doc(compiler_symbol_doc)
        if (
            not any(isinstance(item, lsp.Symbol) and item.name == "CompilerOnly" for item in compiler_completion_items)
            or not any(isinstance(item, lsp.Symbol) and item.name == "compiler_take" for item in compiler_completion_items)
            or any(isinstance(item, lsp.Symbol) and item.name == "PythonOnly" for item in compiler_completion_items)
            or compiler_symbol_server.workspace.find_variable(compiler_symbol_doc, "compiler_global") is None
            or compiler_symbol_server.workspace.find_variable(compiler_symbol_doc, "compiler_param") is None
            or compiler_symbol_server.workspace.find_variable(compiler_symbol_doc, "compiler_local") is None
            or compiler_symbol_server.workspace.find_variable(compiler_symbol_doc, "compiler_local").scope != "main"
            or compiler_symbol_server.workspace.find_variable(compiler_symbol_doc, "python_local") is not None
        ):
            print("lsp: workspace completion/index should prefer compiler-backed top-level symbols and variables")
            print(compiler_completion_items)
            return 1
        compiler_fields = compiler_symbol_server.workspace.fields_for_owner("CompilerOnly")
        if (
            not any(field.name == "compiler_field" and field.attrs == "editor" for field in compiler_fields)
            or compiler_symbol_server.workspace.fields_for_owner("PythonOnly")
        ):
            print("lsp: workspace field index should prefer compiler-backed fields")
            print(compiler_symbol_server.workspace.fields)
            return 1
        compiler_symbol_server.handle(
            {
                "jsonrpc": "2.0",
                "id": 99,
                "method": "textDocument/documentSymbol",
                "params": {"textDocument": {"uri": compiler_symbol_uri}},
            }
        )
        compiler_symbol_server.handle(
            {
                "jsonrpc": "2.0",
                "id": 100,
                "method": "textDocument/definition",
                "params": {
                    "textDocument": {"uri": compiler_symbol_uri},
                    "position": {"line": compiler_type_line, "character": compiler_type_col},
                },
            }
        )
        compiler_definition_response = compiler_symbol_server.sent[-1] if compiler_symbol_server.sent else {}
        if (
            compiler_definition_response.get("id") != 100
            or compiler_definition_response.get("result", {}).get("uri") != compiler_symbol_uri
            or compiler_definition_response.get("result", {}).get("range", {}).get("start", {}).get("line") != 0
        ):
            print("lsp: definition should use compiler-backed symbols when available")
            print(compiler_definition_response)
            return 1
        compiler_symbol_server.handle(
            {
                "jsonrpc": "2.0",
                "id": 101,
                "method": "textDocument/hover",
                "params": {
                    "textDocument": {"uri": compiler_symbol_uri},
                    "position": {"line": compiler_type_line, "character": compiler_type_col},
                },
            }
        )
        compiler_hover_response = compiler_symbol_server.sent[-1] if compiler_symbol_server.sent else {}
        if (
            compiler_hover_response.get("id") != 101
            or "`CompilerOnly:struct = {`"
            not in compiler_hover_response.get("result", {}).get("contents", {}).get("value", "")
        ):
            print("lsp: hover should use compiler-backed symbols when available")
            print(compiler_hover_response)
            return 1
        compiler_symbol_server.handle(
            {
                "jsonrpc": "2.0",
                "id": 102,
                "method": "textDocument/completion",
                "params": {
                    "textDocument": {"uri": compiler_symbol_uri},
                    "position": {"line": compiler_global_line, "character": compiler_global_col},
                },
            }
        )
        compiler_completion_response = compiler_symbol_server.sent[-1] if compiler_symbol_server.sent else {}
        compiler_completion_labels = [
            item.get("label") for item in compiler_completion_response.get("result", {}).get("items", [])
        ]
        if (
            compiler_completion_response.get("id") != 102
            or "CompilerOnly" not in compiler_completion_labels
            or "compiler_global" not in compiler_completion_labels
            or "PythonOnly" in compiler_completion_labels
        ):
            print("lsp: textDocument/completion should use compiler-backed workspace symbols and globals")
            print(compiler_completion_response)
            return 1
        compiler_tokens = decoded_semantic_tokens(
            lsp,
            lsp.semantic_tokens_for_doc(compiler_symbol_server.workspace, compiler_symbol_doc),
        )
        compiler_tokens_with_modifiers = decoded_semantic_tokens_with_modifiers(
            lsp,
            lsp.semantic_tokens_for_doc(compiler_symbol_server.workspace, compiler_symbol_doc),
        )
        python_only_line = next(i for i, line in enumerate(compiler_symbol_lines) if "PythonOnly:struct" in line)
        python_only_col = compiler_symbol_lines[python_only_line].index("PythonOnly")
        compiler_local_line = next(i for i, line in enumerate(compiler_symbol_lines) if "compiler_local = 1" in line)
        compiler_local_col = compiler_symbol_lines[compiler_local_line].index("compiler_local")
        if (
            (compiler_type_line, compiler_type_col, len("CompilerOnly"), "type") not in compiler_tokens
            or (compiler_call_line, compiler_symbol_lines[compiler_call_line].index("compiler_take"), len("compiler_take"), "function") not in compiler_tokens
            or (python_only_line, python_only_col, len("PythonOnly"), "type") in compiler_tokens
        ):
            print("lsp: semantic tokens should use compiler-backed symbols over Python-parsed symbols")
            print(compiler_tokens)
            return 1
        if (
            not has_semantic_token(compiler_tokens_with_modifiers, compiler_global_line, compiler_global_col, len("compiler_global"), "variable", "global")
            or not has_semantic_token(compiler_tokens_with_modifiers, compiler_local_line, compiler_local_col, len("compiler_local"), "variable", "local")
        ):
            print("lsp: semantic token modifiers should use compiler-backed variable metadata")
            print(compiler_tokens_with_modifiers)
            return 1
        compiler_signature = lsp.signature_help_at(
            compiler_symbol_server.workspace,
            compiler_symbol_doc,
            compiler_call_line,
            compiler_call_col,
        )
        if (
            compiler_signature is None
            or compiler_signature.get("activeParameter") != 0
            or not compiler_signature.get("signatures")
            or compiler_signature["signatures"][0].get("label") != "compiler_take:proc(value:i32)->i32"
            or compiler_signature["signatures"][0].get("parameters") != [{"label": "value:i32"}]
        ):
            print("lsp: signature help should use compiler-backed proc symbols")
            print(compiler_signature)
            return 1
        compiler_arg_items = lsp.proc_argument_completions_at(
            compiler_symbol_server.workspace,
            compiler_symbol_doc,
            compiler_call_line,
            compiler_call_col,
        )
        if (
            not any(item.get("label") == "compiler_local" and item.get("detail") == "compiler_local: i32" for item in compiler_arg_items)
            or any(item.get("label") == "other_scope_local" for item in compiler_arg_items)
            or any(item.get("label") == "python_local" for item in compiler_arg_items)
        ):
            print("lsp: proc argument completion should use compiler-backed proc signatures and variables")
            print(compiler_arg_items)
            return 1
        compiler_symbol_server.handle(
            {
                "jsonrpc": "2.0",
                "id": 103,
                "method": "workspace/symbol",
                "params": {"query": "compiler"},
            }
        )
        compiler_workspace_symbol_response = compiler_symbol_server.sent[-1] if compiler_symbol_server.sent else {}
        compiler_workspace_symbol_names = [
            item.get("name") for item in compiler_workspace_symbol_response.get("result", [])
        ]
        if (
            compiler_workspace_symbol_response.get("id") != 103
            or "CompilerOnly" not in compiler_workspace_symbol_names
            or "compiler_global" not in compiler_workspace_symbol_names
            or "PythonOnly" in compiler_workspace_symbol_names
        ):
            print("lsp: workspace/symbol should search compiler-backed workspace symbols and globals")
            print(compiler_workspace_symbol_response)
            return 1
    finally:
        lsp.run_compiler_symbols = real_run_compiler_symbols
    compiler_symbol_response = next(
        (message for message in compiler_symbol_server.sent if message.get("id") == 99),
        {},
    )
    compiler_symbol_names = [item.get("name") for item in compiler_symbol_response.get("result", [])]
    if compiler_symbol_names != ["CompilerOnly", "Mode", "compiler_take", "main"]:
        print("lsp: documentSymbol should prefer compiler-backed symbols when available")
        print(compiler_symbol_response)
        return 1
    compiler_symbol_children = compiler_symbol_response.get("result", [{}])[0].get("children", [])
    if not any(child.get("name") == "compiler_field" and child.get("detail") == "i32" for child in compiler_symbol_children):
        print("lsp: documentSymbol should include compiler-backed field children")
        print(compiler_symbol_response)
        return 1
    compiler_mode_symbol = next(
        (item for item in compiler_symbol_response.get("result", []) if item.get("name") == "Mode"),
        {},
    )
    compiler_mode_children = compiler_mode_symbol.get("children", [])
    if not any(child.get("name") == "Ready" and child.get("detail") == "Wrong_Ready" for child in compiler_mode_children):
        print("lsp: documentSymbol should group enum members by compiler-backed owner metadata")
        print(compiler_symbol_response)
        return 1
    compiler_mode_hover = lsp.hover_markdown_for_symbol(
        compiler_symbol_server.workspace,
        compiler_symbol_server.workspace.find_symbol("Mode"),
    )
    if "- `Ready` = `Wrong_Ready`" not in compiler_mode_hover:
        print("lsp: enum hover should list members by compiler-backed owner/item metadata")
        print(compiler_mode_hover)
        return 1

    graph_root = TEST_DIR / "compiler_workspace_root.i"
    graph_import = TEST_DIR / "compiler_workspace_import.i"
    graph_import.write_text("GraphImport:struct = {\n    value:i32;\n}\n", encoding="utf-8", newline="\n")
    graph_root.write_text(
        f"import \"{graph_import.as_posix()}\"\n\nGraphRoot:proc()->i32 = {{\n    return 0;\n}}\n",
        encoding="utf-8",
        newline="\n",
    )
    graph_server = CaptureServer()
    graph_root_uri = lsp.path_to_uri(graph_root)
    graph_import_uri = lsp.path_to_uri(graph_import)
    graph_symbol_calls: list[tuple[str, bool]] = []
    graph_diagnostic_calls: list[str] = []
    real_run_compiler_symbols = lsp.run_compiler_symbols
    real_run_compiler_diagnostics = lsp.run_compiler_diagnostics

    graph_symbols = [
        lsp.Symbol("GraphRoot", "proc", graph_root_uri, 2, 0, "GraphRoot:proc()->i32", len("GraphRoot"), (), "i32"),
        lsp.Symbol("GraphImport", "struct", graph_import_uri, 0, 0, "GraphImport:struct = {", len("GraphImport")),
    ]
    graph_fields = [
        lsp.FieldSymbol("GraphImport", "value", "i32", "", graph_import_uri, 1, 4, "GraphImport.value: i32")
    ]

    try:
        def fake_graph_run_compiler_symbols(doc: lsp.Document, include_imports: bool = False):
            graph_symbol_calls.append((doc.uri, include_imports))
            if not include_imports:
                return False, [], [], []
            return True, graph_symbols, [], graph_fields

        def fake_graph_run_compiler_diagnostics(doc: lsp.Document):
            graph_diagnostic_calls.append(doc.uri)
            return True, []

        lsp.run_compiler_symbols = fake_graph_run_compiler_symbols
        lsp.run_compiler_diagnostics = fake_graph_run_compiler_diagnostics
        graph_server.handle(
            {
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": graph_root_uri,
                        "text": graph_root.read_text(encoding="utf-8"),
                    }
                },
            }
        )
        if graph_symbol_calls:
            print("lsp: didOpen should not run compiler symbols while attaching")
            print(graph_symbol_calls)
            return 1
        graph_server.handle(
            {
                "jsonrpc": "2.0",
                "id": 87,
                "method": "textDocument/semanticTokens/full",
                "params": {"textDocument": {"uri": graph_root_uri}},
            }
        )
        if graph_symbol_calls:
            print("lsp: cold semantic tokens should not synchronously run compiler symbols while idle prefetch is pending")
            print(graph_symbol_calls)
            return 1
        graph_cold_tokens_response = next((message for message in graph_server.sent if message.get("id") == 87), {})
        if "data" not in graph_cold_tokens_response.get("result", {}):
            print("lsp: cold semantic tokens should still return a token payload")
            print(graph_cold_tokens_response)
            return 1
        graph_server.handle(
            {
                "jsonrpc": "2.0",
                "id": 86,
                "method": "textDocument/documentSymbol",
                "params": {"textDocument": {"uri": graph_root_uri}},
            }
        )
        graph_server.handle(
            {
                "jsonrpc": "2.0",
                "id": 85,
                "method": "textDocument/completion",
                "params": {
                    "textDocument": {"uri": graph_root_uri},
                    "position": {"line": 3, "character": 11},
                },
            }
        )
        graph_server.handle(
            {
                "jsonrpc": "2.0",
                "id": 84,
                "method": "textDocument/documentHighlight",
                "params": {
                    "textDocument": {"uri": graph_root_uri},
                    "position": {"line": 3, "character": 11},
                },
            }
        )
        if graph_symbol_calls:
            print("lsp: cold document symbols/completion/highlight should not synchronously run compiler symbols while idle prefetch is pending")
            print(graph_symbol_calls)
            return 1
        graph_cold_symbol_response = next((message for message in graph_server.sent if message.get("id") == 86), {})
        graph_cold_completion_response = next((message for message in graph_server.sent if message.get("id") == 85), {})
        graph_cold_highlight_response = next((message for message in graph_server.sent if message.get("id") == 84), {})
        if not isinstance(graph_cold_symbol_response.get("result"), list):
            print("lsp: cold document symbols should still return a symbol list")
            print(graph_cold_symbol_response)
            return 1
        if "items" not in graph_cold_completion_response.get("result", {}):
            print("lsp: cold completion should still return a completion payload")
            print(graph_cold_completion_response)
            return 1
        if not isinstance(graph_cold_highlight_response.get("result"), list):
            print("lsp: cold document highlight should still return a highlight list")
            print(graph_cold_highlight_response)
            return 1
        graph_server.flush_pending_diagnostics(graph_root_uri)
        if graph_diagnostic_calls != [graph_root_uri] or graph_symbol_calls:
            print("lsp: debounced diagnostics should run only compiler diagnostics, leaving symbols for the idle prefetch")
            print(graph_diagnostic_calls)
            print(graph_symbol_calls)
            return 1
        if graph_root_uri not in graph_server.pending_workspace_symbols:
            print("lsp: diagnostics should not cancel the idle workspace-symbol prefetch")
            print(graph_server.pending_workspace_symbols)
            return 1
        graph_server.flush_pending_workspace_symbols(graph_root_uri)
        if graph_symbol_calls != [(graph_root_uri, True)]:
            print("lsp: idle workspace-symbol prefetch should warm imported symbols with one --symbols=json graph call")
            print(graph_symbol_calls)
            return 1
        graph_symbol_calls.clear()
        graph_server.handle(
            {
                "jsonrpc": "2.0",
                "id": 83,
                "method": "textDocument/semanticTokens/full",
                "params": {"textDocument": {"uri": graph_root_uri}},
            }
        )
        if graph_symbol_calls:
            print("lsp: default semantic tokens should stay lexical after workspace symbols are warm")
            print(graph_symbol_calls)
            return 1
        graph_warm_tokens_response = next((message for message in graph_server.sent if message.get("id") == 83), {})
        if "data" not in graph_warm_tokens_response.get("result", {}):
            print("lsp: warm lexical semantic tokens should still return a token payload")
            print(graph_warm_tokens_response)
            return 1
        graph_diagnostic_calls.clear()
        graph_server.handle(
            {
                "jsonrpc": "2.0",
                "method": "textDocument/didChange",
                "params": {
                    "textDocument": {"uri": graph_root_uri},
                    "contentChanges": [
                        {
                            "text": f"import \"{graph_import.as_posix()}\"\n\nGraphRoot:proc()->i32 = {{\n    return 1;\n}}\n",
                        }
                    ],
                },
            }
        )
        if graph_root_uri not in graph_server.pending_workspace_symbols or graph_symbol_calls:
            print("lsp: dirty edits should keep compiler symbols hot but defer symbol refresh to idle")
            print(graph_server.pending_workspace_symbols)
            print(graph_symbol_calls)
            return 1
        graph_server.flush_pending_diagnostics(graph_root_uri)
        if graph_diagnostic_calls != [graph_root_uri]:
            print("lsp: dirty edits should still publish compiler diagnostics")
            print(graph_diagnostic_calls)
            return 1
        graph_server.flush_pending_workspace_symbols(graph_root_uri)
        if graph_symbol_calls != [(graph_root_uri, True)]:
            print("lsp: flushing after a dirty edit should run one idle compiler symbol refresh")
            print(graph_symbol_calls)
            return 1
        graph_symbol_calls.clear()
        graph_server.handle({"jsonrpc": "2.0", "id": 88, "method": "workspace/symbol", "params": {"query": "Graph"}})
        graph_server.handle({"jsonrpc": "2.0", "id": 89, "method": "workspace/symbol", "params": {"query": "Graph"}})
    finally:
        lsp.run_compiler_symbols = real_run_compiler_symbols
        lsp.run_compiler_diagnostics = real_run_compiler_diagnostics
        graph_server.cancel_pending_diagnostics()
        graph_server.cancel_pending_workspace_symbols()
    if graph_symbol_calls:
        print("lsp: warmed symbol-stable edits should not require another compiler symbol graph call")
        print(graph_symbol_calls)
        return 1
    graph_response = next((message for message in graph_server.sent if message.get("id") == 88), {})
    graph_names = sorted(item.get("name") for item in graph_response.get("result", []))
    if graph_names != ["GraphImport", "GraphRoot"]:
        print("lsp: workspace symbols should include compiler-backed imported symbols from the one graph call")
        print(graph_response)
        return 1
    graph_import_fields = graph_server.workspace.fields_for_owner("GraphImport")
    if not any(field.name == "value" and field.uri == graph_import_uri for field in graph_import_fields):
        print("lsp: one-shot compiler workspace symbols should index imported fields by their real URI")
        print(graph_import_fields)
        return 1

    init_server = CaptureServer()
    init_server.handle({"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {}})
    init_result = init_server.sent[-1].get("result", {}) if init_server.sent else {}
    signature_caps = init_result.get("capabilities", {}).get("signatureHelpProvider")
    if signature_caps != {"triggerCharacters": ["(", ","]}:
        print("lsp: expected initialize to advertise signature help provider")
        print(init_server.sent)
        return 1
    completion_caps = init_result.get("capabilities", {}).get("completionProvider")
    if completion_caps != {"triggerCharacters": [".", "<", "\"", "/"], "resolveProvider": True}:
        print("lsp: expected initialize to advertise import path completion triggers")
        print(completion_caps)
        return 1
    if init_result.get("capabilities", {}).get("documentHighlightProvider") is not True:
        print("lsp: expected initialize to advertise document highlight provider")
        print(init_result.get("capabilities", {}))
        return 1
    if init_result.get("capabilities", {}).get("workspaceSymbolProvider") is not True:
        print("lsp: expected initialize to advertise workspace symbol provider")
        print(init_result.get("capabilities", {}))
        return 1
    semantic_legend = init_result.get("capabilities", {}).get("semanticTokensProvider", {}).get("legend", {})
    expected_modifiers = {"declaration", "definition", "defaultLibrary", "local", "global", "member", "generic"}
    if not expected_modifiers.issubset(set(semantic_legend.get("tokenModifiers", []))):
        print("lsp: expected initialize to advertise semantic token modifiers")
        print(semantic_legend)
        return 1

    publish_server = CaptureServer()
    publish_server.handle(
        {
            "jsonrpc": "2.0",
            "method": "textDocument/didOpen",
            "params": {
                "textDocument": {
                    "uri": lsp.path_to_uri(app),
                    "text": app.read_text(encoding="utf-8"),
                }
            },
        }
    )
    published = [
        msg.get("params", {})
        for msg in publish_server.sent
        if msg.get("method") == "textDocument/publishDiagnostics"
    ]
    if published:
        print("lsp: didOpen should debounce diagnostics instead of publishing workspace diagnostics inline")
        print(published)
        return 1
    module_uri = lsp.path_to_uri(module)
    module_doc_count = sum(
        1
        for checked_doc in publish_server.workspace.documents.values()
        if checked_doc.path and checked_doc.path.resolve() == module.resolve()
    )
    if module_doc_count != 0:
        print("lsp: didOpen should not recursively parse imported modules on the attach path")
        print(publish_server.workspace.documents.keys())
        return 1
    first_import_line = 0
    first_import_text = doc.text.splitlines()[first_import_line]
    first_import_col = first_import_text.index(module.as_posix())
    first_import_symbol = lsp.import_at(doc, first_import_line, first_import_col)
    if (
        first_import_symbol is None
        or first_import_symbol.path != module.as_posix()
        or first_import_symbol.target_uri != module_uri
    ):
        print("lsp: expected import path lookup to resolve imported module")
        print(first_import_symbol)
        return 1
    import_definition_server = CaptureServer()
    import_definition_server.workspace = workspace
    import_definition_server.handle(
        {
            "jsonrpc": "2.0",
            "id": 2,
            "method": "textDocument/definition",
            "params": {
                "textDocument": {"uri": doc.uri},
                "position": {"line": first_import_line, "character": first_import_col},
            },
        }
    )
    import_definition_response = import_definition_server.sent[-1] if import_definition_server.sent else {}
    if import_definition_response.get("result") != lsp.import_location_to_lsp(first_import_symbol):
        print("lsp: expected definition on import path to jump to imported module")
        print(import_definition_response)
        return 1
    import_hover_server = CaptureServer()
    import_hover_server.workspace = workspace
    import_hover_server.handle(
        {
            "jsonrpc": "2.0",
            "id": 3,
            "method": "textDocument/hover",
            "params": {
                "textDocument": {"uri": doc.uri},
                "position": {"line": first_import_line, "character": first_import_col},
            },
        }
    )
    import_hover_response = import_hover_server.sent[-1] if import_hover_server.sent else {}
    import_hover_value = (
        import_hover_response.get("result", {})
        .get("contents", {})
        .get("value", "")
    )
    if f'import "{module.as_posix()}"' not in import_hover_value or str(module.resolve()) not in import_hover_value:
        print("lsp: expected hover on import path to show resolved module path")
        print(import_hover_response)
        return 1
    import_completion_lines = import_completion_doc.text.splitlines()
    import_completion_line = 0
    import_completion_col = len(import_completion_lines[import_completion_line])
    import_items = lsp.import_path_completions_at(workspace, import_completion_doc, import_completion_line, import_completion_col)
    shared_import_item = next((item for item in import_items if item.get("label") == "shared.i"), None)
    if (
        shared_import_item is None
        or shared_import_item.get("kind") != 17
        or shared_import_item.get("textEdit", {}).get("newText") != "shared.i"
        or shared_import_item.get("textEdit", {}).get("range", {}).get("start", {}).get("character") != import_completion_lines[import_completion_line].index("sh")
    ):
        print("lsp: expected import path completion for sibling .i module")
        print(import_items)
        return 1
    nested_completion_line = 1
    nested_completion_col = import_completion_lines[nested_completion_line].rindex('"')
    nested_items = lsp.import_path_completions_at(workspace, import_completion_doc, nested_completion_line, nested_completion_col)
    if not any(item.get("label") == "modules/nested.i" and item.get("kind") == 17 for item in nested_items):
        print("lsp: expected import path completion inside typed directory")
        print(nested_items)
        return 1
    import_completion_server = CaptureServer()
    import_completion_server.workspace = workspace
    import_completion_server.handle(
        {
            "jsonrpc": "2.0",
            "id": 4,
            "method": "textDocument/completion",
            "params": {
                "textDocument": {"uri": import_completion_doc.uri},
                "position": {"line": import_completion_line, "character": import_completion_col},
            },
        }
    )
    import_completion_response = import_completion_server.sent[-1] if import_completion_server.sent else {}
    import_completion_response_items = import_completion_response.get("result", {}).get("items", [])
    if not any(item.get("label") == "shared.i" for item in import_completion_response_items):
        print("lsp: expected textDocument/completion in import string to return module paths")
        print(import_completion_response)
        return 1
    cinclude_completion_lines = cinclude_completion_doc.text.splitlines()
    cinclude_line = 0
    cinclude_col = cinclude_completion_lines[cinclude_line].index("ven")
    cinclude_symbol = lsp.cinclude_at(cinclude_completion_doc, cinclude_line, cinclude_col)
    if (
        cinclude_symbol is None
        or cinclude_symbol.path != "vendor.h"
        or cinclude_symbol.target_uri != lsp.path_to_uri(cinclude_header)
    ):
        print("lsp: expected cinclude path lookup to resolve local header")
        print(cinclude_symbol)
        return 1
    cinclude_definition_server = CaptureServer()
    cinclude_definition_server.workspace = workspace
    cinclude_definition_server.handle(
        {
            "jsonrpc": "2.0",
            "id": 5,
            "method": "textDocument/definition",
            "params": {
                "textDocument": {"uri": cinclude_completion_doc.uri},
                "position": {"line": cinclude_line, "character": cinclude_col},
            },
        }
    )
    cinclude_definition_response = cinclude_definition_server.sent[-1] if cinclude_definition_server.sent else {}
    if cinclude_definition_response.get("result") != lsp.cinclude_location_to_lsp(cinclude_symbol):
        print("lsp: expected definition on cinclude path to jump to local header")
        print(cinclude_definition_response)
        return 1
    cinclude_hover_server = CaptureServer()
    cinclude_hover_server.workspace = workspace
    cinclude_hover_server.handle(
        {
            "jsonrpc": "2.0",
            "id": 6,
            "method": "textDocument/hover",
            "params": {
                "textDocument": {"uri": cinclude_completion_doc.uri},
                "position": {"line": cinclude_line, "character": cinclude_col},
            },
        }
    )
    cinclude_hover_response = cinclude_hover_server.sent[-1] if cinclude_hover_server.sent else {}
    cinclude_hover_value = (
        cinclude_hover_response.get("result", {})
        .get("contents", {})
        .get("value", "")
    )
    if 'cinclude "vendor.h"' not in cinclude_hover_value or str(cinclude_header.resolve()) not in cinclude_hover_value:
        print("lsp: expected hover on cinclude path to show resolved header path")
        print(cinclude_hover_response)
        return 1
    cinclude_completion_line = 1
    cinclude_completion_col = len(cinclude_completion_lines[cinclude_completion_line])
    cinclude_items = lsp.cinclude_path_completions_at(workspace, cinclude_completion_doc, cinclude_completion_line, cinclude_completion_col)
    vendor_item = next((item for item in cinclude_items if item.get("label") == "vendor.h"), None)
    if (
        vendor_item is None
        or vendor_item.get("kind") != 17
        or vendor_item.get("textEdit", {}).get("newText") != "vendor.h"
        or vendor_item.get("textEdit", {}).get("range", {}).get("start", {}).get("character") != cinclude_completion_lines[cinclude_completion_line].index("ven")
    ):
        print("lsp: expected cinclude path completion for sibling header")
        print(cinclude_items)
        return 1
    nested_cinclude_line = 2
    nested_cinclude_col = cinclude_completion_lines[nested_cinclude_line].rindex('"')
    nested_cinclude_items = lsp.cinclude_path_completions_at(workspace, cinclude_completion_doc, nested_cinclude_line, nested_cinclude_col)
    if not any(item.get("label") == "modules/nested.h" and item.get("kind") == 17 for item in nested_cinclude_items):
        print("lsp: expected cinclude path completion inside typed directory")
        print(nested_cinclude_items)
        return 1
    cinclude_completion_server = CaptureServer()
    cinclude_completion_server.workspace = workspace
    cinclude_completion_server.handle(
        {
            "jsonrpc": "2.0",
            "id": 7,
            "method": "textDocument/completion",
            "params": {
                "textDocument": {"uri": cinclude_completion_doc.uri},
                "position": {"line": cinclude_completion_line, "character": cinclude_completion_col},
            },
        }
    )
    cinclude_completion_response = cinclude_completion_server.sent[-1] if cinclude_completion_server.sent else {}
    cinclude_completion_response_items = cinclude_completion_response.get("result", {}).get("items", [])
    if not any(item.get("label") == "vendor.h" for item in cinclude_completion_response_items):
        print("lsp: expected textDocument/completion in cinclude string to return local headers")
        print(cinclude_completion_response)
        return 1
    cinclude_tokens = decoded_semantic_tokens(lsp, lsp.semantic_tokens_for_doc(workspace, cinclude_completion_doc))
    cinclude_keyword_token = (
        cinclude_line,
        cinclude_completion_lines[cinclude_line].index("cinclude"),
        len("cinclude"),
        "keyword",
    )
    cinclude_string_token = (
        cinclude_line,
        cinclude_completion_lines[cinclude_line].index('"vendor.h"'),
        len('"vendor.h"'),
        "string",
    )
    if cinclude_keyword_token not in cinclude_tokens or cinclude_string_token not in cinclude_tokens:
        print("lsp: expected cinclude keyword and path string semantic tokens")
        print(cinclude_tokens)
        return 1
    published_doc = publish_server.workspace.documents.get(lsp.path_to_uri(app))
    if published_doc is None or publish_server.workspace.collect_python_diagnostics or published_doc.diagnostics:
        print("lsp: compiler-backed LSP didOpen should keep only the opened buffer and avoid Python fallback diagnostics")
        print(published_doc.diagnostics if published_doc else None)
        return 1

    compiler_publish_server = CaptureServer()
    compiler_publish_server.handle(
        {
            "jsonrpc": "2.0",
            "method": "textDocument/didOpen",
            "params": {
                "textDocument": {
                    "uri": lsp.path_to_uri(compiler_diag_app),
                    "text": compiler_diag_app.read_text(encoding="utf-8"),
                }
            },
        }
    )
    compiler_immediate_published = [
        msg.get("params", {})
        for msg in compiler_publish_server.sent
        if msg.get("method") == "textDocument/publishDiagnostics"
    ]
    if compiler_immediate_published:
        print("lsp: didOpen should debounce compiler diagnostics instead of publishing inline")
        print(compiler_immediate_published)
        return 1
    compiler_publish_server.flush_pending_diagnostics(lsp.path_to_uri(compiler_diag_app))
    compiler_published = [
        msg.get("params", {})
        for msg in compiler_publish_server.sent
        if msg.get("method") == "textDocument/publishDiagnostics"
    ]
    compiler_diag_uri = lsp.path_to_uri(compiler_diag_app)
    compiler_diag_publish = next((item for item in compiler_published if item.get("uri") == compiler_diag_uri), None)
    compiler_missing_diag = None
    if compiler_diag_publish is not None:
        compiler_missing_diag = next(
            (
                diag
                for diag in compiler_diag_publish.get("diagnostics", [])
                if "use of undeclared identifier 'missing_symbol'" in diag.get("message", "")
            ),
            None,
        )
    if (
        compiler_diag_publish is None
        or compiler_missing_diag is None
    ):
        print("lsp: didOpen should publish compiler JSON diagnostics")
        print(compiler_published)
        return 1
    compiler_range = compiler_missing_diag.get("range", {})
    compiler_start = compiler_range.get("start", {})
    compiler_end = compiler_range.get("end", {})
    if compiler_end.get("character") != compiler_start.get("character", 0) + len("missing_symbol"):
        print("lsp: compiler diagnostic range should span missing_symbol")
        print(compiler_missing_diag)
        return 1
    if compiler_missing_diag.get("source") != "I":
        print("lsp: compiler diagnostics should publish with source I")
        print(compiler_missing_diag)
        return 1

    dirty_source = """
main:proc()->i32 = {
    return dirty_buffer_missing_symbol;
}
""".strip() + "\n"
    dirty_publish_server = CaptureServer()
    dirty_uri = lsp.path_to_uri(compiler_dirty_app)
    dirty_publish_server.handle(
        {
            "jsonrpc": "2.0",
            "method": "textDocument/didOpen",
            "params": {
                "textDocument": {
                    "uri": dirty_uri,
                    "text": compiler_dirty_app.read_text(encoding="utf-8"),
                }
            },
        }
    )
    dirty_change_start = len(dirty_publish_server.sent)
    dirty_publish_server.diagnostic_debounce_seconds = 3600.0
    symbol_refresh_calls: list[tuple[str, bool]] = []
    diagnostic_calls: list[str] = []
    lsp_calls: list[str] = []
    real_run_compiler_symbols = lsp.run_compiler_symbols
    real_run_compiler_diagnostics = lsp.run_compiler_diagnostics
    real_run_compiler_lsp = lsp.run_compiler_lsp

    def counting_run_compiler_symbols(doc: lsp.Document, include_imports: bool = False):
        symbol_refresh_calls.append((doc.uri, include_imports))
        return real_run_compiler_symbols(doc, include_imports)

    def counting_run_compiler_diagnostics(doc: lsp.Document):
        diagnostic_calls.append(doc.uri)
        return real_run_compiler_diagnostics(doc)

    def counting_run_compiler_lsp(doc: lsp.Document):
        lsp_calls.append(doc.uri)
        return real_run_compiler_lsp(doc)

    try:
        lsp.run_compiler_symbols = counting_run_compiler_symbols
        lsp.run_compiler_diagnostics = counting_run_compiler_diagnostics
        lsp.run_compiler_lsp = counting_run_compiler_lsp
        dirty_publish_server.handle(
            {
                "jsonrpc": "2.0",
                "method": "textDocument/didChange",
                "params": {
                    "textDocument": {"uri": dirty_uri},
                    "contentChanges": [{"text": dirty_source}],
                },
            }
        )
        dirty_immediate_published = [
            msg.get("params", {})
            for msg in dirty_publish_server.sent[dirty_change_start:]
            if msg.get("method") == "textDocument/publishDiagnostics"
        ]
        if dirty_immediate_published:
            print("lsp: didChange should debounce live diagnostics instead of publishing inline")
            print(dirty_immediate_published)
            return 1
        if diagnostic_calls:
            print("lsp: didChange should not run compiler diagnostics inline on the live edit path")
            print(diagnostic_calls)
            return 1
        if lsp_calls:
            print("lsp: didChange should not run combined compiler LSP checks inline on the live edit path")
            print(lsp_calls)
            return 1
        dirty_publish_server.flush_pending_diagnostics(dirty_uri)
        if symbol_refresh_calls:
            print("lsp: didChange should not run compiler symbol extraction inline on the live edit path")
            print(symbol_refresh_calls)
            return 1
        dirty_publish_server.flush_pending_workspace_symbols(dirty_uri)
    finally:
        lsp.run_compiler_symbols = real_run_compiler_symbols
        lsp.run_compiler_diagnostics = real_run_compiler_diagnostics
        lsp.run_compiler_lsp = real_run_compiler_lsp
    dirty_change_published = [
        msg.get("params", {})
        for msg in dirty_publish_server.sent[dirty_change_start:]
        if msg.get("method") == "textDocument/publishDiagnostics"
    ]
    if symbol_refresh_calls != [(dirty_uri, True)]:
        print("lsp: flushing debounced didChange workspace symbols should run one compiler symbol prefetch for the changed document")
        print(symbol_refresh_calls)
        return 1
    if diagnostic_calls != [dirty_uri] or lsp_calls:
        print("lsp: flushing debounced didChange diagnostics should run one fast compiler diagnostic check")
        print(lsp_calls, diagnostic_calls)
        return 1
    if [item.get("uri") for item in dirty_change_published] != [dirty_uri]:
        print("lsp: didChange should publish diagnostics only for the changed document")
        print(dirty_change_published)
        return 1
    dirty_published = [
        msg.get("params", {})
        for msg in dirty_publish_server.sent
        if msg.get("method") == "textDocument/publishDiagnostics" and msg.get("params", {}).get("uri") == dirty_uri
    ]
    dirty_last = dirty_published[-1] if dirty_published else {}
    dirty_diag = next(
        (
            diag
            for diag in dirty_last.get("diagnostics", [])
            if "dirty_buffer_missing_symbol" in diag.get("message", "")
        ),
        None,
    )
    if dirty_diag is None or dirty_diag.get("source") != "I":
        print("lsp: compiler diagnostics should use dirty buffer text from didChange")
        print(dirty_published)
        return 1
    dirty_range = dirty_diag.get("range", {})
    dirty_start = dirty_range.get("start", {})
    dirty_end = dirty_range.get("end", {})
    if dirty_end.get("character") != dirty_start.get("character", 0) + len("dirty_buffer_missing_symbol"):
        print("lsp: dirty-buffer compiler diagnostic range should span the unsaved identifier")
        print(dirty_diag)
        return 1

    noop_sent_count = len(dirty_publish_server.sent)
    dirty_publish_server.handle(
        {
            "jsonrpc": "2.0",
            "method": "textDocument/didChange",
            "params": {
                "textDocument": {"uri": dirty_uri},
                "contentChanges": [{"text": dirty_source}],
            },
        }
    )
    if (
        len(dirty_publish_server.sent) != noop_sent_count
        or dirty_uri in dirty_publish_server.pending_diagnostics
        or dirty_uri in dirty_publish_server.pending_workspace_symbols
    ):
        print("lsp: no-op didChange should not publish or schedule diagnostics/symbol refresh")
        print(dirty_publish_server.sent[noop_sent_count:])
        print(dirty_publish_server.pending_diagnostics)
        print(dirty_publish_server.pending_workspace_symbols)
        return 1

    entry_project = TEST_DIR / "entry_project"
    entry_src = entry_project / "src"
    entry_src.mkdir(parents=True, exist_ok=True)
    (entry_project / "bunyan.py").write_text("# test project root\n", encoding="utf-8", newline="\n")
    entry_root_i = entry_src / "app_win32.i"
    entry_mod_i = entry_src / "entry_mod.i"
    entry_mod_i.write_text(
        "mod_value:proc()->i32 = {\n    return root_value;\n}\n",
        encoding="utf-8",
        newline="\n",
    )
    entry_root_i.write_text(
        f"import \"{entry_mod_i.as_posix()}\"\n\nroot_value:i32 = 7;\n\nmain:proc()->i32 = {{\n    return mod_value();\n}}\n",
        encoding="utf-8",
        newline="\n",
    )
    entry_uri = lsp.path_to_uri(entry_mod_i)
    entry_server = CaptureServer()
    entry_server.handle(
        {
            "jsonrpc": "2.0",
            "method": "textDocument/didOpen",
            "params": {
                "textDocument": {
                    "uri": entry_uri,
                    "text": entry_mod_i.read_text(encoding="utf-8"),
                }
            },
        }
    )
    entry_doc = entry_server.workspace.documents.get(entry_uri)
    if not entry_doc or lsp.project_entry_for_doc(entry_doc) != entry_root_i.resolve():
        print("lsp: project entry inference should choose the reachable top-level I entry")
        print(lsp.project_entry_for_doc(entry_doc) if entry_doc else None)
        return 1
    entry_dirty_source = "mod_value:proc()->i32 = {\n    return dirty_import_missing;\n}\n"
    entry_server.handle(
        {
            "jsonrpc": "2.0",
            "method": "textDocument/didChange",
            "params": {
                "textDocument": {"uri": entry_uri},
                "contentChanges": [{"text": entry_dirty_source}],
            },
        }
    )
    entry_server.flush_pending_diagnostics(entry_uri)
    entry_published = [
        msg.get("params", {})
        for msg in entry_server.sent
        if msg.get("method") == "textDocument/publishDiagnostics" and msg.get("params", {}).get("uri") == entry_uri
    ]
    entry_last = entry_published[-1] if entry_published else {}
    if not any("dirty_import_missing" in diag.get("message", "") for diag in entry_last.get("diagnostics", [])):
        print("lsp: dirty imported module diagnostics should be checked through the project entry")
        print(entry_published)
        return 1
    entry_server.cancel_pending_workspace_symbols()

    imported_diag_project = TEST_DIR / "imported_diag_project"
    imported_diag_src = imported_diag_project / "src"
    imported_diag_src.mkdir(parents=True, exist_ok=True)
    (imported_diag_project / "bunyan.py").write_text("# test project root\n", encoding="utf-8", newline="\n")
    imported_diag_root = imported_diag_src / "app_win32.i"
    imported_diag_mod = imported_diag_src / "diag_mod.i"
    imported_diag_mod.write_text(
        "diag_mod:proc()->i32 = {\n    return imported_file_missing;\n}\n",
        encoding="utf-8",
        newline="\n",
    )
    imported_diag_root.write_text(
        f"import \"{imported_diag_mod.as_posix()}\"\n\nmain:proc()->i32 = {{\n    return 0;\n}}\n",
        encoding="utf-8",
        newline="\n",
    )
    imported_diag_root_uri = lsp.path_to_uri(imported_diag_root)
    imported_diag_mod_uri = lsp.path_to_uri(imported_diag_mod)
    imported_diag_server = CaptureServer()
    imported_diag_server.handle(
        {
            "jsonrpc": "2.0",
            "method": "textDocument/didOpen",
            "params": {
                "textDocument": {
                    "uri": imported_diag_root_uri,
                    "text": imported_diag_root.read_text(encoding="utf-8"),
                }
            },
        }
    )
    imported_diag_server.flush_pending_diagnostics(imported_diag_root_uri)
    imported_diag_publishes = [
        msg.get("params", {})
        for msg in imported_diag_server.sent
        if msg.get("method") == "textDocument/publishDiagnostics"
    ]
    imported_diag_mod_publish = next(
        (
            params
            for params in reversed(imported_diag_publishes)
            if params.get("uri") == imported_diag_mod_uri
        ),
        {},
    )
    if not any(
        "imported_file_missing" in diag.get("message", "")
        for diag in imported_diag_mod_publish.get("diagnostics", [])
    ):
        print("lsp: compiler diagnostics from imported files should publish to the imported file URI")
        print(imported_diag_publishes)
        return 1
    imported_diag_mod.write_text(
        "diag_mod:proc()->i32 = {\n    return 0;\n}\n",
        encoding="utf-8",
        newline="\n",
    )
    imported_diag_root_clean_text = imported_diag_root.read_text(encoding="utf-8") + "\n// touch\n"
    imported_diag_server.handle(
        {
            "jsonrpc": "2.0",
            "method": "textDocument/didChange",
            "params": {
                "textDocument": {"uri": imported_diag_root_uri},
                "contentChanges": [{"text": imported_diag_root_clean_text}],
            },
        }
    )
    imported_diag_server.flush_pending_diagnostics(imported_diag_root_uri)
    imported_diag_clean_publishes = [
        msg.get("params", {})
        for msg in imported_diag_server.sent
        if msg.get("method") == "textDocument/publishDiagnostics"
    ]
    imported_diag_mod_clear = next(
        (
            params
            for params in reversed(imported_diag_clean_publishes)
            if params.get("uri") == imported_diag_mod_uri
        ),
        {},
    )
    if imported_diag_mod_clear.get("diagnostics") != []:
        print("lsp: clean compiler runs should clear stale imported-file diagnostics")
        print(imported_diag_clean_publishes)
        return 1
    imported_diag_server.cancel_pending_workspace_symbols()

    compiler_cache_doc = lsp.analyze(
        lsp.path_to_uri(compiler_dirty_app),
        compiler_dirty_app.read_text(encoding="utf-8"),
        compiler_dirty_app,
    )
    compiler_cache_server = CaptureServer()
    real_i_exe = lsp.I_EXE
    try:
        lsp.I_EXE = TEST_DIR / "missing-I-for-cache-test.exe"
        unavailable, unavailable_diags = compiler_cache_server.compiler_diagnostics(compiler_cache_doc)
        lsp.I_EXE = real_i_exe
        available, available_diags = compiler_cache_server.compiler_diagnostics(compiler_cache_doc)
    finally:
        lsp.I_EXE = real_i_exe
    if unavailable or unavailable_diags or not available or available_diags:
        print("lsp: compiler diagnostic cache should recover when I.exe becomes available")
        print((unavailable, unavailable_diags, available, available_diags))
        return 1

    completion_context_lines = completion_context_doc.text.splitlines()
    struct_init_line = next(i for i, line in enumerate(completion_context_lines) if "payload:Payload" in line)
    struct_init_col = completion_context_lines[struct_init_line].rindex(".") + 1
    struct_init_items = lsp.struct_literal_field_completions_at(workspace, completion_context_doc, struct_init_line, struct_init_col)
    if (
        not any(item.get("label") == "values" and item.get("insertText") == "values = " for item in struct_init_items)
        or any(item.get("label") == "value" for item in struct_init_items)
    ):
        print("lsp: expected struct literal field completion to offer unused fields")
        print(struct_init_items)
        return 1
    struct_completion_server = CaptureServer()
    struct_completion_server.workspace = workspace
    struct_completion_server.handle(
        {
            "jsonrpc": "2.0",
            "id": 23,
            "method": "textDocument/completion",
            "params": {
                "textDocument": {"uri": completion_context_doc.uri},
                "position": {"line": struct_init_line, "character": struct_init_col},
            },
        }
    )
    struct_completion_response = struct_completion_server.sent[-1] if struct_completion_server.sent else {}
    struct_completion_items = struct_completion_response.get("result", {}).get("items", [])
    if (
        struct_completion_response.get("id") != 23
        or not any(item.get("label") == "values" for item in struct_completion_items)
        or any(item.get("label") == "Payload" for item in struct_completion_items)
    ):
        print("lsp: expected textDocument/completion in struct literal to return field items")
        print(struct_completion_response)
        return 1
    call_struct_line = next(i for i, line in enumerate(completion_context_lines) if "call_struct_field" in line)
    call_struct_col = completion_context_lines[call_struct_line].rindex(".") + 1
    call_struct_items = lsp.struct_literal_field_completions_at(workspace, completion_context_doc, call_struct_line, call_struct_col)
    if (
        not any(item.get("label") == "value" and item.get("detail") == "Payload.value: i32" for item in call_struct_items)
        or not any(item.get("label") == "values" and item.get("detail") == "Payload.values: [4]i32" for item in call_struct_items)
    ):
        print("lsp: expected struct literal field completion in proc argument to use active parameter type")
        print(call_struct_items)
        return 1
    call_struct_completion_server = CaptureServer()
    call_struct_completion_server.workspace = workspace
    call_struct_completion_server.handle(
        {
            "jsonrpc": "2.0",
            "id": 26,
            "method": "textDocument/completion",
            "params": {
                "textDocument": {"uri": completion_context_doc.uri},
                "position": {"line": call_struct_line, "character": call_struct_col},
            },
        }
    )
    call_struct_completion_response = call_struct_completion_server.sent[-1] if call_struct_completion_server.sent else {}
    call_struct_completion_items = call_struct_completion_response.get("result", {}).get("items", [])
    if (
        call_struct_completion_response.get("id") != 26
        or not any(item.get("label") == "value" for item in call_struct_completion_items)
        or any(item.get("label") == "Payload" for item in call_struct_completion_items)
    ):
        print("lsp: expected textDocument/completion in proc argument initializer to return field items")
        print(call_struct_completion_response)
        return 1
    enum_context_line = next(i for i, line in enumerate(completion_context_lines) if "k:Kind" in line)
    enum_context_col = completion_context_lines[enum_context_line].index("K;") + 1
    enum_context_items = lsp.enum_completions_at(workspace, completion_context_doc, enum_context_line, enum_context_col)
    if not any(item.get("label") == "Kind_Ready" for item in enum_context_items):
        print("lsp: expected enum assignment completion to offer enum members")
        print(enum_context_items)
        return 1
    enum_completion_server = CaptureServer()
    enum_completion_server.workspace = workspace
    enum_completion_server.handle(
        {
            "jsonrpc": "2.0",
            "id": 24,
            "method": "textDocument/completion",
            "params": {
                "textDocument": {"uri": completion_context_doc.uri},
                "position": {"line": enum_context_line, "character": enum_context_col},
            },
        }
    )
    enum_completion_response = enum_completion_server.sent[-1] if enum_completion_server.sent else {}
    enum_completion_items = enum_completion_response.get("result", {}).get("items", [])
    if (
        enum_completion_response.get("id") != 24
        or not any(item.get("label") == "Kind_Ready" for item in enum_completion_items)
        or any(item.get("label") == "Payload" for item in enum_completion_items)
    ):
        print("lsp: expected textDocument/completion in enum assignment to return enum items")
        print(enum_completion_response)
        return 1

    enum_dot_line = next(i for i, line in enumerate(completion_context_lines) if "dot_kind:Kind" in line)
    enum_dot_col = completion_context_lines[enum_dot_line].index("Kind.") + len("Kind.")
    enum_dot_items = lsp.enum_dot_completions_at(workspace, completion_context_doc, enum_dot_line, enum_dot_col)
    if (
        not any(item.get("label") == "Ready" and item.get("insertText") == "Ready" for item in enum_dot_items)
        or any(item.get("label") == "Kind_Ready" for item in enum_dot_items)
    ):
        print("lsp: expected enum dot completion to insert bare enum member names")
        print(enum_dot_items)
        return 1
    enum_dot_completion_server = CaptureServer()
    enum_dot_completion_server.workspace = workspace
    enum_dot_completion_server.handle(
        {
            "jsonrpc": "2.0",
            "id": 27,
            "method": "textDocument/completion",
            "params": {
                "textDocument": {"uri": completion_context_doc.uri},
                "position": {"line": enum_dot_line, "character": enum_dot_col},
            },
        }
    )
    enum_dot_completion_response = enum_dot_completion_server.sent[-1] if enum_dot_completion_server.sent else {}
    enum_dot_completion_items = enum_dot_completion_response.get("result", {}).get("items", [])
    if (
        enum_dot_completion_response.get("id") != 27
        or not any(item.get("label") == "Ready" and item.get("insertText") == "Ready" for item in enum_dot_completion_items)
        or any(item.get("label") == "Kind_Ready" for item in enum_dot_completion_items)
    ):
        print("lsp: expected textDocument/completion after Enum. to return bare enum members")
        print(enum_dot_completion_response)
        return 1

    expected_value_line = next(i for i, line in enumerate(completion_context_lines) if "score:i32" in line)
    expected_value_col = completion_context_lines[expected_value_line].index("v;") + 1
    expected_value_items = lsp.expected_type_completions_at(
        workspace,
        completion_context_doc,
        expected_value_line,
        expected_value_col,
    )
    if (
        not any(item.get("label") == "value" and item.get("detail") == "value: i32" for item in expected_value_items)
        or any(item.get("label") == "payload" for item in expected_value_items)
    ):
        print("lsp: expected assignment completion to offer values matching the expected type")
        print(expected_value_items)
        return 1
    expected_pointer_line = next(i for i, line in enumerate(completion_context_lines) if "payload_ptr:*Payload" in line)
    expected_pointer_col = completion_context_lines[expected_pointer_line].index("p;") + 1
    expected_pointer_items = lsp.expected_type_completions_at(
        workspace,
        completion_context_doc,
        expected_pointer_line,
        expected_pointer_col,
    )
    if not any(item.get("label") == "payload.&" and item.get("detail") == "payload: Payload -> *Payload" for item in expected_pointer_items):
        print("lsp: expected pointer assignment completion to offer address sugar")
        print(expected_pointer_items)
        return 1
    expected_completion_server = CaptureServer()
    expected_completion_server.workspace = workspace
    expected_completion_server.handle(
        {
            "jsonrpc": "2.0",
            "id": 25,
            "method": "textDocument/completion",
            "params": {
                "textDocument": {"uri": completion_context_doc.uri},
                "position": {"line": expected_value_line, "character": expected_value_col},
            },
        }
    )
    expected_completion_response = expected_completion_server.sent[-1] if expected_completion_server.sent else {}
    expected_completion_items = expected_completion_response.get("result", {}).get("items", [])
    if (
        expected_completion_response.get("id") != 25
        or not any(item.get("label") == "value" for item in expected_completion_items)
        or any(item.get("label") == "Payload" for item in expected_completion_items)
    ):
        print("lsp: expected textDocument/completion in typed assignment to return value items")
        print(expected_completion_response)
        return 1

    type_field_line = next(i for i, line in enumerate(completion_context_lines) if "payload_type_field" in line)
    type_field_col = completion_context_lines[type_field_line].index(".") + 1
    type_field_items = lsp.type_field_completions_at(
        workspace,
        completion_context_doc,
        type_field_line,
        type_field_col,
    )
    if not any(item.name == "value" and item.detail == "Payload.value: i32" for item in type_field_items):
        print("lsp: expected type field completion after Payload.")
        print(type_field_items)
        return 1
    reflect_field_line = next(i for i, line in enumerate(completion_context_lines) if "payload_reflect_field" in line)
    reflect_field_col = completion_context_lines[reflect_field_line].index("Payload<>.") + len("Payload<>.")
    reflect_field_items = lsp.reflect_field_completions_at(
        workspace,
        completion_context_doc,
        reflect_field_line,
        reflect_field_col,
    )
    if (
        not any(item.name == "name" and item.detail == "Payload<>.name: *const char" for item in reflect_field_items)
        or not any(item.name == "count" and item.detail == "Payload<>.count: u64" for item in reflect_field_items)
        or not any(item.name == "kind" and item.detail == "Payload<>.kind: i32" for item in reflect_field_items)
        or not any(item.name == "variant" and item.detail == "Payload<>.variant: const reflect_variant" for item in reflect_field_items)
        or any(item.name == "value" for item in reflect_field_items)
    ):
        print("lsp: expected Payload<>. completion to return reflection metadata fields only")
        print(reflect_field_items)
        return 1
    reflect_enum_line = next(i for i, line in enumerate(completion_context_lines) if "kind_reflect_field" in line)
    reflect_enum_col = completion_context_lines[reflect_enum_line].index("Kind<>.") + len("Kind<>.")
    reflect_enum_items = lsp.reflect_field_completions_at(
        workspace,
        completion_context_doc,
        reflect_enum_line,
        reflect_enum_col,
    )
    if (
        not any(item.name == "count" and item.detail == "Kind<>.count: u64" for item in reflect_enum_items)
        or not any(item.name == "kind" and item.detail == "Kind<>.kind: i32" for item in reflect_enum_items)
        or not any(item.name == "variant" and item.detail == "Kind<>.variant: const reflect_variant" for item in reflect_enum_items)
        or {item.name for item in reflect_enum_items} != {item.name for item in reflect_field_items}
    ):
        print("lsp: expected Kind<>. completion to offer the same merged reflect members a struct does")
        print(reflect_enum_items)
        return 1
    reflect_completion_server = CaptureServer()
    reflect_completion_server.workspace = workspace
    reflect_completion_server.handle(
        {
            "jsonrpc": "2.0",
            "id": 126,
            "method": "textDocument/completion",
            "params": {
                "textDocument": {"uri": completion_context_doc.uri},
                "position": {"line": reflect_field_line, "character": reflect_field_col},
            },
        }
    )
    reflect_completion_response = reflect_completion_server.sent[-1] if reflect_completion_server.sent else {}
    reflect_completion_items = reflect_completion_response.get("result", {}).get("items", [])
    if (
        reflect_completion_response.get("id") != 126
        or not any(item.get("label") == "count" for item in reflect_completion_items)
        or not any(item.get("label") == "variant" for item in reflect_completion_items)
        or any(item.get("label") == "value" for item in reflect_completion_items)
    ):
        print("lsp: expected textDocument/completion after Payload<>. to return reflect metadata fields")
        print(reflect_completion_response)
        return 1
    generic_owner_proc_line = next(i for i, line in enumerate(completion_context_lines) if "payload_array" in line)
    generic_owner_proc_col = completion_context_lines[generic_owner_proc_line].index("Array<Payload>r") + len("Array<Payload>r")
    generic_owner_proc_items = lsp.generic_owner_proc_completions_at(
        workspace,
        completion_context_doc,
        generic_owner_proc_line,
        generic_owner_proc_col,
    )
    if not any(
        item.name == "Array<Payload>reserve"
        and lsp.proc_signature_label_for_symbol(item, item.name) == "Array<Payload>reserve:proc(length:u64)->Array<Payload>"
        for item in generic_owner_proc_items
    ):
        print("lsp: expected generic owner proc completion after Array<Payload>r")
        print(generic_owner_proc_items)
        return 1
    generic_owner_proc_context_items = lsp.generic_owner_proc_completion_items_at(
        workspace,
        completion_context_doc,
        generic_owner_proc_line,
        generic_owner_proc_col,
    )
    typed_member_start = completion_context_lines[generic_owner_proc_line].index("Array<Payload>r") + len("Array<Payload>")
    if not any(
        item.get("label") == "reserve"
        and item.get("detail") == "Array<Payload>reserve:proc(length:u64)->Array<Payload>"
        and item.get("insertTextFormat") == 2
        and item.get("insertText") == "reserve(${1:length})$0"
        and item.get("textEdit", {}).get("range", {}).get("start", {}).get("character") == typed_member_start
        and item.get("textEdit", {}).get("newText") == "reserve(${1:length})$0"
        for item in generic_owner_proc_context_items
    ):
        print("lsp: expected generic owner proc context item to insert a member suffix snippet")
        print(generic_owner_proc_context_items)
        return 1
    generic_owner_empty_line = next(i for i, line in enumerate(completion_context_lines) if "payload_array_empty" in line)
    generic_owner_empty_col = completion_context_lines[generic_owner_empty_line].index("Array<Payload>;") + len("Array<Payload>")
    generic_owner_empty_items = lsp.generic_owner_proc_completion_items_at(
        workspace,
        completion_context_doc,
        generic_owner_empty_line,
        generic_owner_empty_col,
    )
    if not any(
        item.get("label") == "reserve"
        and item.get("insertTextFormat") == 2
        and item.get("textEdit", {}).get("range", {}).get("start", {}).get("character") == generic_owner_empty_col
        and item.get("textEdit", {}).get("newText") == "reserve(${1:length})$0"
        for item in generic_owner_empty_items
    ):
        print("lsp: expected generic owner proc context item after Array<Payload> to insert reserve snippet")
        print(generic_owner_empty_items)
        return 1
    generic_owner_completion_server = CaptureServer()
    generic_owner_completion_server.workspace = workspace
    generic_owner_completion_server.handle(
        {
            "jsonrpc": "2.0",
            "id": 26,
            "method": "textDocument/completion",
            "params": {
                "textDocument": {"uri": completion_context_doc.uri},
                "position": {"line": generic_owner_proc_line, "character": generic_owner_proc_col},
            },
        }
    )
    generic_owner_completion_response = generic_owner_completion_server.sent[-1] if generic_owner_completion_server.sent else {}
    generic_owner_completion_items = generic_owner_completion_response.get("result", {}).get("items", [])
    if (
        generic_owner_completion_response.get("id") != 26
        or not any(
            item.get("label") == "reserve"
            and item.get("detail") == "Array<Payload>reserve:proc(length:u64)->Array<Payload>"
            and item.get("insertTextFormat") == 2
            and item.get("textEdit", {}).get("newText") == "reserve(${1:length})$0"
            for item in generic_owner_completion_items
        )
        or any(item.get("label") == "Payload" for item in generic_owner_completion_items)
    ):
        print("lsp: expected textDocument/completion after Array<Payload>r to return owner proc items")
        print(generic_owner_completion_response)
        return 1

    if not any("type error: type 'Payload' has no field 'missing'" in diag.message for diag in doc.diagnostics):
        print("lsp: expected semantic diagnostic for missing Payload field")
        print(doc.diagnostics)
        return 1
    payload_missing_diags = [
        diag for diag in doc.diagnostics if "type error: type 'Payload' has no field 'missing'" in diag.message
    ]
    if len(payload_missing_diags) != 3:
        print("lsp: expected missing Payload field diagnostics for direct, pointer, and chained generic access")
        print(doc.diagnostics)
        return 1
    if not any("type error: type 'i32' has no field 'missing'" in diag.message for diag in doc.diagnostics):
        print("lsp: expected chained generic diagnostic for values.data[0].missing")
        print(doc.diagnostics)
        return 1
    if not any("type error: type 'Other' has no field 'values'" in diag.message for diag in doc.diagnostics):
        print("lsp: expected local shadow field diagnostic to use the local Other type")
        print(doc.diagnostics)
        return 1
    if any("type error: type 'Other' has no field 'value'" in diag.message for diag in doc.diagnostics):
        print("lsp: valid Other.value access should not produce diagnostics")
        print(doc.diagnostics)
        return 1
    if not any("type error: cannot assign 'Payload' to 'i32' in declaration" in diag.message for diag in doc.diagnostics):
        print("lsp: expected declaration assignment type diagnostic")
        print(doc.diagnostics)
        return 1
    if not any("type error: cannot assign 'Payload' to 'i32' in p.value" in diag.message for diag in doc.diagnostics):
        print("lsp: expected field assignment type diagnostic")
        print(doc.diagnostics)
        return 1
    if not any("type error: cannot assign 'Payload' to 'i32' in total" in diag.message for diag in doc.diagnostics):
        print("lsp: expected variable assignment type diagnostic")
        print(doc.diagnostics)
        return 1
    if not any("type error: cannot assign 'Payload' to '*Payload' in payload_ptr" in diag.message for diag in doc.diagnostics):
        print("lsp: expected pointer/value assignment type diagnostic")
        print(doc.diagnostics)
        return 1
    if not any("type error: call 'payload_add' arg 1 expects '*Payload', got 'Payload'" in diag.message for diag in doc.diagnostics):
        print("lsp: expected proc call argument type diagnostic")
        print(doc.diagnostics)
        return 1
    if not any("type error: call 'cb' arg 2 expects 'i32', got 'Payload'" in diag.message for diag in doc.diagnostics):
        print("lsp: expected proc pointer call argument type diagnostic")
        print(doc.diagnostics)
        return 1
    if not any("type error: call 'handler.cb' arg 1 expects '*Payload', got 'Payload'" in diag.message for diag in doc.diagnostics):
        print("lsp: expected field proc pointer call argument type diagnostic")
        print(doc.diagnostics)
        return 1
    if not any("type error: call 'global_cb' arg 2 expects 'i32', got 'Payload'" in diag.message for diag in doc.diagnostics):
        print("lsp: expected global proc pointer call argument type diagnostic")
        print(doc.diagnostics)
        return 1
    if not any("type error: call 'payload_add' expects 2 args, got 1" in diag.message for diag in doc.diagnostics):
        print("lsp: expected semantic diagnostic for proc call arg count")
        print(doc.diagnostics)
        return 1
    if not any("type error: call 'cb' expects 2 args, got 3" in diag.message for diag in doc.diagnostics):
        print("lsp: expected semantic diagnostic for proc pointer call arg count")
        print(doc.diagnostics)
        return 1
    if not any("type error: call 'handler.cb' expects 2 args, got 1" in diag.message for diag in doc.diagnostics):
        print("lsp: expected semantic diagnostic for proc pointer field call arg count")
        print(doc.diagnostics)
        return 1
    if not any("type error: call 'global_cb' expects 2 args, got 1" in diag.message for diag in doc.diagnostics):
        print("lsp: expected semantic diagnostic for imported global proc pointer call arg count")
        print(doc.diagnostics)
        return 1
    if not any("semantic error: duplicate local declaration 'dup'" in diag.message for diag in doc.diagnostics):
        print("lsp: expected duplicate local declaration diagnostic")
        print(doc.diagnostics)
        return 1
    continue_diagnostics = [diag for diag in doc.diagnostics if "semantic error: continue outside loop" in diag.message]
    if len(continue_diagnostics) != 2:
        print("lsp: expected diagnostics for continue outside loops only")
        print(doc.diagnostics)
        return 1
    break_diagnostics = [diag for diag in doc.diagnostics if "semantic error: break outside loop or switch" in diag.message]
    if len(break_diagnostics) != 1:
        print("lsp: expected diagnostic for break outside loop/switch only")
        print(doc.diagnostics)
        return 1
    duplicate_doc = workspace.documents[lsp.path_to_uri(duplicate_module)]
    duplicate_proc_diags = [
        diag for diag in duplicate_doc.diagnostics if "module error: duplicate declaration 'payload_add'" in diag.message
    ]
    if not duplicate_proc_diags:
        print("lsp: expected cross-import duplicate declaration diagnostic")
        print(duplicate_doc.diagnostics)
        return 1
    if not any("imported through:" in diag.message and "app.i" in diag.message for diag in duplicate_proc_diags):
        print("lsp: expected duplicate declaration diagnostic to include import chain")
        print(duplicate_proc_diags)
        return 1
    duplicate_global_diags = [
        diag for diag in duplicate_doc.diagnostics if "module error: duplicate global declaration 'global_payload'" in diag.message
    ]
    if not duplicate_global_diags:
        print("lsp: expected cross-import duplicate global declaration diagnostic")
        print(duplicate_doc.diagnostics)
        return 1
    if not any("imported through:" in diag.message and "app.i" in diag.message for diag in duplicate_global_diags):
        print("lsp: expected duplicate global diagnostic to include import chain")
        print(duplicate_global_diags)
        return 1
    module_doc = workspace.documents[lsp.path_to_uri(module)]
    if not any("semantic error: duplicate proc parameter 'value'" in diag.message for diag in module_doc.diagnostics):
        print("lsp: expected duplicate proc parameter diagnostic")
        print(module_doc.diagnostics)
        return 1
    if not any("semantic error: duplicate local declaration 'value'" in diag.message for diag in module_doc.diagnostics):
        print("lsp: expected parameter/local duplicate diagnostic")
        print(module_doc.diagnostics)
        return 1
    if not any("semantic error: duplicate field 'value'" in diag.message for diag in module_doc.diagnostics):
        print("lsp: expected duplicate field diagnostic")
        print(module_doc.diagnostics)
        return 1
    if not any("semantic error: duplicate enum item 'One'" in diag.message for diag in module_doc.diagnostics):
        print("lsp: expected duplicate enum item diagnostic")
        print(module_doc.diagnostics)
        return 1
    payload_doc_symbol = lsp.symbol_to_lsp(workspace.find_symbol("Payload"), module_doc)
    payload_children = payload_doc_symbol.get("children", [])
    if (
        payload_doc_symbol.get("name") != "Payload"
        or not any(child.get("name") == "value" and child.get("detail") == "i32" for child in payload_children)
        or not any(child.get("name") == "values" and child.get("detail") == "[4]i32" for child in payload_children)
    ):
        print("lsp: expected Payload document symbol to include field children")
        print(payload_doc_symbol)
        return 1
    payload_hover = lsp.hover_markdown_for_symbol(workspace, workspace.find_symbol("Payload"))
    if (
        "`Payload:struct = {`" not in payload_hover
        or "fields:" not in payload_hover
        or "- `value: i32` attrs: `editor,serialize`" not in payload_hover
        or "- `values: [4]i32`" not in payload_hover
    ):
        print("lsp: expected struct hover to summarize fields and attrs")
        print(payload_hover)
        return 1
    kind_doc_symbol = lsp.symbol_to_lsp(workspace.find_symbol("Kind"), module_doc)
    kind_children = kind_doc_symbol.get("children", [])
    if (
        kind_doc_symbol.get("name") != "Kind"
        or not any(child.get("name") == "None" and child.get("detail") == "Kind_None" for child in kind_children)
        or not any(child.get("name") == "Ready" and child.get("detail") == "Kind_Ready" for child in kind_children)
    ):
        print("lsp: expected Kind document symbol to include enum member children")
        print(kind_doc_symbol)
        return 1
    kind_hover = lsp.hover_markdown_for_symbol(workspace, workspace.find_symbol("Kind"))
    if (
        "`Kind:enum = {`" not in kind_hover
        or "values:" not in kind_hover
        or "- `None` = `Kind_None`" not in kind_hover
        or "- `Ready` = `Kind_Ready`" not in kind_hover
    ):
        print("lsp: expected enum hover to summarize generated values")
        print(kind_hover)
        return 1
    value_duplicate_diags = [
        diag
        for checked_doc in workspace.documents.values()
        for diag in checked_doc.diagnostics
        if "module error: duplicate value declaration 'shared_value'" in diag.message
    ]
    if not value_duplicate_diags or not any(
        "app.i" in diag.message or "value_duplicate.i" in diag.message for diag in value_duplicate_diags
    ):
        print("lsp: expected proc/global generated value namespace diagnostic")
        print({uri: checked_doc.diagnostics for uri, checked_doc in workspace.documents.items()})
        return 1
    if not any("imported through:" in diag.message for diag in value_duplicate_diags):
        print("lsp: expected proc/global value duplicate diagnostic to include import chain")
        print(value_duplicate_diags)
        return 1

    if "Payload" not in workspace.symbols or "payload_add" not in workspace.symbols:
        print("lsp: import symbols were not indexed")
        return 1
    enum_member = workspace.find_symbol("Kind_Ready")
    if enum_member is None or enum_member.kind != "enumMember":
        print("lsp: imported enum member was not indexed")
        return 1
    enum_refs = workspace.enum_member_references(enum_member)
    if len(enum_refs) != 2:
        print("lsp: expected enum member references to include declaration and usage")
        print(enum_refs)
        return 1
    enum_rename = workspace.enum_member_rename_edits(enum_member, "Kind_Done")
    flattened_enum_edits = [
        (uri, edit["range"]["start"]["line"], edit["range"]["start"]["character"], edit["newText"])
        for uri, edits in enum_rename.get("changes", {}).items()
        for edit in edits
    ]
    if len(flattened_enum_edits) != 2:
        print("lsp: expected enum member rename edits for declaration and usage")
        print(enum_rename)
        return 1
    refs = workspace.references("Payload")
    if len(refs) < 2:
        print("lsp: expected cross-import Payload references")
        return 1
    rename = workspace.rename_edits("payload_add", "payload_add2")
    if not rename.get("changes"):
        print("lsp: expected rename edits")
        return 1
    tokens = lsp.semantic_tokens_for_doc(workspace, doc)
    if not tokens:
        print("lsp: expected semantic tokens")
        return 1
    decoded_tokens = decoded_semantic_tokens(lsp, tokens)
    decoded_tokens_with_modifiers = decoded_semantic_tokens_with_modifiers(lsp, tokens)
    module_tokens = decoded_semantic_tokens(lsp, lsp.semantic_tokens_for_doc(workspace, module_doc))
    module_tokens_with_modifiers = decoded_semantic_tokens_with_modifiers(lsp, lsp.semantic_tokens_for_doc(workspace, module_doc))
    fields = workspace.fields_for_owner("Payload")
    if not any(field.name == "value" for field in fields):
        print("lsp: expected imported Payload.value field")
        return 1
    app_lines = doc.text.splitlines()
    generic_payload_decl_line = next(i for i, line in enumerate(app_lines) if "payload_values:Array<Payload>" in line)
    generic_array_decl_col = app_lines[generic_payload_decl_line].index("Array")
    generic_payload_decl_col = app_lines[generic_payload_decl_line].index("Payload")
    if not any(
        ref["uri"] == doc.uri
        and ref["range"]["start"]["line"] == generic_payload_decl_line
        and ref["range"]["start"]["character"] == generic_payload_decl_col
        and ref["range"]["end"]["character"] == generic_payload_decl_col + len("Payload")
        for ref in refs
    ):
        print("lsp: expected Payload references to include generic type argument range")
        print(refs)
        return 1
    array_refs = workspace.references("Array")
    if not any(
        ref["uri"] == doc.uri
        and ref["range"]["start"]["line"] == generic_payload_decl_line
        and ref["range"]["start"]["character"] == generic_array_decl_col
        and ref["range"]["end"]["character"] == generic_array_decl_col + len("Array")
        for ref in array_refs
    ):
        print("lsp: expected Array references to include only the generic base range")
        print(array_refs)
        return 1
    payload_rename = workspace.rename_edits("Payload", "Player")
    payload_rename_edits = [
        (uri, edit["range"]["start"]["line"], edit["range"]["start"]["character"], edit["range"]["end"]["character"], edit["newText"])
        for uri, edits in payload_rename.get("changes", {}).items()
        for edit in edits
    ]
    if (
        (doc.uri, generic_payload_decl_line, generic_payload_decl_col, generic_payload_decl_col + len("Payload"), "Player")
        not in payload_rename_edits
    ):
        print("lsp: expected Payload rename to edit only the generic type argument")
        print(payload_rename)
        return 1
    field_line = next(i for i, line in enumerate(app_lines) if "p.value" in line)
    field_col = app_lines[field_line].index("value")
    field = lsp.field_access_at(workspace, doc, field_line, field_col)
    if field is None or field.name != "value" or field.type_name != "i32" or field.attrs != "editor,serialize":
        print("lsp: expected field hover/definition lookup for p.value")
        return 1
    field_refs = workspace.field_references(field)
    if len(field_refs) != 8:
        print("lsp: expected Payload.value references to include declaration and seven usages")
        print(field_refs)
        return 1
    highlight_server = lsp.LspServer()
    highlight_server.workspace = workspace
    field_highlights = highlight_server.document_highlights_at_request(
        {
            "textDocument": {"uri": doc.uri},
            "position": {"line": field_line, "character": field_col},
        }
    )
    expected_field_highlights = [ref for ref in field_refs if ref.get("uri") == doc.uri]
    if (
        len(field_highlights) != len(expected_field_highlights)
        or not any(highlight.get("range", {}).get("start", {}).get("line") == field_line for highlight in field_highlights)
        or any(highlight.get("kind") != 1 for highlight in field_highlights)
    ):
        print("lsp: expected documentHighlight for Payload.value to mirror same-document references")
        print(field_highlights)
        return 1
    other_line = next(i for i, line in enumerate(app_lines) if "o.value" in line)
    other_col = app_lines[other_line].index("value")
    if any(ref["uri"] == doc.uri and ref["range"]["start"]["line"] == other_line and ref["range"]["start"]["character"] == other_col for ref in field_refs):
        print("lsp: Payload.value references should not include Other.value")
        print(field_refs)
        return 1
    field_rename = workspace.field_rename_edits(field, "payload_value")
    flattened_field_edits = [
        (uri, edit["range"]["start"]["line"], edit["range"]["start"]["character"], edit["newText"])
        for uri, edits in field_rename.get("changes", {}).items()
        for edit in edits
    ]
    if len(flattened_field_edits) != 8 or any(edit[3] != "payload_value" for edit in flattened_field_edits):
        print("lsp: expected field-aware rename edits for Payload.value")
        print(field_rename)
        return 1
    if any(edit[0] == doc.uri and edit[1] == other_line and edit[2] == other_col for edit in flattened_field_edits):
        print("lsp: field-aware rename should not edit Other.value")
        print(field_rename)
        return 1
    module_lines = module_doc.text.splitlines()
    field_decl_line = next(i for i, line in enumerate(module_lines) if "value:i32" in line)
    field_decl_col = module_lines[field_decl_line].index("value")
    decl_server = lsp.LspServer()
    decl_server.workspace = workspace
    field_decl_symbol = decl_server.symbol_at_request(
        {
            "textDocument": {"uri": module_doc.uri},
            "position": {"line": field_decl_line, "character": field_decl_col},
        }
    )
    if not isinstance(field_decl_symbol, lsp.FieldSymbol) or field_decl_symbol.owner != "Payload":
        print("lsp: expected field declaration request to resolve Payload.value")
        print(field_decl_symbol)
        return 1
    field_decl_prepare = decl_server.prepare_rename_at_request(
        {
            "textDocument": {"uri": module_doc.uri},
            "position": {"line": field_decl_line, "character": field_decl_col},
        }
    )
    if field_decl_prepare != lsp.position_to_lsp(field_decl_line, field_decl_col, field_decl_col + len("value")):
        print("lsp: expected prepareRename range for field declaration")
        print(field_decl_prepare)
        return 1
    field_usage_prepare = decl_server.prepare_rename_at_request(
        {
            "textDocument": {"uri": doc.uri},
            "position": {"line": field_line, "character": field_col},
        }
    )
    if field_usage_prepare != lsp.position_to_lsp(field_line, field_col, field_col + len("value")):
        print("lsp: expected prepareRename range for field usage")
        print(field_usage_prepare)
        return 1
    whitespace_prepare = decl_server.prepare_rename_at_request(
        {
            "textDocument": {"uri": doc.uri},
            "position": {"line": field_line, "character": 0},
        }
    )
    if whitespace_prepare is not None:
        print("lsp: prepareRename should reject whitespace")
        print(whitespace_prepare)
        return 1
    field_decl_rename = workspace.field_rename_edits(field_decl_symbol, "payload_value")
    flattened_field_decl_edits = [
        (uri, edit["range"]["start"]["line"], edit["range"]["start"]["character"], edit["newText"])
        for uri, edits in field_decl_rename.get("changes", {}).items()
        for edit in edits
    ]
    if flattened_field_decl_edits != flattened_field_edits:
        print("lsp: field declaration rename should match field usage rename")
        print(flattened_field_decl_edits)
        print(flattened_field_edits)
        return 1
    missing_line = next(i for i, line in enumerate(app_lines) if "p.missing" in line)
    missing_col = app_lines[missing_line].index("missing")
    value_property = (
        field_line,
        field_col,
        len("value"),
        "property",
    )
    missing_property = (
        missing_line,
        missing_col,
        len("missing"),
        "property",
    )
    if value_property not in decoded_tokens:
        print("lsp: expected real field access to receive property semantic token")
        print(decoded_tokens)
        return 1
    if missing_property in decoded_tokens:
        print("lsp: missing field should not receive property semantic token")
        print(decoded_tokens)
        return 1
    field_decl_token = (
        field_decl_line,
        field_decl_col,
        len("value"),
        "property",
    )
    if field_decl_token not in module_tokens:
        print("lsp: expected struct field declaration to receive property semantic token")
        print(module_tokens)
        return 1
    if not has_semantic_token(module_tokens_with_modifiers, field_decl_line, field_decl_col, len("value"), "property", "declaration", "member"):
        print("lsp: expected struct field declaration semantic token modifiers")
        print(module_tokens_with_modifiers)
        return 1
    attr_operator_col = module_lines[field_decl_line].index("@")
    attr_string_col = module_lines[field_decl_line].index('"editor,serialize"')
    attr_operator_token = (
        field_decl_line,
        attr_operator_col,
        len("@"),
        "operator",
    )
    attr_string_token = (
        field_decl_line,
        attr_string_col,
        len('"editor,serialize"'),
        "string",
    )
    if attr_operator_token not in module_tokens or attr_string_token not in module_tokens:
        print("lsp: expected field attribute operator and string semantic tokens")
        print(module_tokens)
        return 1
    array_count_line = next(i for i, line in enumerate(module_lines) if "values:[4]i32" in line)
    array_count_col = module_lines[array_count_line].index("4")
    array_count_token = (
        array_count_line,
        array_count_col,
        len("4"),
        "number",
    )
    if array_count_token not in module_tokens:
        print("lsp: expected fixed array count to receive number semantic token")
        print(module_tokens)
        return 1
    enum_line = next(i for i, line in enumerate(app_lines) if "Kind_Ready" in line)
    enum_col = app_lines[enum_line].index("Kind_Ready")
    enum_token = (
        enum_line,
        enum_col,
        len("Kind_Ready"),
        "enumMember",
    )
    if enum_token not in decoded_tokens:
        print("lsp: expected enum member semantic token")
        print(decoded_tokens)
        return 1
    generic_line = next(i for i, line in enumerate(app_lines) if "Array<i32>reserve" in line)
    generic_array_col = app_lines[generic_line].index("Array<i32>reserve")
    generic_arg_col = generic_array_col + len("Array<")
    generic_proc_col = generic_array_col + len("Array<i32>")
    generic_type_token = (
        generic_line,
        generic_array_col,
        len("Array"),
        "type",
    )
    generic_arg_token = (
        generic_line,
        generic_arg_col,
        len("i32"),
        "type",
    )
    generic_proc_token = (
        generic_line,
        generic_proc_col,
        len("reserve"),
        "function",
    )
    if generic_type_token not in decoded_tokens or generic_arg_token not in decoded_tokens or generic_proc_token not in decoded_tokens:
        print("lsp: expected generic proc semantic tokens to split type args from proc tail")
        print(decoded_tokens)
        return 1
    if (
        not has_semantic_token(decoded_tokens_with_modifiers, generic_line, generic_array_col, len("Array"), "type", "generic")
        or not has_semantic_token(decoded_tokens_with_modifiers, generic_line, generic_arg_col, len("i32"), "type", "defaultLibrary")
        or not has_semantic_token(decoded_tokens_with_modifiers, generic_line, generic_proc_col, len("reserve"), "function", "generic")
    ):
        print("lsp: expected generic proc semantic token modifiers")
        print(decoded_tokens_with_modifiers)
        return 1
    generic_decl_line = next(i for i, line in enumerate(module_lines) if "Array<T>reserve:proc<T>" in line)
    generic_decl_array_col = module_lines[generic_decl_line].index("Array<T>reserve")
    generic_decl_arg_col = generic_decl_array_col + len("Array<")
    generic_decl_proc_col = generic_decl_array_col + len("Array<T>")
    generic_decl_type_token = (
        generic_decl_line,
        generic_decl_array_col,
        len("Array"),
        "type",
    )
    generic_decl_arg_token = (
        generic_decl_line,
        generic_decl_arg_col,
        len("T"),
        "type",
    )
    generic_decl_proc_token = (
        generic_decl_line,
        generic_decl_proc_col,
        len("reserve"),
        "function",
    )
    if (
        generic_decl_type_token not in module_tokens
        or generic_decl_arg_token not in module_tokens
        or generic_decl_proc_token not in module_tokens
    ):
        print("lsp: expected generic proc declaration semantic tokens to split type args from proc tail")
        print(module_tokens)
        return 1
    if (
        not has_semantic_token(module_tokens_with_modifiers, generic_decl_line, generic_decl_array_col, len("Array"), "type", "generic")
        or not has_semantic_token(module_tokens_with_modifiers, generic_decl_line, generic_decl_proc_col, len("reserve"), "function", "definition", "generic")
    ):
        print("lsp: expected generic proc declaration semantic token modifiers")
        print(module_tokens_with_modifiers)
        return 1
    proc_ptr_call_line = next(i for i, line in enumerate(app_lines) if "total = cb(p.&, 4);" in line)
    proc_ptr_call_col = app_lines[proc_ptr_call_line].index("cb")
    proc_ptr_call_token = (
        proc_ptr_call_line,
        proc_ptr_call_col,
        len("cb"),
        "function",
    )
    global_proc_ptr_call_line = next(i for i, line in enumerate(app_lines) if "global_cb(p.&, 4)" in line)
    global_proc_ptr_call_col = app_lines[global_proc_ptr_call_line].index("global_cb")
    global_proc_ptr_call_token = (
        global_proc_ptr_call_line,
        global_proc_ptr_call_col,
        len("global_cb"),
        "function",
    )
    field_proc_ptr_call_line = next(i for i, line in enumerate(app_lines) if "handler.cb(p.&, 4)" in line)
    field_proc_ptr_call_col = app_lines[field_proc_ptr_call_line].index("cb")
    field_proc_ptr_call_token = (
        field_proc_ptr_call_line,
        field_proc_ptr_call_col,
        len("cb"),
        "function",
    )
    if (
        proc_ptr_call_token not in decoded_tokens
        or global_proc_ptr_call_token not in decoded_tokens
        or field_proc_ptr_call_token not in decoded_tokens
    ):
        print("lsp: expected proc pointer call-site semantic tokens to be functions")
        print(decoded_tokens)
        return 1
    if not has_semantic_token(decoded_tokens_with_modifiers, field_proc_ptr_call_line, field_proc_ptr_call_col, len("cb"), "function", "member"):
        print("lsp: expected proc pointer field call semantic token member modifier")
        print(decoded_tokens_with_modifiers)
        return 1
    ready_decl_line = next(i for i, line in enumerate(module_lines) if "Ready" in line)
    ready_decl_col = module_lines[ready_decl_line].index("Ready")
    ready_decl_token = (
        ready_decl_line,
        ready_decl_col,
        len("Ready"),
        "enumMember",
    )
    if ready_decl_token not in module_tokens:
        print("lsp: expected enum item declaration to receive enumMember semantic token")
        print(module_tokens)
        return 1
    enum_decl_edit = (
        module_doc.uri,
        ready_decl_line,
        ready_decl_col,
        "Done",
    )
    enum_usage_edit = (
        doc.uri,
        enum_line,
        enum_col,
        "Kind_Done",
    )
    if enum_decl_edit not in flattened_enum_edits or enum_usage_edit not in flattened_enum_edits:
        print("lsp: expected enum rename to edit source item and generated usage names")
        print(flattened_enum_edits)
        return 1
    completion_col = app_lines[field_line].index(".") + 1
    completions = lsp.field_completions_at(workspace, doc, field_line, completion_col)
    if not any(item.name == "value" for item in completions):
        print("lsp: expected field completion for p.")
        return 1
    value_completion = next(item for item in completions if item.name == "value")
    value_completion_lsp = lsp.field_completion_to_lsp(workspace, value_completion)
    if "attrs: `editor,serialize`" not in value_completion_lsp.get("documentation", {}).get("value", ""):
        print("lsp: expected field completion documentation to include reflection attrs")
        print(value_completion_lsp)
        return 1
    if value_completion_lsp.get("data") != {
        "kind": "field",
        "name": "Payload.value",
        "uri": module_doc.uri,
        "line": field_decl_line,
        "character": field_decl_col,
    }:
        print("lsp: expected field completion data to identify source field")
        print(value_completion_lsp)
        return 1
    stale_field_completion = {
        "label": "value",
        "data": value_completion_lsp["data"],
    }
    resolved_field_completion = lsp.resolve_completion_item(workspace, stale_field_completion)
    if (
        resolved_field_completion.get("detail") != "Payload.value: i32"
        or "attrs: `editor,serialize`" not in resolved_field_completion.get("documentation", {}).get("value", "")
        or resolved_field_completion.get("data") != value_completion_lsp["data"]
    ):
        print("lsp: expected completionItem/resolve to rehydrate field detail and docs")
        print(resolved_field_completion)
        return 1
    pointer_field_line = next(i for i, line in enumerate(app_lines) if "payload_ptr[0].value" in line)
    pointer_field_col = app_lines[pointer_field_line].index("value")
    pointer_field = lsp.field_access_at(workspace, doc, pointer_field_line, pointer_field_col)
    if pointer_field is None or pointer_field.owner != "Payload" or pointer_field.name != "value":
        print("lsp: expected pointer field lookup for payload_ptr[0].value")
        return 1
    nested_field_line = next(i for i, line in enumerate(app_lines) if "payload_values.data[0].value" in line)
    nested_field_col = app_lines[nested_field_line].rindex("value")
    nested_field = lsp.field_access_at(workspace, doc, nested_field_line, nested_field_col)
    if nested_field is None or nested_field.owner != "Payload" or nested_field.name != "value":
        print("lsp: expected chained generic field lookup for payload_values.data[0].value")
        return 1
    nested_data_col = app_lines[nested_field_line].index("data")
    nested_data_field = lsp.field_access_at(workspace, doc, nested_field_line, nested_data_col)
    if (
        nested_data_field is None
        or nested_data_field.owner != "Array"
        or nested_data_field.name != "data"
        or nested_data_field.type_name != "*Payload"
        or nested_data_field.detail != "Array<Payload>.data: *Payload"
    ):
        print("lsp: expected generic field access hover detail to substitute concrete type")
        print(nested_data_field)
        return 1
    nested_completion_col = app_lines[nested_field_line].rindex(".") + 1
    nested_completions = lsp.field_completions_at(workspace, doc, nested_field_line, nested_completion_col)
    if not any(item.owner == "Payload" and item.name == "value" for item in nested_completions):
        print("lsp: expected chained generic field completion for payload_values.data[0].")
        return 1
    generic_data_completion_col = app_lines[nested_field_line].index(".") + 1
    generic_data_completions = lsp.field_completions_at(workspace, doc, nested_field_line, generic_data_completion_col)
    generic_data_completion = next((item for item in generic_data_completions if item.name == "data"), None)
    if (
        generic_data_completion is None
        or generic_data_completion.type_name != "*Payload"
        or generic_data_completion.detail != "Array<Payload>.data: *Payload"
    ):
        print("lsp: expected generic field completion to show substituted concrete type")
        print(generic_data_completions)
        return 1
    generic_data_completion_lsp = lsp.field_completion_to_lsp(workspace, generic_data_completion)
    if generic_data_completion_lsp.get("detail") != "Array<Payload>.data: *Payload":
        print("lsp: expected generic field completion item detail to use substituted concrete type")
        print(generic_data_completion_lsp)
        return 1
    mat3_alias = workspace.find_symbol("Mat3")
    if mat3_alias is None or mat3_alias.kind != "alias" or "Mat3:alias = [3]Vec3" not in mat3_alias.detail:
        print("lsp: expected fixed-array alias symbol for Mat3")
        print(mat3_alias)
        return 1
    basis_element_line = next(i for i, line in enumerate(app_lines) if "basis_element:f32" in line)
    basis_element_type = lsp.infer_simple_expr_type(workspace, doc, "basis[0][0]", basis_element_line)
    if basis_element_type != "f32":
        print("lsp: expected nested fixed-array alias indexing to infer f32")
        print(basis_element_type)
        return 1
    local = workspace.find_variable(doc, "total")
    if local is None or local.type_name != "i32" or local.detail != "total: i32":
        print("lsp: expected local variable type info")
        return 1
    imported_global = workspace.find_variable(doc, "global_payload")
    if imported_global is None or imported_global.kind != "global" or imported_global.type_name != "Payload":
        print("lsp: expected imported global variable type info")
        return 1
    param = workspace.documents[lsp.path_to_uri(module)].variables.get("p")
    if param is None or param.kind != "parameter" or param.type_name != "*Payload":
        print("lsp: expected proc parameter type info")
        return 1
    completion_items = workspace.completion_symbols_for_doc(doc)
    if not any(isinstance(item, lsp.VariableSymbol) and item.name == "total" for item in completion_items):
        print("lsp: expected local variables in completion symbols")
        return 1
    if not any(isinstance(item, lsp.VariableSymbol) and item.name == "global_payload" for item in completion_items):
        print("lsp: expected imported global variables in completion symbols")
        return 1
    global_line = next(i for i, line in enumerate(app_lines) if "global_payload.value = 7" in line)
    helper_global_line = next(i for i, line in enumerate(app_lines) if "global_payload:Other" in line)
    main_completion_items = workspace.completion_symbols_at(doc, global_line)
    main_global_completion = next(
        item for item in main_completion_items if isinstance(item, lsp.VariableSymbol) and item.name == "global_payload"
    )
    if main_global_completion.kind != "global":
        print("lsp: main completion should use imported global, not unrelated local shadow")
        print(main_global_completion)
        return 1
    helper_completion_items = workspace.completion_symbols_at(doc, helper_global_line)
    helper_global_completion = next(
        item for item in helper_completion_items if isinstance(item, lsp.VariableSymbol) and item.name == "global_payload"
    )
    if helper_global_completion.kind != "variable" or helper_global_completion.line != helper_global_line:
        print("lsp: helper completion should prefer same-proc local shadow")
        print(helper_global_completion)
        return 1
    helper_shadow_field_line = next(i for i, line in enumerate(app_lines) if "global_payload.value = total" in line)
    helper_shadow_field_col = app_lines[helper_shadow_field_line].index(".") + 1
    helper_shadow_fields = lsp.field_completions_at(workspace, doc, helper_shadow_field_line, helper_shadow_field_col)
    if (
        not any(item.owner == "Other" and item.name == "value" for item in helper_shadow_fields)
        or any(item.owner == "Payload" and item.name == "values" for item in helper_shadow_fields)
    ):
        print("lsp: field completion should use local shadow type instead of imported global type")
        print(helper_shadow_fields)
        return 1
    payload_completion_count = sum(
        1 for item in completion_items if isinstance(item, lsp.Symbol) and item.name == "payload_add"
    )
    if payload_completion_count != 1:
        print("lsp: duplicate imported declarations should not duplicate completion entries")
        print([item.name for item in completion_items])
        return 1
    generic_completion = next(
        item for item in completion_items if isinstance(item, lsp.Symbol) and item.name == "Array<T>reserve"
    )
    generic_completion_lsp = lsp.completion_to_lsp(workspace, generic_completion)
    if (
        generic_completion_lsp.get("label") != "Array<T>reserve"
        or generic_completion_lsp.get("insertTextFormat") != 2
        or generic_completion_lsp.get("insertText") != "Array<T>reserve(${1:length})$0"
        or generic_completion_lsp.get("filterText") != "Arrayreserve"
        or generic_completion_lsp.get("data", {}).get("kind") != "proc"
        or generic_completion_lsp.get("data", {}).get("name") != "Array<T>reserve"
        or generic_completion_lsp.get("data", {}).get("uri") != module_doc.uri
    ):
        print("lsp: expected generic completion to provide simplified filterText")
        print(generic_completion_lsp)
        return 1
    stale_generic_completion = {
        "label": "Array<T>reserve",
        "data": generic_completion_lsp["data"],
    }
    resolved_generic_completion = lsp.resolve_completion_item(workspace, stale_generic_completion)
    if (
        resolved_generic_completion.get("detail") != "Array<T>reserve:proc(length:u64)->Array<T>"
        or resolved_generic_completion.get("filterText") != "Arrayreserve"
        or resolved_generic_completion.get("insertTextFormat") != 2
        or resolved_generic_completion.get("insertText") != "Array<T>reserve(${1:length})$0"
        or resolved_generic_completion.get("data") != generic_completion_lsp["data"]
    ):
        print("lsp: expected completionItem/resolve to rehydrate generic proc completion")
        print(resolved_generic_completion)
        return 1
    payload_completion = next(
        item for item in completion_items if isinstance(item, lsp.Symbol) and item.name == "Payload"
    )
    payload_completion_lsp = lsp.completion_to_lsp(workspace, payload_completion)
    if (
        payload_completion_lsp.get("detail") != "Payload:struct = {"
        or "fields:" not in payload_completion_lsp.get("documentation", {}).get("value", "")
        or "- `value: i32` attrs: `editor,serialize`" not in payload_completion_lsp.get("documentation", {}).get("value", "")
        or payload_completion_lsp.get("data", {}).get("kind") != "struct"
        or payload_completion_lsp.get("data", {}).get("name") != "Payload"
    ):
        print("lsp: expected struct completion documentation to summarize fields")
        print(payload_completion_lsp)
        return 1
    resolved_payload_completion = lsp.resolve_completion_item(
        workspace,
        {"label": "Payload", "data": payload_completion_lsp["data"]},
    )
    if (
        resolved_payload_completion.get("detail") != "Payload:struct = {"
        or "fields:" not in resolved_payload_completion.get("documentation", {}).get("value", "")
        or "- `values: [4]i32`" not in resolved_payload_completion.get("documentation", {}).get("value", "")
    ):
        print("lsp: expected completionItem/resolve to rehydrate struct field docs")
        print(resolved_payload_completion)
        return 1
    kind_completion = next(
        item for item in completion_items if isinstance(item, lsp.Symbol) and item.name == "Kind"
    )
    kind_completion_lsp = lsp.completion_to_lsp(workspace, kind_completion)
    if (
        kind_completion_lsp.get("detail") != "Kind:enum = {"
        or "values:" not in kind_completion_lsp.get("documentation", {}).get("value", "")
        or "- `Ready` = `Kind_Ready`" not in kind_completion_lsp.get("documentation", {}).get("value", "")
        or kind_completion_lsp.get("data", {}).get("kind") != "enum"
        or kind_completion_lsp.get("data", {}).get("name") != "Kind"
    ):
        print("lsp: expected enum completion documentation to summarize generated values")
        print(kind_completion_lsp)
        return 1
    cb_completion = next(
        item for item in completion_items if isinstance(item, lsp.VariableSymbol) and item.name == "cb"
    )
    cb_completion_lsp = lsp.variable_completion_to_lsp(workspace, cb_completion)
    if cb_completion_lsp.get("data", {}).get("kind") != "variable" or cb_completion_lsp.get("data", {}).get("name") != "cb":
        print("lsp: expected variable completion data")
        print(cb_completion_lsp)
        return 1
    if "resolves to `*proc(payload:*Payload, amount:i32)->i32`" not in cb_completion_lsp.get("documentation", {}).get("value", ""):
        print("lsp: expected proc pointer variable completion documentation to resolve alias")
        print(cb_completion_lsp)
        return 1
    resolved_cb_completion = lsp.resolve_completion_item(workspace, {"label": "cb", "data": cb_completion_lsp["data"]})
    if (
        resolved_cb_completion.get("detail") != "cb: Callback"
        or "resolves to `*proc(payload:*Payload, amount:i32)->i32`"
        not in resolved_cb_completion.get("documentation", {}).get("value", "")
    ):
        print("lsp: expected completionItem/resolve to rehydrate variable completion")
        print(resolved_cb_completion)
        return 1
    unresolved_completion = {"label": "missing", "data": {"kind": "proc", "name": "missing", "uri": doc.uri}}
    if lsp.resolve_completion_item(workspace, unresolved_completion) != unresolved_completion:
        print("lsp: unresolved completionItem/resolve should return the original item")
        return 1
    callback_alias = workspace.find_symbol("Callback")
    callback_alias_completion_lsp = lsp.completion_to_lsp(workspace, callback_alias) if callback_alias else {}
    if "resolves to `*proc(payload:*Payload, amount:i32)->i32`" not in callback_alias_completion_lsp.get("documentation", {}).get("value", ""):
        print("lsp: expected alias completion documentation to resolve alias chain")
        print(callback_alias_completion_lsp)
        return 1
    server = lsp.LspServer()
    server.workspace = workspace
    generic_payload_line = next(i for i, line in enumerate(app_lines) if "Array<Payload>reserve" in line)
    generic_payload_call_col = app_lines[generic_payload_line].rindex("Array<Payload>reserve")
    generic_base_resolved = server.symbol_at_request(
        {
            "textDocument": {"uri": doc.uri},
            "position": {"line": generic_payload_line, "character": generic_payload_call_col},
        }
    )
    if not isinstance(generic_base_resolved, lsp.Symbol) or generic_base_resolved.name != "Array":
        print("lsp: expected generic base type lookup to resolve Array")
        print(generic_base_resolved)
        return 1
    generic_arg_resolved = server.symbol_at_request(
        {
            "textDocument": {"uri": doc.uri},
            "position": {"line": generic_payload_line, "character": generic_payload_call_col + len("Array<")},
        }
    )
    if not isinstance(generic_arg_resolved, lsp.Symbol) or generic_arg_resolved.name != "Payload":
        print("lsp: expected generic type argument lookup to resolve Payload")
        print(generic_arg_resolved)
        return 1
    generic_proc_resolved = server.symbol_at_request(
        {
            "textDocument": {"uri": doc.uri},
            "position": {"line": generic_payload_line, "character": generic_payload_call_col + len("Array<Payload>")},
        }
    )
    if (
        not isinstance(generic_proc_resolved, lsp.Symbol)
        or generic_proc_resolved.name != "Array<Payload>reserve"
        or "Array<Payload>reserve:proc(length:u64)->Array<Payload>" not in generic_proc_resolved.detail
    ):
        print("lsp: expected generic proc tail lookup to resolve Array<T>reserve")
        print(generic_proc_resolved)
        return 1
    generic_proc_hover = lsp.hover_markdown_for_symbol(workspace, generic_proc_resolved)
    if "Array<Payload>reserve:proc(length:u64)->Array<Payload>" not in generic_proc_hover:
        print("lsp: expected generic proc hover to show substituted concrete type")
        print(generic_proc_hover)
        return 1
    json_array_line = next(i for i, line in enumerate(app_lines) if "json_read<Array<Payload>>(payload_values" in line)
    json_array_col = app_lines[json_array_line].index("json_read")
    json_array_resolved = server.symbol_at_request(
        {
            "textDocument": {"uri": doc.uri},
            "position": {"line": json_array_line, "character": json_array_col},
        }
    )
    if (
        not isinstance(json_array_resolved, lsp.Symbol)
        or json_array_resolved.name != "json_read<Array<Payload>>"
        or "json_read<Array<Payload>>:proc(out:*Array<Payload>)->b32" not in json_array_resolved.detail
    ):
        print("lsp: expected generic pattern proc lookup to resolve json_read<Array<Payload>>")
        print(json_array_resolved)
        return 1
    json_vec_line = next(i for i, line in enumerate(app_lines) if "json_read<Vec<Payload>>(payload_vec" in line)
    json_vec_col = app_lines[json_vec_line].index("json_read")
    json_vec_signature = lsp.signature_help_at(
        workspace,
        doc,
        json_vec_line,
        app_lines[json_vec_line].rindex("payload_vec"),
    )
    if (
        json_vec_signature is None
        or json_vec_signature.get("activeParameter") != 0
        or not json_vec_signature.get("signatures")
        or json_vec_signature["signatures"][0].get("label") != "json_read<Vec<Payload>>:proc(out:*Vec<Payload>)->b32"
        or json_vec_signature["signatures"][0].get("parameters") != [{"label": "out:*Vec<Payload>"}]
    ):
        print("lsp: expected generic pattern proc signature help for json_read<Vec<Payload>>")
        print(json_vec_signature)
        return 1
    json_value_line = next(i for i, line in enumerate(app_lines) if "json_read<i32>(total" in line)
    json_value_col = app_lines[json_value_line].index("json_read")
    json_value_resolved = server.symbol_at_request(
        {
            "textDocument": {"uri": doc.uri},
            "position": {"line": json_value_line, "character": json_value_col},
        }
    )
    if (
        not isinstance(json_value_resolved, lsp.Symbol)
        or json_value_resolved.name != "json_read<i32>"
        or "json_read<i32>:proc(out:*i32)->b32" not in json_value_resolved.detail
    ):
        print("lsp: expected concrete proc specialization lookup to resolve json_read<i32>")
        print(json_value_resolved)
        return 1
    json_value_signature = lsp.signature_help_at(
        workspace,
        doc,
        json_value_line,
        app_lines[json_value_line].index("total"),
    )
    if (
        json_value_signature is None
        or json_value_signature["signatures"][0].get("label") != "json_read<i32>:proc(out:*i32)->b32"
        or json_value_signature["signatures"][0].get("parameters") != [{"label": "out:*i32"}]
    ):
        print("lsp: expected concrete proc specialization signature help for json_read<i32>")
        print(json_value_signature)
        return 1
    json_tokens = decoded_semantic_tokens(
        lsp,
        lsp.semantic_tokens_for_doc(workspace, doc),
    )
    if (
        (json_array_line, json_array_col, len("json_read"), "function") not in json_tokens
        or (json_array_line, app_lines[json_array_line].index("Array"), len("Array"), "type") not in json_tokens
        or (json_array_line, app_lines[json_array_line].index("Payload"), len("Payload"), "type") not in json_tokens
    ):
        print("lsp: expected semantic tokens for nested generic proc call")
        print(json_tokens)
        return 1
    field_decl_resolved = server.symbol_at_request(
        {
            "textDocument": {"uri": module_doc.uri},
            "position": {"line": field_decl_line, "character": field_decl_col},
        }
    )
    if not isinstance(field_decl_resolved, lsp.FieldSymbol) or field_decl_resolved.detail != "Payload.value: i32":
        print("lsp: expected field declaration lookup to resolve Payload.value")
        return 1
    field_decl_hover = lsp.hover_markdown_for_symbol(workspace, field_decl_resolved)
    if "attrs: `editor,serialize`" not in field_decl_hover:
        print("lsp: expected field hover to include reflection attributes")
        print(field_decl_hover)
        return 1
    total_line = max(i for i, line in enumerate(app_lines) if "return total" in line)
    total_col = app_lines[total_line].index("total")
    resolved = server.symbol_at_request(
        {
            "textDocument": {"uri": doc.uri},
            "position": {"line": total_line, "character": total_col},
        }
    )
    if not isinstance(resolved, lsp.VariableSymbol) or resolved.detail != "total: i32":
        print("lsp: expected hover/definition lookup for local variable")
        return 1
    if not has_semantic_token(decoded_tokens_with_modifiers, total_line, total_col, len("total"), "variable", "local"):
        print("lsp: expected local variable semantic token modifier")
        print(decoded_tokens_with_modifiers)
        return 1
    local_refs = workspace.variable_references(resolved)
    helper_total_line = min(i for i, line in enumerate(app_lines) if "return total" in line)
    if any(ref["range"]["start"]["line"] == helper_total_line for ref in local_refs):
        print("lsp: local variable references should not include same-named local in another proc")
        print(local_refs)
        return 1
    local_highlights = server.document_highlights_at_request(
        {
            "textDocument": {"uri": doc.uri},
            "position": {"line": total_line, "character": total_col},
        }
    )
    if (
        not local_highlights
        or any(highlight.get("range", {}).get("start", {}).get("line") == helper_total_line for highlight in local_highlights)
        or not any(highlight.get("range", {}).get("start", {}).get("line") == total_line for highlight in local_highlights)
    ):
        print("lsp: documentHighlight for local variable should stay inside the owning proc")
        print(local_highlights)
        return 1
    local_rename = workspace.variable_rename_edits(resolved, "main_total")
    local_rename_lines = {
        edit["range"]["start"]["line"]
        for edits in local_rename.get("changes", {}).values()
        for edit in edits
    }
    if helper_total_line in local_rename_lines or total_line not in local_rename_lines:
        print("lsp: local variable rename should stay inside the owning proc")
        print(local_rename)
        return 1
    global_line = next(i for i, line in enumerate(app_lines) if "global_payload.value = 7" in line)
    global_col = app_lines[global_line].index("global_payload")
    global_resolved = server.symbol_at_request(
        {
            "textDocument": {"uri": doc.uri},
            "position": {"line": global_line, "character": global_col},
        }
    )
    if not isinstance(global_resolved, lsp.VariableSymbol) or global_resolved.kind != "global" or global_resolved.detail != "global_payload: Payload":
        print("lsp: expected hover/definition lookup for imported global variable")
        return 1
    if not has_semantic_token(decoded_tokens_with_modifiers, global_line, global_col, len("global_payload"), "variable", "global"):
        print("lsp: expected global variable semantic token modifier")
        print(decoded_tokens_with_modifiers)
        return 1
    global_refs = workspace.variable_references(global_resolved)
    helper_global_line = next(i for i, line in enumerate(app_lines) if "global_payload:Other" in line)
    if any(ref["uri"] == doc.uri and ref["range"]["start"]["line"] == helper_global_line for ref in global_refs):
        print("lsp: global variable references should not include same-named local shadow")
        print(global_refs)
        return 1
    global_request = {
        "textDocument": {"uri": doc.uri},
        "position": {"line": global_line, "character": global_col},
    }
    global_request_refs = server.references_at_request(global_request)
    if any(ref["uri"] == doc.uri and ref["range"]["start"]["line"] == helper_global_line for ref in global_request_refs):
        print("lsp: textDocument/references for global should skip same-named local shadow")
        print(global_request_refs)
        return 1
    global_highlights = server.document_highlights_at_request(global_request)
    if (
        not global_highlights
        or any(highlight.get("range", {}).get("start", {}).get("line") == helper_global_line for highlight in global_highlights)
        or not any(highlight.get("range", {}).get("start", {}).get("line") == global_line for highlight in global_highlights)
    ):
        print("lsp: documentHighlight for global should stay in the current document and skip local shadows")
        print(global_highlights)
        return 1
    global_rename = workspace.variable_rename_edits(global_resolved, "renamed_global_payload")
    global_rename_lines = {
        edit["range"]["start"]["line"]
        for edits in global_rename.get("changes", {}).values()
        for edit in edits
    }
    if helper_global_line in global_rename_lines or global_line not in global_rename_lines:
        print("lsp: global variable rename should skip same-named local shadow")
        print(global_rename)
        return 1
    global_request_rename = server.rename_at_request(global_request, "renamed_global_payload")
    global_request_rename_lines = {
        edit["range"]["start"]["line"]
        for edits in global_request_rename.get("changes", {}).values()
        for edit in edits
    }
    if helper_global_line in global_request_rename_lines or global_line not in global_request_rename_lines:
        print("lsp: textDocument/rename for global should skip same-named local shadow")
        print(global_request_rename)
        return 1
    enum_resolved = server.symbol_at_request(
        {
            "textDocument": {"uri": doc.uri},
            "position": {"line": enum_line, "character": enum_col},
        }
    )
    if not isinstance(enum_resolved, lsp.Symbol) or enum_resolved.detail != "Kind.Ready: enum member":
        print("lsp: expected hover/definition lookup for enum member")
        return 1
    enum_hover = lsp.hover_markdown_for_symbol(workspace, enum_resolved)
    if "`Kind.Ready: enum member`" not in enum_hover or "type `Kind`" not in enum_hover:
        print("lsp: expected enum member hover to include owning enum type")
        print(enum_hover)
        return 1
    enum_prepare = server.prepare_rename_at_request(
        {
            "textDocument": {"uri": doc.uri},
            "position": {"line": enum_line, "character": enum_col},
        }
    )
    if enum_prepare != lsp.position_to_lsp(enum_line, enum_col, enum_col + len("Kind_Ready")):
        print("lsp: expected prepareRename range for enum usage")
        print(enum_prepare)
        return 1
    enum_highlights = server.document_highlights_at_request(
        {
            "textDocument": {"uri": doc.uri},
            "position": {"line": enum_line, "character": enum_col},
        }
    )
    if (
        len(enum_highlights) != 1
        or enum_highlights[0].get("range", {}).get("start", {}).get("line") != enum_line
        or enum_highlights[0].get("kind") != 1
    ):
        print("lsp: documentHighlight for enum member should include only same-document usage")
        print(enum_highlights)
        return 1
    enum_decl_resolved = server.symbol_at_request(
        {
            "textDocument": {"uri": module_doc.uri},
            "position": {"line": ready_decl_line, "character": ready_decl_col},
        }
    )
    if not isinstance(enum_decl_resolved, lsp.Symbol) or enum_decl_resolved.detail != "Kind.Ready: enum member":
        print("lsp: expected enum declaration lookup to resolve enum member")
        return 1
    enum_decl_hover = lsp.hover_markdown_for_symbol(workspace, enum_decl_resolved)
    if "`Kind.Ready: enum member`" not in enum_decl_hover or "type `Kind`" not in enum_decl_hover:
        print("lsp: expected enum member declaration hover to include owning enum type")
        print(enum_decl_hover)
        return 1
    enum_location = lsp.location_to_lsp(enum_resolved)
    ready_line = next(i for i, line in enumerate(module_lines) if "Ready" in line)
    ready_col = module_lines[ready_line].index("Ready")
    if (
        enum_location["uri"] != lsp.path_to_uri(module)
        or enum_location["range"]["start"]["line"] != ready_line
        or enum_location["range"]["start"]["character"] != ready_col
        or enum_location["range"]["end"]["character"] != ready_col + len("Ready")
    ):
        print("lsp: expected enum member definition to point at source enum item")
        print(enum_location)
        return 1
    call_line = next(i for i, line in enumerate(app_lines) if "payload_add(p.&" in line)
    call_col = app_lines[call_line].index("3")
    signature = lsp.signature_help_at(workspace, doc, call_line, call_col)
    if (
        signature is None
        or signature.get("activeParameter") != 1
        or not signature.get("signatures")
        or signature["signatures"][0].get("parameters") != [{"label": "p:*Payload"}, {"label": "amount:i32"}]
    ):
        print("lsp: expected structured signature help with active parameter")
        print(signature)
        return 1
    pointer_arg_col = app_lines[call_line].index("(") + 1
    pointer_arg_items = lsp.proc_argument_completions_at(workspace, doc, call_line, pointer_arg_col)
    if not any(item.get("label") == "p.&" and item.get("detail") == "p: Payload -> *Payload" for item in pointer_arg_items):
        print("lsp: expected proc argument completion to offer address sugar for pointer parameter")
        print(pointer_arg_items)
        return 1
    if not any(item.get("label") == "payload_ptr" and item.get("detail") == "payload_ptr: *Payload" for item in pointer_arg_items):
        print("lsp: expected proc argument completion to offer exact pointer locals")
        print(pointer_arg_items)
        return 1
    int_arg_items = lsp.proc_argument_completions_at(workspace, doc, call_line, call_col)
    if not any(item.get("label") == "total" and item.get("detail") == "total: i32" for item in int_arg_items):
        print("lsp: expected proc argument completion to offer matching integer locals")
        print(int_arg_items)
        return 1
    arg_completion_server = CaptureServer()
    arg_completion_server.workspace = workspace
    arg_completion_server.handle(
        {
            "jsonrpc": "2.0",
            "id": 22,
            "method": "textDocument/completion",
            "params": {
                "textDocument": {"uri": doc.uri},
                "position": {"line": call_line, "character": call_col},
            },
        }
    )
    arg_completion_response = arg_completion_server.sent[-1] if arg_completion_server.sent else {}
    arg_completion_items = arg_completion_response.get("result", {}).get("items", [])
    if (
        arg_completion_response.get("id") != 22
        or not any(item.get("label") == "total" for item in arg_completion_items)
        or any(item.get("label") == "payload_add" for item in arg_completion_items)
    ):
        print("lsp: expected textDocument/completion in proc args to return argument-specific items")
        print(arg_completion_response)
        return 1
    signature_server = CaptureServer()
    signature_server.workspace = workspace
    signature_server.handle(
        {
            "jsonrpc": "2.0",
            "id": 2,
            "method": "textDocument/signatureHelp",
            "params": {
                "textDocument": {"uri": doc.uri},
                "position": {"line": call_line, "character": call_col},
            },
        }
    )
    signature_response = signature_server.sent[-1] if signature_server.sent else {}
    if signature_response.get("id") != 2 or signature_response.get("result") != signature:
        print("lsp: expected textDocument/signatureHelp request to return structured signature help")
        print(signature_response)
        return 1
    generic_signature = lsp.signature_help_at(
        workspace,
        doc,
        generic_payload_line,
        app_lines[generic_payload_line].rindex("2"),
    )
    if (
        generic_signature is None
        or generic_signature.get("activeParameter") != 0
        or not generic_signature.get("signatures")
        or "Array<Payload>reserve:proc(length:u64)->Array<Payload>" not in generic_signature["signatures"][0].get("label", "")
        or generic_signature["signatures"][0].get("parameters") != [{"label": "length:u64"}]
    ):
        print("lsp: expected generic proc signature help to show substituted concrete type")
        print(generic_signature)
        return 1
    callback_line = next(i for i, line in enumerate(app_lines) if "cb(p.&" in line)
    callback_col = app_lines[callback_line].index("4")
    callback_signature = lsp.signature_help_at(workspace, doc, callback_line, callback_col)
    if (
        callback_signature is None
        or callback_signature.get("activeParameter") != 1
        or not callback_signature.get("signatures")
        or callback_signature["signatures"][0].get("parameters")
        != [{"label": "payload:*Payload"}, {"label": "amount:i32"}]
        or callback_signature["signatures"][0].get("label") != "cb: Callback = *proc(payload:*Payload, amount:i32)->i32"
    ):
        print("lsp: expected signature help for proc pointer alias variable")
        print(callback_signature)
        return 1
    nested_outer_call_line = next(i for i, line in enumerate(app_lines) if "mix3(payload_add" in line)
    nested_outer_call_col = app_lines[nested_outer_call_line].index("2, 3")
    nested_outer_signature = lsp.signature_help_at(workspace, doc, nested_outer_call_line, nested_outer_call_col)
    if (
        nested_outer_signature is None
        or nested_outer_signature.get("activeParameter") != 1
        or not nested_outer_signature.get("signatures")
        or not nested_outer_signature["signatures"][0].get("label", "").startswith("mix3:proc(a:i32, b:i32, c:i32)->i32")
        or nested_outer_signature["signatures"][0].get("parameters")
        != [{"label": "a:i32"}, {"label": "b:i32"}, {"label": "c:i32"}]
    ):
        print("lsp: expected signature help to return outer call after nested call closes")
        print(nested_outer_signature)
        return 1
    nested_inner_signature = lsp.signature_help_at(
        workspace,
        doc,
        nested_outer_call_line,
        app_lines[nested_outer_call_line].index("1),"),
    )
    if (
        nested_inner_signature is None
        or nested_inner_signature.get("activeParameter") != 1
        or not nested_inner_signature.get("signatures")
        or not nested_inner_signature["signatures"][0].get("label", "").startswith("payload_add:proc(p:*Payload, amount:i32)->i32")
        or nested_inner_signature["signatures"][0].get("parameters")
        != [{"label": "p:*Payload"}, {"label": "amount:i32"}]
    ):
        print("lsp: expected signature help to return inner nested call before it closes")
        print(nested_inner_signature)
        return 1
    string_call_line = next(i for i, line in enumerate(app_lines) if 'text_amount("a,b"' in line)
    string_inner_col = app_lines[string_call_line].index("b")
    string_inner_signature = lsp.signature_help_at(workspace, doc, string_call_line, string_inner_col)
    if (
        string_inner_signature is None
        or string_inner_signature.get("activeParameter") != 0
        or not string_inner_signature.get("signatures")
        or string_inner_signature["signatures"][0].get("parameters")
        != [{"label": "text:*const char"}, {"label": "amount:i32"}]
    ):
        print("lsp: expected comma inside string literal to keep signature help on first parameter")
        print(string_inner_signature)
        return 1
    string_after_col = app_lines[string_call_line].index("5")
    string_after_signature = lsp.signature_help_at(workspace, doc, string_call_line, string_after_col)
    if string_after_signature is None or string_after_signature.get("activeParameter") != 1:
        print("lsp: expected comma after string literal to advance signature help parameter")
        print(string_after_signature)
        return 1
    callback_field_line = next(i for i, line in enumerate(app_lines) if "handler.cb(p.&, 4)" in line)
    callback_field_col = app_lines[callback_field_line].index("4")
    callback_field_signature = lsp.signature_help_at(workspace, doc, callback_field_line, callback_field_col)
    if (
        callback_field_signature is None
        or callback_field_signature.get("activeParameter") != 1
        or not callback_field_signature.get("signatures")
        or callback_field_signature["signatures"][0].get("parameters")
        != [{"label": "payload:*Payload"}, {"label": "amount:i32"}]
        or callback_field_signature["signatures"][0].get("label")
        != "handler.cb: Callback = *proc(payload:*Payload, amount:i32)->i32"
    ):
        print("lsp: expected signature help for proc pointer field")
        print(callback_field_signature)
        return 1
    callback_field_name_col = app_lines[callback_field_line].index("cb")
    callback_field_resolved = server.symbol_at_request(
        {
            "textDocument": {"uri": doc.uri},
            "position": {"line": callback_field_line, "character": callback_field_name_col},
        }
    )
    callback_field_hover = lsp.hover_markdown_for_symbol(workspace, callback_field_resolved) if callback_field_resolved else ""
    if (
        not isinstance(callback_field_resolved, lsp.FieldSymbol)
        or "`Handler.cb: Callback`" not in callback_field_hover
        or "resolves to `*proc(payload:*Payload, amount:i32)->i32`" not in callback_field_hover
    ):
        print("lsp: expected callback field hover to expand proc pointer alias")
        print(callback_field_hover)
        return 1
    cb_col = app_lines[callback_line].index("cb")
    cb_resolved = server.symbol_at_request(
        {
            "textDocument": {"uri": doc.uri},
            "position": {"line": callback_line, "character": cb_col},
        }
    )
    cb_hover = lsp.hover_markdown_for_symbol(workspace, cb_resolved) if cb_resolved else ""
    if (
        not isinstance(cb_resolved, lsp.VariableSymbol)
        or "`cb: Callback`" not in cb_hover
        or "resolves to `*proc(payload:*Payload, amount:i32)->i32`" not in cb_hover
    ):
        print("lsp: expected callback variable hover to expand proc pointer alias")
        print(cb_hover)
        return 1
    callback_alias = workspace.find_symbol("Callback")
    callback_alias_hover = lsp.hover_markdown_for_symbol(workspace, callback_alias) if callback_alias else ""
    if (
        callback_alias is None
        or "`Callback:alias = CallbackBase;`" not in callback_alias_hover
        or "resolves to `*proc(payload:*Payload, amount:i32)->i32`" not in callback_alias_hover
    ):
        print("lsp: expected callback alias hover to resolve alias chain")
        print(callback_alias_hover)
        return 1

    print("ok lsp_semantics")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
