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


def main() -> int:
    lsp = load_lsp()
    TEST_DIR.mkdir(parents=True, exist_ok=True)
    module = TEST_DIR / "shared.i"
    duplicate_module = TEST_DIR / "duplicate.i"
    value_duplicate_module = TEST_DIR / "value_duplicate.i"
    app = TEST_DIR / "app.i"
    module.write_text(
        """
Payload:struct = {
    value:i32 @ "editor,serialize";
    values:[4]i32;
}

CallbackBase:alias = *proc(payload:*Payload, amount:i32)->i32;
Callback:alias = CallbackBase;

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

payload_add:proc(p:*Payload, amount:i32)->i32 = {
    return p[0].value + amount;
}

bad_params:proc(value:i32, value:i32)->i32 = {
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
    app.write_text(
        f"""
import "{module.as_posix()}"
import "{duplicate_module.as_posix()}"
import "{value_duplicate_module.as_posix()}"

shared_value:i32 = 2;

main:proc()->i32 = {{
    p:Payload = {{}};
    o:Other = {{}};
    cb:Callback = payload_add;
    handler:Handler = {{}};
    k:Kind = Kind_Ready;
    values:Array<i32> = Array<i32>reserve(4);
    payload_values:Array<Payload> = Array<Payload>reserve(2);
    payload_ptr:*Payload = p.&;
    total:i32 = 0;
    p.value = 4;
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
    total = payload_add(p.&);
    total = cb(p.&, 4, 5);
    total = handler.cb(p.&, 4);
    total = handler.cb(p.&);
    total = global_cb(p.&, 4);
    total = global_cb(p.&);
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
    if any("Other" in diag.message or "value" in diag.message for diag in doc.diagnostics):
        print("lsp: valid field accesses should not produce diagnostics")
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
    if not any("module error: duplicate declaration 'payload_add'" in diag.message for diag in duplicate_doc.diagnostics):
        print("lsp: expected cross-import duplicate declaration diagnostic")
        print(duplicate_doc.diagnostics)
        return 1
    if not any("module error: duplicate global declaration 'global_payload'" in diag.message for diag in duplicate_doc.diagnostics):
        print("lsp: expected cross-import duplicate global declaration diagnostic")
        print(duplicate_doc.diagnostics)
        return 1
    module_doc = workspace.documents[lsp.path_to_uri(module)]
    if not any("semantic error: duplicate proc parameter 'value'" in diag.message for diag in module_doc.diagnostics):
        print("lsp: expected duplicate proc parameter diagnostic")
        print(module_doc.diagnostics)
        return 1
    if not any("semantic error: duplicate field 'value'" in diag.message for diag in module_doc.diagnostics):
        print("lsp: expected duplicate field diagnostic")
        print(module_doc.diagnostics)
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
    module_tokens = decoded_semantic_tokens(lsp, lsp.semantic_tokens_for_doc(workspace, module_doc))
    fields = workspace.fields_for_owner("Payload")
    if not any(field.name == "value" for field in fields):
        print("lsp: expected imported Payload.value field")
        return 1
    app_lines = doc.text.splitlines()
    field_line = next(i for i, line in enumerate(app_lines) if "p.value" in line)
    field_col = app_lines[field_line].index("value")
    field = lsp.field_access_at(workspace, doc, field_line, field_col)
    if field is None or field.name != "value" or field.type_name != "i32" or field.attrs != "editor,serialize":
        print("lsp: expected field hover/definition lookup for p.value")
        return 1
    field_refs = workspace.field_references(field)
    if len(field_refs) != 6:
        print("lsp: expected Payload.value references to include declaration and five usages")
        print(field_refs)
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
    if len(flattened_field_edits) != 6 or any(edit[3] != "payload_value" for edit in flattened_field_edits):
        print("lsp: expected field-aware rename edits for Payload.value")
        print(field_rename)
        return 1
    if any(edit[0] == doc.uri and edit[1] == other_line and edit[2] == other_col for edit in flattened_field_edits):
        print("lsp: field-aware rename should not edit Other.value")
        print(field_rename)
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
    module_lines = module_doc.text.splitlines()
    field_decl_line = next(i for i, line in enumerate(module_lines) if "value:i32" in line)
    field_decl_col = module_lines[field_decl_line].index("value")
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
    nested_completion_col = app_lines[nested_field_line].rindex(".") + 1
    nested_completions = lsp.field_completions_at(workspace, doc, nested_field_line, nested_completion_col)
    if not any(item.owner == "Payload" and item.name == "value" for item in nested_completions):
        print("lsp: expected chained generic field completion for payload_values.data[0].")
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
    payload_completion_count = sum(
        1 for item in completion_items if isinstance(item, lsp.Symbol) and item.name == "payload_add"
    )
    if payload_completion_count != 1:
        print("lsp: duplicate imported declarations should not duplicate completion entries")
        print([item.name for item in completion_items])
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
    if not isinstance(generic_proc_resolved, lsp.Symbol) or generic_proc_resolved.name != "Array<T>reserve":
        print("lsp: expected generic proc tail lookup to resolve Array<T>reserve")
        print(generic_proc_resolved)
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
    total_line = next(i for i, line in enumerate(app_lines) if "return total" in line)
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
    global_line = next(i for i, line in enumerate(app_lines) if "global_payload.value" in line)
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
    enum_resolved = server.symbol_at_request(
        {
            "textDocument": {"uri": doc.uri},
            "position": {"line": enum_line, "character": enum_col},
        }
    )
    if not isinstance(enum_resolved, lsp.Symbol) or enum_resolved.detail != "Kind.Ready: enum member":
        print("lsp: expected hover/definition lookup for enum member")
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
