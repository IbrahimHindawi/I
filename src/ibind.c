#include <clang-c/Index.h>

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct string_builder {
    char *data;
    size_t length;
    size_t cap;
} string_builder;

static const char *g_filter_path = 0;
static const char *g_symbol_prefix = 0;
static int g_preprocess = 0;
static int g_print_cmd = 0;

typedef struct emitted_name_node {
    char *name;
    struct emitted_name_node *next;
} emitted_name_node;

static emitted_name_node *g_emitted_opaque_records = 0;

static char *ibind_strdup(const char *s);

static char *cxstr_dup(CXString s) {
    const char *c = clang_getCString(s);
    char *out = 0;
    if (c) {
        size_t n = strlen(c);
        out = (char *)malloc(n + 1);
        if (out) memcpy(out, c, n + 1);
    }
    clang_disposeString(s);
    return out ? out : ibind_strdup("");
}

static char *ibind_strdup(const char *s) {
    if (!s) s = "";
    size_t n = strlen(s);
    char *out = (char *)malloc(n + 1);
    if (out) memcpy(out, s, n + 1);
    return out;
}

static FILE *ibind_fopen(const char *path, const char *mode) {
#if defined(_WIN32)
    FILE *file = 0;
    if (fopen_s(&file, path, mode) != 0) return 0;
    return file;
#else
    return fopen(path, mode);
#endif
}

static void sb_reserve(string_builder *sb, size_t add) {
    size_t need = sb->length + add + 1;
    if (need <= sb->cap) return;
    size_t cap = sb->cap ? sb->cap * 2 : 256;
    while (cap < need) cap *= 2;
    char *data = (char *)realloc(sb->data, cap);
    if (!data) {
        fprintf(stderr, "ibind: out of memory\n");
        exit(1);
    }
    sb->data = data;
    sb->cap = cap;
}

static void sb_append_n(string_builder *sb, const char *s, size_t n) {
    sb_reserve(sb, n);
    memcpy(sb->data + sb->length, s, n);
    sb->length += n;
    sb->data[sb->length] = 0;
}

static void sb_append(string_builder *sb, const char *s) {
    sb_append_n(sb, s, strlen(s));
}

static void sb_appendf(string_builder *sb, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    va_list copy;
    va_copy(copy, args);
    int n = vsnprintf(0, 0, fmt, copy);
    va_end(copy);
    if (n < 0) {
        va_end(args);
        return;
    }
    sb_reserve(sb, (size_t)n);
    vsnprintf(sb->data + sb->length, sb->cap - sb->length, fmt, args);
    sb->length += (size_t)n;
    va_end(args);
}

static int streq(const char *a, const char *b) {
    return strcmp(a, b) == 0;
}

static int starts_with(const char *s, const char *prefix) {
    return strncmp(s, prefix, strlen(prefix)) == 0;
}

static const char *builtin_typedef_type(const char *name) {
    if (streq(name, "size_t")) return "usize";
    return 0;
}

static int path_has_separator(const char *s) {
    for (const char *p = s; *p; p++) {
        if (*p == '/' || *p == '\\') return 1;
    }
    return 0;
}

static const char *path_basename_const(const char *s) {
    const char *base = s;
    for (const char *p = s; *p; p++) {
        if (*p == '/' || *p == '\\') base = p + 1;
    }
    return base;
}

static int path_matches_filter(const char *path, const char *filter) {
    if (!filter || !filter[0]) return 1;
    if (!path || !path[0]) return 0;
    if (!path_has_separator(filter)) {
        return streq(path_basename_const(path), filter);
    }

    size_t path_len = strlen(path);
    size_t filter_len = strlen(filter);
    if (path_len < filter_len) return 0;
    const char *tail = path + path_len - filter_len;
    if (!streq(tail, filter)) return 0;
    return tail == path || tail[-1] == '/' || tail[-1] == '\\';
}

static char *strip_tag_prefix(char *s) {
    if (starts_with(s, "const ")) s += 6;
    if (starts_with(s, "struct ")) return s + 7;
    if (starts_with(s, "union ")) return s + 6;
    if (starts_with(s, "enum ")) return s + 5;
    return s;
}

static char *sanitize_ident(const char *s) {
    size_t n = strlen(s);
    char *out = (char *)malloc(n + 2);
    if (!out) exit(1);
    size_t j = 0;
    if (n == 0 || !(isalpha((unsigned char)s[0]) || s[0] == '_')) {
        out[j++] = '_';
    }
    for (size_t i = 0; i < n; i++) {
        unsigned char c = (unsigned char)s[i];
        out[j++] = (isalnum(c) || c == '_') ? (char)c : '_';
    }
    out[j] = 0;
    return out;
}

static int emitted_name_contains(emitted_name_node *head, const char *name) {
    for (emitted_name_node *node = head; node; node = node->next) {
        if (streq(node->name, name)) return 1;
    }
    return 0;
}

static void emitted_name_add(emitted_name_node **head, const char *name) {
    if (emitted_name_contains(*head, name)) return;
    emitted_name_node *node = (emitted_name_node *)calloc(1, sizeof(*node));
    if (!node) {
        fprintf(stderr, "ibind: out of memory\n");
        exit(1);
    }
    node->name = ibind_strdup(name);
    node->next = *head;
    *head = node;
}

static const char *builtin_type(enum CXTypeKind kind) {
    switch (kind) {
        case CXType_Void: return "void";
        case CXType_Bool: return "b32";
        case CXType_Char_U:
        case CXType_Char_S:
        case CXType_SChar: return "char";
        case CXType_UChar: return "u8";
        case CXType_WChar:
        case CXType_Char16: return "u16";
        case CXType_Char32: return "u32";
        case CXType_Short: return "i16";
        case CXType_UShort: return "u16";
        case CXType_Int: return "i32";
        case CXType_UInt: return "u32";
        case CXType_Long: return "long";
        case CXType_ULong: return "ulong";
        case CXType_LongLong: return "i64";
        case CXType_ULongLong: return "u64";
        case CXType_Float: return "f32";
        case CXType_Double: return "f64";
        default: return 0;
    }
}

static char *emit_type(CXType type);

static const char *calling_conv_name(CXType fn_type) {
    switch (clang_getFunctionTypeCallingConv(fn_type)) {
        case CXCallingConv_X86StdCall: return "__stdcall";
        case CXCallingConv_X86FastCall: return "__fastcall";
        case CXCallingConv_X86ThisCall: return "__thiscall";
        case CXCallingConv_X86VectorCall: return "__vectorcall";
        case CXCallingConv_X86RegCall: return "__regcall";
        default: return 0;
    }
}

static char *emit_proc_type_with_names(CXType fn_type, int as_pointer, char **param_names, int param_name_count) {
    string_builder sb = {0};
    if (as_pointer) sb_append(&sb, "*");
    sb_append(&sb, "proc");
    const char *callconv = calling_conv_name(fn_type);
    if (callconv) {
        sb_append(&sb, "[");
        sb_append(&sb, callconv);
        sb_append(&sb, "]");
    }
    sb_append(&sb, "(");
    int argc = clang_getNumArgTypes(fn_type);
    for (int i = 0; i < argc; i++) {
        CXType arg = clang_getArgType(fn_type, (unsigned)i);
        char *arg_s = emit_type(arg);
        const char *param_name = (param_names && i < param_name_count && param_names[i] && param_names[i][0]) ? param_names[i] : 0;
        char fallback[32];
        if (!param_name) {
            snprintf(fallback, sizeof(fallback), "arg%d", i);
            param_name = fallback;
        }
        char *param_clean = sanitize_ident(param_name);
        if (i) sb_append(&sb, ", ");
        sb_appendf(&sb, "%s:%s", param_clean, arg_s);
        free(param_clean);
        free(arg_s);
    }
    if (clang_isFunctionTypeVariadic(fn_type)) {
        if (argc > 0) sb_append(&sb, ", ");
        sb_append(&sb, "...");
    }
    sb_append(&sb, ")->");
    char *ret = emit_type(clang_getResultType(fn_type));
    sb_append(&sb, ret);
    free(ret);
    return sb.data;
}

static char *emit_proc_type(CXType fn_type, int as_pointer) {
    return emit_proc_type_with_names(fn_type, as_pointer, 0, 0);
}

static char *emit_type(CXType type) {
    int is_const = clang_isConstQualifiedType(type);
    CXType canonical = clang_getCanonicalType(type);
    if (type.kind == CXType_Typedef) {
        char *s = cxstr_dup(clang_getTypeSpelling(type));
        char *clean = sanitize_ident(strip_tag_prefix(s));
        const char *builtin_typedef = builtin_typedef_type(clean);
        if (builtin_typedef) {
            string_builder builtin_sb = {0};
            if (is_const) sb_append(&builtin_sb, "const ");
            sb_append(&builtin_sb, builtin_typedef);
            free(clean);
            free(s);
            return builtin_sb.data;
        }
        string_builder sb = {0};
        if (is_const) sb_append(&sb, "const ");
        sb_append(&sb, clean);
        free(clean);
        free(s);
        return sb.data;
    }

    const char *builtin = builtin_type(type.kind);
    if (!builtin) builtin = builtin_type(canonical.kind);
    if (builtin) {
        string_builder sb = {0};
        if (is_const && !streq(builtin, "void")) sb_append(&sb, "const ");
        sb_append(&sb, builtin);
        return sb.data;
    }

    if (type.kind == CXType_Pointer) {
        CXType pointee = clang_getPointeeType(type);
        CXType pointee_canon = clang_getCanonicalType(pointee);
        if (pointee.kind == CXType_FunctionProto || pointee_canon.kind == CXType_FunctionProto) {
            return emit_proc_type(pointee.kind == CXType_FunctionProto ? pointee : pointee_canon, 1);
        }
        char *inner = emit_type(pointee);
        string_builder sb = {0};
        if (clang_isConstQualifiedType(pointee)) {
            if (starts_with(inner, "const ")) {
                sb_append(&sb, "*");
                sb_append(&sb, inner);
            } else {
                sb_append(&sb, "*const ");
                sb_append(&sb, inner);
            }
        } else {
            sb_append(&sb, "*");
            sb_append(&sb, inner);
        }
        free(inner);
        return sb.data;
    }

    if (type.kind == CXType_IncompleteArray) {
        CXType elem = clang_getArrayElementType(type);
        char *elem_s = emit_type(elem);
        string_builder sb = {0};
        sb_append(&sb, "*");
        sb_append(&sb, elem_s);
        free(elem_s);
        return sb.data;
    }

    if (type.kind == CXType_ConstantArray) {
        CXType elem = clang_getArrayElementType(type);
        long long count = clang_getArraySize(type);
        char *elem_s = emit_type(elem);
        string_builder sb = {0};
        sb_appendf(&sb, "[%lld]%s", count, elem_s);
        free(elem_s);
        return sb.data;
    }

    if (type.kind == CXType_FunctionProto || canonical.kind == CXType_FunctionProto) {
        return emit_proc_type(type.kind == CXType_FunctionProto ? type : canonical, 0);
    }

    if (type.kind == CXType_Record || type.kind == CXType_Enum ||
        canonical.kind == CXType_Record || canonical.kind == CXType_Enum ||
        type.kind == CXType_Elaborated) {
        char *s = cxstr_dup(clang_getTypeSpelling(type));
        char *base = strip_tag_prefix(s);
        char *clean = sanitize_ident(base);
        free(s);
        return clean;
    }

    char *s = cxstr_dup(clang_getTypeSpelling(type));
    char *clean = sanitize_ident(strip_tag_prefix(s));
    free(s);
    return clean;
}

static int cursor_is_from_main_file(CXCursor c) {
    CXSourceLocation loc = clang_getCursorLocation(c);
    if (g_filter_path) {
        CXFile file = 0;
        clang_getSpellingLocation(loc, &file, 0, 0, 0);
        if (!file) return 0;
        char *path = cxstr_dup(clang_getFileName(file));
        int ok = path_matches_filter(path, g_filter_path);
        free(path);
        return ok;
    }
    return clang_Location_isFromMainFile(loc);
}

static int cursor_matches_symbol_prefix(CXCursor c) {
    if (!g_symbol_prefix) return 1;
    char *name = cxstr_dup(clang_getCursorSpelling(c));
    int ok = starts_with(name, g_symbol_prefix);
    free(name);
    return ok;
}

static char *shell_quote(const char *s) {
    string_builder sb = {0};
    sb_append(&sb, "\"");
    for (const char *p = s; *p; p++) {
        if (*p == '"') sb_append(&sb, "\\\"");
        else sb_append_n(&sb, p, 1);
    }
    sb_append(&sb, "\"");
    return sb.data;
}

static char *make_temp_path(const char *output) {
    string_builder sb = {0};
    sb_append(&sb, output);
    sb_append(&sb, ".pp.h");
    return sb.data;
}

static int preprocess_header(const char *input, const char *pp_path, const char **clang_args, int clang_arg_count) {
    string_builder cmd = {0};
    sb_append(&cmd, "clang -E ");
    for (int i = 0; i < clang_arg_count; i++) {
        const char *arg = clang_args[i];
        if (arg[0] == '-' && !starts_with(arg, "-I")) {
            sb_append(&cmd, arg);
        } else {
            char *q = shell_quote(arg);
            sb_append(&cmd, q);
            free(q);
        }
        sb_append(&cmd, " ");
    }
    char *input_q = shell_quote(input);
    char *out_q = shell_quote(pp_path);
    sb_append(&cmd, input_q);
    sb_append(&cmd, " -o ");
    sb_append(&cmd, out_q);
    if (g_print_cmd) fprintf(stderr, "%s\n", cmd.data);
    free(input_q);
    free(out_q);
    int code = system(cmd.data);
    free(cmd.data);
    return code;
}

static const char **normalize_clang_args(const char **args, int count, int *out_count) {
    const char **out = (const char **)calloc((size_t)count + 1, sizeof(char *));
    if (!out) return 0;
    int n = 0;
    for (int i = 0; i < count; i++) {
        const char *arg = args[i];
        size_t len = strlen(arg);
        if (len == 4 && arg[0] == '-' && arg[1] == 'I' && isalpha((unsigned char)arg[2]) && arg[3] == ':' &&
            i + 1 < count && (args[i + 1][0] == '\\' || args[i + 1][0] == '/')) {
            string_builder sb = {0};
            sb_append(&sb, arg);
            sb_append(&sb, args[i + 1]);
            out[n++] = sb.data;
            i++;
        } else {
            out[n++] = arg;
        }
    }
    *out_count = n;
    return out;
}

static void free_normalized_clang_args(const char **args, int count) {
    for (int i = 0; i < count; i++) {
        if (args[i] && starts_with(args[i], "-I") && strlen(args[i]) > 4 && isalpha((unsigned char)args[i][2]) && args[i][3] == ':') {
            free((void *)args[i]);
        }
    }
    free((void *)args);
}

static enum CXChildVisitResult enum_item_visitor(CXCursor child, CXCursor parent, CXClientData data) {
    (void)parent;
    FILE *f = (FILE *)data;
    if (clang_getCursorKind(child) != CXCursor_EnumConstantDecl) return CXChildVisit_Continue;
    char *item = cxstr_dup(clang_getCursorSpelling(child));
    if (item[0] == 0 || (g_symbol_prefix && !starts_with(item, g_symbol_prefix))) {
        free(item);
        return CXChildVisit_Continue;
    }
    char *item_clean = sanitize_ident(item);
    long long value = clang_getEnumConstantDeclValue(child);
    fprintf(f, "    %s = %lld,\n", item_clean, value);
    free(item_clean);
    free(item);
    return CXChildVisit_Continue;
}

static void emit_enum(CXCursor c, FILE *out) {
    char *name = cxstr_dup(clang_getCursorSpelling(c));
    if (name[0] == 0) {
        free(name);
        return;
    }
    char *clean = sanitize_ident(name);
    fprintf(out, "%s: enum[external] = {\n", clean);
    clang_visitChildren(c, enum_item_visitor, out);
    fprintf(out, "}\n\n");
    free(clean);
    free(name);
}

static enum CXChildVisitResult anonymous_enum_constant_visitor(CXCursor child, CXCursor parent, CXClientData data) {
    (void)parent;
    FILE *f = (FILE *)data;
    if (clang_getCursorKind(child) != CXCursor_EnumConstantDecl) return CXChildVisit_Continue;

    char *item = cxstr_dup(clang_getCursorSpelling(child));
    if (item[0] == 0 || (g_symbol_prefix && !starts_with(item, g_symbol_prefix))) {
        free(item);
        return CXChildVisit_Continue;
    }

    char *item_clean = sanitize_ident(item);
    long long value = clang_getEnumConstantDeclValue(child);
    fprintf(f, "#define %s %lld\n", item_clean, value);
    free(item_clean);
    free(item);
    return CXChildVisit_Continue;
}

static void emit_anonymous_enum_constants(CXCursor c, FILE *out) {
    char *name = cxstr_dup(clang_getCursorSpelling(c));
    int anonymous = name[0] == 0 || strstr(name, "unnamed") != 0 || strstr(name, "anonymous") != 0;
    free(name);
    if (!anonymous) return;
    clang_visitChildren(c, anonymous_enum_constant_visitor, out);
}

typedef struct record_emit_ctx {
    FILE *out;
    char *owner;
    int anon_index;
} record_emit_ctx;

static void emit_record_named(CXCursor c, FILE *out, int is_union, const char *forced_name);

typedef struct param_name_ctx {
    char **names;
    int count;
    int capacity;
} param_name_ctx;

static enum CXChildVisitResult param_name_visitor(CXCursor child, CXCursor parent, CXClientData data) {
    (void)parent;
    param_name_ctx *ctx = (param_name_ctx *)data;
    if (clang_getCursorKind(child) != CXCursor_ParmDecl) return CXChildVisit_Continue;
    if (ctx->count >= ctx->capacity) return CXChildVisit_Break;
    ctx->names[ctx->count++] = cxstr_dup(clang_getCursorSpelling(child));
    return CXChildVisit_Continue;
}

static char *emit_cursor_proc_type_if_available(CXCursor c, CXType type) {
    CXType canonical = clang_getCanonicalType(type);
    CXType fn_type = (CXType){0};
    int fn_pointer = 0;
    if (type.kind == CXType_FunctionProto || canonical.kind == CXType_FunctionProto) {
        fn_type = type.kind == CXType_FunctionProto ? type : canonical;
    } else if (type.kind == CXType_Pointer) {
        CXType pointee = clang_getPointeeType(type);
        CXType pointee_canon = clang_getCanonicalType(pointee);
        if (pointee.kind == CXType_FunctionProto || pointee_canon.kind == CXType_FunctionProto) {
            fn_type = pointee.kind == CXType_FunctionProto ? pointee : pointee_canon;
            fn_pointer = 1;
        }
    }
    if (fn_type.kind != CXType_FunctionProto) return 0;

    int argc = clang_getNumArgTypes(fn_type);
    char **param_names = 0;
    param_name_ctx ctx = {0};
    if (argc > 0) {
        param_names = (char **)calloc((size_t)argc, sizeof(char *));
        if (!param_names) exit(1);
        ctx.names = param_names;
        ctx.capacity = argc;
        clang_visitChildren(c, param_name_visitor, &ctx);
    }
    char *type_s = emit_proc_type_with_names(fn_type, fn_pointer, param_names, ctx.count);
    for (int i = 0; i < ctx.count; i++) free(param_names[i]);
    free(param_names);
    return type_s;
}

static void emit_opaque_record_decl_for_type(CXType type, FILE *out) {
    if (type.kind == CXType_Pointer) {
        emit_opaque_record_decl_for_type(clang_getPointeeType(type), out);
        return;
    }
    if (type.kind == CXType_ConstantArray || type.kind == CXType_IncompleteArray) {
        emit_opaque_record_decl_for_type(clang_getArrayElementType(type), out);
        return;
    }

    CXType canonical = clang_getCanonicalType(type);
    if (type.kind != CXType_Record &&
        type.kind != CXType_Elaborated &&
        canonical.kind != CXType_Record) {
        return;
    }

    CXCursor decl = clang_getTypeDeclaration(type);
    if (clang_Cursor_isNull(decl)) decl = clang_getTypeDeclaration(canonical);
    if (clang_Cursor_isNull(decl)) return;
    if (clang_isCursorDefinition(decl)) return;
    CXCursor definition = clang_getCursorDefinition(decl);
    if (!clang_Cursor_isNull(definition)) return;

    char *name = cxstr_dup(clang_getCursorSpelling(decl));
    if (name[0] == 0) {
        free(name);
        return;
    }
    char *clean = sanitize_ident(strip_tag_prefix(name));
    if (!emitted_name_contains(g_emitted_opaque_records, clean)) {
        emitted_name_add(&g_emitted_opaque_records, clean);
        fprintf(out, "%s: struct[external] = {}\n\n", clean);
    }
    free(clean);
    free(name);
}

static int cursor_has_empty_record_name(CXCursor c) {
    enum CXCursorKind kind = clang_getCursorKind(c);
    if (kind != CXCursor_StructDecl && kind != CXCursor_UnionDecl) return 0;
    char *name = cxstr_dup(clang_getCursorSpelling(c));
    int empty = name[0] == 0 || strstr(name, "unnamed") != 0 || strstr(name, "anonymous") != 0;
    free(name);
    return empty;
}

static int cursor_is_direct_anonymous_record(CXCursor c) {
    enum CXCursorKind kind = clang_getCursorKind(c);
    if (kind != CXCursor_StructDecl && kind != CXCursor_UnionDecl) return 0;
    return clang_Cursor_isAnonymousRecordDecl(c) != 0;
}

static CXCursor field_anonymous_record_decl(CXCursor field) {
    CXType type = clang_getCursorType(field);
    CXCursor decl = clang_getTypeDeclaration(type);
    if (!clang_isCursorDefinition(decl) || !cursor_has_empty_record_name(decl)) {
        return clang_getNullCursor();
    }
    return decl;
}

typedef struct record_attr_ctx {
    int is_packed;
} record_attr_ctx;

static enum CXChildVisitResult record_attr_visitor(CXCursor child, CXCursor parent, CXClientData data) {
    (void)parent;
    record_attr_ctx *ctx = (record_attr_ctx *)data;
    if (clang_getCursorKind(child) == CXCursor_PackedAttr) {
        ctx->is_packed = 1;
    }
    return CXChildVisit_Continue;
}

static int record_is_packed(CXCursor c) {
    record_attr_ctx ctx = {0};
    clang_visitChildren(c, record_attr_visitor, &ctx);
    return ctx.is_packed;
}

static enum CXChildVisitResult record_anon_type_visitor(CXCursor child, CXCursor parent, CXClientData data) {
    (void)parent;
    record_emit_ctx *ctx = (record_emit_ctx *)data;
    enum CXCursorKind kind = clang_getCursorKind(child);

    CXCursor anon = clang_getNullCursor();
    if (kind == CXCursor_FieldDecl) {
        anon = field_anonymous_record_decl(child);
        if (!clang_Cursor_isNull(anon) && cursor_is_direct_anonymous_record(anon)) {
            return CXChildVisit_Continue;
        }
    } else if ((kind == CXCursor_StructDecl || kind == CXCursor_UnionDecl) && cursor_is_direct_anonymous_record(child)) {
        anon = child;
    } else {
        return CXChildVisit_Continue;
    }
    if (clang_Cursor_isNull(anon)) return CXChildVisit_Continue;

    char forced[512];
    snprintf(forced, sizeof(forced), "%s_anon%d", ctx->owner, ctx->anon_index++);
    emit_record_named(anon, ctx->out, clang_getCursorKind(anon) == CXCursor_UnionDecl, forced);
    return CXChildVisit_Continue;
}

static enum CXChildVisitResult record_opaque_dep_visitor(CXCursor child, CXCursor parent, CXClientData data) {
    (void)parent;
    FILE *out = (FILE *)data;
    if (clang_getCursorKind(child) != CXCursor_FieldDecl) return CXChildVisit_Continue;
    emit_opaque_record_decl_for_type(clang_getCursorType(child), out);
    return CXChildVisit_Continue;
}

static enum CXChildVisitResult record_field_visitor(CXCursor child, CXCursor parent, CXClientData data) {
    (void)parent;
    record_emit_ctx *ctx = (record_emit_ctx *)data;
    enum CXCursorKind kind = clang_getCursorKind(child);
    if (kind != CXCursor_FieldDecl &&
        !((kind == CXCursor_StructDecl || kind == CXCursor_UnionDecl) && cursor_is_direct_anonymous_record(child))) {
        return CXChildVisit_Continue;
    }

    char *name = kind == CXCursor_FieldDecl ? cxstr_dup(clang_getCursorSpelling(child)) : ibind_strdup("");
    char fallback[64];
    if (name[0] == 0) {
        snprintf(fallback, sizeof(fallback), "_anon%d", ctx->anon_index);
        free(name);
        name = ibind_strdup(fallback);
    }

    CXCursor anon = kind == CXCursor_FieldDecl ? field_anonymous_record_decl(child) : child;
    char *type_s = 0;
    if (!clang_Cursor_isNull(anon)) {
        char forced[512];
        snprintf(forced, sizeof(forced), "%s_anon%d", ctx->owner, ctx->anon_index++);
        type_s = ibind_strdup(forced);
    } else {
        CXType field_type = clang_getCursorType(child);
        type_s = emit_cursor_proc_type_if_available(child, field_type);
        if (!type_s) type_s = emit_type(field_type);
    }

    char *clean = sanitize_ident(name);
    if (kind == CXCursor_FieldDecl && clang_Cursor_isBitField(child)) {
        int width = clang_getFieldDeclBitWidth(child);
        fprintf(ctx->out, "    // ibind: bitfield %s:%d\n", clean, width);
    }
    if (kind == CXCursor_FieldDecl && clang_getCursorType(child).kind == CXType_IncompleteArray) {
        fprintf(ctx->out, "    // ibind: incomplete_array %s\n", clean);
    }
    if (kind == CXCursor_FieldDecl) {
        long long offset_bits = clang_Cursor_getOffsetOfField(child);
        if (offset_bits >= 0) {
            fprintf(ctx->out, "    // ibind: field_offset %s:%lld\n", clean, offset_bits);
        }
    }
    fprintf(ctx->out, "    %s:%s;\n", clean, type_s);
    free(type_s);
    free(clean);
    free(name);
    return CXChildVisit_Continue;
}

static void emit_record_named(CXCursor c, FILE *out, int is_union, const char *forced_name) {
    char *name = cxstr_dup(clang_getCursorSpelling(c));
    const char *emit_name = forced_name ? forced_name : name;
    if (emit_name[0] == 0) {
        free(name);
        return;
    }
    char *clean = sanitize_ident(emit_name);

    record_emit_ctx anon_ctx = {out, clean, 0};
    clang_visitChildren(c, record_anon_type_visitor, &anon_ctx);
    clang_visitChildren(c, record_opaque_dep_visitor, out);

    if (record_is_packed(c)) {
        fprintf(out, "// ibind: packed\n");
    }
    CXType record_type = clang_getCursorType(c);
    long long size = clang_Type_getSizeOf(record_type);
    long long align = clang_Type_getAlignOf(record_type);
    if (size >= 0 && align >= 0) {
        fprintf(out, "// ibind: layout size=%lld align=%lld\n", size, align);
    }
    /* A forced name means the record is anonymous in the header and exists
       only under a name ibind invented, so C cannot be asked its layout. */
    fprintf(out, "%s: %s[external%s] = {\n", clean, is_union ? "union" : "struct",
            forced_name ? ", no_layout_check" : "");
    record_emit_ctx ctx = {out, clean, 0};
    clang_visitChildren(c, record_field_visitor, &ctx);
    fprintf(out, "}\n\n");
    free(clean);
    free(name);
}

static void emit_record(CXCursor c, FILE *out, int is_union) {
    emit_record_named(c, out, is_union, 0);
}

static void emit_function(CXCursor c, FILE *out) {
    char *name = cxstr_dup(clang_getCursorSpelling(c));
    if (name[0] == 0) {
        free(name);
        return;
    }
    char *clean = sanitize_ident(name);
    CXType fn = clang_getCursorType(c);
    int argc = clang_Cursor_getNumArguments(c);
    emit_opaque_record_decl_for_type(clang_getResultType(fn), out);
    const char *callconv = calling_conv_name(fn);
    if (callconv) {
        fprintf(out, "%s: proc[external_emit, %s](", clean, callconv);
    } else {
        fprintf(out, "%s: proc[external_emit](", clean);
    }
    for (int i = 0; i < argc; i++) {
        CXCursor arg = clang_Cursor_getArgument(c, (unsigned)i);
        char *arg_name = cxstr_dup(clang_getCursorSpelling(arg));
        char fallback[32];
        if (arg_name[0] == 0) {
            snprintf(fallback, sizeof(fallback), "arg%d", i);
            free(arg_name);
            arg_name = ibind_strdup(fallback);
        }
        char *arg_clean = sanitize_ident(arg_name);
        CXType arg_type = clang_getArgType(fn, (unsigned)i);
        emit_opaque_record_decl_for_type(arg_type, out);
        char *type_s = emit_type(arg_type);
        if (i) fprintf(out, ", ");
        fprintf(out, "%s: %s", arg_clean, type_s);
        free(type_s);
        free(arg_clean);
        free(arg_name);
    }
    if (clang_isFunctionTypeVariadic(fn)) {
        if (argc > 0) fprintf(out, ", ");
        fprintf(out, "...");
    }
    char *ret = emit_type(clang_getResultType(fn));
    fprintf(out, ")->%s = {}\n", ret);
    free(ret);
    free(clean);
    free(name);
}

static void emit_typedef(CXCursor c, FILE *out) {
    char *name = cxstr_dup(clang_getCursorSpelling(c));
    if (name[0] == 0) {
        free(name);
        return;
    }
    CXType underlying = clang_getTypedefDeclUnderlyingType(c);
    CXType canonical = clang_getCanonicalType(underlying);
    emit_opaque_record_decl_for_type(underlying, out);
    if ((canonical.kind == CXType_Record || canonical.kind == CXType_Enum) && !clang_isCursorDefinition(c)) {
        free(name);
        return;
    }
    char *clean = sanitize_ident(name);
    char *type_s = 0;
    CXType fn_type = {0};
    int fn_pointer = 0;
    if (underlying.kind == CXType_FunctionProto || canonical.kind == CXType_FunctionProto) {
        fn_type = underlying.kind == CXType_FunctionProto ? underlying : canonical;
    } else if (underlying.kind == CXType_Pointer) {
        CXType pointee = clang_getPointeeType(underlying);
        CXType pointee_canon = clang_getCanonicalType(pointee);
        if (pointee.kind == CXType_FunctionProto || pointee_canon.kind == CXType_FunctionProto) {
            fn_type = pointee.kind == CXType_FunctionProto ? pointee : pointee_canon;
            fn_pointer = 1;
        }
    }
    if (fn_type.kind == CXType_FunctionProto) {
        int argc = clang_getNumArgTypes(fn_type);
        char **param_names = 0;
        param_name_ctx ctx = {0};
        if (argc > 0) {
            param_names = (char **)calloc((size_t)argc, sizeof(char *));
            if (!param_names) exit(1);
            ctx.names = param_names;
            ctx.capacity = argc;
            clang_visitChildren(c, param_name_visitor, &ctx);
        }
        type_s = emit_proc_type_with_names(fn_type, fn_pointer, param_names, ctx.count);
        for (int i = 0; i < ctx.count; i++) free(param_names[i]);
        free(param_names);
    } else {
        type_s = emit_type(underlying);
    }
    if (!streq(clean, type_s)) fprintf(out, "%s: alias = %s;\n\n", clean, type_s);
    free(type_s);
    free(clean);
    free(name);
}

static int token_spelling_is_define(const char *s) {
    return streq(s, "#") || streq(s, "define");
}

static char *trim_dup_range(const char *start, const char *end) {
    while (start < end && isspace((unsigned char)*start)) start++;
    while (end > start && isspace((unsigned char)end[-1])) end--;
    size_t n = (size_t)(end - start);
    char *out = (char *)malloc(n + 1);
    if (!out) exit(1);
    memcpy(out, start, n);
    out[n] = 0;
    return out;
}

static char *trim_dup(const char *s) {
    return trim_dup_range(s, s + strlen(s));
}

static int matching_outer_parens(const char *s, size_t first, size_t last) {
    int depth = 0;
    for (size_t i = first; i <= last; i++) {
        if (s[i] == '(') {
            depth++;
        } else if (s[i] == ')') {
            depth--;
            if (depth == 0 && i != last) return 0;
            if (depth < 0) return 0;
        }
    }
    return depth == 0;
}

static char *strip_redundant_outer_parens(const char *input) {
    char *out = trim_dup(input);
    for (;;) {
        size_t n = strlen(out);
        size_t first = 0;
        while (first < n && isspace((unsigned char)out[first])) first++;
        size_t last = n;
        while (last > first && isspace((unsigned char)out[last - 1])) last--;
        if (last <= first + 1 || out[first] != '(' || out[last - 1] != ')') break;
        if (!matching_outer_parens(out, first, last - 1)) break;
        char *next = trim_dup_range(out + first + 1, out + last - 1);
        free(out);
        out = next;
    }
    return out;
}

static int cast_type_text_looks_c_like(const char *s) {
    int saw_typeish = 0;
    for (const char *p = s; *p; p++) {
        unsigned char c = (unsigned char)*p;
        if (!(isalnum(c) || c == '_' || c == '*' || isspace(c))) return 0;
        if (c == '*') saw_typeish = 1;
    }
    if (strstr(s, "int") || strstr(s, "char") || strstr(s, "short") || strstr(s, "long") ||
        strstr(s, "float") || strstr(s, "double") || strstr(s, "signed") || strstr(s, "unsigned") ||
        strstr(s, "size_t") || strstr(s, "intptr") || strstr(s, "uintptr") || strstr(s, "_t")) {
        saw_typeish = 1;
    }
    return saw_typeish;
}

static char *strip_leading_numeric_cast(const char *input) {
    char *out = trim_dup(input);
    if (out[0] != '(') return out;
    int depth = 0;
    size_t close = 0;
    for (size_t i = 0; out[i]; i++) {
        if (out[i] == '(') depth++;
        else if (out[i] == ')') {
            depth--;
            if (depth == 0) {
                close = i;
                break;
            }
        }
    }
    if (close == 0) return out;
    char *cast_text = trim_dup_range(out + 1, out + close);
    char *tail = trim_dup(out + close + 1);
    if (!tail[0] || !cast_type_text_looks_c_like(cast_text)) {
        free(cast_text);
        free(tail);
        return out;
    }
    char first = tail[0];
    if (!(isdigit((unsigned char)first) || first == '-' || first == '+' || first == '\'' || first == '"' || first == '(')) {
        free(cast_text);
        free(tail);
        return out;
    }
    free(cast_text);
    free(out);
    return tail;
}

static int c_integer_suffix_len(const char *s) {
    int len = (int)strlen(s);
    int i = len;
    while (i > 0) {
        char c = s[i - 1];
        if (c == 'u' || c == 'U' || c == 'l' || c == 'L') {
            i--;
            continue;
        }
        break;
    }
    if (i == len) return 0;

    int suffix_len = len - i;
    int u_count = 0;
    int l_count = 0;
    for (int j = i; j < len; j++) {
        char c = s[j];
        if (c == 'u' || c == 'U') u_count++;
        else if (c == 'l' || c == 'L') l_count++;
        else return 0;
    }
    if (u_count > 1 || l_count > 2) return 0;
    return suffix_len;
}

static char *strip_integer_literal_suffix(const char *input) {
    char *out = trim_dup(input);
    const char *p = out;
    if (*p == '+' || *p == '-') p++;
    if (!isdigit((unsigned char)*p)) return out;

    if (p[0] == '0' && (p[1] == 'x' || p[1] == 'X')) {
        p += 2;
        if (!isxdigit((unsigned char)*p)) return out;
        while (isxdigit((unsigned char)*p)) p++;
    } else {
        while (isdigit((unsigned char)*p)) p++;
    }

    if (*p == '.' || *p == 'e' || *p == 'E') return out;
    if (*p == 0) return out;

    int suffix_len = c_integer_suffix_len(p);
    if (suffix_len <= 0 || p[suffix_len] != 0) return out;

    size_t n = strlen(out) - (size_t)suffix_len;
    char *trimmed = (char *)malloc(n + 1);
    if (!trimmed) exit(1);
    memcpy(trimmed, out, n);
    trimmed[n] = 0;
    free(out);
    return trimmed;
}

static char *normalize_macro_constant_value(const char *value) {
    char *outer = strip_redundant_outer_parens(value);
    char *castless = strip_leading_numeric_cast(outer);
    free(outer);
    char *unwrapped = strip_redundant_outer_parens(castless);
    free(castless);
    char *normalized = strip_integer_literal_suffix(unwrapped);
    free(unwrapped);
    return normalized;
}

static void emit_macro(CXCursor c, CXTranslationUnit tu, FILE *out) {
    if (clang_Cursor_isMacroBuiltin(c) || clang_Cursor_isMacroFunctionLike(c)) return;

    char *name = cxstr_dup(clang_getCursorSpelling(c));
    if (name[0] == 0) {
        free(name);
        return;
    }
    char *clean = sanitize_ident(name);
    if (g_symbol_prefix && !starts_with(clean, g_symbol_prefix)) {
        free(clean);
        free(name);
        return;
    }

    CXSourceRange range = clang_getCursorExtent(c);
    CXToken *tokens = 0;
    unsigned token_count = 0;
    clang_tokenize(tu, range, &tokens, &token_count);
    if (!tokens || token_count == 0) {
        free(clean);
        free(name);
        return;
    }

    string_builder value = {0};
    int saw_name = 0;
    for (unsigned i = 0; i < token_count; i++) {
        char *tok = cxstr_dup(clang_getTokenSpelling(tu, tokens[i]));
        if (!saw_name) {
            if (streq(tok, name)) saw_name = 1;
            free(tok);
            continue;
        }
        if (token_spelling_is_define(tok)) {
            free(tok);
            continue;
        }
        if (value.length > 0) sb_append(&value, " ");
        sb_append(&value, tok);
        free(tok);
    }
    clang_disposeTokens(tu, tokens, token_count);

    if (value.length > 0) {
        char *normalized = normalize_macro_constant_value(value.data);
        fprintf(out, "#define %s %s\n", clean, normalized);
        free(normalized);
    }

    free(value.data);
    free(clean);
    free(name);
}

static void fprint_c_string_literal(FILE *out, const char *s) {
    fputc('"', out);
    for (const char *p = s; *p; p++) {
        unsigned char c = (unsigned char)*p;
        switch (c) {
            case '\\': fputs("\\\\", out); break;
            case '"': fputs("\\\"", out); break;
            case '\n': fputs("\\n", out); break;
            case '\r': fputs("\\r", out); break;
            case '\t': fputs("\\t", out); break;
            default:
                if (c < 32 || c >= 127) fprintf(out, "\\x%02x", c);
                else fputc(c, out);
                break;
        }
    }
    fputc('"', out);
}

static int type_is_const(CXType type) {
    if (clang_isConstQualifiedType(type)) return 1;
    if (type.kind == CXType_Pointer) {
        CXType pointee = clang_getPointeeType(type);
        return clang_isConstQualifiedType(pointee);
    }
    return 0;
}

static void emit_var_constant(CXCursor c, FILE *out) {
    if (!cursor_matches_symbol_prefix(c)) return;

    CXType type = clang_getCursorType(c);
    if (!type_is_const(type)) return;

    CXEvalResult value = clang_Cursor_Evaluate(c);
    if (!value) return;

    CXEvalResultKind kind = clang_EvalResult_getKind(value);
    if (kind != CXEval_Int && kind != CXEval_Float && kind != CXEval_StrLiteral) {
        clang_EvalResult_dispose(value);
        return;
    }

    char *name = cxstr_dup(clang_getCursorSpelling(c));
    if (name[0] == 0) {
        free(name);
        clang_EvalResult_dispose(value);
        return;
    }
    char *clean = sanitize_ident(name);

    fprintf(out, "#define %s ", clean);
    if (kind == CXEval_Int) {
        if (clang_EvalResult_isUnsignedInt(value)) {
            fprintf(out, "%llu", clang_EvalResult_getAsUnsigned(value));
        } else {
            fprintf(out, "%lld", clang_EvalResult_getAsLongLong(value));
        }
    } else if (kind == CXEval_Float) {
        fprintf(out, "%.17g", clang_EvalResult_getAsDouble(value));
    } else {
        const char *s = clang_EvalResult_getAsStr(value);
        fprint_c_string_literal(out, s ? s : "");
    }
    fprintf(out, "\n");

    free(clean);
    free(name);
    clang_EvalResult_dispose(value);
}

typedef struct tu_emit_ctx {
    CXTranslationUnit tu;
    FILE *out;
} tu_emit_ctx;

static enum CXChildVisitResult tu_visitor(CXCursor c, CXCursor parent, CXClientData data) {
    (void)parent;
    tu_emit_ctx *ctx = (tu_emit_ctx *)data;
    FILE *out = ctx->out;
    if (!cursor_is_from_main_file(c)) return CXChildVisit_Continue;
    enum CXCursorKind kind = clang_getCursorKind(c);
    switch (kind) {
        case CXCursor_MacroDefinition:
            emit_macro(c, ctx->tu, out);
            break;
        case CXCursor_VarDecl:
            emit_var_constant(c, out);
            break;
        case CXCursor_EnumDecl:
            if (!clang_isCursorDefinition(c)) break;
            emit_anonymous_enum_constants(c, out);
            if (!cursor_matches_symbol_prefix(c)) break;
            emit_enum(c, out);
            break;
        case CXCursor_StructDecl:
            if (!cursor_matches_symbol_prefix(c)) break;
            if (clang_isCursorDefinition(c)) emit_record(c, out, 0);
            break;
        case CXCursor_UnionDecl:
            if (!cursor_matches_symbol_prefix(c)) break;
            if (clang_isCursorDefinition(c)) emit_record(c, out, 1);
            break;
        case CXCursor_FunctionDecl:
            if (!cursor_matches_symbol_prefix(c)) break;
            emit_function(c, out);
            break;
        case CXCursor_TypedefDecl:
            if (!cursor_matches_symbol_prefix(c)) break;
            emit_typedef(c, out);
            break;
        default:
            break;
    }
    return CXChildVisit_Continue;
}

int main(int argc, char **argv) {
    if (argc < 3) {
        fprintf(stderr, "usage: ibind <input.h> <output.i> [--preprocess] [--print-cmd] [--filter path-fragment] [--prefix symbol-prefix] [-- <clang args...>]\n");
        return 2;
    }

    const char *input = argv[1];
    const char *output = argv[2];
    int clang_arg_start = 3;
    while (clang_arg_start < argc && strcmp(argv[clang_arg_start], "--") != 0) {
        if (strcmp(argv[clang_arg_start], "--filter") == 0 && clang_arg_start + 1 < argc) {
            g_filter_path = argv[clang_arg_start + 1];
            clang_arg_start += 2;
            continue;
        }
        if (strcmp(argv[clang_arg_start], "--prefix") == 0 && clang_arg_start + 1 < argc) {
            g_symbol_prefix = argv[clang_arg_start + 1];
            clang_arg_start += 2;
            continue;
        }
        if (strcmp(argv[clang_arg_start], "--preprocess") == 0) {
            g_preprocess = 1;
            clang_arg_start += 1;
            continue;
        }
        if (strcmp(argv[clang_arg_start], "--print-cmd") == 0) {
            g_print_cmd = 1;
            clang_arg_start += 1;
            continue;
        }
        fprintf(stderr, "ibind: unknown option %s\n", argv[clang_arg_start]);
        return 2;
    }
    if (clang_arg_start < argc && strcmp(argv[clang_arg_start], "--") == 0) clang_arg_start++;
    int raw_clang_arg_count = argc - clang_arg_start;
    const char **raw_clang_args = (const char **)(argv + clang_arg_start);
    int clang_arg_count = 0;
    const char **clang_args = normalize_clang_args(raw_clang_args, raw_clang_arg_count, &clang_arg_count);
    if (!clang_args) {
        fprintf(stderr, "ibind: out of memory\n");
        return 1;
    }
    char *pp_path = 0;
    const char *parse_input = input;
    if (g_preprocess) {
        pp_path = make_temp_path(output);
        int pp_code = preprocess_header(input, pp_path, clang_args, clang_arg_count);
        if (pp_code != 0) {
            fprintf(stderr, "ibind: clang preprocessing failed for %s\n", input);
            free(pp_path);
            free_normalized_clang_args(clang_args, clang_arg_count);
            return 1;
        }
        parse_input = pp_path;
    }
    const char **full_args = (const char **)calloc((size_t)clang_arg_count + 2, sizeof(char *));
    if (!full_args) {
        fprintf(stderr, "ibind: out of memory\n");
        free_normalized_clang_args(clang_args, clang_arg_count);
        return 1;
    }
    full_args[0] = "clang";
    for (int i = 0; i < clang_arg_count; i++) full_args[i + 1] = clang_args[i];

    CXIndex index = clang_createIndex(0, 0);
    CXTranslationUnit tu = 0;
    unsigned options = CXTranslationUnit_SkipFunctionBodies | CXTranslationUnit_DetailedPreprocessingRecord;
    enum CXErrorCode err = clang_parseTranslationUnit2FullArgv(index, parse_input, full_args, clang_arg_count + 1, 0, 0, options, &tu);
    free(full_args);
    if (err != CXError_Success || !tu) {
        fprintf(stderr, "ibind: failed to parse %s (libclang error %d)\n", parse_input, (int)err);
        free(pp_path);
        free_normalized_clang_args(clang_args, clang_arg_count);
        clang_disposeIndex(index);
        return 1;
    }

    unsigned diag_count = clang_getNumDiagnostics(tu);
    for (unsigned i = 0; i < diag_count; i++) {
        CXDiagnostic diag = clang_getDiagnostic(tu, i);
        enum CXDiagnosticSeverity sev = clang_getDiagnosticSeverity(diag);
        if (sev >= CXDiagnostic_Error) {
            char *text = cxstr_dup(clang_formatDiagnostic(diag, clang_defaultDiagnosticDisplayOptions()));
            fprintf(stderr, "%s\n", text);
            free(text);
        }
        clang_disposeDiagnostic(diag);
    }

    FILE *out = ibind_fopen(output, "wb");
    if (!out) {
        fprintf(stderr, "ibind: failed to write %s\n", output);
        clang_disposeTranslationUnit(tu);
        clang_disposeIndex(index);
        free_normalized_clang_args(clang_args, clang_arg_count);
        return 1;
    }

    CXCursor root = clang_getTranslationUnitCursor(tu);
    tu_emit_ctx ctx = {tu, out};
    clang_visitChildren(root, tu_visitor, &ctx);
    fclose(out);

    clang_disposeTranslationUnit(tu);
    clang_disposeIndex(index);
    free(pp_path);
    free_normalized_clang_args(clang_args, clang_arg_count);
    return 0;
}
