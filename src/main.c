//---------------------------------------------------------------------------------------------------
// monomorphization codegen limitations:
//---------------------------------------------------------------------------------------------------
// for containers that have value types eg `T`,
// the type must be included before the generated header.
// this is because the container expects to know the type in it's struct.
// Warning: cannot be recursive type
//
// for containers that have pointer types eg `T *`,
// the type can be included before or after the generated header.
// this is because the container has `T` forward declared.
// Warning: can be recursive type
//
// for types that include a container of themselves eg `struct T { Vec_T arr; };`
// the type must be included after the generated header.
// this is because the type needs to know the container definition.
// Warning: can be recursive type with `T *` but not `T`
//---------------------------------------------------------------------------------------------------
// primitives
//---------------------------------------------------------------------------------------------------
// haikal@Vec:voidptr:p
// haikal@Vec:i8:p
// haikal@Vec:i32:p
// haikal@Vec:f32:p
// haikal@Vec:char:p
// haikal@Vec:u8:p
// haikal@Map:i32:p
// haikal@Map:u64:p
// haikal@Node:i32:p
// haikal@List:i32:p
// haikal@BiNode:i32:p
// haikal@DList:i32:p
// haikal@Queue:i32:p
// haikal@Stack:i32:p
//---------------------------------------------------------------------------------------------------
// structs
//---------------------------------------------------------------------------------------------------
// haikal@Vec:string8:s
// haikal@Vec:string8slice:s
// haikal@Vec:Token:s
//---------------------------------------------------------------------------------------------------
// unions
//---------------------------------------------------------------------------------------------------
#include <stdlib.h>
#define SAHA_IMPLEMENTATION
#include <saha.h>

#define CORE_IMPL
#include <core.h>
bool i32_eq(i32 a, i32 b) { return a == b; }

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>
#endif

#include "string8.h"
#include "string8slice.h"

static const char *g_source_path = "<input>";
static const char *g_diag_source_path = null;
static const char *g_diag_import_chain = null;
static const char *g_stdin_override_path = null;
static string8 g_stdin_override_source = {0};
static memops_arena *g_index_arena = null; // owns compiler-internal lookup tables
static bool g_diag_json = false;
static bool g_profile = false;

/* Diagnostics are collected rather than fatal, so one bad line does not hide the
   rest of the file. Reporting stops once the list stops being useful, because
   past that point the errors are usually cascades from the first few. */
#define I_MAX_REPORTED_ERRORS 25
static i32 g_error_count = 0;
static bool g_json_array_open = false;

static void diag_json_finish(void) {
    if (g_json_array_open) {
        printf("]\n");
        g_json_array_open = false;
    }
}

static void diag_exit_with_errors(void) {
    diag_json_finish();
    exit(1);
}

static void diag_record_error(void) {
    g_error_count++;
    if (g_error_count >= I_MAX_REPORTED_ERRORS) {
        if (!g_diag_json) {
            printf("i: too many errors; stopping after %d\n", g_error_count);
        }
        diag_exit_with_errors();
    }
}
static const char *g_import_dirs[64] = {0};
static i32 g_import_dir_count = 0;

static double g_profile_import_probe_ms = 0.0;
static double g_profile_import_read_ms = 0.0;
static double g_profile_import_lex_ms = 0.0;
static double g_profile_import_parse_ms = 0.0;
static i32 g_profile_import_parse_count = 0;

static double profile_now_ms(void) {
#if defined(_WIN32)
    static LARGE_INTEGER freq;
    static bool initialized = false;
    LARGE_INTEGER now;
    if (!initialized) {
        QueryPerformanceFrequency(&freq);
        initialized = true;
    }
    QueryPerformanceCounter(&now);
    return ((double)now.QuadPart * 1000.0) / (double)freq.QuadPart;
#else
    return ((double)clock() * 1000.0) / (double)CLOCKS_PER_SEC;
#endif
}

static void profile_mark(const char *label, double *last_ms, double start_ms) {
    if (!g_profile) return;
    double now = profile_now_ms();
    fprintf(
        stderr,
        "i profile: %-28s %8.3f ms  total %8.3f ms\n",
        label,
        now - *last_ms,
        now - start_ms
    );
    *last_ms = now;
}

static void profile_import_summary(void) {
    if (!g_profile) return;
    fprintf(stderr, "i profile: imports parsed              %8d\n", g_profile_import_parse_count);
    fprintf(stderr, "i profile: import probe reads          %8.3f ms\n", g_profile_import_probe_ms);
    fprintf(stderr, "i profile: import source reads         %8.3f ms\n", g_profile_import_read_ms);
    fprintf(stderr, "i profile: import lex                  %8.3f ms\n", g_profile_import_lex_ms);
    fprintf(stderr, "i profile: import parse                %8.3f ms\n", g_profile_import_parse_ms);
}

static void diag_note_import_chain(void);

static bool cstr_equals(const char *a, const char *b) {
    return a && b && strcmp(a, b) == 0;
}

static bool cstr_starts_with(const char *s, const char *prefix) {
    return s && prefix && strncmp(s, prefix, strlen(prefix)) == 0;
}

static FILE *i_fopen(const char *path, const char *mode) {
#if defined(_WIN32)
    FILE *file = null;
    if (fopen_s(&file, path, mode) != 0) return null;
    return file;
#else
    return fopen(path, mode);
#endif
}

static bool file_exists_cstr(const char *path) {
    if (!path) return false;
    FILE *file = i_fopen(path, "rb");
    if (!file) return false;
    fclose(file);
    return true;
}

static void add_import_dir(const char *dir) {
    if (!dir || !dir[0]) return;
    if (g_import_dir_count >= 64) return;
    g_import_dirs[g_import_dir_count++] = dir;
}

static void diag_json_print_cstr(const char *s) {
    putchar('"');
    if (s) {
        for (const unsigned char *p = (const unsigned char *)s; *p; p++) {
            switch (*p) {
                case '\\': printf("\\\\"); break;
                case '"': printf("\\\""); break;
                case '\n': printf("\\n"); break;
                case '\r': printf("\\r"); break;
                case '\t': printf("\\t"); break;
                default:
                    if (*p < 0x20) {
                        printf("\\u%04x", *p);
                    } else {
                        putchar(*p);
                    }
                    break;
            }
        }
    }
    putchar('"');
}

static void diag_json_open_range(
    const char *path,
    i32 line,
    i32 col,
    i32 end_line,
    i32 end_col,
    const char *category,
    const char *message
) {
    if (end_line <= 0) {
        end_line = line;
    }
    if (end_col <= 0) {
        end_col = col > 0 ? col + 1 : col;
    }
    printf(g_json_array_open ? ",{\"severity\":\"error\",\"file\":" : "[{\"severity\":\"error\",\"file\":");
    g_json_array_open = true;
    diag_json_print_cstr(path ? path : g_source_path);
    printf(",\"line\":%d,\"column\":%d,\"end_line\":%d,\"end_column\":%d,\"category\":", line, col, end_line, end_col);
    diag_json_print_cstr(category ? category : "error");
    printf(",\"message\":");
    diag_json_print_cstr(message ? message : "");
    printf(",\"notes\":[");
}

static void diag_json_open(const char *path, i32 line, i32 col, const char *category, const char *message) {
    diag_json_open_range(path, line, col, line, col > 0 ? col + 1 : col, category, message);
}

static void diag_json_note_sep(bool *has_note) {
    if (*has_note) printf(",");
    *has_note = true;
}

static void diag_json_note_cstr(bool *has_note, const char *path, i32 line, i32 col, const char *message) {
    diag_json_note_sep(has_note);
    printf("{\"file\":");
    diag_json_print_cstr(path ? path : g_source_path);
    printf(",\"line\":%d,\"column\":%d,\"message\":", line, col);
    diag_json_print_cstr(message ? message : "");
    printf("}");
}

static void diag_json_note_import_chain(bool *has_note) {
    if (g_diag_import_chain && g_diag_import_chain[0]) {
        diag_json_note_sep(has_note);
        printf("{\"message\":");
        printf("\"imported through: ");
        for (const unsigned char *p = (const unsigned char *)g_diag_import_chain; *p; p++) {
            switch (*p) {
                case '\\': printf("\\\\"); break;
                case '"': printf("\\\""); break;
                case '\n': printf("\\n"); break;
                case '\r': printf("\\r"); break;
                case '\t': printf("\\t"); break;
                default:
                    if (*p < 0x20) printf("\\u%04x", *p);
                    else putchar(*p);
                    break;
            }
        }
        printf("\"}");
    }
}

static void diag_json_close(void) {
    printf("]}"); // closes the notes array and the diagnostic object
}

static void diag_json_error(const char *path, i32 line, i32 col, const char *category, const char *message) {
    bool has_note = false;
    diag_json_open(path, line, col, category, message);
    diag_json_note_import_chain(&has_note);
    diag_json_close();
}

static void diag_json_error_range(
    const char *path,
    i32 line,
    i32 col,
    i32 end_line,
    i32 end_col,
    const char *category,
    const char *message
) {
    bool has_note = false;
    diag_json_open_range(path, line, col, end_line, end_col, category, message);
    diag_json_note_import_chain(&has_note);
    diag_json_close();
}

static void diag_json_error_with_note(
    const char *path,
    i32 line,
    i32 col,
    const char *category,
    const char *message,
    const char *note_path,
    i32 note_line,
    i32 note_col,
    const char *note_message
) {
    bool has_note = false;
    diag_json_open(path, line, col, category, message);
    diag_json_note_import_chain(&has_note);
    if (note_message) {
        diag_json_note_cstr(&has_note, note_path, note_line, note_col, note_message);
    }
    diag_json_close();
}

static void diag_appendf(char *buf, size_t cap, size_t *used, const char *fmt, ...) {
    if (!buf || cap == 0 || !used || *used >= cap) return;
    va_list args;
    va_start(args, fmt);
    int wrote = vsnprintf(buf + *used, cap - *used, fmt, args);
    va_end(args);
    if (wrote < 0) return;
    size_t remaining = cap - *used;
    if ((size_t)wrote >= remaining) {
        *used = cap - 1;
        buf[cap - 1] = 0;
    } else {
        *used += (size_t)wrote;
    }
}

template(Vec(voidptr));
template(Vec(i8));
template(Vec(i32));
template(Vec(f32));
template(Vec(char));
template(Vec(u8));
template(Vec(string8));
template(Vec(string8slice));

typedef enum TokenKind {
    Token_EOF = 0,
    Token_Identifier,
    Token_Number,
    Token_String,
    Token_Char,
    Token_Colon,
    Token_Semicolon,
    Token_Directive,
    Token_Equal,
    Token_EqualEqual,
    Token_BangEqual,
    Token_PlusEqual,
    Token_MinusEqual,
    Token_StarEqual,
    Token_SlashEqual,
    Token_AmpersandEqual,
    Token_CaretEqual,
    Token_PipeEqual,
    Token_PercentEqual,
    Token_ShlEqual,
    Token_ShrEqual,
    Token_LessEqual,
    Token_GreaterEqual,
    Token_LBrace,
    Token_RBrace,
    Token_LParen,
    Token_RParen,
    Token_LBracket,
    Token_RBracket,
    Token_LAngle,
    Token_RAngle,
    Token_Comma,
    Token_Arrow,
    Token_Ampersand,
    Token_Caret,
    Token_Pipe,
    Token_Dot,
    Token_Ellipsis,
    Token_At,
    Token_Question,
    Token_Bang,
    Token_Tilde,
    Token_Plus,
    Token_Minus,
    Token_Star,
    Token_Slash,
    Token_Percent,
    Token_Keyword_Proc,
    Token_Keyword_Struct,
    Token_Keyword_Ret,
    Token_Keyword_For,
    Token_Keyword_If,
    Token_Keyword_Else,
    Token_Keyword_Import,
    Token_Keyword_Enum,
    Token_Keyword_Union,
    Token_Keyword_Alias,
    Token_Keyword_Const,
    Token_Keyword_Volatile,
    Token_Keyword_While,
    Token_Keyword_Do,
    Token_Keyword_Break,
    Token_Keyword_Continue,
    Token_Keyword_Switch,
    Token_Keyword_Case,
    Token_Keyword_Default,
    Token_Keyword_And,
    Token_Keyword_Or,
    Token_Keyword_Shl,
    Token_Keyword_Shr,
    Token_Keyword_Goto,
    Token_Keyword_Static,
    Token_Keyword_True,
    Token_Keyword_False,
} TokenKind;

typedef struct Token Token;
struct Token {
    TokenKind kind;
    string8slice text;
    i32 line;
    i32 col;
};

static const char *token_kind_name(TokenKind kind) {
    switch (kind) {
        case Token_EOF: return "end of file";
        case Token_Identifier: return "identifier";
        case Token_Number: return "number";
        case Token_String: return "string";
        case Token_Char: return "character literal";
        case Token_Colon: return "':'";
        case Token_Keyword_True: return "'true'";
        case Token_Keyword_False: return "'false'";
        case Token_Semicolon: return "';'";
        case Token_Directive: return "preprocessor directive";
        case Token_Equal: return "'='";
        case Token_EqualEqual: return "'=='";
        case Token_BangEqual: return "'!='";
        case Token_PlusEqual: return "'+='";
        case Token_MinusEqual: return "'-='";
        case Token_StarEqual: return "'*='";
        case Token_SlashEqual: return "'/='";
        case Token_AmpersandEqual: return "'&='";
        case Token_CaretEqual: return "'^='";
        case Token_PipeEqual: return "'|='";
        case Token_PercentEqual: return "'%='";
        case Token_ShlEqual: return "'shl='";
        case Token_ShrEqual: return "'shr='";
        case Token_LessEqual: return "'<='";
        case Token_GreaterEqual: return "'>='";
        case Token_LBrace: return "'{'";
        case Token_RBrace: return "'}'";
        case Token_LParen: return "'('";
        case Token_RParen: return "')'";
        case Token_LBracket: return "'['";
        case Token_RBracket: return "']'";
        case Token_LAngle: return "'<'";
        case Token_RAngle: return "'>'";
        case Token_Comma: return "','";
        case Token_Arrow: return "'->'";
        case Token_Ampersand: return "'&'";
        case Token_Caret: return "'^'";
        case Token_Pipe: return "'|'";
        case Token_Dot: return "'.'";
        case Token_Ellipsis: return "'...'";
        case Token_At: return "'@'";
        case Token_Question: return "'?'";
        case Token_Bang: return "'!'";
        case Token_Tilde: return "'~'";
        case Token_Plus: return "'+'";
        case Token_Minus: return "'-'";
        case Token_Star: return "'*'";
        case Token_Slash: return "'/'";
        case Token_Percent: return "'%'";
        case Token_Keyword_Proc: return "'proc'";
        case Token_Keyword_Struct: return "'struct'";
        case Token_Keyword_Ret: return "'return'";
        case Token_Keyword_For: return "'for'";
        case Token_Keyword_If: return "'if'";
        case Token_Keyword_Else: return "'else'";
        case Token_Keyword_Import: return "'import'";
        case Token_Keyword_Enum: return "'enum'";
        case Token_Keyword_Union: return "'union'";
        case Token_Keyword_Alias: return "'alias'";
        case Token_Keyword_Const: return "'const'";
        case Token_Keyword_Volatile: return "'volatile'";
        case Token_Keyword_While: return "'while'";
        case Token_Keyword_Do: return "'do'";
        case Token_Keyword_Break: return "'break'";
        case Token_Keyword_Continue: return "'continue'";
        case Token_Keyword_Switch: return "'switch'";
        case Token_Keyword_Case: return "'case'";
        case Token_Keyword_Default: return "'default'";
        case Token_Keyword_And: return "'and'";
        case Token_Keyword_Or: return "'or'";
        case Token_Keyword_Shl: return "'shl'";
        case Token_Keyword_Shr: return "'shr'";
        case Token_Keyword_Goto: return "'goto'";
        case Token_Keyword_Static: return "'static'";
    }
    return "unknown";
}

template(Vec(Token));

#include <Vec.h>

typedef struct TypeExpr TypeExpr;
typedef struct Expr Expr;
typedef struct Stmt Stmt;
typedef struct StructDecl StructDecl;
typedef struct EnumDecl EnumDecl;
typedef struct EnumItem EnumItem;
typedef struct SwitchCase SwitchCase;
typedef struct ProcDecl ProcDecl;
typedef struct Field Field;
typedef struct Param Param;

typedef enum TypeKind {
    Type_Name = 0,
    Type_Ptr,
    Type_Generic,
    Type_Array,
    Type_Proc,
} TypeKind;

struct TypeExpr {
    TypeKind kind;
    string8 name;
    string8 array_count;
    bool is_const;
    bool is_volatile;
    i32 line;
    i32 col;
    TypeExpr *elem;
    TypeExpr *ret_type;
    bool is_variadic;
    Vec_voidptr args; // TypeExpr*
    Vec_string8 arg_names;
};

typedef enum ExprKind {
    Expr_Name = 0,
    Expr_Number,
    Expr_String,
    Expr_Char,
    Expr_Call,
    Expr_Addr,
    Expr_Binary,
    Expr_Index,
    Expr_Field,
    Expr_SizeofType,
    Expr_AlignofType,
    Expr_ZeroInit,
    Expr_InitList,
    Expr_CompoundInit,
    Expr_Cast,
    Expr_Unary,
    Expr_Ternary,
} ExprKind;

typedef enum InitDesignatorKind {
    InitDesignator_None = 0,
    InitDesignator_Index,
    InitDesignator_Field,
} InitDesignatorKind;

struct Expr {
    ExprKind kind;
    string8 name;
    string8 number;
    /* `x<>` is written as `x_reflect` by the parser, which cannot know
       whether `x` is a type or a value. This keeps the identifier so the
       type phase, which does know, can rewrite a value's `<>` to its
       type's reflection record. Empty for `Type<>`. */
    string8 reflect_base;
    string8 string_lit;
    Vec_voidptr args;      // Expr*
    Vec_voidptr type_args; // TypeExpr*
    Vec_voidptr designators; // Expr* or null, parallel with args for init lists
    Vec_voidptr arg_directives; // Vec_string8* or null; args.length + 1 entries, last is the tail
    Vec_i32 designator_kinds; // InitDesignatorKind, parallel with args for init lists
    Expr *inner;
    Expr *left;
    Expr *right;
    Expr *third;
    TokenKind op;
    Expr *base;
    Expr *index_expr;
    TypeExpr *cast_type;
    i32 line;
    i32 col;
};

typedef enum StmtKind {
    Stmt_Var = 0,
    Stmt_Return,
    Stmt_Expr,
    Stmt_Assign,
    Stmt_For,
    Stmt_If,
    Stmt_While,
    Stmt_DoWhile,
    Stmt_Break,
    Stmt_Continue,
    Stmt_Switch,
    Stmt_Goto,
    Stmt_Label,
    Stmt_Directive, // a '#' line written inside a body; `name` holds it verbatim
} StmtKind;

struct Stmt {
    StmtKind kind;
    string8 name;
    string8 align;  /* the N in a trailing align(N); empty when unset */
    TypeExpr *type;
    const char *source_path;
    const char *import_chain;
    bool is_external;
    bool is_static;
    bool is_uninitialized; // '= ?': deliberately left with indeterminate contents
    Expr *lhs;
    Expr *expr;
    TokenKind assign_op;
    Stmt *for_init;
    Expr *for_cond;
    Stmt *for_step;
    Vec_voidptr for_body; // Stmt*
    Expr *while_cond;
    Vec_voidptr while_body; // Stmt*
    Expr *if_cond;
    Vec_voidptr if_then_body; // Stmt*
    Vec_voidptr if_else_body; // Stmt*
    Stmt *if_else_if;           // nested else-if
    Expr *switch_expr;
    Vec_voidptr switch_cases; // SwitchCase*
    Vec_voidptr switch_default_body; // Stmt*
    bool has_switch_default; // an empty 'default: { }' is still a default
    i32 line;
    i32 col;
};

struct SwitchCase {
    Expr *expr;
    Vec_voidptr body; // Stmt*
    i32 line;
    i32 col;
};

struct Field {
    /* '#' lines written just above this field, emitted verbatim before it. */
    Vec_string8 pre_directives;
    string8 name;
    string8 align;  /* the N in a trailing align(N); empty when unset */
    TypeExpr *type;
    string8 attrs;
    string8 bit_width;  // set for bitfields: 'flags: u32 : 4;'
    StructDecl *anon;   // set for anonymous struct/union members; name is empty
    i32 line;
    i32 col;
};

struct Param {
    string8 name;
    TypeExpr *type;
    i32 line;
    i32 col;
};

struct StructDecl {
    string8 name;
    bool no_layout_check;
    bool packed;
    string8 align;
    const char *source_path;
    const char *import_chain;
    bool is_generic;
    bool is_union;
    bool is_external;
    /* `struct<T>` is one entry; `struct<K, V>` is two. */
    Vec_string8 type_params;
    Vec_voidptr fields; // Field*
    Vec_string8 tail_directives; // '#' lines after the last field
    i32 line;
    i32 col;
};

struct EnumItem {
    /* '#' lines written just above this item, emitted verbatim before it. */
    Vec_string8 pre_directives;
    string8 name;
    string8 value;
    Expr *value_expr; // set when the value is a constant expression, not a bare token
    i32 line;
    i32 col;
};

struct EnumDecl {
    string8 underlying;  /* enum[u32]: the C underlying type, empty when unset */
    string8 name;
    const char *source_path;
    const char *import_chain;
    bool is_external;
    Vec_voidptr items; // EnumItem*
    Vec_string8 tail_directives; // '#' lines after the last item
    i32 line;
    i32 col;
};

struct ProcDecl {
    string8 name;
    const char *source_path;
    const char *import_chain;
    bool is_generic;
    bool is_external;
    bool emit_external_proto;
    bool is_variadic;
    bool is_static;
    /* `proc<T>` is one entry; `proc<T, U, V>` is three. A non-generic proc has
       none. Structs are still single-parameter, so StructDecl keeps a bare
       string8. */
    Vec_string8 type_params;
    string8 constraint;
    string8 callconv;
    TypeExpr *angle_type;
    /* Set instead of angle_type when the declaration wrote more than one, as in
       `proc<T, U>`. Resolved to type_params in the semantic pass. */
    Vec_voidptr angle_types; // TypeExpr*
    TypeExpr *generic_pattern;
    Vec_voidptr params; // Param*
    TypeExpr *ret_type;
    Vec_voidptr body; // Stmt*
    i32 line;
    i32 col;
};

typedef struct AliasDecl {
    string8 name;
    const char *source_path;
    const char *import_chain;
    TypeExpr *type;
    i32 line;
    i32 col;
} AliasDecl;

typedef struct Program {
    Vec_string8 preprocessor_lines;
    Vec_string8 defines; // macro name
    Vec_string8 imports; // string literal I import path token text
    Vec_string8 c_imports; // string literal C include path token text
    Vec_string8 i_imports; // string literal import paths ending in .i
    Vec_i32 i_import_lines;
    Vec_i32 i_import_cols;
    Vec_voidptr structs; // StructDecl*
    Vec_voidptr enums;   // EnumDecl*
    Vec_voidptr aliases; // AliasDecl*
    Vec_voidptr procs;   // ProcDecl*
    Vec_voidptr globals; // Stmt* (var decl)
    /* Array types whose count is a symbolic constant (`[Enum.Member]`) rather
       than a literal. The enum may be declared after — or imported alongside —
       the use, so the text is kept as written and resolved to its C name once
       every declaration is in scope. */
    Vec_voidptr pending_array_counts; // TypeExpr*
    /* Module dependency edges, recorded while imports are expanded. The
       expanded Program is flat, so without these the per-module headers would
       have no way to know which other headers to include. */
    Vec_string8 module_edge_from;
    Vec_string8 module_edge_to;
} Program;

typedef struct Scope {
    Vec_string8 locals;
    Vec_voidptr local_sites; // LocalDeclSite*
    Vec_string8 globals;
    Vec_string8 procs;
    Vec_string8 enum_types;
    i32 loop_depth;
    i32 switch_depth;
} Scope;

typedef struct LocalDeclSite {
    string8 name;
    i32 line;
    i32 col;
} LocalDeclSite;

typedef struct Parser {
    memops_arena *arena;
    string8 source;
    Vec_Token tokens;
    i32 index;
    bool pending_equal;
    bool reported_eof; // so an unclosed '{' reports once, not once per nesting level
    Vec_voidptr *pending_array_counts; // borrowed from the Program being parsed
} Parser;

static bool is_alpha(u8 c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static bool is_digit(u8 c) {
    return (c >= '0' && c <= '9');
}

static bool is_alnum(u8 c) {
    return is_alpha(c) || is_digit(c);
}

static bool is_hex_digit(u8 c) {
    return is_digit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

static bool char_escape_is_simple(u8 c) {
    return c == 'n' || c == 't' || c == 'r' || c == '\\' || c == '\'' || c == '"' ||
           c == 'a' || c == 'b' || c == 'f' || c == 'v' || c == '?';
}

static bool preprocessor_line_is_c_directive(u8 *line, u64 length);
static void preprocessor_error(i32 line, const char *message);

static void lex_error(i32 line, i32 col, const char *message) {
    if (g_diag_json) {
        diag_json_error(g_source_path, line, col, "lexer", message);
    } else {
        printf("%s:%d:%d: lexer error: %s\n", g_source_path, line, col, message);
    }
    diag_record_error();
}

static Token token_make(TokenKind kind, string8slice text, i32 line, i32 col) {
    Token t;
    t.kind = kind;
    t.text = text;
    t.line = line;
    t.col = col;
    return t;
}


/* ---------------------------------------------------------------------------
   Conditional compilation.

   ilang evaluates '#if' family directives itself and never emits them. A dead
   arm is skipped in the lexer, so it is not parsed, not type-checked, and does
   not exist as far as the rest of the compiler is concerned -- the same deal C
   makes, and the same deal Rust's cfg and Zig's comptime make. The cost is that
   a branch you are not currently building can rot; that is accepted
   deliberately, and is the reason `#else` works at file scope at all. Two arms
   would otherwise be two declarations of one name, and ilang would reject the
   pair before C ever saw it.

   What is *not* evaluated: '#define', '#include', '#pragma' and friends still
   pass through to C untouched. ilang records which names are defined so it can
   answer '#ifdef', but it does not expand macros -- that stays C's job. So a
   name defined by a C header, rather than by a '#define' ilang can see, is
   unknown here and its '#ifdef' takes the false arm. That divergence is not
   silent: whatever the arm declared is simply absent, and the first use of it
   is an ordinary `undeclared name` error.
   --------------------------------------------------------------------------- */

typedef struct PreprocFrame {
    bool live;   // this arm is being compiled
    bool taken;  // some arm of this chain has already been taken
    i32 line;    // where the '#if' opened, for the unterminated diagnostic
} PreprocFrame;

typedef struct PreprocState {
    Vec_string8 defines;      // names currently defined, in source order
    PreprocFrame frames[64];
    i32 depth;
    bool overflowed;
} PreprocState;

static bool preproc_live(PreprocState *pp) {
    return pp->depth == 0 || pp->frames[pp->depth - 1].live;
}

static bool preproc_is_defined(PreprocState *pp, u8 *name, u64 length) {
    for (i32 i = 0; i < pp->defines.length; i++) {
        string8 d = pp->defines.data[i];
        if (d.length == length && memcmp(d.data, name, (size_t)length) == 0) return true;
    }
    return false;
}

static void preproc_define(memops_arena *arena, PreprocState *pp, u8 *name, u64 length) {
    if (preproc_is_defined(pp, name, length)) return;
    Vec_string8_append(arena, &pp->defines, string8_copy_from_slice(arena, name, length));
}

static void preproc_undef(PreprocState *pp, u8 *name, u64 length) {
    for (i32 i = 0; i < pp->defines.length; i++) {
        string8 d = pp->defines.data[i];
        if (d.length == length && memcmp(d.data, name, (size_t)length) == 0) {
            for (i32 j = i; j + 1 < pp->defines.length; j++) {
                pp->defines.data[j] = pp->defines.data[j + 1];
            }
            pp->defines.length--;
            return;
        }
    }
}

/* --- the condition language -----------------------------------------------
   `defined(X)`, `defined X`, a bare name (undefined reads as 0, as in C),
   integer literals, `!`, `&&`, `||` and parentheses. Comparison and arithmetic
   are rejected rather than half-supported: nothing in the tree uses them, and
   quietly getting `#if VERSION > 2` wrong would be worse than refusing it. */

typedef struct PreprocScan {
    u8 *p;
    u8 *end;
    i32 line;
    bool failed;
} PreprocScan;

static void preproc_skip_space(PreprocScan *sc) {
    while (sc->p < sc->end && (*sc->p == ' ' || *sc->p == '\t')) sc->p++;
}

static bool preproc_ident_char(u8 c, bool first) {
    if (c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) return true;
    return !first && c >= '0' && c <= '9';
}

static bool preproc_eval_or(PreprocScan *sc, PreprocState *pp);

static bool preproc_eval_primary(PreprocScan *sc, PreprocState *pp) {
    preproc_skip_space(sc);
    if (sc->p >= sc->end) {
        preprocessor_error(sc->line, "expected a condition after the directive");
        sc->failed = true;
        return false;
    }
    if (*sc->p == '!') {
        sc->p++;
        return !preproc_eval_primary(sc, pp);
    }
    if (*sc->p == '(') {
        sc->p++;
        bool inner = preproc_eval_or(sc, pp);
        preproc_skip_space(sc);
        if (sc->p < sc->end && *sc->p == ')') sc->p++;
        return inner;
    }
    if (*sc->p >= '0' && *sc->p <= '9') {
        u64 value = 0;
        while (sc->p < sc->end && *sc->p >= '0' && *sc->p <= '9') {
            value = value * 10 + (u64)(*sc->p - '0');
            sc->p++;
        }
        return value != 0;
    }
    if (preproc_ident_char(*sc->p, true)) {
        u8 *name = sc->p;
        while (sc->p < sc->end && preproc_ident_char(*sc->p, false)) sc->p++;
        u64 length = (u64)(sc->p - name);
        if (length == 7 && memcmp(name, "defined", 7) == 0) {
            preproc_skip_space(sc);
            bool parenthesised = sc->p < sc->end && *sc->p == '(';
            if (parenthesised) {
                sc->p++;
                preproc_skip_space(sc);
            }
            u8 *target = sc->p;
            while (sc->p < sc->end && preproc_ident_char(*sc->p, false)) sc->p++;
            u64 target_length = (u64)(sc->p - target);
            if (parenthesised) {
                preproc_skip_space(sc);
                if (sc->p < sc->end && *sc->p == ')') sc->p++;
            }
            if (target_length == 0) {
                preprocessor_error(sc->line, "expected a macro name after 'defined'");
                sc->failed = true;
                return false;
            }
            return preproc_is_defined(pp, target, target_length);
        }
        /* A bare name: defined-ness stands in for its value, because ilang does
           not expand macros. `#if FOO` where FOO is `0` therefore reads as
           true here and false in C -- so it is refused rather than guessed. */
        preproc_skip_space(sc);
        if (sc->p < sc->end) {
            preprocessor_error(sc->line,
                               "only 'defined(X)', '!', '&&', '||' and integer literals are "
                               "supported in a condition; ilang does not expand macros");
            sc->failed = true;
            return false;
        }
        return preproc_is_defined(pp, name, length);
    }
    preprocessor_error(sc->line, "unsupported expression in a preprocessor condition");
    sc->failed = true;
    return false;
}

static bool preproc_eval_and(PreprocScan *sc, PreprocState *pp) {
    bool value = preproc_eval_primary(sc, pp);
    for (;;) {
        preproc_skip_space(sc);
        if (sc->p + 1 < sc->end && sc->p[0] == '&' && sc->p[1] == '&') {
            sc->p += 2;
            bool rhs = preproc_eval_primary(sc, pp);
            value = value && rhs;
            continue;
        }
        return value;
    }
}

static bool preproc_eval_or(PreprocScan *sc, PreprocState *pp) {
    bool value = preproc_eval_and(sc, pp);
    for (;;) {
        preproc_skip_space(sc);
        if (sc->p + 1 < sc->end && sc->p[0] == '|' && sc->p[1] == '|') {
            sc->p += 2;
            bool rhs = preproc_eval_and(sc, pp);
            value = value || rhs;
            continue;
        }
        return value;
    }
}

/* Splits `#name rest` and returns the directive name; `rest` is left pointing
   at whatever followed it. */
static string8slice preproc_directive_name(u8 *line, u64 length, u8 **out_rest) {
    u64 i = 0;
    while (i < length && (line[i] == ' ' || line[i] == '\t')) i++;
    if (i < length && line[i] == '#') i++;
    while (i < length && (line[i] == ' ' || line[i] == '\t')) i++;
    u64 start = i;
    while (i < length && preproc_ident_char(line[i], i == start)) i++;
    *out_rest = line + i;
    return string8slice_from_parts(line + start, i - start);
}

static bool preproc_name_is(string8slice name, const char *text) {
    u64 length = (u64)strlen(text);
    return name.length == length && memcmp(name.data, text, (size_t)length) == 0;
}


/* --- seeding the define table -------------------------------------------

   `import` is not `#include`. Each file is lexed separately, and the entry
   file is lexed before its imports are even known, so a `#define` in an
   imported file would not be visible to a conditional in the file importing it
   -- which is exactly how njinn's `#define gin_debug_draw` lives in pch.i and
   is tested for in gin.i.

   Every one of those defines ends up in a single generated translation unit
   anyway, so C sees them all regardless of order. This pre-pass makes ilang see
   the same set: it walks the entry file and its imports textually, before any
   lexing, and collects the `#define`s that are *not* themselves inside a
   conditional.

   Unconditional only, and deliberately so. A `#define` inside an `#ifdef` can
   only be resolved by evaluating that `#ifdef`, which needs the table this pass
   is building. Rather than close the loop, the rule is: an unconditional
   file-scope define is visible program-wide, a conditional one is visible from
   where it appears onward in its own file. The conservative direction -- a
   define in a dead arm is never collected. */

static const char *resolve_import_path(memops_arena *arena, string8 import_lit);
static string8 read_i_source_for_path(memops_arena *arena, const char *path);

static void preproc_seed_scan(memops_arena *arena, const char *path, PreprocState *pp,
                              Vec_string8 *visited, i32 depth) {
    if (depth > 32 || !path) return;
    for (i32 i = 0; i < visited->length; i++) {
        if (string8_equals_cstr(&visited->data[i], path)) return;
    }
    Vec_string8_append(arena, visited, string8_from_cstr(arena, path));

    string8 src = read_i_source_for_path(arena, path);
    if (!src.data) return;

    i32 conditional_depth = 0;
    u8 *p = src.data;
    u8 *end = src.data + src.length;
    while (p < end) {
        u8 *line_start = p;
        while (p < end && *p != '\n') p++;
        u8 *line_end = p;
        if (line_end > line_start && line_end[-1] == '\r') line_end--;
        u64 length = (u64)(line_end - line_start);
        if (p < end) p++;

        u64 i = 0;
        while (i < length && (line_start[i] == ' ' || line_start[i] == '\t')) i++;
        if (i >= length) continue;

        if (line_start[i] == '#') {
            u8 *rest = null;
            string8slice name = preproc_directive_name(line_start, length, &rest);
            if (preproc_name_is(name, "if") || preproc_name_is(name, "ifdef") ||
                preproc_name_is(name, "ifndef")) {
                conditional_depth++;
            } else if (preproc_name_is(name, "endif")) {
                if (conditional_depth > 0) conditional_depth--;
            } else if (conditional_depth == 0 &&
                       (preproc_name_is(name, "define") || preproc_name_is(name, "undef"))) {
                PreprocScan sc = {rest, line_end, 0, false};
                preproc_skip_space(&sc);
                u8 *target = sc.p;
                while (sc.p < sc.end && preproc_ident_char(*sc.p, false)) sc.p++;
                u64 target_length = (u64)(sc.p - target);
                if (target_length > 0) {
                    if (preproc_name_is(name, "define")) {
                        preproc_define(arena, pp, target, target_length);
                    } else {
                        preproc_undef(pp, target, target_length);
                    }
                }
            }
            continue;
        }

        /* `import "path.i"` -- matched textually, because parsing it would need
           the lexing this pass runs before. */
        static const char *keyword = "import";
        u64 keyword_length = 6;
        if (i + keyword_length >= length) continue;
        if (memcmp(line_start + i, keyword, (size_t)keyword_length) != 0) continue;
        i += keyword_length;
        while (i < length && (line_start[i] == ' ' || line_start[i] == '\t')) i++;
        if (i >= length || line_start[i] != '"') continue;
        u64 quote_start = i;
        i++;
        while (i < length && line_start[i] != '"') i++;
        if (i >= length) continue;
        string8 literal = string8_copy_from_slice(arena, line_start + quote_start,
                                                  (i - quote_start) + 1);
        preproc_seed_scan(arena, resolve_import_path(arena, literal), pp, visited, depth + 1);
    }
}

/* Defines visible to every file's conditionals. Built once, before any lexing. */
static PreprocState g_preproc_seed;
static bool g_preproc_seed_ready = false;

static void preproc_build_seed(memops_arena *arena, const char *entry_path) {
    g_preproc_seed.defines = Vec_string8_reserve(arena, 8);
    g_preproc_seed.depth = 0;
    Vec_string8 visited = Vec_string8_reserve(arena, 8);
    preproc_seed_scan(arena, entry_path, &g_preproc_seed, &visited, 0);
    g_preproc_seed_ready = true;
}

static void lex_tokens(memops_arena *arena, string8 src, Vec_Token *out_tokens,
                       Vec_string8 *out_directives) {
    i32 line = 1;
    i32 col = 1;
    i32 brace_depth = 0; // file scope is 0; a directive deeper than that cannot be hoisted
    u8 *p = src.data;
    u8 *end = src.data + src.length;

    PreprocState pp = {0};
    pp.defines = Vec_string8_reserve(arena, 8);
    /* Start from the program-wide unconditional defines, so a conditional can
       see a `#define` that lives in an imported file. */
    for (i32 i = 0; g_preproc_seed_ready && i < g_preproc_seed.defines.length; i++) {
        string8 seeded = g_preproc_seed.defines.data[i];
        preproc_define(arena, &pp, seeded.data, seeded.length);
    }

    *out_tokens = Vec_Token_reserve(arena, 256);
    if (out_directives) *out_directives = Vec_string8_reserve(arena, 8);

    while (p < end) {
        u8 c = *p;
        if (c == ' ' || c == '\t' || c == '\r') {
            p++;
            col++;
            continue;
        }
        if (c == '\n') {
            p++;
            line++;
            col = 1;
            continue;
        }
        if (c == '#') {
            u8 *line_start = p;
            u8 *line_end = null;
            while (p < end && *p != '\n') {
                p++;
            }
            line_end = p;
            if (line_end > line_start && line_end[-1] == '\r') line_end--;
            u64 line_length = (u64)(line_end - line_start);
            if (!preprocessor_line_is_c_directive(line_start, line_length)) {
                lex_error(line, col,
                          "unknown preprocessor directive; '#' starts a C preprocessor line, use '//' for comments");
            }

            u8 *rest = null;
            string8slice name = preproc_directive_name(line_start, line_length, &rest);
            u64 rest_length = (u64)(line_end - rest);
            bool parent_live = preproc_live(&pp);

            if (preproc_name_is(name, "if") || preproc_name_is(name, "ifdef") ||
                preproc_name_is(name, "ifndef")) {
                bool value = false;
                if (parent_live) {
                    PreprocScan sc = {rest, rest + rest_length, line, false};
                    if (preproc_name_is(name, "if")) {
                        value = preproc_eval_or(&sc, &pp);
                    } else {
                        preproc_skip_space(&sc);
                        u8 *target = sc.p;
                        while (sc.p < sc.end && preproc_ident_char(*sc.p, false)) sc.p++;
                        value = preproc_is_defined(&pp, target, (u64)(sc.p - target));
                        if (preproc_name_is(name, "ifndef")) value = !value;
                    }
                }
                if (pp.depth >= (i32)(sizeof(pp.frames) / sizeof(pp.frames[0]))) {
                    if (!pp.overflowed) {
                        preprocessor_error(line, "conditionals nested too deeply");
                        pp.overflowed = true;
                    }
                } else {
                    pp.frames[pp.depth].live = parent_live && value;
                    pp.frames[pp.depth].taken = value;
                    pp.frames[pp.depth].line = line;
                    pp.depth++;
                }
                continue;
            }
            if (preproc_name_is(name, "elif") || preproc_name_is(name, "else")) {
                if (pp.depth == 0) {
                    preprocessor_error(line, preproc_name_is(name, "else")
                                                 ? "'#else' without a matching '#if'"
                                                 : "'#elif' without a matching '#if'");
                    continue;
                }
                PreprocFrame *frame = &pp.frames[pp.depth - 1];
                bool grandparent_live = pp.depth == 1 || pp.frames[pp.depth - 2].live;
                bool value = true;
                if (preproc_name_is(name, "elif")) {
                    value = false;
                    if (grandparent_live && !frame->taken) {
                        PreprocScan sc = {rest, rest + rest_length, line, false};
                        value = preproc_eval_or(&sc, &pp);
                    }
                }
                frame->live = grandparent_live && !frame->taken && value;
                if (value) frame->taken = true;
                continue;
            }
            if (preproc_name_is(name, "endif")) {
                if (pp.depth == 0) {
                    preprocessor_error(line, "'#endif' without a matching '#if'");
                } else {
                    pp.depth--;
                }
                continue;
            }

            /* Everything else is C's business and passes through -- but only
               from an arm that is being compiled. */
            if (!preproc_live(&pp)) continue;

            if (preproc_name_is(name, "define") || preproc_name_is(name, "undef")) {
                PreprocScan sc = {rest, rest + rest_length, line, false};
                preproc_skip_space(&sc);
                u8 *target = sc.p;
                while (sc.p < sc.end && preproc_ident_char(*sc.p, false)) sc.p++;
                u64 target_length = (u64)(sc.p - target);
                if (target_length > 0) {
                    if (preproc_name_is(name, "define")) {
                        preproc_define(arena, &pp, target, target_length);
                    } else {
                        preproc_undef(&pp, target, target_length);
                    }
                }
            }

            if (brace_depth > 0) {
                string8slice text = string8slice_from_parts(line_start, line_length);
                Vec_Token_append(arena, out_tokens, token_make(Token_Directive, text, line, col));
            } else if (out_directives) {
                Vec_string8_append(arena, out_directives,
                                   string8_copy_from_slice(arena, line_start, line_length));
            }
            continue; // the whole line is consumed either way
        }

        /* Inside a dead arm nothing is lexed: the code is not parsed, not
           checked, and does not exist. Skipping by line rather than by token
           keeps a '#' inside a dead string literal from being read as a
           directive, and is what lets a dead arm hold text that is not even
           valid ilang. */
        if (!preproc_live(&pp)) {
            while (p < end && *p != '\n') p++;
            continue;
        }
        if (c == '/' && (p + 1) < end && p[1] == '/') {
            while (p < end && *p != '\n') {
                p++;
                col++;
            }
            continue;
        }
        if (c == '/' && (p + 1) < end && p[1] == '*') {
            i32 start_line = line;
            i32 start_col = col;
            p += 2;
            col += 2;
            bool closed = false;
            while (p < end) {
                if (*p == '*' && (p + 1) < end && p[1] == '/') {
                    p += 2;
                    col += 2;
                    closed = true;
                    break;
                }
                if (*p == '\n') {
                    p++;
                    line++;
                    col = 1;
                } else {
                    p++;
                    col++;
                }
            }
            if (!closed) {
                lex_error(start_line, start_col, "unterminated block comment");
            }
            continue; // input is exhausted when unterminated, so the loop ends next
        }

        if (is_alpha(c)) {
            u8 *start = p;
            i32 start_col = col;
            while (p < end && is_alnum(*p)) {
                p++;
                col++;
            }
            string8slice text = string8slice_from_parts(start, (u64)(p - start));
            TokenKind kind = Token_Identifier;
            if (string8slice_equals_cstr(text, "proc")) kind = Token_Keyword_Proc;
            else if (string8slice_equals_cstr(text, "struct")) kind = Token_Keyword_Struct;
            else if (string8slice_equals_cstr(text, "return")) kind = Token_Keyword_Ret;
            else if (string8slice_equals_cstr(text, "for")) kind = Token_Keyword_For;
            else if (string8slice_equals_cstr(text, "if")) kind = Token_Keyword_If;
            else if (string8slice_equals_cstr(text, "else")) kind = Token_Keyword_Else;
            else if (string8slice_equals_cstr(text, "import")) kind = Token_Keyword_Import;
            else if (string8slice_equals_cstr(text, "enum")) kind = Token_Keyword_Enum;
            else if (string8slice_equals_cstr(text, "union")) kind = Token_Keyword_Union;
            else if (string8slice_equals_cstr(text, "alias")) kind = Token_Keyword_Alias;
            else if (string8slice_equals_cstr(text, "const")) kind = Token_Keyword_Const;
            else if (string8slice_equals_cstr(text, "volatile")) kind = Token_Keyword_Volatile;
            else if (string8slice_equals_cstr(text, "while")) kind = Token_Keyword_While;
            else if (string8slice_equals_cstr(text, "do")) kind = Token_Keyword_Do;
            else if (string8slice_equals_cstr(text, "break")) kind = Token_Keyword_Break;
            else if (string8slice_equals_cstr(text, "continue")) kind = Token_Keyword_Continue;
            else if (string8slice_equals_cstr(text, "switch")) kind = Token_Keyword_Switch;
            else if (string8slice_equals_cstr(text, "case")) kind = Token_Keyword_Case;
            else if (string8slice_equals_cstr(text, "default")) kind = Token_Keyword_Default;
            else if (string8slice_equals_cstr(text, "and")) kind = Token_Keyword_And;
            else if (string8slice_equals_cstr(text, "or")) kind = Token_Keyword_Or;
            else if (string8slice_equals_cstr(text, "shl")) kind = Token_Keyword_Shl;
            else if (string8slice_equals_cstr(text, "shr")) kind = Token_Keyword_Shr;
            else if (string8slice_equals_cstr(text, "goto")) kind = Token_Keyword_Goto;
            else if (string8slice_equals_cstr(text, "static")) kind = Token_Keyword_Static;
            else if (string8slice_equals_cstr(text, "true")) kind = Token_Keyword_True;
            else if (string8slice_equals_cstr(text, "false")) kind = Token_Keyword_False;
            /* 'shl='/'shr=' are compound assignment; the '=' must touch the keyword. */
            if ((kind == Token_Keyword_Shl || kind == Token_Keyword_Shr) &&
                p < end && *p == '=' && !((p + 1) < end && p[1] == '=')) {
                p++;
                col++;
                text = string8slice_from_parts(start, (u64)(p - start));
                kind = (kind == Token_Keyword_Shl) ? Token_ShlEqual : Token_ShrEqual;
            }
            Vec_Token_append(arena, out_tokens, token_make(kind, text, line, start_col));
            continue;
        }
        if (is_digit(c)) {
            u8 *start = p;
            i32 start_col = col;
            while (p < end && is_digit(*p)) {
                p++;
                col++;
            }
            if (p < end && *p == '.' && (p + 1) < end && is_digit(p[1])) {
                p++;
                col++;
                while (p < end && is_digit(*p)) {
                    p++;
                    col++;
                }
            }
            /* A signed exponent is part of the literal, not a following operator.
               Without this `3.4e+38f` lexes as `3.4e`, `+`, `38f` and emits C
               that will not compile. Hex literals are excluded because their
               'e' digits are mantissa, not an exponent marker. */
            bool is_hex = (p - start) > 1 && start[0] == '0' &&
                          (start[1] == 'x' || start[1] == 'X');
            if (!is_hex && p < end && (*p == 'e' || *p == 'E')) {
                u8 *exp_start = p;
                i32 exp_col = col;
                u8 *q = p + 1;
                i32 qcol = col + 1;
                if (q < end && (*q == '+' || *q == '-')) {
                    q++;
                    qcol++;
                }
                if (q < end && is_digit(*q)) {
                    p = q;
                    col = qcol;
                    while (p < end && is_digit(*p)) {
                        p++;
                        col++;
                    }
                } else {
                    p = exp_start;
                    col = exp_col;
                }
            }
            while (p < end && is_alnum(*p)) {
                p++;
                col++;
            }
            string8slice text = string8slice_from_parts(start, (u64)(p - start));
            Vec_Token_append(arena, out_tokens, token_make(Token_Number, text, line, start_col));
            continue;
        }
        if (c == '"') {
            u8 *start = p;
            i32 start_col = col;
            p++;
            col++;
            while (p < end) {
                if (*p == '\\' && (p + 1) < end) {
                    p += 2;
                    col += 2;
                    continue;
                }
                if (*p == '"') {
                    p++;
                    col++;
                    break;
                }
                if (*p == '\n') break; // reported below; the newline ends the token
                p++;
                col++;
            }
            if (p > end || start == p || p[-1] != '"') {
                lex_error(line, start_col, "unterminated string");
                continue; // resume at the newline or end of input
            }
            string8slice text = string8slice_from_parts(start, (u64)(p - start));
            Vec_Token_append(arena, out_tokens, token_make(Token_String, text, line, start_col));
            continue;
        }
        if (c == '\'') {
            u8 *start = p;
            i32 start_col = col;
            p++;
            col++;
            bool closed = false;
            i32 char_count = 0;
            const char *error_message = null;
            while (p < end && *p != '\n') {
                if (*p == '\'') {
                    p++;
                    col++;
                    closed = true;
                    break;
                }
                if (*p == '\\') {
                    if ((p + 1) >= end) break;
                    u8 esc = p[1];
                    p += 2;
                    col += 2;
                    if (esc == 'x') {
                        i32 digits = 0;
                        while (p < end && is_hex_digit(*p)) {
                            p++;
                            col++;
                            digits++;
                        }
                        if (digits == 0 && !error_message) {
                            error_message = "\\x escape needs at least one hex digit";
                        }
                    } else if (esc >= '0' && esc <= '7') {
                        i32 digits = 1;
                        while (p < end && digits < 3 && *p >= '0' && *p <= '7') {
                            p++;
                            col++;
                            digits++;
                        }
                    } else if (!char_escape_is_simple(esc) && !error_message) {
                        error_message = "unknown escape sequence in character literal";
                    }
                    char_count++;
                    continue;
                }
                p++;
                col++;
                char_count++;
            }
            if (!error_message) {
                if (!closed) error_message = "unterminated character literal";
                else if (char_count == 0) error_message = "empty character literal";
                else if (char_count > 1) error_message = "multi-character literal; character literals hold one character";
            }
            if (error_message) {
                lex_error(line, start_col, error_message);
                continue; // p is past the literal already
            }
            string8slice text = string8slice_from_parts(start, (u64)(p - start));
            Vec_Token_append(arena, out_tokens, token_make(Token_Char, text, line, start_col));
            continue;
        }

        if ((p + 1) < end) {
            if ((p + 2) < end && c == '.' && p[1] == '.' && p[2] == '.') {
                Vec_Token_append(arena, out_tokens, token_make(Token_Ellipsis, string8slice_from_parts(p, 3), line, col));
                p += 3;
                col += 3;
                continue;
            }
            if (c == '=' && p[1] == '=') {
                Vec_Token_append(arena, out_tokens, token_make(Token_EqualEqual, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
            if (c == '!' && p[1] == '=') {
                Vec_Token_append(arena, out_tokens, token_make(Token_BangEqual, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
            if (c == '<' && p[1] == '=') {
                Vec_Token_append(arena, out_tokens, token_make(Token_LessEqual, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
            if (c == '>' && p[1] == '=') {
                Vec_Token_append(arena, out_tokens, token_make(Token_GreaterEqual, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
            if (c == '+' && p[1] == '=') {
                Vec_Token_append(arena, out_tokens, token_make(Token_PlusEqual, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
            if (c == '-' && p[1] == '=') {
                Vec_Token_append(arena, out_tokens, token_make(Token_MinusEqual, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
            if (c == '*' && p[1] == '=') {
                Vec_Token_append(arena, out_tokens, token_make(Token_StarEqual, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
            if (c == '/' && p[1] == '=') {
                Vec_Token_append(arena, out_tokens, token_make(Token_SlashEqual, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
            if (c == '%' && p[1] == '=') {
                Vec_Token_append(arena, out_tokens, token_make(Token_PercentEqual, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
            if (c == '&' && p[1] == '=') {
                Vec_Token_append(arena, out_tokens, token_make(Token_AmpersandEqual, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
            if (c == '^' && p[1] == '=') {
                Vec_Token_append(arena, out_tokens, token_make(Token_CaretEqual, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
            if (c == '|' && p[1] == '=') {
                Vec_Token_append(arena, out_tokens, token_make(Token_PipeEqual, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
            if (c == '-' && p[1] == '>') {
                Vec_Token_append(arena, out_tokens, token_make(Token_Arrow, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
        }

        TokenKind kind = Token_EOF;
        switch (c) {
            case ':': kind = Token_Colon; break;
            case ';': kind = Token_Semicolon; break;
            case '=': kind = Token_Equal; break;
            case '{': kind = Token_LBrace; brace_depth++; break;
            case '}': kind = Token_RBrace; if (brace_depth > 0) brace_depth--; break;
            case '(': kind = Token_LParen; break;
            case ')': kind = Token_RParen; break;
            case '[': kind = Token_LBracket; break;
            case ']': kind = Token_RBracket; break;
            case '<': kind = Token_LAngle; break;
            case '>': kind = Token_RAngle; break;
            case ',': kind = Token_Comma; break;
            case '&': kind = Token_Ampersand; break;
            case '^': kind = Token_Caret; break;
            case '|': kind = Token_Pipe; break;
            case '.': kind = Token_Dot; break;
            case '@': kind = Token_At; break;
            case '?': kind = Token_Question; break;
            case '!': kind = Token_Bang; break;
            case '~': kind = Token_Tilde; break;
            case '+': kind = Token_Plus; break;
            case '-': kind = Token_Minus; break;
            case '*': kind = Token_Star; break;
            case '/': kind = Token_Slash; break;
            case '%': kind = Token_Percent; break;
            default: break;
        }

        if (kind != Token_EOF) {
            Vec_Token_append(arena, out_tokens, token_make(kind, string8slice_from_parts(p, 1), line, col));
            p++;
            col++;
            continue;
        }

        char message[128];
        snprintf(message, sizeof(message), "unexpected char '%c'", c);
        lex_error(line, col, message);
        p++; // must advance or the loop never terminates
        col++;
    }

    /* Reported innermost-first, at the line that opened it rather than at the
       end of the file, which is where the fix belongs. */
    for (i32 i = pp.depth; i > 0; i--) {
        preprocessor_error(pp.frames[i - 1].line, "unterminated '#if': missing '#endif'");
    }

    Vec_Token_append(arena, out_tokens, token_make(Token_EOF, string8slice_from_parts(end, 0), line, col));
}

static Token *parser_peek(Parser *p) {
    if (p->index < p->tokens.length) {
        return &p->tokens.data[p->index];
    }
    return &p->tokens.data[p->tokens.length - 1];
}

static Token *parser_prev(Parser *p) {
    if (p->index > 0) {
        return &p->tokens.data[p->index - 1];
    }
    return &p->tokens.data[0];
}

static Token *parser_peek_n(Parser *p, i32 n) {
    i32 idx = p->index + n;
    if (idx < p->tokens.length) {
        return &p->tokens.data[idx];
    }
    return &p->tokens.data[p->tokens.length - 1];
}

static Token *parser_next(Parser *p) {
    if (p->index < p->tokens.length) {
        p->index++;
    }
    return parser_prev(p);
}

static Token *parser_expect(Parser *p, TokenKind kind, const char *msg);

static bool parser_match(Parser *p, TokenKind kind) {
    if (kind == Token_Equal && p->pending_equal) {
        p->pending_equal = false;
        return true;
    }
    if (parser_peek(p)->kind == kind) {
        parser_next(p);
        return true;
    }
    return false;
}

static void parser_expect_generic_close(Parser *p) {
    if (parser_match(p, Token_RAngle)) return;
    if (parser_match(p, Token_GreaterEqual)) {
        p->pending_equal = true;
        return;
    }
    parser_expect(p, Token_RAngle, "expected '>'");
}

static bool parser_next_is_generic_call(Parser *p) {
    if (parser_peek(p)->kind != Token_LAngle) return false;
    i32 idx = p->index;
    i32 depth = 0;
    while (idx < p->tokens.length) {
        TokenKind kind = p->tokens.data[idx].kind;
        if (kind == Token_LAngle) {
            depth++;
        } else if (kind == Token_RAngle) {
            depth--;
            if (depth == 0) {
                idx++;
                return idx < p->tokens.length && p->tokens.data[idx].kind == Token_LParen;
            }
        } else if (kind == Token_Semicolon || kind == Token_RParen || kind == Token_LBrace || kind == Token_RBrace) {
            return false;
        }
        idx++;
    }
    return false;
}

static bool parser_next_is_generic_qualified_call(Parser *p) {
    if (parser_peek(p)->kind != Token_LAngle) return false;
    i32 idx = p->index;
    i32 depth = 0;
    while (idx < p->tokens.length) {
        TokenKind kind = p->tokens.data[idx].kind;
        if (kind == Token_LAngle) {
            depth++;
        } else if (kind == Token_RAngle) {
            depth--;
            if (depth == 0) {
                idx++;
                return idx + 1 < p->tokens.length &&
                       p->tokens.data[idx].kind == Token_Identifier &&
                       p->tokens.data[idx + 1].kind == Token_LParen;
            }
        } else if (kind == Token_Semicolon || kind == Token_RParen || kind == Token_LBrace || kind == Token_RBrace) {
            return false;
        }
        idx++;
    }
    return false;
}

static bool parser_next_is_generic_compound_init(Parser *p) {
    if (parser_peek(p)->kind != Token_LAngle) return false;
    i32 idx = p->index;
    i32 depth = 0;
    while (idx < p->tokens.length) {
        TokenKind kind = p->tokens.data[idx].kind;
        if (kind == Token_LAngle) {
            depth++;
        } else if (kind == Token_RAngle) {
            depth--;
            if (depth == 0) {
                idx++;
                return idx < p->tokens.length && p->tokens.data[idx].kind == Token_LBrace;
            }
        } else if (kind == Token_Semicolon || kind == Token_RParen || kind == Token_RBrace) {
            return false;
        }
        idx++;
    }
    return false;
}

static bool parser_paren_operand_looks_like_type(Parser *p) {
    i32 idx = p->index;
    i32 angle_depth = 0;
    bool saw_any = false;
    while (idx < p->tokens.length) {
        TokenKind kind = p->tokens.data[idx].kind;
        if (kind == Token_RParen && angle_depth == 0) {
            return saw_any;
        }
        if (kind == Token_LAngle) {
            angle_depth++;
            idx++;
            continue;
        }
        if (kind == Token_RAngle) {
            if (angle_depth <= 0) return false;
            angle_depth--;
            idx++;
            continue;
        }
        if (kind == Token_Identifier || kind == Token_Star || kind == Token_Comma) {
            saw_any = true;
            idx++;
            continue;
        }
        return false;
    }
    return false;
}

static string8slice parser_source_line(Parser *p, i32 line) {
    if (!p || !p->source.data || p->source.length == 0 || line <= 0) {
        return string8slice_from_parts((u8 *)"", 0);
    }
    u8 *at = p->source.data;
    u8 *end = p->source.data + p->source.length;
    i32 current = 1;
    while (at < end && current < line) {
        if (*at == '\n') current++;
        at++;
    }
    u8 *start = at;
    while (at < end && *at != '\n' && *at != '\r') {
        at++;
    }
    return string8slice_from_parts(start, (u64)(at - start));
}

static void parser_print_context_range(Parser *p, Token *t, i32 range_len) {
    i32 source_line = t->line;
    string8slice line = parser_source_line(p, source_line);
    if (t->kind == Token_EOF && line.length == 0) {
        while (source_line > 1) {
            source_line -= 1;
            line = parser_source_line(p, source_line);
            if (line.length > 0) break;
        }
    }
    if (line.length == 0) return;
    printf("    %.*s\n", (int)line.length, line.data);
    i32 caret_col = t->col > 0 ? t->col : 1;
    if (t->kind == Token_EOF) {
        caret_col = (i32)line.length + 1;
    }
    if (range_len <= 0) {
        range_len = 1;
    }
    printf("    ");
    for (i32 i = 1; i < caret_col; i++) {
        printf(" ");
    }
    printf("^");
    for (i32 i = 1; i < range_len; i++) {
        printf("~");
    }
    printf("\n");
}

static void parser_print_error(Parser *p, Token *t, const char *msg, TokenKind expected, bool has_expected) {
    if (g_diag_json) {
        char message[1024];
        if (t->text.length > 0) {
            if (has_expected) {
                snprintf(
                    message,
                    sizeof(message),
                    "%s; expected %s, got %s `%.*s`",
                    msg,
                    token_kind_name(expected),
                    token_kind_name(t->kind),
                    (int)t->text.length,
                    t->text.data
                );
            } else {
                snprintf(
                    message,
                    sizeof(message),
                    "%s; got %s `%.*s`",
                    msg,
                    token_kind_name(t->kind),
                    (int)t->text.length,
                    t->text.data
                );
            }
        } else if (has_expected) {
            snprintf(
                message,
                sizeof(message),
                "%s; expected %s, got %s",
                msg,
                token_kind_name(expected),
                token_kind_name(t->kind)
            );
        } else {
            snprintf(
                message,
                sizeof(message),
                "%s; got %s",
                msg,
                token_kind_name(t->kind)
            );
        }
        i32 range_len = t->text.length > 0 && t->text.length < 1024 ? (i32)t->text.length : 1;
        diag_json_error_range(g_source_path, t->line, t->col, t->line, t->col + range_len, "parse", message);
        diag_record_error();
        return;
    }
    if (t->text.length > 0) {
        if (has_expected) {
            printf(
                "%s:%d:%d: parse error: %s; expected %s, got %s `%.*s`\n",
                g_source_path,
                t->line,
                t->col,
                msg,
                token_kind_name(expected),
                token_kind_name(t->kind),
                (int)t->text.length,
                t->text.data
            );
        } else {
            printf(
                "%s:%d:%d: parse error: %s; got %s `%.*s`\n",
                g_source_path,
                t->line,
                t->col,
                msg,
                token_kind_name(t->kind),
                (int)t->text.length,
                t->text.data
            );
        }
    } else if (has_expected) {
        printf(
            "%s:%d:%d: parse error: %s; expected %s, got %s\n",
            g_source_path,
            t->line,
            t->col,
            msg,
            token_kind_name(expected),
            token_kind_name(t->kind)
        );
    } else {
        printf(
            "%s:%d:%d: parse error: %s; got %s\n",
            g_source_path,
            t->line,
            t->col,
            msg,
            token_kind_name(t->kind)
        );
    }
    i32 range_len = t->text.length > 0 && t->text.length < 1024 ? (i32)t->text.length : 1;
    parser_print_context_range(p, t, range_len);
    diag_note_import_chain();
    diag_record_error();
}

static void parser_error_token(Parser *p, Token *t, const char *msg) {
    parser_print_error(p, t, msg, Token_EOF, false);
}

/* Ends a '{ ... }' body at '}' or at end of file. Without the end-of-file case an
   unclosed brace would spin forever, because parse errors no longer exit and
   parser_next() stops advancing once the token stream is exhausted. */
static bool parser_at_block_end(Parser *p) {
    if (parser_match(p, Token_RBrace)) return true;
    if (parser_peek(p)->kind != Token_EOF) return false;
    if (!p->reported_eof) {
        p->reported_eof = true;
        parser_error_token(p, parser_peek(p), "unexpected end of file: unclosed '{'");
    }
    return true;
}

static Token *parser_expect(Parser *p, TokenKind kind, const char *msg) {
    if (kind == Token_Equal && p->pending_equal) {
        p->pending_equal = false;
        return parser_prev(p);
    }
    if (parser_peek(p)->kind != kind) {
        Token *t = parser_peek(p);
        parser_print_error(p, t, msg, kind, true);
    }
    return parser_next(p);
}

static string8 token_to_string8(memops_arena *arena, Token *t) {
    return string8_copy_from_slice(arena, t->text.data, t->text.length);
}


static string8 string_lit_inner(memops_arena *arena, string8 lit) {
    if (lit.length >= 2 && lit.data[0] == '"' && lit.data[lit.length - 1] == '"') {
        return string8_copy_from_slice(arena, lit.data + 1, lit.length - 2);
    }
    return lit;
}

static bool string8_ends_with_cstr(string8 s, const char *suffix) {
    u64 suffix_len = (u64)strlen(suffix);
    if (s.length < suffix_len) return false;
    return strncmp((const char *)(s.data + s.length - suffix_len), suffix, suffix_len) == 0;
}

static Vec_voidptr ptr_array_reserve(memops_arena *arena, i32 capacity) {
    return Vec_voidptr_reserve(arena, capacity);
}

static void ptr_array_append(memops_arena *arena, Vec_voidptr *arr, void *ptr) {
    Vec_voidptr_append(arena, arr, ptr);
}

static TypeExpr *type_new(memops_arena *arena, TypeKind kind) {
    TypeExpr *t = memops_arena_push_struct(arena, TypeExpr);
    memset(t, 0, sizeof(TypeExpr));
    t->kind = kind;
    return t;
}

static TypeExpr *parse_type(Parser *p);
static string8 concat_name2(memops_arena *arena, string8 a, const char *sep, string8 b);

static string8 type_mangle_concrete(memops_arena *arena, TypeExpr *type) {
    string8 out = string8_reserve(arena, 32);
    if (!type) return out;
    if (type->is_const) {
        string8_append_cstr(arena, &out, "const_");
    }
    if (type->kind == Type_Name) {
        string8_append_bytes(arena, &out, type->name.data, type->name.length);
        return out;
    }
    if (type->kind == Type_Ptr) {
        string8_append_cstr(arena, &out, "ptr_");
        string8 inner = type_mangle_concrete(arena, type->elem);
        string8_append_bytes(arena, &out, inner.data, inner.length);
        return out;
    }
    if (type->kind == Type_Generic) {
        string8_append_bytes(arena, &out, type->name.data, type->name.length);
        for (i32 i = 0; i < type->args.length; i++) {
            string8_append_cstr(arena, &out, "_");
            TypeExpr *arg = (TypeExpr *)type->args.data[i];
            string8 inner = type_mangle_concrete(arena, arg);
            string8_append_bytes(arena, &out, inner.data, inner.length);
        }
        return out;
    }
    if (type->kind == Type_Array) {
        string8_append_cstr(arena, &out, "array_");
        string8_append_bytes(arena, &out, type->array_count.data, type->array_count.length);
        string8_append_cstr(arena, &out, "_");
        string8 inner = type_mangle_concrete(arena, type->elem);
        string8_append_bytes(arena, &out, inner.data, inner.length);
        return out;
    }
    if (type->kind == Type_Proc) {
        string8_append_cstr(arena, &out, "proc_");
        if (type->ret_type) {
            string8 ret = type_mangle_concrete(arena, type->ret_type);
            string8_append_bytes(arena, &out, ret.data, ret.length);
        } else {
            string8_append_cstr(arena, &out, "void");
        }
        for (i32 i = 0; i < type->args.length; i++) {
            string8_append_cstr(arena, &out, "_");
            string8 arg = type_mangle_concrete(arena, (TypeExpr *)type->args.data[i]);
            string8_append_bytes(arena, &out, arg.data, arg.length);
        }
        return out;
    }
    return out;
}

/* One attribute slot per declaration: `struct[external]`, `proc[external,
   WINCALL]`, `enum[external]`.

   The slot already existed on procs, holding a single calling-convention
   identifier (`platform_add: proc[WINCALL](...)`). This generalises it to a
   comma-separated list and gives structs and enums the same one.

   The bracket is where `external` belongs, because it says something about the
   *declaration* -- do not emit it, C already has it -- while the braces are
   where fields and statements go. Putting it inside the body made
   `FILE: struct = { external; }` -- the old spelling, since replaced by
   `struct[external]` -- read as a struct with one strange member, and
   left procs and structs marking the same concept two different ways.

   The old spellings still parse, so the tree can migrate a file at a time.

   The rule for what may live here: an attribute may change how a declaration is
   lowered; it may not change what the declaration means in I. See
   docs/attributes.md. */
typedef struct DeclAttributes {
    bool is_external;
    bool emit_external_proto;
    bool no_layout_check;
    bool packed;
    string8 align;       /* the N in align(N), empty when unset */
    string8 underlying;  /* enum[u32] and friends, empty when unset */
    string8 callconv;
} DeclAttributes;
static DeclAttributes parse_decl_attributes(Parser *p, bool is_enum);

/* True when a type's text ends in a return type: `proc(...)->T` and pointers
   to it. A trailing attribute there reads as though it belonged to the return
   type, so it is rejected rather than silently bound to the declaration. */
static bool type_text_ends_in_return_type(TypeExpr *type) {
    for (i32 depth = 0; type && depth < 32; depth++) {
        if (type->kind == Type_Proc) return true;
        if (type->kind != Type_Ptr && type->kind != Type_Array) return false;
        type = type->elem;
    }
    return false;
}

/* Reads the attribute list that may follow a variable's or field's type.
   `align(N)` is the only one that means anything on a value; the rest describe
   procs or records, and accepting them silently is how `[align(16)] i32` used
   to compile to a plainly unaligned `i32`. */
static string8 parse_trailing_decl_attributes(Parser *p, TypeExpr *type) {
    if (parser_peek(p)->kind != Token_LBracket) return (string8){0};
    Token *bracket = parser_peek(p);
    if (type_text_ends_in_return_type(type)) {
        parser_error_token(p, bracket,
                           "an attribute cannot follow a return type; it would read as though "
                           "it belonged to the return type rather than to the declaration");
        return (string8){0};
    }
    DeclAttributes attrs = parse_decl_attributes(p, false);
    if (attrs.is_external || attrs.emit_external_proto) {
        parser_error_token(p, bracket,
                           "'external' does not apply to a variable; a global with no "
                           "initializer is the one C owns");
    }
    if (attrs.packed || attrs.no_layout_check || attrs.callconv.data) {
        parser_error_token(p, bracket,
                           "that attribute describes a record or a proc, not a value; "
                           "'align(N)' is the only one that applies here");
    }
    return attrs.align;
}

static TypeExpr *parse_type(Parser *p) {
    if (parser_match(p, Token_Keyword_Const)) {
        Token *qualifier = parser_prev(p);
        TypeExpr *inner = parse_type(p);
        if (inner && inner->kind == Type_Array) {
            parser_error_token(p, qualifier,
                               "'const' cannot qualify an array; C qualifies the element type "
                               "rather than the array, so write '[N]const T'");
        }
        inner->is_const = true;
        return inner;
    }

    if (parser_match(p, Token_Keyword_Volatile)) {
        Token *qualifier = parser_prev(p);
        TypeExpr *inner = parse_type(p);
        if (inner && inner->kind == Type_Array) {
            parser_error_token(p, qualifier,
                               "'volatile' cannot qualify an array; C qualifies the element type "
                               "rather than the array, so write '[N]volatile T'");
        }
        inner->is_volatile = true;
        return inner;
    }

    if (parser_match(p, Token_Keyword_Proc)) {
        TypeExpr *t = type_new(p->arena, Type_Proc);
        t->args = ptr_array_reserve(p->arena, 8);
        t->arg_names = Vec_string8_reserve(p->arena, 8);

        /* A proc *type* takes the same attribute slot as a proc declaration, so
           `*proc[callconv(WINCALL)](...)` parses the same way the declaration
           does. Only the calling convention is meaningful on a type -- a type
           is never external, and has nothing to lay out. */
        if (parser_peek(p)->kind == Token_LBracket) {
            DeclAttributes type_attrs = parse_decl_attributes(p, false);
            if (type_attrs.callconv.data) t->name = type_attrs.callconv;
        }

        parser_expect(p, Token_LParen, "expected '(' after proc type");
        if (!parser_match(p, Token_RParen)) {
            do {
                if (parser_match(p, Token_Ellipsis)) {
                    t->is_variadic = true;
                    break;
                }

                string8 param_name = {0};
                if (parser_peek(p)->kind == Token_Identifier &&
                    parser_peek_n(p, 1)->kind == Token_Colon) {
                    Token *param_tok = parser_next(p);
                    param_name = token_to_string8(p->arena, param_tok);
                    parser_next(p); // ':'
                }

                TypeExpr *arg = parse_type(p);
                ptr_array_append(p->arena, &t->args, arg);
                Vec_string8_append(p->arena, &t->arg_names, param_name);
            } while (parser_match(p, Token_Comma));
            parser_expect(p, Token_RParen, "expected ')' after proc type params");
        }

        parser_expect(p, Token_Arrow, "expected '->' after proc type params");
        t->ret_type = parse_type(p);
        return t;
    }

    if (parser_match(p, Token_LBracket)) {
        TypeExpr *array = type_new(p->arena, Type_Array);
        bool symbolic = false;
        if (parser_peek(p)->kind == Token_Identifier) {
            /* A symbolic count: an enum member (`Enum.Member`) or a bare C
               constant. Kept as written and resolved once all enums are known. */
            Token *first = parser_next(p);
            array->line = first->line;
            array->col = first->col;
            array->array_count = token_to_string8(p->arena, first);
            /* `Type<>.count` sizes an array by how many members its enum
               declares, so a table indexed by an enum cannot fall out of step
               with it. Reflection is a runtime value, but the count is known at
               compile time, so it resolves to a literal below. */
            if (parser_peek(p)->kind == Token_LAngle && parser_peek_n(p, 1)->kind == Token_RAngle) {
                parser_next(p);
                parser_next(p);
                parser_expect(p, Token_Dot, "expected '.' after '<>' in array count");
                Token *member = parser_expect(p, Token_Identifier, "expected reflection member after '<>.' in array count");
                if (member) {
                    array->array_count = concat_name2(p->arena, array->array_count, "<>.",
                                                      token_to_string8(p->arena, member));
                }
            } else if (parser_match(p, Token_Dot)) {
                Token *member = parser_expect(p, Token_Identifier, "expected enum member after '.' in array count");
                if (member) {
                    array->array_count = concat_name2(p->arena, array->array_count, ".",
                                                      token_to_string8(p->arena, member));
                }
            }
            symbolic = true;
        } else {
            Token *count_tok = parser_expect(p, Token_Number, "expected array count");
            array->array_count = token_to_string8(p->arena, count_tok);
            /* `[0]T` is a GNU extension, not ISO C, and clang takes it without a
               word. Nothing in the tree uses one, and the C trick it exists for
               -- a flexible array member -- has its own spelling. */
            if (string8_equals_cstr(&array->array_count, "0")) {
                parser_error_token(p, count_tok, "array length must be greater than zero");
            }
        }
        parser_expect(p, Token_RBracket, "expected ']' after array count");
        array->elem = parse_type(p);
        if (symbolic && p->pending_array_counts) {
            ptr_array_append(p->arena, p->pending_array_counts, array);
        }
        return array;
    }

    if (parser_match(p, Token_Star)) {
        TypeExpr *inner = parse_type(p);
        TypeExpr *ptr = type_new(p->arena, Type_Ptr);
        ptr->elem = inner;
        return ptr;
    }

    Token *name_tok = parser_expect(p, Token_Identifier, "expected type name");
    string8 name = token_to_string8(p->arena, name_tok);

    if (parser_match(p, Token_LAngle)) {
        Vec_voidptr args = ptr_array_reserve(p->arena, 4);
        do {
            TypeExpr *arg = parse_type(p);
            ptr_array_append(p->arena, &args, arg);
        } while (parser_match(p, Token_Comma));
        parser_expect_generic_close(p);

        TypeExpr *t = type_new(p->arena, Type_Generic);
        t->name = name;
        t->line = name_tok->line;
        t->col = name_tok->col;
        t->args = args;
        return t;
    }

    TypeExpr *t = type_new(p->arena, Type_Name);
    /* One type, two spellings: normalising here rather than adding an
       equivalence rule means nothing downstream has to know about both, and
       `*const char` and `*const c8` cannot become incompatible types. */
    t->name = string8_equals_cstr(&name, "char") ? string8_from_cstr(p->arena, "c8") : name;
    t->line = name_tok->line;
    t->col = name_tok->col;
    return t;
}

static string8 concat_name2(memops_arena *arena, string8 a, const char *sep, string8 b) {
    u64 sep_len = (u64)strlen(sep);
    string8 out = string8_reserve(arena, a.length + sep_len + b.length);
    string8_append_bytes(arena, &out, a.data, a.length);
    string8_append_cstr(arena, &out, sep);
    string8_append_bytes(arena, &out, b.data, b.length);
    return out;
}

static void emit_cstr(memops_arena *arena, string8 *out, const char *cstr);
static void emit_string8(memops_arena *arena, string8 *out, string8 s);

static bool split_qualified_name(string8 name, string8 *owner, string8 *member) {
    for (u64 i = 0; i < name.length; i++) {
        if (name.data[i] == '@') {
            if (owner) {
                owner->data = name.data;
                owner->length = i;
                owner->capacity = i;
            }
            if (member) {
                member->data = name.data + i + 1;
                member->length = name.length - i - 1;
                member->capacity = member->length;
            }
            return true;
        }
    }
    return false;
}

static string8 mono_proc_name_from_mangle(memops_arena *arena, string8 base_name, string8 type_mangled) {
    string8 out = string8_reserve(arena, base_name.length + type_mangled.length + 2);
    string8 owner = {0};
    string8 member = {0};
    if (split_qualified_name(base_name, &owner, &member)) {
        string8_append_bytes(arena, &out, owner.data, owner.length);
        string8_append_cstr(arena, &out, "_");
        string8_append_bytes(arena, &out, type_mangled.data, type_mangled.length);
        string8_append_cstr(arena, &out, "_");
        string8_append_bytes(arena, &out, member.data, member.length);
    } else {
        string8_append_bytes(arena, &out, base_name.data, base_name.length);
        string8_append_cstr(arena, &out, "_");
        string8_append_bytes(arena, &out, type_mangled.data, type_mangled.length);
    }
    return out;
}

static void emit_mono_proc_name(memops_arena *arena, string8 *out, string8 base_name, string8 type_mangled) {
    string8 name = mono_proc_name_from_mangle(arena, base_name, type_mangled);
    emit_string8(arena, out, name);
}

static string8 parse_decl_name(Parser *p) {
    Token *base_tok = parser_expect(p, Token_Identifier, "expected identifier");
    string8 base = token_to_string8(p->arena, base_tok);

    if (!parser_match(p, Token_LAngle)) {
        return base;
    }

    // Qualified generic name sugar in declarations:
    // array<T>reserve:proc<T>(...)  -> canonical name: array_reserve
    parse_type(p);
    while (parser_match(p, Token_Comma)) {
        parse_type(p);
    }
    parser_expect_generic_close(p);

    Token *tail_tok = parser_expect(p, Token_Identifier, "expected identifier after generic qualifier");
    string8 tail = token_to_string8(p->arena, tail_tok);
    return concat_name2(p->arena, base, "@", tail);
}

static Expr *expr_new(memops_arena *arena, ExprKind kind) {
    Expr *e = memops_arena_push_struct(arena, Expr);
    memset(e, 0, sizeof(Expr));
    e->kind = kind;
    return e;
}

static Expr *expr_number_zero(memops_arena *arena, i32 line, i32 col) {
    Expr *e = expr_new(arena, Expr_Number);
    e->number = string8_from_cstr(arena, "0");
    e->line = line;
    e->col = col;
    return e;
}

static Expr *parse_expr(Parser *p);
static Expr *parse_unary(Parser *p);
static Expr *parse_multiplicative(Parser *p);
static Expr *parse_additive(Parser *p);
static Expr *parse_relational(Parser *p);
static Expr *parse_equality(Parser *p);
static Expr *parse_bitwise_or(Parser *p);
static Expr *parse_postfix(Parser *p, Expr *base);

static Expr *parse_initializer_list_after_lbrace(Parser *p, Token *lb) {
    if (parser_match(p, Token_RBrace)) {
        Expr *e = expr_new(p->arena, Expr_ZeroInit);
        e->line = lb->line;
        e->col = lb->col;
        return e;
    }

    Expr *e = expr_new(p->arena, Expr_InitList);
    e->args = ptr_array_reserve(p->arena, 8);
    e->designators = ptr_array_reserve(p->arena, 8);
    e->designator_kinds = Vec_i32_reserve(p->arena, 8);
    e->arg_directives = ptr_array_reserve(p->arena, 8);
    e->line = lb->line;
    e->col = lb->col;
    Vec_string8 *pending = null;
    do {
        Expr *designator = null;
        InitDesignatorKind designator_kind = InitDesignator_None;
        while (parser_peek(p)->kind == Token_Directive) {
            Token *d = parser_next(p);
            if (!pending) {
                pending = memops_arena_push_struct(p->arena, Vec_string8);
                *pending = Vec_string8_reserve(p->arena, 2);
            }
            Vec_string8_append(p->arena, pending, token_to_string8(p->arena, d));
        }
        if (parser_peek(p)->kind == Token_RBrace) break; // trailing comma, or only directives left
        if (parser_match(p, Token_LBracket)) {
            designator = parse_expr(p);
            designator_kind = InitDesignator_Index;
            parser_expect(p, Token_RBracket, "expected ']' after initializer designator");
            parser_expect(p, Token_Equal, "expected '=' after initializer designator");
        } else if (parser_match(p, Token_Dot)) {
            Token *name = parser_expect(p, Token_Identifier, "expected field name after initializer designator '.'");
            designator = expr_new(p->arena, Expr_Name);
            designator->name = token_to_string8(p->arena, name);
            designator->line = name->line;
            designator->col = name->col;
            designator_kind = InitDesignator_Field;
            parser_expect(p, Token_Equal, "expected '=' after initializer designator");
        }
        Expr *value = parse_expr(p);
        ptr_array_append(p->arena, &e->args, value);
        ptr_array_append(p->arena, &e->designators, designator);
        ptr_array_append(p->arena, &e->arg_directives, pending);
        pending = null;
        Vec_i32_append(p->arena, &e->designator_kinds, designator_kind);
    } while (parser_match(p, Token_Comma));
    /* Anything still pending closes after the final element. */
    ptr_array_append(p->arena, &e->arg_directives, pending);
    parser_expect(p, Token_RBrace, "expected '}' after initializer list");
    return e;
}

static TypeExpr *type_from_name_and_args(memops_arena *arena, string8 name, Token *name_tok, Vec_voidptr args) {
    TypeExpr *t = type_new(arena, args.length > 0 ? Type_Generic : Type_Name);
    t->name = name;
    t->line = name_tok->line;
    t->col = name_tok->col;
    t->args = args;
    return t;
}

static Expr *expr_compound_init(memops_arena *arena, TypeExpr *type, Expr *init, Token *name_tok) {
    Expr *e = expr_new(arena, Expr_CompoundInit);
    e->cast_type = type;
    e->inner = init;
    e->line = name_tok->line;
    e->col = name_tok->col;
    return e;
}

static Expr *parse_primary(Parser *p) {
    if (parser_match(p, Token_LBrace)) {
        Token *lb = parser_prev(p);
        return parse_initializer_list_after_lbrace(p, lb);
    }

    if (parser_peek(p)->kind == Token_Keyword_True || parser_peek(p)->kind == Token_Keyword_False) {
        Token *t = parser_next(p);
        Expr *e = expr_new(p->arena, Expr_Number);
        e->number = string8_from_cstr(p->arena, t->kind == Token_Keyword_True ? "1" : "0");
        e->line = t->line;
        e->col = t->col;
        return parse_postfix(p, e);
    }

    if (parser_match(p, Token_Number)) {
        Token *t = parser_prev(p);
        Expr *e = expr_new(p->arena, Expr_Number);
        e->number = token_to_string8(p->arena, t);
        e->line = t->line;
        e->col = t->col;
        return parse_postfix(p, e);
    }

    if (parser_match(p, Token_Char)) {
        Token *t = parser_prev(p);
        Expr *e = expr_new(p->arena, Expr_Char);
        e->string_lit = token_to_string8(p->arena, t);
        e->line = t->line;
        e->col = t->col;
        return parse_postfix(p, e);
    }

    if (parser_match(p, Token_String)) {
        Token *t = parser_prev(p);
        Expr *e = expr_new(p->arena, Expr_String);
        e->string_lit = token_to_string8(p->arena, t);
        while (parser_peek(p)->kind == Token_String) {
            parser_next(p);
            string8 next = token_to_string8(p->arena, parser_prev(p));
            if (e->string_lit.length >= 2 && next.length >= 2) {
                string8 joined = string8_reserve(p->arena, e->string_lit.length + next.length - 1);
                string8_append_bytes(p->arena, &joined, e->string_lit.data, e->string_lit.length - 1);
                string8_append_bytes(p->arena, &joined, next.data + 1, next.length - 1);
                e->string_lit = joined;
            }
        }
        e->line = t->line;
        e->col = t->col;
        return parse_postfix(p, e);
    }

    if (parser_match(p, Token_Identifier)) {
        Token *t = parser_prev(p);
        string8 name = token_to_string8(p->arena, t);
        string8 base_name = name;

        if (string8_equals_cstr(&name, "cast") && parser_match(p, Token_LParen)) {
            Expr *value = parse_expr(p);
            parser_expect(p, Token_Comma, "expected ',' in cast");
            TypeExpr *target = parse_type(p);
            parser_expect(p, Token_RParen, "expected ')' after cast");
            Expr *cast = expr_new(p->arena, Expr_Cast);
            cast->inner = value;
            cast->cast_type = target;
            cast->line = t->line;
            cast->col = t->col;
            return parse_postfix(p, cast);
        }

        if ((string8_equals_cstr(&name, "sizeof") || string8_equals_cstr(&name, "alignof")) &&
            parser_match(p, Token_LParen)) {
            bool type_form = parser_paren_operand_looks_like_type(p);
            if (type_form) {
                TypeExpr *target = parse_type(p);
                parser_expect(p, Token_RParen, "expected ')' after type");
                Expr *e = expr_new(
                    p->arena,
                    string8_equals_cstr(&name, "sizeof") ? Expr_SizeofType : Expr_AlignofType
                );
                e->cast_type = target;
                e->line = t->line;
                e->col = t->col;
                return parse_postfix(p, e);
            }

            Expr *operand = parse_expr(p);
            if (parser_match(p, Token_Comma)) {
                parser_error_token(
                    p, parser_peek(p),
                    string8_equals_cstr(&name, "sizeof")
                        ? "sizeof takes exactly one operand"
                        : "alignof takes exactly one operand");
                /* Swallow the surplus operands so the closing paren is still
                   found. Reporting and stopping here left the `)` unmatched and
                   turned one mistake into three cascading parse errors. */
                do { parse_expr(p); } while (parser_match(p, Token_Comma));
            }
            parser_expect(p, Token_RParen, "expected ')'");
            Expr *e = expr_new(
                p->arena,
                string8_equals_cstr(&name, "sizeof") ? Expr_SizeofType : Expr_AlignofType
            );
            e->inner = operand;
            e->line = t->line;
            e->col = t->col;
            return parse_postfix(p, e);
        }

        if (false) {
            Vec_voidptr args = ptr_array_reserve(p->arena, 2);
            Expr *call = expr_new(p->arena, Expr_Call);
            call->name = name;
            call->args = args;
            call->line = t->line;
            call->col = t->col;
            return parse_postfix(p, call);
        }

        if (parser_next_is_generic_compound_init(p) && parser_match(p, Token_LAngle)) {
            Vec_voidptr type_args = ptr_array_reserve(p->arena, 2);
            do {
                TypeExpr *arg = parse_type(p);
                ptr_array_append(p->arena, &type_args, arg);
            } while (parser_match(p, Token_Comma));
            parser_expect_generic_close(p);

            Token *lb = parser_expect(p, Token_LBrace, "expected '{' after type args");
            Expr *init = parse_initializer_list_after_lbrace(p, lb);
            TypeExpr *type = type_from_name_and_args(p->arena, name, t, type_args);
            return parse_postfix(p, expr_compound_init(p->arena, type, init, t));
        }

        if (parser_match(p, Token_LBrace)) {
            Token *lb = parser_prev(p);
            Expr *init = parse_initializer_list_after_lbrace(p, lb);
            TypeExpr *type = type_from_name_and_args(p->arena, name, t, (Vec_voidptr){0});
            return parse_postfix(p, expr_compound_init(p->arena, type, init, t));
        }

        if ((parser_next_is_generic_call(p) || parser_next_is_generic_qualified_call(p)) &&
            parser_match(p, Token_LAngle)) {
            Vec_voidptr type_args = ptr_array_reserve(p->arena, 2);
            do {
                TypeExpr *arg = parse_type(p);
                ptr_array_append(p->arena, &type_args, arg);
            } while (parser_match(p, Token_Comma));
            parser_expect_generic_close(p);

            // Qualified generic call sugar:
            // array<T>reserve(...) -> array_reserve<T>(...)
            if (parser_peek(p)->kind == Token_Identifier && parser_peek_n(p, 1)->kind == Token_LParen) {
                Token *tail_tok = parser_next(p);
                string8 tail = token_to_string8(p->arena, tail_tok);
                name = concat_name2(p->arena, base_name, "@", tail);
            }

            parser_expect(p, Token_LParen, "expected '(' after type args");
            Vec_voidptr args = ptr_array_reserve(p->arena, 4);
            if (!parser_match(p, Token_RParen)) {
                do {
                    Expr *arg = parse_expr(p);
                    ptr_array_append(p->arena, &args, arg);
                } while (parser_match(p, Token_Comma));
                parser_expect(p, Token_RParen, "expected ')'");
            }

            Expr *call = expr_new(p->arena, Expr_Call);
            call->name = name;
            call->args = args;
            call->type_args = type_args;
            call->line = t->line;
            call->col = t->col;
            return parse_postfix(p, call);
        }

        if (parser_peek(p)->kind == Token_LAngle && parser_peek_n(p, 1)->kind == Token_RAngle) {
            parser_next(p);
            parser_next(p);
            Expr *e = expr_new(p->arena, Expr_Name);
            e->name = concat_name2(p->arena, name, "_", string8_from_cstr(p->arena, "reflect"));
            e->reflect_base = name;
            e->line = t->line;
            e->col = t->col;
            return parse_postfix(p, e);
        }

        if (parser_match(p, Token_LParen)) {
            Vec_voidptr args = ptr_array_reserve(p->arena, 4);
            if (!parser_match(p, Token_RParen)) {
                do {
                    Expr *arg = parse_expr(p);
                    ptr_array_append(p->arena, &args, arg);
                } while (parser_match(p, Token_Comma));
                parser_expect(p, Token_RParen, "expected ')'");
            }
            Expr *call = expr_new(p->arena, Expr_Call);
            call->name = name;
            call->args = args;
            call->line = t->line;
            call->col = t->col;
            return parse_postfix(p, call);
        }

        Expr *e = expr_new(p->arena, Expr_Name);
        e->name = name;
        e->line = t->line;
        e->col = t->col;
        return parse_postfix(p, e);
    }

    if (parser_match(p, Token_LParen)) {
        Expr *inner = parse_expr(p);
        parser_expect(p, Token_RParen, "expected ')'");
        return parse_postfix(p, inner);
    }

    Token *t = parser_peek(p);
    parser_error_token(p, t, "expected expression");
    return null;
}

static Expr *parse_postfix(Parser *p, Expr *base) {
    Expr *result = base;
    for (;;) {
        if (parser_match(p, Token_LBracket)) {
            Token *lb = parser_prev(p);
            Expr *index = parse_expr(p);
            parser_expect(p, Token_RBracket, "expected ']'");
            Expr *idx = expr_new(p->arena, Expr_Index);
            idx->base = result;
            idx->index_expr = index;
            idx->line = lb->line;
            idx->col = lb->col;
            result = idx;
            continue;
        }
        if (parser_match(p, Token_Dot)) {
            Token *dot = parser_prev(p);
            if (parser_match(p, Token_Star)) {
                Expr *idx = expr_new(p->arena, Expr_Index);
                idx->base = result;
                idx->index_expr = expr_number_zero(p->arena, dot->line, dot->col);
                idx->line = dot->line;
                idx->col = dot->col;
                result = idx;
                continue;
            }
            if (parser_match(p, Token_Ampersand)) {
                Expr *addr = expr_new(p->arena, Expr_Addr);
                addr->inner = result;
                addr->line = dot->line;
                addr->col = dot->col;
                result = addr;
                continue;
            }
            Token *field_tok = parser_expect(p, Token_Identifier, "expected field name after '.'");
            Expr *field = expr_new(p->arena, Expr_Field);
            field->base = result;
            field->name = token_to_string8(p->arena, field_tok);
            field->line = dot->line;
            field->col = dot->col;
            result = field;
            continue;
        }
        /* Calling something that is not a plain name -- a proc pointer held in a
           field or an array of them. A direct call keeps its name; here the
           callee is the expression in `base`, which is what marks it indirect. */
        if (parser_match(p, Token_LParen)) {
            Token *lp = parser_prev(p);
            Vec_voidptr args = ptr_array_reserve(p->arena, 2);
            if (!parser_match(p, Token_RParen)) {
                do {
                    Expr *arg = parse_expr(p);
                    ptr_array_append(p->arena, &args, arg);
                } while (parser_match(p, Token_Comma));
                parser_expect(p, Token_RParen, "expected ')'");
            }
            Expr *call = expr_new(p->arena, Expr_Call);
            call->base = result;
            call->args = args;
            call->line = lp->line;
            call->col = lp->col;
            result = call;
            continue;
        }
        break;
    }
    return result;
}

static Expr *parse_unary(Parser *p) {
    if (parser_match(p, Token_Ampersand)) {
        Token *op_tok = parser_prev(p);
        Expr *e = expr_new(p->arena, Expr_Addr);
        e->inner = parse_unary(p);
        e->line = op_tok->line;
        e->col = op_tok->col;
        return e;
    }
    if (parser_match(p, Token_Bang)) {
        Token *op_tok = parser_prev(p);
        Expr *e = expr_new(p->arena, Expr_Unary);
        e->op = Token_Bang;
        e->inner = parse_unary(p);
        e->line = op_tok->line;
        e->col = op_tok->col;
        return e;
    }
    if (parser_match(p, Token_Minus)) {
        Token *op_tok = parser_prev(p);
        Expr *e = expr_new(p->arena, Expr_Unary);
        e->op = Token_Minus;
        e->inner = parse_unary(p);
        e->line = op_tok->line;
        e->col = op_tok->col;
        return e;
    }
    if (parser_match(p, Token_Tilde)) {
        Token *op_tok = parser_prev(p);
        Expr *e = expr_new(p->arena, Expr_Unary);
        e->op = Token_Tilde;
        e->inner = parse_unary(p);
        e->line = op_tok->line;
        e->col = op_tok->col;
        return e;
    }
    return parse_primary(p);
}

static Expr *parse_multiplicative(Parser *p) {
    Expr *left = parse_unary(p);
    while (parser_peek(p)->kind == Token_Star ||
           parser_peek(p)->kind == Token_Slash ||
           parser_peek(p)->kind == Token_Percent) {
        Token *op_tok = parser_peek(p);
        TokenKind op = op_tok->kind;
        parser_next(p);
        Expr *right = parse_unary(p);
        Expr *bin = expr_new(p->arena, Expr_Binary);
        bin->left = left;
        bin->right = right;
        bin->op = op;
        bin->line = op_tok->line;
        bin->col = op_tok->col;
        left = bin;
    }
    return left;
}

static Expr *parse_additive(Parser *p) {
    Expr *left = parse_multiplicative(p);
    while (parser_peek(p)->kind == Token_Plus || parser_peek(p)->kind == Token_Minus) {
        Token *op_tok = parser_peek(p);
        TokenKind op = op_tok->kind;
        parser_next(p);
        Expr *right = parse_multiplicative(p);
        Expr *bin = expr_new(p->arena, Expr_Binary);
        bin->left = left;
        bin->right = right;
        bin->op = op;
        bin->line = op_tok->line;
        bin->col = op_tok->col;
        left = bin;
    }
    return left;
}

static Expr *parse_shift(Parser *p) {
    Expr *left = parse_additive(p);
    while (parser_peek(p)->kind == Token_Keyword_Shl || parser_peek(p)->kind == Token_Keyword_Shr) {
        Token *op_tok = parser_peek(p);
        TokenKind op = op_tok->kind;
        parser_next(p);
        Expr *right = parse_additive(p);
        Expr *bin = expr_new(p->arena, Expr_Binary);
        bin->left = left;
        bin->right = right;
        bin->op = op;
        bin->line = op_tok->line;
        bin->col = op_tok->col;
        left = bin;
    }
    return left;
}

static Expr *parse_relational(Parser *p) {
    Expr *left = parse_bitwise_or(p);
    while (parser_peek(p)->kind == Token_LAngle ||
           parser_peek(p)->kind == Token_RAngle ||
           parser_peek(p)->kind == Token_LessEqual ||
           parser_peek(p)->kind == Token_GreaterEqual) {
        Token *op_tok = parser_peek(p);
        TokenKind op = op_tok->kind;
        parser_next(p);
        Expr *right = parse_bitwise_or(p);
        Expr *bin = expr_new(p->arena, Expr_Binary);
        bin->left = left;
        bin->right = right;
        bin->op = op;
        bin->line = op_tok->line;
        bin->col = op_tok->col;
        left = bin;
    }
    return left;
}

static Expr *parse_equality(Parser *p) {
    Expr *left = parse_relational(p);
    while (parser_peek(p)->kind == Token_EqualEqual || parser_peek(p)->kind == Token_BangEqual) {
        Token *op_tok = parser_peek(p);
        TokenKind op = op_tok->kind;
        parser_next(p);
        Expr *right = parse_relational(p);
        Expr *bin = expr_new(p->arena, Expr_Binary);
        bin->left = left;
        bin->right = right;
        bin->op = op;
        bin->line = op_tok->line;
        bin->col = op_tok->col;
        left = bin;
    }
    return left;
}

static Expr *parse_bitwise_and(Parser *p) {
    Expr *left = parse_shift(p);
    while (parser_peek(p)->kind == Token_Ampersand) {
        Token *op_tok = parser_peek(p);
        parser_next(p);
        Expr *right = parse_shift(p);
        Expr *bin = expr_new(p->arena, Expr_Binary);
        bin->left = left;
        bin->right = right;
        bin->op = Token_Ampersand;
        bin->line = op_tok->line;
        bin->col = op_tok->col;
        left = bin;
    }
    return left;
}

static Expr *parse_bitwise_xor(Parser *p) {
    Expr *left = parse_bitwise_and(p);
    while (parser_peek(p)->kind == Token_Caret) {
        Token *op_tok = parser_peek(p);
        parser_next(p);
        Expr *right = parse_bitwise_and(p);
        Expr *bin = expr_new(p->arena, Expr_Binary);
        bin->left = left;
        bin->right = right;
        bin->op = Token_Caret;
        bin->line = op_tok->line;
        bin->col = op_tok->col;
        left = bin;
    }
    return left;
}

static Expr *parse_bitwise_or(Parser *p) {
    Expr *left = parse_bitwise_xor(p);
    while (parser_peek(p)->kind == Token_Pipe) {
        Token *op_tok = parser_peek(p);
        parser_next(p);
        Expr *right = parse_bitwise_xor(p);
        Expr *bin = expr_new(p->arena, Expr_Binary);
        bin->left = left;
        bin->right = right;
        bin->op = Token_Pipe;
        bin->line = op_tok->line;
        bin->col = op_tok->col;
        left = bin;
    }
    return left;
}

static Expr *parse_logical_and(Parser *p) {
    Expr *left = parse_equality(p);
    while (parser_peek(p)->kind == Token_Keyword_And) {
        Token *op_tok = parser_peek(p);
        parser_next(p);
        Expr *right = parse_equality(p);
        Expr *bin = expr_new(p->arena, Expr_Binary);
        bin->left = left;
        bin->right = right;
        bin->op = Token_Keyword_And;
        bin->line = op_tok->line;
        bin->col = op_tok->col;
        left = bin;
    }
    return left;
}

static Expr *parse_logical_or(Parser *p) {
    Expr *left = parse_logical_and(p);
    while (parser_peek(p)->kind == Token_Keyword_Or) {
        Token *op_tok = parser_peek(p);
        parser_next(p);
        Expr *right = parse_logical_and(p);
        Expr *bin = expr_new(p->arena, Expr_Binary);
        bin->left = left;
        bin->right = right;
        bin->op = Token_Keyword_Or;
        bin->line = op_tok->line;
        bin->col = op_tok->col;
        left = bin;
    }
    return left;
}

static Expr *parse_ternary(Parser *p) {
    Expr *cond = parse_logical_or(p);
    if (!parser_match(p, Token_Question)) {
        return cond;
    }

    Token *q = parser_prev(p);
    Expr *then_expr = parse_expr(p);
    parser_expect(p, Token_Colon, "expected ':' in ternary expression");
    Expr *else_expr = parse_ternary(p);

    Expr *e = expr_new(p->arena, Expr_Ternary);
    e->left = cond;
    e->right = then_expr;
    e->third = else_expr;
    e->line = q->line;
    e->col = q->col;
    return e;
}

static Expr *parse_expr(Parser *p) {
    return parse_ternary(p);
}

static Stmt *stmt_new(memops_arena *arena, StmtKind kind) {
    Stmt *s = memops_arena_push_struct(arena, Stmt);
    memset(s, 0, sizeof(Stmt));
    s->kind = kind;
    s->source_path = g_source_path;
    s->assign_op = Token_Equal;
    return s;
}

static bool is_assign_op_kind(TokenKind kind) {
    return kind == Token_Equal ||
           kind == Token_PlusEqual ||
           kind == Token_MinusEqual ||
           kind == Token_StarEqual ||
           kind == Token_SlashEqual ||
           kind == Token_PercentEqual ||
           kind == Token_ShlEqual ||
           kind == Token_ShrEqual ||
           kind == Token_AmpersandEqual ||
           kind == Token_CaretEqual ||
           kind == Token_PipeEqual;
}

static TokenKind parser_match_assign_op(Parser *p) {
    if (parser_match(p, Token_Equal)) return Token_Equal;
    if (parser_match(p, Token_PlusEqual)) return Token_PlusEqual;
    if (parser_match(p, Token_MinusEqual)) return Token_MinusEqual;
    if (parser_match(p, Token_StarEqual)) return Token_StarEqual;
    if (parser_match(p, Token_SlashEqual)) return Token_SlashEqual;
    if (parser_match(p, Token_PercentEqual)) return Token_PercentEqual;
    if (parser_match(p, Token_ShlEqual)) return Token_ShlEqual;
    if (parser_match(p, Token_ShrEqual)) return Token_ShrEqual;
    if (parser_match(p, Token_AmpersandEqual)) return Token_AmpersandEqual;
    if (parser_match(p, Token_CaretEqual)) return Token_CaretEqual;
    if (parser_match(p, Token_PipeEqual)) return Token_PipeEqual;
    return Token_EOF;
}

static Stmt *parse_stmt(Parser *p);

/* Skips the remains of a statement that failed to parse. Stopping before '}' lets
   the enclosing body loop close the block normally. */
static void parser_sync_to_stmt_end(Parser *p) {
    while (parser_peek(p)->kind != Token_EOF) {
        TokenKind kind = parser_peek(p)->kind;
        if (kind == Token_Semicolon) {
            parser_next(p);
            return;
        }
        if (kind == Token_RBrace) return;
        parser_next(p);
    }
}

/* Every statement either consumes tokens or is resynchronized here, so a body loop
   can never spin on a statement the parser cannot make sense of. */
static Stmt *parse_stmt_recovering(Parser *p) {
    i32 errors_before = g_error_count;
    i32 index_before = p->index;
    Stmt *s = parse_stmt(p);
    if (p->index == index_before) parser_next(p);
    if (g_error_count > errors_before) parser_sync_to_stmt_end(p);
    return s;
}

/* Variables must say what they start as. '= {}' zeroes, and '= ?' opts out for the
   cases where zeroing a large buffer would be real work the C never asked for. */
static void parse_var_initializer(Parser *p, Stmt *s, Token *name_tok) {
    if (!parser_match(p, Token_Equal)) {
        parser_error_token(p, name_tok,
                           "variable declaration needs an initializer; use '= {}' to zero it "
                           "or '= ?' to leave it uninitialized");
        return;
    }
    if (parser_match(p, Token_Question)) {
        s->is_uninitialized = true;
        return;
    }
    s->expr = parse_expr(p);
}

static Stmt *parse_for_clause_stmt(Parser *p, bool allow_var_decl) {
    if (parser_peek(p)->kind == Token_Identifier &&
        allow_var_decl &&
        parser_peek_n(p, 1)->kind == Token_Colon) {
        Token *name_tok = parser_next(p);
        parser_next(p); // ':'
        Stmt *s = stmt_new(p->arena, Stmt_Var);
        s->name = token_to_string8(p->arena, name_tok);
        s->type = parse_type(p);
        s->align = parse_trailing_decl_attributes(p, s->type);
        s->line = name_tok->line;
        s->col = name_tok->col;
        parse_var_initializer(p, s, name_tok);
        return s;
    }

    if (parser_peek(p)->kind == Token_Identifier &&
        is_assign_op_kind(parser_peek_n(p, 1)->kind)) {
        Token *name_tok = parser_next(p);
        TokenKind op = parser_match_assign_op(p);
        Stmt *s = stmt_new(p->arena, Stmt_Assign);
        s->name = token_to_string8(p->arena, name_tok);
        Expr *lhs = expr_new(p->arena, Expr_Name);
        lhs->name = s->name;
        lhs->line = name_tok->line;
        lhs->col = name_tok->col;
        s->lhs = lhs;
        s->assign_op = op;
        s->expr = parse_expr(p);
        s->line = name_tok->line;
        s->col = name_tok->col;
        return s;
    }

    Stmt *s = stmt_new(p->arena, Stmt_Expr);
    s->expr = parse_expr(p);
    s->line = s->expr ? s->expr->line : parser_peek(p)->line;
    s->col = s->expr ? s->expr->col : parser_peek(p)->col;
    return s;
}

static Stmt *parse_stmt(Parser *p) {
    /* A preprocessor line inside a body is a statement in its own right so that
       it lands where it was written. Only in-body directives are lexed into
       tokens; file-scope ones are still hoisted to the top of the generated C. */
    if (parser_match(p, Token_Directive)) {
        Token *tok = parser_prev(p);
        Stmt *s = stmt_new(p->arena, Stmt_Directive);
        s->name = token_to_string8(p->arena, tok);
        s->line = tok->line;
        s->col = tok->col;
        return s;
    }
    if (parser_match(p, Token_Keyword_Do)) {
        Token *do_tok = parser_prev(p);
        Stmt *s = stmt_new(p->arena, Stmt_DoWhile);
        s->line = do_tok->line;
        s->col = do_tok->col;
        s->while_body = ptr_array_reserve(p->arena, 8);
        parser_expect(p, Token_LBrace, "expected '{' after do");
        while (!parser_at_block_end(p)) {
            ptr_array_append(p->arena, &s->while_body, parse_stmt_recovering(p));
        }
        parser_expect(p, Token_Keyword_While, "expected 'while' after do body");
        parser_expect(p, Token_LParen, "expected '(' after while");
        s->while_cond = parse_expr(p);
        parser_expect(p, Token_RParen, "expected ')' after do-while condition");
        parser_expect(p, Token_Semicolon, "expected ';' after do-while");
        return s;
    }

    if (parser_match(p, Token_Keyword_While)) {
        Token *while_tok = parser_prev(p);
        Stmt *s = stmt_new(p->arena, Stmt_While);
        s->line = while_tok->line;
        s->col = while_tok->col;
        s->while_body = ptr_array_reserve(p->arena, 8);

        parser_expect(p, Token_LParen, "expected '(' after while");
        s->while_cond = parse_expr(p);
        parser_expect(p, Token_RParen, "expected ')' after while condition");
        parser_expect(p, Token_LBrace, "expected '{' in while body");
        while (!parser_at_block_end(p)) {
            ptr_array_append(p->arena, &s->while_body, parse_stmt_recovering(p));
        }
        return s;
    }

    if (parser_match(p, Token_Keyword_Break)) {
        Token *break_tok = parser_prev(p);
        Stmt *s = stmt_new(p->arena, Stmt_Break);
        s->line = break_tok->line;
        s->col = break_tok->col;
        parser_expect(p, Token_Semicolon, "expected ';' after break");
        return s;
    }

    if (parser_match(p, Token_Keyword_Goto)) {
        Token *goto_tok = parser_prev(p);
        Token *target_tok = parser_expect(p, Token_Identifier, "expected label name after goto");
        Stmt *s = stmt_new(p->arena, Stmt_Goto);
        s->name = token_to_string8(p->arena, target_tok);
        s->line = goto_tok->line;
        s->col = goto_tok->col;
        parser_expect(p, Token_Semicolon, "expected ';' after goto");
        return s;
    }

    if (parser_match(p, Token_Keyword_Continue)) {
        Token *continue_tok = parser_prev(p);
        Stmt *s = stmt_new(p->arena, Stmt_Continue);
        s->line = continue_tok->line;
        s->col = continue_tok->col;
        parser_expect(p, Token_Semicolon, "expected ';' after continue");
        return s;
    }

    if (parser_match(p, Token_Keyword_Switch)) {
        Token *switch_tok = parser_prev(p);
        Stmt *s = stmt_new(p->arena, Stmt_Switch);
        s->line = switch_tok->line;
        s->col = switch_tok->col;
        s->switch_cases = ptr_array_reserve(p->arena, 8);
        s->switch_default_body = ptr_array_reserve(p->arena, 8);

        parser_expect(p, Token_LParen, "expected '(' after switch");
        s->switch_expr = parse_expr(p);
        parser_expect(p, Token_RParen, "expected ')' after switch expression");
        parser_expect(p, Token_LBrace, "expected '{' in switch body");
        while (!parser_at_block_end(p)) {
            if (parser_match(p, Token_Keyword_Case)) {
                Token *case_tok = parser_prev(p);
                SwitchCase *sc = memops_arena_push_struct(p->arena, SwitchCase);
                memset(sc, 0, sizeof(SwitchCase));
                sc->line = case_tok->line;
                sc->col = case_tok->col;
                sc->expr = parse_expr(p);
                sc->body = ptr_array_reserve(p->arena, 8);
                parser_expect(p, Token_Colon, "expected ':' after case");
                /* Cases take a block, so per-case locals get their own C scope
                   instead of colliding in the switch's single shared scope. */
                parser_expect(p, Token_LBrace, "expected '{' after case; a switch case takes a block");
                while (!parser_at_block_end(p)) {
                    ptr_array_append(p->arena, &sc->body, parse_stmt_recovering(p));
                }
                ptr_array_append(p->arena, &s->switch_cases, sc);
                continue;
            }
            if (parser_match(p, Token_Keyword_Default)) {
                s->has_switch_default = true;
                parser_expect(p, Token_Colon, "expected ':' after default");
                parser_expect(p, Token_LBrace, "expected '{' after default; a switch default takes a block");
                while (!parser_at_block_end(p)) {
                    ptr_array_append(p->arena, &s->switch_default_body, parse_stmt_recovering(p));
                }
                continue;
            }
            /* Report once, then skip to the next arm. Without this the loop would
               re-report the same token until the diagnostic cap stopped it. */
            Token *t = parser_peek(p);
            parser_error_token(p, t, "expected case/default in switch");
            while (parser_peek(p)->kind != Token_Keyword_Case &&
                   parser_peek(p)->kind != Token_Keyword_Default &&
                   parser_peek(p)->kind != Token_RBrace &&
                   parser_peek(p)->kind != Token_EOF) {
                parser_next(p);
            }
        }
        return s;
    }

    if (parser_match(p, Token_Keyword_If)) {
        Token *if_tok = parser_prev(p);
        Stmt *s = stmt_new(p->arena, Stmt_If);
        s->line = if_tok->line;
        s->col = if_tok->col;
        s->if_then_body = ptr_array_reserve(p->arena, 8);
        s->if_else_body = ptr_array_reserve(p->arena, 8);

        parser_expect(p, Token_LParen, "expected '(' after if");
        s->if_cond = parse_expr(p);
        parser_expect(p, Token_RParen, "expected ')' after if condition");
        parser_expect(p, Token_LBrace, "expected '{' in if body");
        while (!parser_at_block_end(p)) {
            Stmt *body_stmt = parse_stmt_recovering(p);
            ptr_array_append(p->arena, &s->if_then_body, body_stmt);
        }

        if (parser_match(p, Token_Keyword_Else)) {
            if (parser_peek(p)->kind == Token_Keyword_If) {
                s->if_else_if = parse_stmt(p);
            } else {
                parser_expect(p, Token_LBrace, "expected '{' in else body");
                while (!parser_at_block_end(p)) {
                    Stmt *else_stmt = parse_stmt_recovering(p);
                    ptr_array_append(p->arena, &s->if_else_body, else_stmt);
                }
            }
        }
        return s;
    }

    if (parser_match(p, Token_Keyword_For)) {
        Token *for_tok = parser_prev(p);
        Stmt *s = stmt_new(p->arena, Stmt_For);
        s->line = for_tok->line;
        s->col = for_tok->col;
        s->for_body = ptr_array_reserve(p->arena, 8);

        parser_expect(p, Token_LParen, "expected '(' after for");
        if (parser_peek(p)->kind != Token_Semicolon) {
            s->for_init = parse_for_clause_stmt(p, true);
        }
        parser_expect(p, Token_Semicolon, "expected ';' after for init");

        if (parser_peek(p)->kind != Token_Semicolon) {
            s->for_cond = parse_expr(p);
        }
        parser_expect(p, Token_Semicolon, "expected ';' after for condition");

        if (parser_peek(p)->kind != Token_RParen) {
            s->for_step = parse_for_clause_stmt(p, false);
        }
        parser_expect(p, Token_RParen, "expected ')' after for clauses");
        parser_expect(p, Token_LBrace, "expected '{' in for body");
        while (!parser_at_block_end(p)) {
            Stmt *body_stmt = parse_stmt_recovering(p);
            ptr_array_append(p->arena, &s->for_body, body_stmt);
        }
        return s;
    }

    if (parser_match(p, Token_Keyword_Ret)) {
        Token *ret_tok = parser_prev(p);
        Stmt *s = stmt_new(p->arena, Stmt_Return);
        if (parser_peek(p)->kind != Token_Semicolon) {
            s->expr = parse_expr(p);
        }
        s->line = ret_tok->line;
        s->col = ret_tok->col;
        parser_expect(p, Token_Semicolon, "expected ';' after return");
        return s;
    }

    if (parser_match(p, Token_Identifier)) {
        Token *name_tok = parser_prev(p);
        if (parser_match(p, Token_Colon)) {
            /* 'done: label = { ... }' declares a jump target and the block it
               labels, matching the 'name : kind = value' shape used by every
               other declaration, including switch cases. */
            if (parser_peek(p)->kind == Token_Identifier &&
                string8slice_equals_cstr(parser_peek(p)->text, "label")) {
                parser_next(p); // 'label'
                Stmt *s = stmt_new(p->arena, Stmt_Label);
                s->name = token_to_string8(p->arena, name_tok);
                s->line = name_tok->line;
                s->col = name_tok->col;
                s->while_body = ptr_array_reserve(p->arena, 8);
                parser_expect(p, Token_Equal, "expected '=' after label");
                parser_expect(p, Token_LBrace, "expected '{' after label; a label takes a block");
                while (!parser_at_block_end(p)) {
                    ptr_array_append(p->arena, &s->while_body, parse_stmt_recovering(p));
                }
                return s;
            }
            Stmt *s = stmt_new(p->arena, Stmt_Var);
            s->is_static = parser_match(p, Token_Keyword_Static);
            s->name = token_to_string8(p->arena, name_tok);
            s->type = parse_type(p);
            s->align = parse_trailing_decl_attributes(p, s->type);
            s->line = name_tok->line;
            s->col = name_tok->col;
            parse_var_initializer(p, s, name_tok);
            parser_expect(p, Token_Semicolon, "expected ';' after var decl");
            return s;
        }

        p->index--;
        Expr *lhs = parse_expr(p);
        TokenKind assign_op = parser_match_assign_op(p);
        if (assign_op != Token_EOF) {
            Stmt *s = stmt_new(p->arena, Stmt_Assign);
            s->lhs = lhs;
            if (lhs && lhs->kind == Expr_Name) {
                s->name = lhs->name;
            }
            s->assign_op = assign_op;
            s->expr = parse_expr(p);
            s->line = lhs ? lhs->line : name_tok->line;
            s->col = lhs ? lhs->col : name_tok->col;
            parser_expect(p, Token_Semicolon, "expected ';' after assignment");
            return s;
        }

        Stmt *s = stmt_new(p->arena, Stmt_Expr);
        s->expr = lhs;
        s->line = s->expr ? s->expr->line : name_tok->line;
        s->col = s->expr ? s->expr->col : name_tok->col;
        parser_expect(p, Token_Semicolon, "expected ';' after expression");
        return s;
    }

    Token *t = parser_peek(p);
    parser_error_token(p, t, "expected statement: local declaration, assignment, expression, if, for, while, do, switch, break, continue, or return");
    return null;
}


/* Attribute names are a closed set. Anything unrecognised is an error, not a
   fallback: while an unknown name was quietly taken as a calling convention,
   `struct[externl]` meant *not external* and the struct was emitted as a real
   definition -- a typo that silently changed what the declaration meant.

   That fallback existed because the slot originally held nothing but a calling
   convention. Now that attributes take arguments, a convention says so:
   `proc[callconv(WINAPI)]`. Nothing has to be guessed. */
static bool decl_attribute_is_integer_type(string8slice name) {
    return string8slice_equals_cstr(name, "i8") || string8slice_equals_cstr(name, "i16") ||
           string8slice_equals_cstr(name, "i32") || string8slice_equals_cstr(name, "i64") ||
           string8slice_equals_cstr(name, "u8") || string8slice_equals_cstr(name, "u16") ||
           string8slice_equals_cstr(name, "u32") || string8slice_equals_cstr(name, "u64");
}

static DeclAttributes parse_decl_attributes(Parser *p, bool is_enum) {
    DeclAttributes attrs = {0};
    if (!parser_match(p, Token_LBracket)) return attrs;
    do {
        Token *tok = parser_expect(p, Token_Identifier, "expected attribute name");
        if (!tok) break;

        if (string8slice_equals_cstr(tok->text, "external")) {
            attrs.is_external = true;
        } else if (string8slice_equals_cstr(tok->text, "external_emit")) {
            attrs.is_external = true;
            attrs.emit_external_proto = true;
        } else if (string8slice_equals_cstr(tok->text, "no_layout_check")) {
            /* The record has no C type of that name at all -- ibind synthesises
               one for a genuinely anonymous member, which exists only in ilang.
               Its layout cannot be compared against a type C cannot name. */
            attrs.no_layout_check = true;
        } else if (string8slice_equals_cstr(tok->text, "packed")) {
            attrs.packed = true;
        } else if (string8slice_equals_cstr(tok->text, "align")) {
            Token *n = null;
            parser_expect(p, Token_LParen, "expected '(' after align");
            n = parser_expect(p, Token_Number, "expected an alignment in align(N)");
            if (n) attrs.align = token_to_string8(p->arena, n);
            parser_expect(p, Token_RParen, "expected ')' after align(N)");
        } else if (string8slice_equals_cstr(tok->text, "callconv")) {
            Token *c = null;
            parser_expect(p, Token_LParen, "expected '(' after callconv");
            c = parser_expect(p, Token_Identifier, "expected a name in callconv(NAME)");
            if (c) attrs.callconv = token_to_string8(p->arena, c);
            parser_expect(p, Token_RParen, "expected ')' after callconv(NAME)");
        } else if (is_enum && decl_attribute_is_integer_type(tok->text)) {
            /* `enum[u32]`: the underlying type, which C otherwise leaves to the
               implementation. Only meaningful on an enum, so only accepted
               there -- on a struct the same spelling is a mistake. */
            attrs.underlying = token_to_string8(p->arena, tok);
        } else {
            parser_error_token(p, tok,
                "unknown attribute; expected one of external, external_emit, "
                "no_layout_check, packed, align(N), callconv(NAME)");
        }
    } while (parser_match(p, Token_Comma));
    parser_expect(p, Token_RBracket, "expected ']' after attributes");
    return attrs;
}

/* Reads fields up to the closing '}'. Shared by named structs and the anonymous
   struct/union members that C headers use, so both accept the same field forms. */
static void parse_struct_fields(Parser *p, StructDecl *decl) {
    Vec_string8 pending = Vec_string8_reserve(p->arena, 4);
    while (!parser_at_block_end(p)) {
        if (parser_peek(p)->kind == Token_Directive) {
            Token *d = parser_next(p);
            Vec_string8_append(p->arena, &pending, token_to_string8(p->arena, d));
            continue;
        }
        if (parser_peek(p)->kind == Token_Identifier &&
            string8slice_equals_cstr(parser_peek(p)->text, "external") &&
            parser_peek_n(p, 1)->kind == Token_Semicolon) {
            parser_error_token(p, parser_peek(p),
                               "'external;' in a body is no longer accepted; write the attribute instead, as in 'X: struct[external] = {}'");
        }

        /* 'union = { ... }' / 'struct = { ... }' with no name is an anonymous member. */
        TokenKind head = parser_peek(p)->kind;
        if ((head == Token_Keyword_Union || head == Token_Keyword_Struct) &&
            parser_peek_n(p, 1)->kind == Token_Equal) {
            Token *kind_tok = parser_next(p);
            parser_next(p); // '='
            parser_expect(p, Token_LBrace, "expected '{' in anonymous member");
            StructDecl *anon = memops_arena_push_struct(p->arena, StructDecl);
            memset(anon, 0, sizeof(StructDecl));
            anon->source_path = g_source_path;
            anon->is_union = (head == Token_Keyword_Union);
            anon->line = kind_tok->line;
            anon->col = kind_tok->col;
            anon->fields = ptr_array_reserve(p->arena, 8);
            parse_struct_fields(p, anon);
            parser_match(p, Token_Semicolon); // optional

            Field *f = memops_arena_push_struct(p->arena, Field);
            memset(f, 0, sizeof(Field));
            f->pre_directives = pending;
            pending = Vec_string8_reserve(p->arena, 4);
            f->anon = anon;
            f->line = kind_tok->line;
            f->col = kind_tok->col;
            ptr_array_append(p->arena, &decl->fields, f);
            continue;
        }

        Token *field_tok = parser_expect(p, Token_Identifier, "expected field name");
        parser_expect(p, Token_Colon, "expected ':' after field name");
        Field *f = memops_arena_push_struct(p->arena, Field);
        memset(f, 0, sizeof(Field));
        f->pre_directives = pending;
        pending = Vec_string8_reserve(p->arena, 4);
        f->name = token_to_string8(p->arena, field_tok);
        f->type = parse_type(p);
        f->align = parse_trailing_decl_attributes(p, f->type);
        /* 'flags: u32 : 4;' gives the field an explicit bit width. */
        if (parser_match(p, Token_Colon)) {
            Token *width_tok = parser_expect(p, Token_Number, "expected bitfield width after ':'");
            f->bit_width = token_to_string8(p->arena, width_tok);
        }
        if (parser_match(p, Token_At)) {
            Token *attr_tok = parser_expect(p, Token_String, "expected string literal after field attribute '@'");
            string8 attr_lit = token_to_string8(p->arena, attr_tok);
            f->attrs = string_lit_inner(p->arena, attr_lit);
        }
        f->line = field_tok->line;
        f->col = field_tok->col;
        ptr_array_append(p->arena, &decl->fields, f);
        parser_expect(p, Token_Semicolon, "expected ';' after field");
    }
    /* A '#endif' closing the last field has no following member to ride on. */
    decl->tail_directives = pending;
}

static StructDecl *parse_struct_decl(Parser *p, Token *name_tok, bool is_union) {
    StructDecl *decl = memops_arena_push_struct(p->arena, StructDecl);
    memset(decl, 0, sizeof(StructDecl));
    decl->name = token_to_string8(p->arena, name_tok);
    decl->source_path = g_source_path;
    decl->is_union = is_union;
    decl->line = name_tok->line;
    decl->col = name_tok->col;
    decl->fields = ptr_array_reserve(p->arena, 8);

    DeclAttributes attrs = parse_decl_attributes(p, false);
    if (attrs.is_external) decl->is_external = true;
    if (attrs.no_layout_check) decl->no_layout_check = true;
    decl->packed = attrs.packed;
    decl->align = attrs.align;

    if (parser_match(p, Token_LAngle)) {
        decl->type_params = Vec_string8_reserve(p->arena, 2);
        do {
            Token *param_tok = parser_expect(p, Token_Identifier, "expected type param");
            if (!param_tok) break;
            Vec_string8_append(p->arena, &decl->type_params, token_to_string8(p->arena, param_tok));
        } while (parser_match(p, Token_Comma));
        decl->is_generic = true;
        parser_expect_generic_close(p);
    }

    parser_expect(p, Token_Equal, is_union ? "expected '=' after union" : "expected '=' after struct");
    parser_expect(p, Token_LBrace, is_union ? "expected '{' in union" : "expected '{' in struct");
    if (parser_peek(p)->kind == Token_Identifier &&
        string8slice_equals_cstr(parser_peek(p)->text, "external") &&
        parser_peek_n(p, 1)->kind == Token_Semicolon) {
        parser_error_token(p, parser_peek(p),
                               "'external;' in a body is no longer accepted; write the attribute instead, as in 'X: struct[external] = {}'");
    }
    parse_struct_fields(p, decl);
    // optional ';' after struct decl
    parser_match(p, Token_Semicolon);
    return decl;
}

static AliasDecl *parse_alias_decl(Parser *p, Token *name_tok) {
    AliasDecl *decl = memops_arena_push_struct(p->arena, AliasDecl);
    memset(decl, 0, sizeof(AliasDecl));
    decl->name = token_to_string8(p->arena, name_tok);
    decl->source_path = g_source_path;
    decl->line = name_tok->line;
    decl->col = name_tok->col;
    parser_expect(p, Token_Equal, "expected '=' after alias");
    decl->type = parse_type(p);
    if (parser_peek(p)->kind == Token_LBracket) {
        parser_error_token(p, parser_peek(p),
                           "an alias takes no attribute; it names an existing type rather than "
                           "declaring one, so there is nothing for the attribute to modify");
    }
    parser_expect(p, Token_Semicolon, "expected ';' after alias");
    return decl;
}

/* Enum values may name sibling items ('B = A'). Those must be emitted with the
   generated C name ('Color_A'), so rewrite sibling references once every item
   is known, which also covers forward references. */
static void enum_qualify_sibling_refs(memops_arena *arena, EnumDecl *decl, Expr *e) {
    if (!e) return;
    if (e->kind == Expr_Name) {
        for (i32 i = 0; i < decl->items.length; i++) {
            EnumItem *sibling = (EnumItem *)decl->items.data[i];
            if (string8_equals(&e->name, &sibling->name)) {
                e->name = concat_name2(arena, decl->name, "_", sibling->name);
                return;
            }
        }
        return;
    }
    enum_qualify_sibling_refs(arena, decl, e->inner);
    enum_qualify_sibling_refs(arena, decl, e->left);
    enum_qualify_sibling_refs(arena, decl, e->right);
    enum_qualify_sibling_refs(arena, decl, e->third);
    enum_qualify_sibling_refs(arena, decl, e->base);
    enum_qualify_sibling_refs(arena, decl, e->index_expr);
    for (i32 i = 0; i < e->args.length; i++) {
        enum_qualify_sibling_refs(arena, decl, (Expr *)e->args.data[i]);
    }
}

static void enum_qualify_item_values(memops_arena *arena, EnumDecl *decl) {
    for (i32 i = 0; i < decl->items.length; i++) {
        EnumItem *item = (EnumItem *)decl->items.data[i];
        if (item->value_expr) {
            enum_qualify_sibling_refs(arena, decl, item->value_expr);
            continue;
        }
        if (!item->value.data) continue;
        for (i32 j = 0; j < decl->items.length; j++) {
            EnumItem *sibling = (EnumItem *)decl->items.data[j];
            if (string8_equals(&item->value, &sibling->name)) {
                item->value = concat_name2(arena, decl->name, "_", sibling->name);
                break;
            }
        }
    }
}

/* Tokens that can open an enum value, so a missing value reports 'expected enum
   value' at the enum instead of a generic 'expected expression' from the parser. */
static bool can_begin_enum_value(TokenKind kind) {
    switch (kind) {
        case Token_Number:
        case Token_Identifier:
        case Token_Char:
        case Token_Minus:
        case Token_Tilde:
        case Token_Bang:
        case Token_LParen:
            return true;
        default:
            return false;
    }
}

/* Tokens that mean an enum value keeps going past its first token, so
   'A = 1 shl 3' parses as one constant expression instead of a bare '1'. */
static bool is_enum_value_continuation(TokenKind kind) {
    switch (kind) {
        case Token_Plus:
        case Token_Minus:
        case Token_Star:
        case Token_Slash:
        case Token_Percent:
        case Token_Ampersand:
        case Token_Caret:
        case Token_Pipe:
        case Token_Question:
        case Token_Dot:
        case Token_LBracket:
        case Token_LAngle:
        case Token_RAngle:
        case Token_Keyword_Shl:
        case Token_Keyword_Shr:
            return true;
        default:
            return false;
    }
}

static EnumDecl *parse_enum_decl(Parser *p, Token *name_tok) {
    EnumDecl *decl = memops_arena_push_struct(p->arena, EnumDecl);
    memset(decl, 0, sizeof(EnumDecl));
    decl->name = token_to_string8(p->arena, name_tok);
    decl->source_path = g_source_path;
    decl->line = name_tok->line;
    decl->col = name_tok->col;
    decl->items = ptr_array_reserve(p->arena, 8);

    DeclAttributes enum_attrs = parse_decl_attributes(p, true);
    if (enum_attrs.is_external) decl->is_external = true;
    decl->underlying = enum_attrs.underlying;

    parser_expect(p, Token_Equal, "expected '=' after enum");
    parser_expect(p, Token_LBrace, "expected '{' in enum");
    Vec_string8 pending = Vec_string8_reserve(p->arena, 4);
    while (!parser_at_block_end(p)) {
        if (parser_peek(p)->kind == Token_Directive) {
            Token *d = parser_next(p);
            Vec_string8_append(p->arena, &pending, token_to_string8(p->arena, d));
            continue;
        }
        if (parser_peek(p)->kind == Token_Identifier &&
            string8slice_equals_cstr(parser_peek(p)->text, "external") &&
            parser_peek_n(p, 1)->kind == Token_Semicolon) {
            parser_error_token(p, parser_peek(p),
                               "'external;' in a body is no longer accepted; write the attribute instead, as in 'E: enum[external] = { A, }'");
        }
        Token *item_tok = parser_expect(p, Token_Identifier, "expected enum item name");
        EnumItem *item = memops_arena_push_struct(p->arena, EnumItem);
        memset(item, 0, sizeof(EnumItem));
        item->pre_directives = pending;
        pending = Vec_string8_reserve(p->arena, 4);
        item->name = token_to_string8(p->arena, item_tok);
        item->line = item_tok->line;
        item->col = item_tok->col;

        if (parser_match(p, Token_Equal)) {
            /* Enum values are constant expressions passed through to C, so a bare
               token stays a bare token and anything else is rendered at emit time. */
            Token *value_tok = parser_peek(p);
            if (!can_begin_enum_value(value_tok->kind)) {
                parser_error_token(p, value_tok, "expected enum value");
            }
            bool bare_token = (value_tok->kind == Token_Number || value_tok->kind == Token_Identifier) &&
                              parser_peek_n(p, 1)->kind != Token_LParen &&
                              !is_enum_value_continuation(parser_peek_n(p, 1)->kind);
            if (bare_token) {
                parser_next(p);
                item->value = token_to_string8(p->arena, value_tok);
            } else {
                item->value_expr = parse_ternary(p);
            }
        }

        ptr_array_append(p->arena, &decl->items, item);
        if (parser_match(p, Token_Comma) || parser_match(p, Token_Semicolon)) {
            continue;
        }
        parser_expect(p, Token_RBrace, "expected ',' or '}' after enum item");
        break;
    }
    parser_match(p, Token_Semicolon);
    /* A '#endif' closing the last item has no following member to ride on. */
    decl->tail_directives = pending;
    enum_qualify_item_values(p->arena, decl);
    return decl;
}

static ProcDecl *parse_proc_decl(Parser *p, Token *name_tok) {
    ProcDecl *decl = memops_arena_push_struct(p->arena, ProcDecl);
    memset(decl, 0, sizeof(ProcDecl));
    decl->name = token_to_string8(p->arena, name_tok);
    decl->source_path = g_source_path;
    decl->line = name_tok->line;
    decl->col = name_tok->col;
    decl->params = ptr_array_reserve(p->arena, 8);
    decl->body = ptr_array_reserve(p->arena, 8);

    DeclAttributes proc_attrs = parse_decl_attributes(p, false);
    if (proc_attrs.callconv.data) decl->callconv = proc_attrs.callconv;
    if (proc_attrs.is_external) {
        decl->is_external = true;
        decl->emit_external_proto = proc_attrs.emit_external_proto;
    }

    if (parser_match(p, Token_LAngle)) {
        Token *first = parser_peek(p);
        Token *second = parser_peek_n(p, 1);
        bool generic_param_form = first->kind == Token_Identifier && second->kind == Token_Colon;

        if (generic_param_form) {
            Token *param_tok = parser_expect(p, Token_Identifier, "expected type param");
            decl->type_params = Vec_string8_reserve(p->arena, 1);
            Vec_string8_append(p->arena, &decl->type_params, token_to_string8(p->arena, param_tok));
            decl->is_generic = true;
            if (parser_match(p, Token_Colon)) {
                Token *constraint_tok = parser_expect(p, Token_Identifier, "expected constraint");
                decl->constraint = token_to_string8(p->arena, constraint_tok);
            }
            parser_expect_generic_close(p);
        } else {
            TypeExpr *first_angle = parse_type(p);
            if (parser_peek(p)->kind == Token_Comma) {
                decl->angle_types = ptr_array_reserve(p->arena, 4);
                ptr_array_append(p->arena, &decl->angle_types, first_angle);
                while (parser_match(p, Token_Comma)) {
                    ptr_array_append(p->arena, &decl->angle_types, parse_type(p));
                }
            } else {
                decl->angle_type = first_angle;
            }
            parser_expect_generic_close(p);
        }
    }

    parser_expect(p, Token_LParen, "expected '(' after proc");
    if (!parser_match(p, Token_RParen)) {
        do {
            if (parser_match(p, Token_Ellipsis)) {
                decl->is_variadic = true;
                break;
            }
            Token *param_name = parser_expect(p, Token_Identifier, "expected param name");
            parser_expect(p, Token_Colon, "expected ':' after param name");
            Param *param = memops_arena_push_struct(p->arena, Param);
            param->name = token_to_string8(p->arena, param_name);
            param->type = parse_type(p);
            if (parser_peek(p)->kind == Token_LBracket) {
                parser_error_token(p, parser_peek(p),
                                   "a parameter takes no attribute; what a parameter wants is a "
                                   "type qualifier, and those belong to the type");
            }
            param->line = param_name->line;
            param->col = param_name->col;
            ptr_array_append(p->arena, &decl->params, param);
        } while (parser_match(p, Token_Comma));
        parser_expect(p, Token_RParen, "expected ')'");
    }

    parser_expect(p, Token_Arrow, "expected '->' after params");
    decl->ret_type = parse_type(p);
    // allow optional '=' before body
    parser_match(p, Token_Equal);

    parser_expect(p, Token_LBrace, "expected '{' in proc body");
    if (parser_peek(p)->kind == Token_Identifier &&
        (string8slice_equals_cstr(parser_peek(p)->text, "external") ||
         string8slice_equals_cstr(parser_peek(p)->text, "external_emit")) &&
        parser_peek_n(p, 1)->kind == Token_Semicolon) {
        parser_error_token(p, parser_peek(p),
                               "'external;' in a body is no longer accepted; write the attribute instead, as in 'f: proc[external]()->i32 = {}'");
        return decl;
    }
    while (!parser_at_block_end(p)) {
        Stmt *s = parse_stmt_recovering(p);
        ptr_array_append(p->arena, &decl->body, s);
    }
    // optional ';' after proc decl
    parser_match(p, Token_Semicolon);
    return decl;
}

/* After a failed declaration the token stream is out of step with the grammar, so
   scanning to the next thing that starts a declaration stops one bad line from
   turning every later line into a cascade of invented errors. Top-level
   declarations start in column 1, which distinguishes them from indented locals
   and struct fields. */
static void parser_sync_to_next_decl(Parser *p) {
    while (parser_peek(p)->kind != Token_EOF) {
        Token *t = parser_peek(p);
        if (t->col == 1) {
            if (t->kind == Token_Keyword_Import) return;
            if (t->kind == Token_Identifier && parser_peek_n(p, 1)->kind == Token_Colon) return;
            if (t->kind == Token_Identifier &&
                (string8slice_equals_cstr(t->text, "cinclude") ||
                 string8slice_equals_cstr(t->text, "define"))) {
                return;
            }
        }
        parser_next(p);
    }
}

static Program parse_program(Parser *p) {
    Program prog = {0};
    prog.preprocessor_lines = Vec_string8_reserve(p->arena, 8);
    prog.defines = Vec_string8_reserve(p->arena, 8);
    prog.imports = Vec_string8_reserve(p->arena, 8);
    prog.c_imports = Vec_string8_reserve(p->arena, 8);
    prog.i_imports = Vec_string8_reserve(p->arena, 8);
    prog.i_import_lines = Vec_i32_reserve(p->arena, 8);
    prog.i_import_cols = Vec_i32_reserve(p->arena, 8);
    prog.structs = ptr_array_reserve(p->arena, 8);
    prog.enums = ptr_array_reserve(p->arena, 8);
    prog.aliases = ptr_array_reserve(p->arena, 8);
    prog.procs = ptr_array_reserve(p->arena, 8);
    prog.globals = ptr_array_reserve(p->arena, 8);
    prog.pending_array_counts = ptr_array_reserve(p->arena, 8);
    p->pending_array_counts = &prog.pending_array_counts;

    i32 errors_at_decl_start = g_error_count;
    i32 index_at_decl_start = -1;

    while (parser_peek(p)->kind != Token_EOF) {
        if (p->index == index_at_decl_start) parser_next(p); // always make progress
        if (g_error_count > errors_at_decl_start) {
            parser_sync_to_next_decl(p);
            if (parser_peek(p)->kind == Token_EOF) break;
        }
        errors_at_decl_start = g_error_count;
        index_at_decl_start = p->index;

        if (parser_peek(p)->kind == Token_Identifier &&
            string8slice_equals_cstr(parser_peek(p)->text, "define")) {
            parser_next(p); // define
            parser_expect(p, Token_LParen, "expected '(' after define");
            Token *name_tok = parser_expect(p, Token_String, "expected string literal in define");
            parser_expect(p, Token_RParen, "expected ')' after define");
            Vec_string8_append(p->arena, &prog.defines, token_to_string8(p->arena, name_tok));
            parser_match(p, Token_Semicolon); // optional
            continue;
        }

        if (parser_match(p, Token_Keyword_Import)) {
            Token *path_tok = parser_expect(p, Token_String, "expected string literal after import");
            string8 path = token_to_string8(p->arena, path_tok);
            string8 inner = string_lit_inner(p->arena, path);
            if (!string8_ends_with_cstr(inner, ".i")) {
                parser_error_token(p, path_tok, "import expects an .i module; use cinclude for C headers");
            }
            Vec_string8_append(p->arena, &prog.i_imports, path);
            Vec_i32_append(p->arena, &prog.i_import_lines, path_tok->line);
            Vec_i32_append(p->arena, &prog.i_import_cols, path_tok->col);
            Vec_string8_append(p->arena, &prog.imports, path);
            parser_match(p, Token_Semicolon);
            continue;
        }

        if (parser_peek(p)->kind == Token_Identifier &&
            string8slice_equals_cstr(parser_peek(p)->text, "cinclude")) {
            parser_next(p);
            Token *path_tok = parser_expect(p, Token_String, "expected string literal after cinclude");
            Vec_string8_append(p->arena, &prog.c_imports, token_to_string8(p->arena, path_tok));
            parser_match(p, Token_Semicolon);
            continue;
        }

        Token *head_tok = parser_peek(p);
        string8 base_name = token_to_string8(p->arena, head_tok);
        string8 parsed_name = parse_decl_name(p);
        parser_expect(p, Token_Colon, "expected ':' after identifier");

        /* 'static' gives a proc or global internal linkage; types have no linkage. */
        Token *static_tok = parser_peek(p);
        bool is_static = parser_match(p, Token_Keyword_Static);
        if (is_static) {
            TokenKind next = parser_peek(p)->kind;
            if (next == Token_Keyword_Struct || next == Token_Keyword_Union ||
                next == Token_Keyword_Enum || next == Token_Keyword_Alias) {
                parser_error_token(p, static_tok, "'static' applies to procs and globals, not type declarations");
            }
        }

        if (parser_match(p, Token_Keyword_Struct)) {
            StructDecl *decl = parse_struct_decl(p, head_tok, false);
            if (!string8_equals(&parsed_name, &base_name)) {
                decl->name = parsed_name;
            }
            ptr_array_append(p->arena, &prog.structs, decl);
            continue;
        }

        if (parser_match(p, Token_Keyword_Union)) {
            StructDecl *decl = parse_struct_decl(p, head_tok, true);
            if (!string8_equals(&parsed_name, &base_name)) {
                decl->name = parsed_name;
            }
            ptr_array_append(p->arena, &prog.structs, decl);
            continue;
        }

        if (parser_match(p, Token_Keyword_Enum)) {
            EnumDecl *decl = parse_enum_decl(p, head_tok);
            if (!string8_equals(&parsed_name, &base_name)) {
                decl->name = parsed_name;
            }
            ptr_array_append(p->arena, &prog.enums, decl);
            continue;
        }

        if (parser_match(p, Token_Keyword_Alias)) {
            AliasDecl *decl = parse_alias_decl(p, head_tok);
            if (!string8_equals(&parsed_name, &base_name)) {
                decl->name = parsed_name;
            }
            ptr_array_append(p->arena, &prog.aliases, decl);
            continue;
        }

        if (parser_match(p, Token_Keyword_Proc)) {
            ProcDecl *decl = parse_proc_decl(p, head_tok);
            if (!string8_equals(&parsed_name, &base_name)) {
                decl->name = parsed_name;
            }
            decl->is_static = is_static;
            ptr_array_append(p->arena, &prog.procs, decl);
            continue;
        }

        Stmt *s = stmt_new(p->arena, Stmt_Var);
        s->is_static = is_static;
        s->name = parsed_name;
        s->type = parse_type(p);
        s->align = parse_trailing_decl_attributes(p, s->type);
        s->line = head_tok->line;
        s->col = head_tok->col;
        if (parser_match(p, Token_Equal)) {
            if (parser_peek(p)->kind == Token_Identifier &&
                string8slice_equals_cstr(parser_peek(p)->text, "external") &&
                parser_peek_n(p, 1)->kind == Token_Semicolon) {
                parser_error_token(p, parser_peek(p),
                                   "'= external' is no longer accepted; a global with no "
                                   "initializer is the one C owns, as in 'g: const T;'");
            } else if (parser_match(p, Token_Question)) {
                s->is_uninitialized = true;
            } else {
                s->expr = parse_expr(p);
            }
        } else {
            /* No initializer: this global is not ilang's to define. C owns it,
               its header declares it, and nothing is emitted here. */
            s->is_external = true;
        }
        parser_expect(p, Token_Semicolon, "expected ';' after global var");
        ptr_array_append(p->arena, &prog.globals, s);
    }

    return prog;
}

static bool scope_has(Vec_string8 *names, string8 name) {
    for (i32 i = 0; i < names->length; i++) {
        if (string8_equals(&names->data[i], &name)) return true;
    }
    return false;
}

static i32 string8_vec_find(Vec_string8 *names, string8 name) {
    for (i32 i = 0; i < names->length; i++) {
        if (string8_equals(&names->data[i], &name)) return i;
    }
    return -1;
}

static LocalDeclSite *local_decl_site_new(memops_arena *arena, string8 name, i32 line, i32 col) {
    LocalDeclSite *site = memops_arena_push_struct(arena, LocalDeclSite);
    site->name = name;
    site->line = line;
    site->col = col;
    return site;
}

static LocalDeclSite *scope_find_local_site(Scope *scope, string8 name) {
    for (i32 i = 0; i < scope->local_sites.length; i++) {
        LocalDeclSite *site = (LocalDeclSite *)scope->local_sites.data[i];
        if (string8_equals(&site->name, &name)) return site;
    }
    return null;
}

static void scope_copy_locals(memops_arena *arena, Scope *dst, Scope *src, i32 extra) {
    dst->locals = Vec_string8_reserve(arena, src->locals.length + extra);
    dst->local_sites = ptr_array_reserve(arena, src->local_sites.length + extra);
    for (i32 i = 0; i < src->locals.length; i++) {
        Vec_string8_append(arena, &dst->locals, src->locals.data[i]);
    }
    for (i32 i = 0; i < src->local_sites.length; i++) {
        ptr_array_append(arena, &dst->local_sites, src->local_sites.data[i]);
    }
}

static void diag_note_import_chain(void);

static const char *diag_current_path(void) {
    return g_diag_source_path ? g_diag_source_path : g_source_path;
}

static void diag_print_file_context_range(const char *path, i32 line, i32 col, i32 range_len) {
    if (!path || line <= 0 || col <= 0 || path[0] == '<') return;
    FILE *f = i_fopen(path, "rb");
    if (!f) return;
    char buf[4096];
    i32 current = 1;
    while (fgets(buf, sizeof(buf), f)) {
        if (current == line) {
            size_t len = strlen(buf);
            while (len > 0 && (buf[len - 1] == '\n' || buf[len - 1] == '\r')) {
                buf[--len] = 0;
            }
            printf("    %s\n", buf);
            printf("    ");
            for (i32 i = 1; i < col; i++) {
                printf(" ");
            }
            if (range_len <= 0) {
                range_len = 1;
            }
            printf("^");
            for (i32 i = 1; i < range_len; i++) {
                printf("~");
            }
            printf("\n");
            break;
        }
        current++;
    }
    fclose(f);
}

static void diag_print_file_context(const char *path, i32 line, i32 col) {
    diag_print_file_context_range(path, line, col, 1);
}

static void diag_finish_range(i32 line, i32 col, i32 range_len) {
    diag_print_file_context_range(diag_current_path(), line, col, range_len);
    diag_note_import_chain();
    diag_record_error();
}

static void diag_finish_at(i32 line, i32 col) {
    diag_finish_range(line, col, 1);
}

static void semantic_error(const char *msg, i32 line, i32 col) {
    if (g_diag_json) {
        diag_json_error(diag_current_path(), line, col, "semantic", msg);
        diag_record_error();
        return;
    }
    printf("%s:%d:%d: semantic error: %s\n",
           diag_current_path(),
           line,
           col,
           msg);
    diag_finish_at(line, col);
}

static void diag_note_import_chain(void) {
    if (g_diag_import_chain && g_diag_import_chain[0]) {
        printf("%s:0:0: note: imported through: %s\n",
               g_diag_source_path ? g_diag_source_path : g_source_path,
               g_diag_import_chain);
    }
}

static void semantic_error_name(const char *msg, string8 name, i32 line, i32 col) {
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "%s '%.*s'",
            msg,
            (int)name.length,
            name.data
        );
        diag_json_error_range(diag_current_path(), line, col, line, col + (i32)name.length, "semantic", message);
        diag_record_error();
        return;
    }
    printf("%s:%d:%d: semantic error: %s '%.*s'\n",
           diag_current_path(),
           line,
           col,
           msg,
           (int)name.length,
           name.data);
    diag_finish_range(line, col, (i32)name.length);
}

static void semantic_error_name_dup(
    const char *msg, string8 name,
    i32 line, i32 col,
    i32 prev_line, i32 prev_col
) {
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "%s '%.*s' (previous at %d:%d)",
            msg,
            (int)name.length,
            name.data,
            prev_line,
            prev_col
        );
        bool has_note = false;
        diag_json_open_range(diag_current_path(), line, col, line, col + (i32)name.length, "semantic", message);
        diag_json_note_import_chain(&has_note);
        diag_json_note_cstr(&has_note, diag_current_path(), prev_line, prev_col, "previous declaration here");
        diag_json_close();
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: semantic error: %s '%.*s' (previous at %d:%d)\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        msg,
        (int)name.length,
        name.data,
        prev_line,
        prev_col
    );
    diag_finish_range(line, col, (i32)name.length);
}

static void semantic_error_name_dup_path(
    const char *msg,
    string8 name,
    const char *path,
    const char *import_chain,
    i32 line,
    i32 col,
    const char *prev_path,
    const char *prev_import_chain,
    i32 prev_line,
    i32 prev_col
) {
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "%s '%.*s' (previous at %s:%d:%d)",
            msg,
            (int)name.length,
            name.data,
            prev_path ? prev_path : g_source_path,
            prev_line,
            prev_col
        );
        bool has_note = false;
        diag_json_open_range(path ? path : g_source_path, line, col, line, col + (i32)name.length, "semantic", message);
        if (import_chain && import_chain[0]) {
            char note[2048];
            snprintf(note, sizeof(note), "imported through: %s", import_chain);
            diag_json_note_cstr(&has_note, path ? path : g_source_path, 0, 0, note);
        }
        diag_json_note_cstr(&has_note, prev_path ? prev_path : g_source_path, prev_line, prev_col, "previous declaration here");
        if (prev_import_chain && prev_import_chain[0]) {
            char note[2048];
            snprintf(note, sizeof(note), "previous declaration imported through: %s", prev_import_chain);
            diag_json_note_cstr(&has_note, prev_path ? prev_path : g_source_path, 0, 0, note);
        }
        diag_json_close();
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: semantic error: %s '%.*s' (previous at %s:%d:%d)\n",
        path ? path : g_source_path,
        line,
        col,
        msg,
        (int)name.length,
        name.data,
        prev_path ? prev_path : g_source_path,
        prev_line,
        prev_col
    );
    diag_print_file_context_range(path ? path : g_source_path, line, col, (i32)name.length);
    if (import_chain && import_chain[0]) {
        printf("%s:0:0: note: imported through: %s\n", path ? path : g_source_path, import_chain);
    }
    if (prev_import_chain && prev_import_chain[0]) {
        printf("%s:0:0: note: previous declaration imported through: %s\n", prev_path ? prev_path : g_source_path, prev_import_chain);
    }
    diag_record_error();
    return;
}

static bool preprocessor_define_is_function_like(string8 line) {
    u64 i = 0;
    while (i < line.length && (line.data[i] == ' ' || line.data[i] == '\t')) i++;
    if (i >= line.length || line.data[i] != '#') return false;
    i++;
    while (i < line.length && (line.data[i] == ' ' || line.data[i] == '\t')) i++;
    if (i + 6 > line.length || strncmp((const char *)(line.data + i), "define", 6) != 0) return false;
    i += 6;
    if (i < line.length && line.data[i] != ' ' && line.data[i] != '\t') return false;
    while (i < line.length && (line.data[i] == ' ' || line.data[i] == '\t')) i++;
    u64 start = i;
    while (i < line.length && (is_alnum(line.data[i]) || line.data[i] == '_')) i++;
    if (i == start) return false;
    /* cpp's rule: the paren must touch the name. A space makes it object-like. */
    return i < line.length && line.data[i] == '(';
}

static string8 preprocessor_define_name(memops_arena *arena, string8 line) {
    u64 i = 0;
    while (i < line.length && (line.data[i] == ' ' || line.data[i] == '\t')) i++;
    if (i >= line.length || line.data[i] != '#') return (string8){0};
    i++;
    while (i < line.length && (line.data[i] == ' ' || line.data[i] == '\t')) i++;
    const char *define = "define";
    u64 define_len = 6;
    if (i + define_len > line.length || strncmp((const char *)(line.data + i), define, define_len) != 0) {
        return (string8){0};
    }
    i += define_len;
    if (i < line.length && line.data[i] != ' ' && line.data[i] != '\t') return (string8){0};
    while (i < line.length && (line.data[i] == ' ' || line.data[i] == '\t')) i++;
    u64 start = i;
    while (i < line.length && (is_alnum(line.data[i]) || line.data[i] == '_')) i++;
    if (i == start) return (string8){0};
    return string8_copy_from_slice(arena, line.data + start, i - start);
}

static const char *canonicalize_path(memops_arena *arena, string8 path) {
    string8 nul_path = string8_reserve(arena, path.length + 1);
    string8_append_bytes(arena, &nul_path, path.data, path.length);
    string8_append_byte(arena, &nul_path, 0);

#if defined(_WIN32)
    char *full = _fullpath(null, (const char *)nul_path.data, 0);
    if (full) {
        string8 out = string8_reserve(arena, (u64)strlen(full) + 1);
        string8_append_cstr(arena, &out, full);
        string8_append_byte(arena, &out, 0);
        free(full);
        return (const char *)out.data;
    }
#endif

    return (const char *)nul_path.data;
}

static const char *current_exe_path(memops_arena *arena, const char *argv0) {
#if defined(_WIN32)
    char buffer[4096];
    DWORD len = GetModuleFileNameA(NULL, buffer, sizeof(buffer));
    if (len > 0 && len < sizeof(buffer)) {
        return canonicalize_path(arena, string8_from_cstr(arena, buffer));
    }
#endif
    return canonicalize_path(arena, string8_from_cstr(arena, argv0));
}

static const char *exe_import_root(memops_arena *arena, const char *argv0) {
    if (!argv0 || !argv0[0]) return null;
    const char *exe_path = current_exe_path(arena, argv0);
    u64 exe_len = (u64)strlen(exe_path);
    u64 dir_len = 0;
    for (u64 i = exe_len; i > 0; i--) {
        char c = exe_path[i - 1];
        if (c == '/' || c == '\\') {
            dir_len = i;
            break;
        }
    }
    if (dir_len == 0) return null;

    string8 dir = string8_reserve(arena, dir_len + 1);
    string8_append_bytes(arena, &dir, (u8 *)exe_path, dir_len);
    return canonicalize_path(arena, dir);
}

static const char *resolve_import_path(memops_arena *arena, string8 import_lit) {
    string8 import_path = string_lit_inner(arena, import_lit);
    if (import_path.length >= 2 && import_path.data[1] == ':') {
        return canonicalize_path(arena, import_path);
    }
    if (import_path.length > 0 && (import_path.data[0] == '/' || import_path.data[0] == '\\')) {
        return canonicalize_path(arena, import_path);
    }

    const char *source = g_source_path;
    u64 source_len = (u64)strlen(source);
    u64 dir_len = 0;
    for (u64 i = source_len; i > 0; i--) {
        char c = source[i - 1];
        if (c == '/' || c == '\\') {
            dir_len = i;
            break;
        }
    }
    string8 out = string8_reserve(arena, dir_len + import_path.length + 1);
    string8_append_bytes(arena, &out, (u8 *)source, dir_len);
    string8_append_bytes(arena, &out, import_path.data, import_path.length);
    const char *source_relative = canonicalize_path(arena, out);
    if (file_exists_cstr(source_relative)) {
        return source_relative;
    }

    for (i32 i = 0; i < g_import_dir_count; i++) {
        string8 dir = string8_from_cstr(arena, g_import_dirs[i]);
        u64 needs_slash = 0;
        if (dir.length > 0) {
            u8 last = dir.data[dir.length - 1];
            needs_slash = (last != '/' && last != '\\');
        }

        string8 candidate = string8_reserve(arena, dir.length + needs_slash + import_path.length + 1);
        string8_append_bytes(arena, &candidate, dir.data, dir.length);
        if (needs_slash) string8_append_byte(arena, &candidate, '/');
        string8_append_bytes(arena, &candidate, import_path.data, import_path.length);
        const char *canonical = canonicalize_path(arena, candidate);
        if (file_exists_cstr(canonical)) {
            return canonical;
        }
    }

    return source_relative;
}

static string8 read_i_source_for_path(memops_arena *arena, const char *path) {
    if (g_stdin_override_path && path && cstr_equals(path, g_stdin_override_path)) {
        return g_stdin_override_source;
    }
    return string8_read_file(arena, path);
}

static void semantic_add_program_symbols(Program *prog, Scope *base, Vec_string8 *structs, memops_arena *arena);

static void semantic_add_import_symbols(Program *prog, Scope *base, Vec_string8 *structs, memops_arena *arena) {
    for (i32 i = 0; i < prog->i_imports.length; i++) {
        const char *path = resolve_import_path(arena, prog->i_imports.data[i]);
        string8 input = read_i_source_for_path(arena, path);
        if (!input.data) {
            printf("%s:0:0: semantic error: failed to read import %s\n", g_source_path, path);
            diag_note_import_chain();
            diag_exit_with_errors();
        }

        Vec_Token tokens = {0};
        Vec_string8 directives = {0};
        lex_tokens(arena, input, &tokens, &directives);

        Parser parser = {0};
        parser.arena = arena;
        parser.source = input;
        parser.tokens = tokens;
        parser.index = 0;

        Program imported = parse_program(&parser);
        imported.preprocessor_lines = directives;
        semantic_add_program_symbols(&imported, base, structs, arena);
    }
}

static void semantic_add_program_symbols(Program *prog, Scope *base, Vec_string8 *structs, memops_arena *arena) {
    semantic_add_import_symbols(prog, base, structs, arena);

    for (i32 i = 0; i < prog->aliases.length; i++) {
        AliasDecl *decl = (AliasDecl *)prog->aliases.data[i];
        if (!scope_has(structs, decl->name)) {
            Vec_string8_append(arena, structs, decl->name);
        }
    }

    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!scope_has(structs, decl->name)) {
            Vec_string8_append(arena, structs, decl->name);
        }
        if (!decl->is_external) {
            Vec_string8_append(arena, &base->globals, concat_name2(arena, decl->name, "_", string8_from_cstr(arena, "reflect")));
        }
    }

    for (i32 i = 0; i < prog->enums.length; i++) {
        EnumDecl *decl = (EnumDecl *)prog->enums.data[i];
        if (!scope_has(structs, decl->name)) {
            Vec_string8_append(arena, structs, decl->name);
        }
        if (!scope_has(&base->enum_types, decl->name)) {
            Vec_string8_append(arena, &base->enum_types, decl->name);
        }
        if (!decl->is_external) {
            Vec_string8_append(arena, &base->globals, concat_name2(arena, decl->name, "_", string8_from_cstr(arena, "reflect")));
            for (i32 j = 0; j < decl->items.length; j++) {
                EnumItem *item = (EnumItem *)decl->items.data[j];
                Vec_string8_append(arena, &base->globals, concat_name2(arena, decl->name, "_", item->name));
            }
        }
    }

    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        if (!scope_has(&base->procs, decl->name)) {
            Vec_string8_append(arena, &base->procs, decl->name);
        }
    }

    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *decl = (Stmt *)prog->globals.data[i];
        if (!scope_has(&base->globals, decl->name)) {
            Vec_string8_append(arena, &base->globals, decl->name);
        }
    }
}

static bool semantic_builtin_type_name(string8 name);
static void semantic_check_expr(Expr *e, Scope *scope, Vec_string8 *known_types, Vec_string8 *generic_params);

static void semantic_check_expr(Expr *e, Scope *scope, Vec_string8 *known_types, Vec_string8 *generic_params) {
    if (!e) return;
    if (e->kind == Expr_Number) return;
    if (e->kind == Expr_String || e->kind == Expr_Char) return;
    if (e->kind == Expr_SizeofType || e->kind == Expr_AlignofType) {
        if (e->inner) {
            semantic_check_expr(e->inner, scope, known_types, generic_params);
            return;
        }
        /* A bare name is the ambiguous case: `sizeof(gin_vertex)` and
           `sizeof(line)` are the same shape, and only the symbol tables can
           separate them. Values are consulted first, so a local shadowing a type
           name resolves the way C resolves it, and so that a variable in
           SHOUTING_CASE is not mistaken for a foreign C type by the ALL_CAPS
           heuristic in semantic_builtin_type_name. Compound operands keep the
           old silence for now; the ambiguity only exists for a bare name. */
        if (e->cast_type && e->cast_type->kind == Type_Name) {
            string8 n = e->cast_type->name;
            if (scope_has(&scope->locals, n) ||
                scope_has(&scope->globals, n) ||
                scope_has(&scope->procs, n)) {
                return;
            }
            if (semantic_builtin_type_name(n)) return;
            if (generic_params && scope_has(generic_params, n)) return;
            if (known_types && scope_has(known_types, n)) return;
            semantic_error_name(
                "sizeof operand is neither a type nor a value", n, e->line, e->col);
        }
        return;
    }
    if (e->kind == Expr_ZeroInit) return;
    if (e->kind == Expr_InitList) {
        for (i32 i = 0; i < e->args.length; i++) {
            if (e->designator_kinds.data[i] == InitDesignator_Index) {
                semantic_check_expr((Expr *)e->designators.data[i], scope, known_types, generic_params);
            }
            semantic_check_expr((Expr *)e->args.data[i], scope, known_types, generic_params);
        }
        return;
    }
    if (e->kind == Expr_CompoundInit) {
        semantic_check_expr(e->inner, scope, known_types, generic_params);
        return;
    }
    if (e->kind == Expr_Name) {
        if (string8_equals_cstr(&e->name, "null")) return;
        if (scope_has(&scope->locals, e->name)) return;
        if (scope_has(&scope->globals, e->name)) return;
        if (scope_has(&scope->procs, e->name)) return;
        if (e->reflect_base.data &&
            (scope_has(&scope->locals, e->reflect_base) ||
             scope_has(&scope->globals, e->reflect_base))) {
            return; /* `value<>`; resolved against the value's type below */
        }
        semantic_error_name("use of undeclared identifier", e->name, e->line, e->col);
    }
    if (e->kind == Expr_Addr) {
        semantic_check_expr(e->inner, scope, known_types, generic_params);
        return;
    }
    if (e->kind == Expr_Index) {
        semantic_check_expr(e->base, scope, known_types, generic_params);
        semantic_check_expr(e->index_expr, scope, known_types, generic_params);
        return;
    }
    if (e->kind == Expr_Field) {
        if (e->base && e->base->kind == Expr_Name && scope_has(&scope->enum_types, e->base->name)) {
            return;
        }
        semantic_check_expr(e->base, scope, known_types, generic_params);
        return;
    }
    if (e->kind == Expr_Cast) {
        semantic_check_expr(e->inner, scope, known_types, generic_params);
        return;
    }
    if (e->kind == Expr_Binary) {
        semantic_check_expr(e->left, scope, known_types, generic_params);
        semantic_check_expr(e->right, scope, known_types, generic_params);
        return;
    }
    if (e->kind == Expr_Ternary) {
        semantic_check_expr(e->left, scope, known_types, generic_params);
        semantic_check_expr(e->right, scope, known_types, generic_params);
        semantic_check_expr(e->third, scope, known_types, generic_params);
        return;
    }
    if (e->kind == Expr_Call) {
        /* An indirect call has no name to resolve; the callee is checked like any
           other expression. */
        if (e->base) {
            semantic_check_expr(e->base, scope, known_types, generic_params);
            for (i32 i = 0; i < e->args.length; i++) {
                semantic_check_expr((Expr *)e->args.data[i], scope, known_types, generic_params);
            }
            return;
        }
        if (string8_equals_cstr(&e->name, "printf")) {
            for (i32 i = 0; i < e->args.length; i++) {
                semantic_check_expr((Expr *)e->args.data[i], scope, known_types, generic_params);
            }
            return;
        }
        // A `cinclude` does not lower anything into the I symbol table, and no
        // longer needs to: every C function is declared in I before it can be
        // called, so an unresolved name here is a mistake rather than interop.
        // The call path reports it; see type_error_call_undeclared.
        for (i32 i = 0; i < e->args.length; i++) {
            semantic_check_expr((Expr *)e->args.data[i], scope, known_types, generic_params);
        }
        return;
    }
}

static void semantic_check_type(Program *prog, TypeExpr *type, Vec_string8 *known_types, Vec_string8 *generic_params, const char *source_path);
static void semantic_check_stmt(Program *prog, Stmt *stmt, Scope *scope, Vec_string8 *known_types, Vec_string8 *generic_params, memops_arena *arena);
static void semantic_error_control_flow(const char *keyword, const char *context, i32 line, i32 col);

/* Identifiers that are not I keywords but are C keywords. I lowers to C, so a
   local named `typedef` emits `i32 typedef = 1;` and the C compiler rejects it
   with an error pointing at generated code the author never wrote. Rejecting
   here turns that into a diagnostic on the real source line.

   This is deliberately a restriction rather than a mangling: relaxing it later
   is backward compatible, and mangling every identifier would make the
   generated C harder to read, which matters because it is meant to be read.
   See docs/compiler-hardening.md. */
static bool ident_is_c_reserved(string8 name) {
    /* Also ilang type spellings, so they cannot be mangled: the same token
       would have to mean a type in one position and a renamed variable in
       another. These stay rejected. */
    static const char *ambiguous[] = {
        "double", "float", "int", "long", "short", "signed", "unsigned",
    };
    for (i32 i = 0; i < (i32)(sizeof(ambiguous) / sizeof(ambiguous[0])); i++) {
        if (string8_equals_cstr(&name, ambiguous[i])) {
            return true;
        }
    }
    return false;
}

/* C keywords ilang has no other use for. A declaration may carry any of these
   as a name; c_ident renames it on the way into the generated C. */
static bool ident_needs_c_mangle(string8 name) {
    static const char *mangled[] = {
        "auto", "extern", "inline", "register", "restrict", "typedef",
        "_Alignas", "_Alignof", "_Atomic", "_Bool", "_Complex", "_Generic",
        "_Imaginary", "_Noreturn", "_Static_assert", "_Thread_local",
        "static_assert",
    };
    for (i32 i = 0; i < (i32)(sizeof(mangled) / sizeof(mangled[0])); i++) {
        if (string8_equals_cstr(&name, mangled[i])) {
            return true;
        }
    }
    return false;
}

/* The C spelling of an ilang identifier. Identity for everything except the
   handful above, so the generated C stays readable -- which is the reason this
   is a rename-on-collision rather than a blanket mangling of every name.

   `_Static_assert` becomes `i_Static_assert`, not `i__Static_assert`: C
   reserves every identifier containing a double underscore to the
   implementation, so the prefix absorbs the leading one. */
static void emit_c_ident(memops_arena *arena, string8 *out, string8 name);

static string8 c_ident(memops_arena *arena, string8 name) {
    if (!ident_needs_c_mangle(name)) return name;
    string8 out = string8_reserve(arena, name.length + 2);
    string8_append_cstr(arena, &out, name.data[0] == '_' ? "i" : "i_");
    string8_append_bytes(arena, &out, name.data, name.length);
    return out;
}

/* `sizeof` and `alignof` are builtins, not identifiers. Declaring a proc named
   `sizeof` used to pass `i: checked` and then emit `i32 sizeof(i32 n);`, which
   clang rejects as a syntax error -- the last C-keyword hole left open by
   ident_is_c_reserved. */
static bool ident_is_i_builtin(string8 name) {
    return string8_equals_cstr(&name, "sizeof") ||
           string8_equals_cstr(&name, "alignof");
}

/* The outputs of c_ident, spelled out rather than derived, so this needs no
   arena to allocate a candidate for each comparison. Keep in step with the list
   in ident_needs_c_mangle; decl_attributes-style tests cover the pairing. */
static bool ident_is_c_mangle_target(string8 name) {
    static const char *targets[] = {
        "i_auto", "i_extern", "i_inline", "i_register", "i_restrict", "i_typedef",
        "i_Alignas", "i_Alignof", "i_Atomic", "i_Bool", "i_Complex", "i_Generic",
        "i_Imaginary", "i_Noreturn", "i_Static_assert", "i_Thread_local",
        "i_static_assert",
    };
    for (i32 i = 0; i < (i32)(sizeof(targets) / sizeof(targets[0])); i++) {
        if (string8_equals_cstr(&name, targets[i])) return true;
    }
    return false;
}

static void semantic_check_ident_available(string8 name, i32 line, i32 col) {
    if (ident_is_c_mangle_target(name)) {
        semantic_error_name(
            "identifier is reserved: it is the C spelling ilang gives a C keyword used as a name",
            name, line, col);
    }
    if (ident_is_i_builtin(name)) {
        semantic_error_name(
            "identifier is a builtin operator and cannot be used as a name",
            name, line, col);
    }
    if (ident_is_c_reserved(name)) {
        semantic_error_name(
            "identifier is reserved by the C backend and cannot be used as a name",
            name, line, col);
    }
}

static void semantic_check_stmt(Program *prog, Stmt *stmt, Scope *scope, Vec_string8 *known_types, Vec_string8 *generic_params, memops_arena *arena) {
    if (stmt->kind == Stmt_Var) {
        semantic_check_type(prog, stmt->type, known_types, generic_params, stmt->source_path);
        if (stmt->expr) semantic_check_expr(stmt->expr, scope, known_types, generic_params);
        semantic_check_ident_available(stmt->name, stmt->line, stmt->col);
        LocalDeclSite *prev = scope_find_local_site(scope, stmt->name);
        if (prev) {
            semantic_error_name_dup("duplicate local declaration", stmt->name, stmt->line, stmt->col, prev->line, prev->col);
        }
        Vec_string8_append(arena, &scope->locals, stmt->name);
        ptr_array_append(arena, &scope->local_sites, local_decl_site_new(arena, stmt->name, stmt->line, stmt->col));
        return;
    }
    if (stmt->kind == Stmt_Assign) {
        if (stmt->lhs && stmt->lhs->kind == Expr_Name) {
            if (!scope_has(&scope->locals, stmt->name) && !scope_has(&scope->globals, stmt->name)) {
                semantic_error_name("assignment to undeclared identifier", stmt->name, stmt->line, stmt->col);
            }
        }
        semantic_check_expr(stmt->lhs, scope, known_types, generic_params);
        semantic_check_expr(stmt->expr, scope, known_types, generic_params);
        return;
    }
    if (stmt->kind == Stmt_Return || stmt->kind == Stmt_Expr) {
        semantic_check_expr(stmt->expr, scope, known_types, generic_params);
        return;
    }
    if (stmt->kind == Stmt_For) {
        Scope loop_scope = *scope;
        loop_scope.loop_depth += 1;
        scope_copy_locals(arena, &loop_scope, scope, 16);
        if (stmt->for_init) semantic_check_stmt(prog, stmt->for_init, &loop_scope, known_types, generic_params, arena);
        if (stmt->for_cond) semantic_check_expr(stmt->for_cond, &loop_scope, known_types, generic_params);
        if (stmt->for_step) semantic_check_stmt(prog, stmt->for_step, &loop_scope, known_types, generic_params, arena);
        for (i32 i = 0; i < stmt->for_body.length; i++) {
            semantic_check_stmt(prog, (Stmt *)stmt->for_body.data[i], &loop_scope, known_types, generic_params, arena);
        }
        return;
    }
    if (stmt->kind == Stmt_While) {
        semantic_check_expr(stmt->while_cond, scope, known_types, generic_params);
        Scope loop_scope = *scope;
        loop_scope.loop_depth += 1;
        scope_copy_locals(arena, &loop_scope, scope, 16);
        for (i32 i = 0; i < stmt->while_body.length; i++) {
            semantic_check_stmt(prog, (Stmt *)stmt->while_body.data[i], &loop_scope, known_types, generic_params, arena);
        }
        return;
    }
    if (stmt->kind == Stmt_DoWhile) {
        Scope loop_scope = *scope;
        loop_scope.loop_depth += 1;
        scope_copy_locals(arena, &loop_scope, scope, 16);
        for (i32 i = 0; i < stmt->while_body.length; i++) {
            semantic_check_stmt(prog, (Stmt *)stmt->while_body.data[i], &loop_scope, known_types, generic_params, arena);
        }
        semantic_check_expr(stmt->while_cond, &loop_scope, known_types, generic_params);
        return;
    }
    if (stmt->kind == Stmt_Break || stmt->kind == Stmt_Continue) {
        if (stmt->kind == Stmt_Break && scope->loop_depth == 0 && scope->switch_depth == 0) {
            semantic_error_control_flow("break", "loop or switch", stmt->line, stmt->col);
        }
        if (stmt->kind == Stmt_Continue && scope->loop_depth == 0) {
            semantic_error_control_flow("continue", "loop", stmt->line, stmt->col);
        }
        return;
    }
    if (stmt->kind == Stmt_Switch) {
        semantic_check_expr(stmt->switch_expr, scope, known_types, generic_params);
        for (i32 i = 0; i < stmt->switch_cases.length; i++) {
            SwitchCase *sc = (SwitchCase *)stmt->switch_cases.data[i];
            semantic_check_expr(sc->expr, scope, known_types, generic_params);
            Scope case_scope = *scope;
            case_scope.switch_depth += 1;
            scope_copy_locals(arena, &case_scope, scope, 16);
            for (i32 j = 0; j < sc->body.length; j++) {
                semantic_check_stmt(prog, (Stmt *)sc->body.data[j], &case_scope, known_types, generic_params, arena);
            }
        }
        Scope default_scope = *scope;
        default_scope.switch_depth += 1;
        scope_copy_locals(arena, &default_scope, scope, 16);
        for (i32 i = 0; i < stmt->switch_default_body.length; i++) {
            semantic_check_stmt(prog, (Stmt *)stmt->switch_default_body.data[i], &default_scope, known_types, generic_params, arena);
        }
        return;
    }
    if (stmt->kind == Stmt_If) {
        semantic_check_expr(stmt->if_cond, scope, known_types, generic_params);

        Scope then_scope = *scope;
        scope_copy_locals(arena, &then_scope, scope, 16);
        for (i32 i = 0; i < stmt->if_then_body.length; i++) {
            semantic_check_stmt(prog, (Stmt *)stmt->if_then_body.data[i], &then_scope, known_types, generic_params, arena);
        }

        if (stmt->if_else_if) {
            Scope else_if_scope = *scope;
            scope_copy_locals(arena, &else_if_scope, scope, 16);
            semantic_check_stmt(prog, stmt->if_else_if, &else_if_scope, known_types, generic_params, arena);
        } else {
            Scope else_scope = *scope;
            scope_copy_locals(arena, &else_scope, scope, 16);
            for (i32 i = 0; i < stmt->if_else_body.length; i++) {
                semantic_check_stmt(prog, (Stmt *)stmt->if_else_body.data[i], &else_scope, known_types, generic_params, arena);
            }
        }
        return;
    }
    if (stmt->kind == Stmt_Directive) {
        /* Nothing to check: the text is handed to the C preprocessor verbatim.
           ilang deliberately does not evaluate the condition, so both arms of an
           inline '#ifdef' are parsed and checked, and neither is folded away. */
        return;
    }
    if (stmt->kind == Stmt_Goto) {
        return; // resolved against the proc-wide label set in semantic_check_proc
    }
    if (stmt->kind == Stmt_Label) {
        /* The body is a scope but not a loop, so break/continue inside it stay
           illegal unless a real loop encloses the label. */
        Scope label_scope = *scope;
        scope_copy_locals(arena, &label_scope, scope, 16);
        for (i32 i = 0; i < stmt->while_body.length; i++) {
            semantic_check_stmt(prog, (Stmt *)stmt->while_body.data[i], &label_scope, known_types, generic_params, arena);
        }
        return;
    }
    semantic_error("unknown statement kind", stmt->line, stmt->col);
}

/* Labels are proc-wide in C, so they are collected across nested blocks before
   any goto is resolved, which also lets a goto jump forward to a later label. */
static void semantic_collect_labels(Stmt *stmt, Vec_string8 *labels, memops_arena *arena);

static void semantic_collect_labels_list(Vec_voidptr *stmts, Vec_string8 *labels, memops_arena *arena) {
    for (i32 i = 0; i < stmts->length; i++) {
        semantic_collect_labels((Stmt *)stmts->data[i], labels, arena);
    }
}

static void semantic_collect_labels(Stmt *stmt, Vec_string8 *labels, memops_arena *arena) {
    if (!stmt) return;
    if (stmt->kind == Stmt_Label) {
        i32 prev = string8_vec_find(labels, stmt->name);
        if (prev >= 0) {
            semantic_error_name("duplicate label", stmt->name, stmt->line, stmt->col);
        }
        Vec_string8_append(arena, labels, stmt->name);
        return;
    }
    semantic_collect_labels_list(&stmt->for_body, labels, arena);
    semantic_collect_labels_list(&stmt->while_body, labels, arena);
    semantic_collect_labels_list(&stmt->if_then_body, labels, arena);
    semantic_collect_labels_list(&stmt->if_else_body, labels, arena);
    semantic_collect_labels(stmt->if_else_if, labels, arena);
    semantic_collect_labels_list(&stmt->switch_default_body, labels, arena);
    for (i32 i = 0; i < stmt->switch_cases.length; i++) {
        SwitchCase *sc = (SwitchCase *)stmt->switch_cases.data[i];
        semantic_collect_labels_list(&sc->body, labels, arena);
    }
}

static void semantic_check_gotos(Stmt *stmt, Vec_string8 *labels);

static void semantic_check_gotos_list(Vec_voidptr *stmts, Vec_string8 *labels) {
    for (i32 i = 0; i < stmts->length; i++) {
        semantic_check_gotos((Stmt *)stmts->data[i], labels);
    }
}

static void semantic_check_gotos(Stmt *stmt, Vec_string8 *labels) {
    if (!stmt) return;
    if (stmt->kind == Stmt_Goto) {
        if (string8_vec_find(labels, stmt->name) < 0) {
            semantic_error_name("use of undeclared label", stmt->name, stmt->line, stmt->col);
        }
        return;
    }
    semantic_check_gotos_list(&stmt->for_body, labels);
    semantic_check_gotos_list(&stmt->while_body, labels);
    semantic_check_gotos_list(&stmt->if_then_body, labels);
    semantic_check_gotos_list(&stmt->if_else_body, labels);
    semantic_check_gotos(stmt->if_else_if, labels);
    semantic_check_gotos_list(&stmt->switch_default_body, labels);
    for (i32 i = 0; i < stmt->switch_cases.length; i++) {
        SwitchCase *sc = (SwitchCase *)stmt->switch_cases.data[i];
        semantic_check_gotos_list(&sc->body, labels);
    }
}

static void semantic_check_proc(Program *prog, ProcDecl *proc, Scope *base_scope, Vec_string8 *known_types, memops_arena *arena) {
    const char *prev_diag_source_path = g_diag_source_path;
    const char *prev_diag_import_chain = g_diag_import_chain;
    if (proc->source_path) {
        g_diag_source_path = proc->source_path;
    }
    g_diag_import_chain = proc->import_chain;

    /* Proc names went unchecked, so `sizeof: proc(n: i32)->i32` passed
       `i: checked` and then emitted `i32 sizeof(i32 n);`, which clang rejects
       as a syntax error -- a diagnostic about generated code the author never
       wrote. Variables and parameters were already covered. */
    semantic_check_ident_available(proc->name, proc->line, proc->col);

    Scope scope = *base_scope;
    scope.locals = Vec_string8_reserve(arena, 32);
    scope.local_sites = ptr_array_reserve(arena, 32);

    /* External procs are checked too. Skipping them meant a C function could be
       declared in terms of types that exist nowhere in ilang -- the signature
       looked precise and enforced nothing, which is the same shape as the
       undeclared-call hole: it reads like a declaration and is not one. */
    semantic_check_type(prog, proc->ret_type, known_types, &proc->type_params, proc->source_path);
    for (i32 i = 0; i < proc->params.length; i++) {
        Param *param = (Param *)proc->params.data[i];
        semantic_check_type(prog, param->type, known_types, &proc->type_params, proc->source_path);
        semantic_check_ident_available(param->name, param->line, param->col);
        if (scope_has(&scope.locals, param->name)) {
            // locate previous parameter declaration
            i32 prev_line = param->line;
            i32 prev_col = param->col;
            for (i32 j = 0; j < i; j++) {
                Param *prev = (Param *)proc->params.data[j];
                if (string8_equals(&prev->name, &param->name)) {
                    prev_line = prev->line;
                    prev_col = prev->col;
                    break;
                }
            }
            semantic_error_name_dup("duplicate proc parameter", param->name, param->line, param->col, prev_line, prev_col);
        }
        Vec_string8_append(arena, &scope.locals, param->name);
        ptr_array_append(arena, &scope.local_sites, local_decl_site_new(arena, param->name, param->line, param->col));
    }

    for (i32 i = 0; i < proc->body.length; i++) {
        Stmt *stmt = (Stmt *)proc->body.data[i];
        semantic_check_stmt(prog, stmt, &scope, known_types, &proc->type_params, arena);
    }

    Vec_string8 labels = Vec_string8_reserve(arena, 4);
    semantic_collect_labels_list(&proc->body, &labels, arena);
    semantic_check_gotos_list(&proc->body, &labels);

    g_diag_source_path = prev_diag_source_path;
    g_diag_import_chain = prev_diag_import_chain;
}

typedef struct SemanticDeclSite {
    string8 name;
    const char *path;
    const char *import_chain;
    i32 line;
    i32 col;
} SemanticDeclSite;

static SemanticDeclSite *semantic_decl_site_new(memops_arena *arena, string8 name, const char *path, const char *import_chain, i32 line, i32 col) {
    SemanticDeclSite *site = memops_arena_push_struct(arena, SemanticDeclSite);
    memset(site, 0, sizeof(*site));
    site->name = name;
    site->path = path;
    site->import_chain = import_chain;
    site->line = line;
    site->col = col;
    return site;
}

static SemanticDeclSite *semantic_decl_site_find(Vec_voidptr *sites, string8 name) {
    for (i32 i = 0; i < sites->length; i++) {
        SemanticDeclSite *site = (SemanticDeclSite *)sites->data[i];
        if (string8_equals(&site->name, &name)) return site;
    }
    return null;
}

static void semantic_decl_site_add(memops_arena *arena, Vec_voidptr *sites, string8 name, const char *path, const char *import_chain, i32 line, i32 col) {
    ptr_array_append(arena, sites, semantic_decl_site_new(arena, name, path, import_chain, line, col));
}

static void semantic_decl_site_add_checked(
    memops_arena *arena,
    Vec_voidptr *sites,
    const char *kind,
    string8 name,
    const char *path,
    const char *import_chain,
    i32 line,
    i32 col
) {
    SemanticDeclSite *prev = semantic_decl_site_find(sites, name);
    if (prev) {
        semantic_error_name_dup_path(kind, name, path, import_chain, line, col, prev->path, prev->import_chain, prev->line, prev->col);
    }
    semantic_decl_site_add(arena, sites, name, path, import_chain, line, col);
}

static bool string8_starts_with_cstr(string8 s, const char *prefix) {
    u64 prefix_len = (u64)strlen(prefix);
    return s.length >= prefix_len && strncmp((const char *)s.data, prefix, prefix_len) == 0;
}

static bool semantic_builtin_type_name(string8 name) {
    bool c_typedef_style = name.data && name.length > 0;
    for (u64 i = 0; c_typedef_style && i < name.length; i++) {
        u8 c = name.data[i];
        c_typedef_style = (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_';
    }
    if (c_typedef_style) return true;
    if (name.length >= 2 &&
        name.data[0] >= 'A' && name.data[0] <= 'Z' &&
        name.data[1] >= 'A' && name.data[1] <= 'Z') {
        return true;
    }
    if (string8_starts_with_cstr(name, "ma_") ||
        string8_starts_with_cstr(name, "cgltf_") ||
        string8_starts_with_cstr(name, "stbi") ||
        string8_starts_with_cstr(name, "stbir")) {
        return true;
    }

    return string8_equals_cstr(&name, "c8") ||
           string8_equals_cstr(&name, "b8") ||
           string8_equals_cstr(&name, "b16") ||
           string8_equals_cstr(&name, "b64") ||
           string8_equals_cstr(&name, "intptr") ||
           string8_equals_cstr(&name, "uintptr") ||
           string8_equals_cstr(&name, "ptrdiff") ||
           string8_equals_cstr(&name, "intmax") ||
           string8_equals_cstr(&name, "uintmax") ||
           string8_equals_cstr(&name, "f32") ||
           string8_equals_cstr(&name, "f64") ||
           string8_equals_cstr(&name, "i8") ||
           string8_equals_cstr(&name, "i16") ||
           string8_equals_cstr(&name, "i32") ||
           string8_equals_cstr(&name, "i64") ||
           string8_equals_cstr(&name, "u8") ||
           string8_equals_cstr(&name, "u16") ||
           string8_equals_cstr(&name, "u32") ||
           string8_equals_cstr(&name, "u64") ||
           string8_equals_cstr(&name, "usize") ||
           string8_equals_cstr(&name, "b32") ||
           string8_equals_cstr(&name, "void") ||
           string8_equals_cstr(&name, "reflect_type_kind") ||
           string8_equals_cstr(&name, "reflect_kind") ||
           string8_equals_cstr(&name, "reflect_field") ||
           string8_equals_cstr(&name, "reflect_value") ||
           string8_equals_cstr(&name, "reflect_variant") ||
           string8_equals_cstr(&name, "reflect") ||
           string8_equals_cstr(&name, "BOOL") ||
           string8_equals_cstr(&name, "BOOLEAN") ||
           string8_equals_cstr(&name, "ATOM") ||
           string8_equals_cstr(&name, "BYTE") ||
           string8_equals_cstr(&name, "CHAR") ||
           string8_equals_cstr(&name, "DWORD") ||
           string8_equals_cstr(&name, "HRESULT") ||
           string8_equals_cstr(&name, "INT") ||
           string8_equals_cstr(&name, "INT8") ||
           string8_equals_cstr(&name, "INT16") ||
           string8_equals_cstr(&name, "INT32") ||
           string8_equals_cstr(&name, "INT64") ||
           string8_equals_cstr(&name, "LONG") ||
           string8_equals_cstr(&name, "LPARAM") ||
           string8_equals_cstr(&name, "LRESULT") ||
           string8_equals_cstr(&name, "SHORT") ||
           string8_equals_cstr(&name, "UINT") ||
           string8_equals_cstr(&name, "UINT8") ||
           string8_equals_cstr(&name, "UINT16") ||
           string8_equals_cstr(&name, "UINT32") ||
           string8_equals_cstr(&name, "UINT64") ||
           string8_equals_cstr(&name, "ULONG") ||
           string8_equals_cstr(&name, "USHORT") ||
           string8_equals_cstr(&name, "WPARAM") ||
           string8_equals_cstr(&name, "WORD") ||
           string8_equals_cstr(&name, "ma_bool32") ||
           string8_equals_cstr(&name, "ma_channel") ||
           string8_equals_cstr(&name, "ma_format") ||
           string8_equals_cstr(&name, "ma_int8") ||
           string8_equals_cstr(&name, "ma_int16") ||
           string8_equals_cstr(&name, "ma_int32") ||
           string8_equals_cstr(&name, "ma_int64") ||
           string8_equals_cstr(&name, "ma_result") ||
           string8_equals_cstr(&name, "ma_uint8") ||
           string8_equals_cstr(&name, "ma_uint16") ||
           string8_equals_cstr(&name, "ma_uint32") ||
           string8_equals_cstr(&name, "ma_uint64") ||
           string8_equals_cstr(&name, "long") ||
           string8_equals_cstr(&name, "ulong") ||
           string8_equals_cstr(&name, "short") ||
           string8_equals_cstr(&name, "int") ||
           string8_equals_cstr(&name, "float") ||
           string8_equals_cstr(&name, "double") ||
           string8_equals_cstr(&name, "HANDLE") ||
           string8_equals_cstr(&name, "HBRUSH") ||
           string8_equals_cstr(&name, "HCURSOR") ||
           string8_equals_cstr(&name, "HDC") ||
           string8_equals_cstr(&name, "HICON") ||
           string8_equals_cstr(&name, "HINSTANCE") ||
           string8_equals_cstr(&name, "HMODULE") ||
           string8_equals_cstr(&name, "HMENU") ||
           string8_equals_cstr(&name, "HWND");
}

static bool semantic_intrinsic_type_name(string8 name) {
    return string8_equals_cstr(&name, "c8") ||
           string8_equals_cstr(&name, "b8") ||
           string8_equals_cstr(&name, "b16") ||
           string8_equals_cstr(&name, "b64") ||
           string8_equals_cstr(&name, "intptr") ||
           string8_equals_cstr(&name, "uintptr") ||
           string8_equals_cstr(&name, "ptrdiff") ||
           string8_equals_cstr(&name, "intmax") ||
           string8_equals_cstr(&name, "uintmax") ||
           string8_equals_cstr(&name, "f32") ||
           string8_equals_cstr(&name, "f64") ||
           string8_equals_cstr(&name, "i8") ||
           string8_equals_cstr(&name, "i16") ||
           string8_equals_cstr(&name, "i32") ||
           string8_equals_cstr(&name, "i64") ||
           string8_equals_cstr(&name, "u8") ||
           string8_equals_cstr(&name, "u16") ||
           string8_equals_cstr(&name, "u32") ||
           string8_equals_cstr(&name, "u64") ||
           string8_equals_cstr(&name, "usize") ||
           string8_equals_cstr(&name, "b32") ||
           string8_equals_cstr(&name, "void") ||
           string8_equals_cstr(&name, "reflect_type_kind") ||
           string8_equals_cstr(&name, "reflect_kind") ||
           string8_equals_cstr(&name, "reflect_field") ||
           string8_equals_cstr(&name, "reflect_value") ||
           string8_equals_cstr(&name, "reflect_variant") ||
           string8_equals_cstr(&name, "reflect") ||
           string8_equals_cstr(&name, "long") ||
           string8_equals_cstr(&name, "ulong") ||
           string8_equals_cstr(&name, "short") ||
           string8_equals_cstr(&name, "int") ||
           string8_equals_cstr(&name, "float") ||
           string8_equals_cstr(&name, "double");
}

static void semantic_error_name_path(const char *msg, string8 name, const char *path, i32 line, i32 col) {
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "%s '%.*s'",
            msg,
            (int)name.length,
            name.data
        );
        diag_json_error_range(path ? path : g_source_path, line, col, line, col + (i32)name.length, "semantic", message);
        diag_record_error();
        return;
    }
    printf("%s:%d:%d: semantic error: %s '%.*s'\n",
           path ? path : g_source_path,
           line,
           col,
           msg,
           (int)name.length,
           name.data);
    diag_print_file_context_range(path ? path : g_source_path, line, col, (i32)name.length);
    diag_note_import_chain();
    diag_record_error();
    return;
}

static void semantic_error_control_flow(const char *keyword, const char *context, i32 line, i32 col) {
    if (g_diag_json) {
        char message[256];
        snprintf(message, sizeof(message), "%s outside %s", keyword, context);
        diag_json_error(diag_current_path(), line, col, "semantic", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: semantic error: %s outside %s\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        keyword,
        context
    );
    diag_finish_at(line, col);
}

static void diag_push_decl_context(const char *source_path, const char *import_chain, const char **prev_source_path, const char **prev_import_chain) {
    *prev_source_path = g_diag_source_path;
    *prev_import_chain = g_diag_import_chain;
    if (source_path) {
        g_diag_source_path = source_path;
    }
    g_diag_import_chain = import_chain;
}

static void diag_pop_decl_context(const char *prev_source_path, const char *prev_import_chain) {
    g_diag_source_path = prev_source_path;
    g_diag_import_chain = prev_import_chain;
}

typedef struct SemanticTypeInfo {
    bool found;
    bool is_generic;
    i32 type_param_count;
    string8 name;
    const char *kind;
    const char *path;
    i32 line;
    i32 col;
} SemanticTypeInfo;

static SemanticTypeInfo semantic_find_type_info(Program *prog, string8 name) {
    if (!prog) return (SemanticTypeInfo){0};
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (string8_equals(&decl->name, &name)) {
            return (SemanticTypeInfo){true, decl->is_generic, decl->type_params.length, decl->name, decl->is_union ? "union" : "struct", decl->source_path, decl->line, decl->col};
        }
    }
    for (i32 i = 0; i < prog->aliases.length; i++) {
        AliasDecl *decl = (AliasDecl *)prog->aliases.data[i];
        if (string8_equals(&decl->name, &name)) {
            return (SemanticTypeInfo){true, false, 0, decl->name, "alias", decl->source_path, decl->line, decl->col};
        }
    }
    for (i32 i = 0; i < prog->enums.length; i++) {
        EnumDecl *decl = (EnumDecl *)prog->enums.data[i];
        if (string8_equals(&decl->name, &name)) {
            return (SemanticTypeInfo){true, false, 0, decl->name, "enum", decl->source_path, decl->line, decl->col};
        }
    }
    return (SemanticTypeInfo){0};
}


/* A type that contains itself by value has no size, so C rejects the generated
   record with `field has incomplete type`. ilang knows the field types well
   enough to say so first, on the real declaration.

   The walk follows anything that needs the type to be *complete* and stops at
   anything that does not:

     P: struct = { inner: P; }              direct
     P: struct = { xs: [4]P; }              an array needs a complete element
     A: struct = { b: B; } B { a: A; }      mutual, hence the visited set
     P: struct = { v: PA; } PA: alias = P;  aliases are transparent
     P: struct = { union = { inner: P; } }  anonymous members are members
     Box: struct<T> = { v: Box<T>; }        matched by base name, see below

   and leaves alone:

     P: struct = { next: *P; }              a pointer needs only the name

   Generics are matched on the declaration's base name rather than on an
   instantiation. `Box<T>` holding a `Box<...>` by value is infinite whatever
   the argument, while `Pair<Pair<i32, i32>, i32>` nests legitimately and is
   never a self-reference in the declaration. */

typedef struct TypeCycleWalk {
    Program *prog;
    Vec_string8 visiting;   // the chain, for the diagnostic
    string8 root;
    const char *root_path;  // used when the cycle is reported on the declaration itself
    i32 root_line;
    i32 root_col;
} TypeCycleWalk;

static void type_cycle_check_type(TypeCycleWalk *w, TypeExpr *type, StructDecl *owner, Field *field, memops_arena *arena);

static void type_cycle_error(TypeCycleWalk *w, string8 name, StructDecl *owner, Field *field, memops_arena *arena) {
    string8 chain = string8_reserve(arena, 64);
    for (i32 i = 0; i < w->visiting.length; i++) {
        string8_append_bytes(arena, &chain, w->visiting.data[i].data, w->visiting.data[i].length);
        string8_append_cstr(arena, &chain, " -> ");
    }
    string8_append_bytes(arena, &chain, name.data, name.length);

    i32 line = field ? field->line : (owner ? owner->line : w->root_line);
    i32 col = field ? field->col : (owner ? owner->col : w->root_col);
    const char *path = owner ? owner->source_path : w->root_path;
    semantic_error_name_path(
        "type contains itself by value, so it has no size; use a pointer to break the cycle",
        chain, path, line, col);
}

/* Walks the members of one record, including anonymous ones, which live in the
   owner's name space and so are part of the owner's layout. */
static void type_cycle_check_struct(TypeCycleWalk *w, StructDecl *decl, memops_arena *arena) {
    if (!decl) return;
    for (i32 i = 0; i < decl->fields.length; i++) {
        Field *f = (Field *)decl->fields.data[i];
        if (f->anon) {
            type_cycle_check_struct(w, f->anon, arena);
            continue;
        }
        type_cycle_check_type(w, f->type, decl, f, arena);
    }
}

static void type_cycle_check_named(TypeCycleWalk *w, string8 name, StructDecl *owner, Field *field, memops_arena *arena) {
    if (string8_equals(&name, &w->root)) {
        type_cycle_error(w, name, owner, field, arena);
        return;
    }
    for (i32 i = 0; i < w->visiting.length; i++) {
        if (string8_equals(&w->visiting.data[i], &name)) return; // already on the chain, reported at its own root
    }
    if (w->visiting.length > 64) return; // pathological nesting; the depth alone is the bug

    Vec_string8_append(arena, &w->visiting, name);

    for (i32 i = 0; i < w->prog->aliases.length; i++) {
        AliasDecl *a = (AliasDecl *)w->prog->aliases.data[i];
        if (string8_equals(&a->name, &name)) {
            type_cycle_check_type(w, a->type, owner, field, arena);
            w->visiting.length--;
            return;
        }
    }
    for (i32 i = 0; i < w->prog->structs.length; i++) {
        StructDecl *d = (StructDecl *)w->prog->structs.data[i];
        if (string8_equals(&d->name, &name)) {
            type_cycle_check_struct(w, d, arena);
            break;
        }
    }
    w->visiting.length--;
}

static void type_cycle_check_type(TypeCycleWalk *w, TypeExpr *type, StructDecl *owner, Field *field, memops_arena *arena) {
    if (!type) return;
    switch (type->kind) {
        case Type_Ptr:
            return;  // a pointer needs the name, not the layout
        case Type_Proc:
            return;  // a proc type is a pointer's worth of nothing here
        case Type_Array:
            type_cycle_check_type(w, type->elem, owner, field, arena);
            return;
        case Type_Name:
        case Type_Generic:
            /* The base name only. Walking the type *arguments* would be
               unsound without substituting them into the generic's fields:
               `P: struct = { items: Vec<P>; }` is legal whenever Vec holds its
               elements behind a pointer, and flagging it would be a false
               positive on correct code. Missing the case where a generic does
               hold its argument by value is a false negative, which is where
               this already was. */
            type_cycle_check_named(w, type->name, owner, field, arena);
            return;
    }
}

/* Entry point: every record and every alias is its own root, so a cycle is
   reported once per declaration that takes part in it rather than once. */
static void semantic_check_type_cycles(Program *prog, memops_arena *arena) {
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        TypeCycleWalk w = {0};
        w.prog = prog;
        w.root = decl->name;
        w.visiting = Vec_string8_reserve(arena, 8);
        type_cycle_check_struct(&w, decl, arena);
    }
    for (i32 i = 0; i < prog->aliases.length; i++) {
        AliasDecl *decl = (AliasDecl *)prog->aliases.data[i];
        TypeCycleWalk w = {0};
        w.prog = prog;
        w.root = decl->name;
        w.root_path = decl->source_path;
        w.root_line = decl->line;
        w.root_col = decl->col;
        w.visiting = Vec_string8_reserve(arena, 8);
        type_cycle_check_type(&w, decl->type, null, null, arena);
    }
}


/* An enum member that does not fit its underlying type is silently wrong: it is
   accepted by ilang, accepted by clang at every default warning level, and then
   reads as one number through i32 and another through u32. Stating the
   underlying type (2.6) is what finally gave this something to check against.

   Deliberately partial. Values that are plain integer literals or implicit and
   sequential are checked -- 435 of the 442 members across njinn, std and the
   tests. The remaining 7 are constant expressions (`1 shl 2`, `~0`, references
   to earlier members), and evaluating those needs a constant evaluator ilang
   does not have. Once a member's value is unknown, every later implicit value
   is unknown too, so the walk stops rather than guessing.

   The generated C carries a pragma that asks clang for the same check, which
   does cover the expression cases; see emit_enum_range_pragma. This half exists
   so the common case is caught by ilang, on the ilang line, without waiting for
   a C compile. */
typedef struct EnumRange {
    i64 min;
    u64 max;
    bool is_signed;
} EnumRange;

static bool enum_underlying_range(string8 underlying, EnumRange *out) {
    struct { const char *name; i64 min; u64 max; bool is_signed; } table[] = {
        {"i8",    -128LL,                  127ULL,                  true},
        {"i16",   -32768LL,                32767ULL,                true},
        {"i32",   -2147483648LL,           2147483647ULL,           true},
        {"i64",   (-9223372036854775807LL - 1), 9223372036854775807ULL, true},
        {"u8",    0,                       255ULL,                  false},
        {"u16",   0,                       65535ULL,                false},
        {"u32",   0,                       4294967295ULL,           false},
        {"u64",   0,                       18446744073709551615ULL, false},
    };
    /* An unattributed enum is i32; see shape.md 2.6. */
    const char *name = (underlying.data && underlying.length > 0) ? null : "i32";
    for (i32 i = 0; i < (i32)(sizeof(table) / sizeof(table[0])); i++) {
        bool hit = name ? (strcmp(name, table[i].name) == 0)
                        : string8_equals_cstr(&underlying, table[i].name);
        if (hit) {
            out->min = table[i].min;
            out->max = table[i].max;
            out->is_signed = table[i].is_signed;
            return true;
        }
    }
    return false;
}

/* Parses a bare decimal or hex enum value. Returns false for anything else --
   a sibling reference, a qualified name, or a token this does not understand. */
static bool enum_literal_value(string8 text, i64 *out) {
    if (!text.data || text.length == 0) return false;
    u64 i = 0;
    bool negative = false;
    if (text.data[0] == '-') {
        negative = true;
        i = 1;
    } else if (text.data[0] == '+') {
        i = 1;
    }
    if (i >= text.length) return false;

    u64 value = 0;
    if ((text.length - i) > 2 && text.data[i] == '0' &&
        (text.data[i + 1] == 'x' || text.data[i + 1] == 'X')) {
        for (u64 j = i + 2; j < text.length; j++) {
            u8 c = text.data[j];
            u64 digit;
            if (c >= '0' && c <= '9') digit = (u64)(c - '0');
            else if (c >= 'a' && c <= 'f') digit = (u64)(c - 'a') + 10;
            else if (c >= 'A' && c <= 'F') digit = (u64)(c - 'A') + 10;
            else return false;
            if (value > (0xFFFFFFFFFFFFFFFFULL - digit) / 16) return false; // overflows u64 itself
            value = value * 16 + digit;
        }
    } else {
        for (u64 j = i; j < text.length; j++) {
            u8 c = text.data[j];
            if (c < '0' || c > '9') return false;
            u64 digit = (u64)(c - '0');
            if (value > (0xFFFFFFFFFFFFFFFFULL - digit) / 10) return false;
            value = value * 10 + digit;
        }
    }
    /* Values above i64's range are out of every underlying type's range except
       u64's, and are reported by the caller through the unsigned bound. */
    if (value > 9223372036854775807ULL) return false;
    *out = negative ? -(i64)value : (i64)value;
    return true;
}

static void semantic_check_enum_ranges(Program *prog) {
    for (i32 i = 0; i < prog->enums.length; i++) {
        EnumDecl *decl = (EnumDecl *)prog->enums.data[i];
        if (decl->is_external) continue; // C owns the definition and its values

        EnumRange range = {0};
        if (!enum_underlying_range(decl->underlying, &range)) continue;

        i64 next = 0;
        bool next_known = true;
        for (i32 j = 0; j < decl->items.length; j++) {
            EnumItem *item = (EnumItem *)decl->items.data[j];
            i64 value = 0;
            if (item->value_expr) {
                /* A leading '-' lexes as its own token, so every negative value
                   arrives as a unary minus over a number rather than as a bare
                   literal. Those are worth reading -- negative members are
                   supported and reflection round-trips them. Anything else is
                   a constant expression this cannot evaluate. */
                Expr *e = item->value_expr;
                i64 magnitude = 0;
                if (e->kind == Expr_Unary && e->op == Token_Minus && e->inner &&
                    e->inner->kind == Expr_Number &&
                    enum_literal_value(e->inner->number, &magnitude)) {
                    value = -magnitude;
                    next_known = true;
                } else {
                    next_known = false;   // and so is every implicit value after it
                    continue;
                }
            } else if (item->value.data && item->value.length > 0) {
                if (!enum_literal_value(item->value, &value)) {
                    next_known = false;
                    continue;
                }
                next_known = true;
            } else {
                if (!next_known) continue;
                value = next;
            }
            next = value + 1;

            bool below = value < range.min;
            bool above = range.is_signed ? (value > (i64)range.max)
                                         : (value < 0 || (u64)value > range.max);
            if (below || above) {
                bool stated = decl->underlying.data && decl->underlying.length > 0;
                char detail[256];
                snprintf(detail, sizeof(detail),
                         "enum member does not fit its underlying type '%.*s'",
                         stated ? (int)decl->underlying.length : 3,
                         stated ? (const char *)decl->underlying.data : "i32");
                semantic_error_name_path(detail, item->name, decl->source_path,
                                         item->line, item->col);
            }
        }
    }
}

static void semantic_note_type_decl(SemanticTypeInfo info) {
    if (!info.found) return;
    printf(
        "%s:%d:%d: note: %s '%.*s' declared here\n",
        info.path ? info.path : g_source_path,
        info.line,
        info.col,
        info.kind ? info.kind : "type",
        (int)info.name.length,
        info.name.data
    );
}

static void semantic_error_generic_type_arity(TypeExpr *type, SemanticTypeInfo info, const char *source_path) {
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "generic type '%.*s' expects %d type arg%s, got %d",
            (int)type->name.length,
            type->name.data,
            (int)info.type_param_count,
            info.type_param_count == 1 ? "" : "s",
            (int)type->args.length
        );
        bool has_note = false;
        diag_json_open(source_path ? source_path : g_source_path, type->line, type->col, "semantic", message);
        diag_json_note_import_chain(&has_note);
        if (info.found) {
            char note[512];
            snprintf(
                note,
                sizeof(note),
                "%s '%.*s' declared here",
                info.kind ? info.kind : "type",
                (int)info.name.length,
                info.name.data
            );
            diag_json_note_cstr(&has_note, info.path ? info.path : g_source_path, info.line, info.col, note);
        }
        diag_json_close();
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: semantic error: generic type '%.*s' expects %d type arg%s, got %d\n",
        source_path ? source_path : g_source_path,
        type->line,
        type->col,
        (int)type->name.length,
        type->name.data,
        (int)info.type_param_count,
        info.type_param_count == 1 ? "" : "s",
        (int)type->args.length
    );
    diag_note_import_chain();
    semantic_note_type_decl(info);
    diag_record_error();
    return;
}

static void semantic_error_nongeneric_type_args(TypeExpr *type, SemanticTypeInfo info, const char *source_path) {
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "type '%.*s' is not generic; got %d type arg%s",
            (int)type->name.length,
            type->name.data,
            (int)type->args.length,
            type->args.length == 1 ? "" : "s"
        );
        bool has_note = false;
        diag_json_open(source_path ? source_path : g_source_path, type->line, type->col, "semantic", message);
        diag_json_note_import_chain(&has_note);
        if (info.found) {
            char note[512];
            snprintf(
                note,
                sizeof(note),
                "%s '%.*s' declared here",
                info.kind ? info.kind : "type",
                (int)info.name.length,
                info.name.data
            );
            diag_json_note_cstr(&has_note, info.path ? info.path : g_source_path, info.line, info.col, note);
        }
        diag_json_close();
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: semantic error: type '%.*s' is not generic; got %d type arg%s\n",
        source_path ? source_path : g_source_path,
        type->line,
        type->col,
        (int)type->name.length,
        type->name.data,
        (int)type->args.length,
        type->args.length == 1 ? "" : "s"
    );
    diag_note_import_chain();
    semantic_note_type_decl(info);
    diag_record_error();
    return;
}

static void semantic_check_type(Program *prog, TypeExpr *type, Vec_string8 *known_types, Vec_string8 *generic_params, const char *source_path) {
    if (!type) return;
    if (type->kind == Type_Name) {
        if (semantic_builtin_type_name(type->name)) return;
        if (generic_params && scope_has(generic_params, type->name)) return;
        if (scope_has(known_types, type->name)) return;
        semantic_error_name_path("use of undeclared type", type->name, source_path, type->line, type->col);
    }
    if (type->kind == Type_Ptr || type->kind == Type_Array) {
        semantic_check_type(prog, type->elem, known_types, generic_params, source_path);
        return;
    }
    if (type->kind == Type_Generic) {
        if (!scope_has(known_types, type->name)) {
            semantic_error_name_path("use of undeclared generic type", type->name, source_path, type->line, type->col);
        }
        SemanticTypeInfo info = semantic_find_type_info(prog, type->name);
        if (info.found && !info.is_generic) {
            semantic_error_nongeneric_type_args(type, info, source_path);
        }
        if (info.found && info.is_generic && type->args.length != info.type_param_count) {
            semantic_error_generic_type_arity(type, info, source_path);
        }
        for (i32 i = 0; i < type->args.length; i++) {
            semantic_check_type(prog, (TypeExpr *)type->args.data[i], known_types, generic_params, source_path);
        }
        return;
    }
    if (type->kind == Type_Proc) {
        for (i32 i = 0; i < type->args.length; i++) {
            semantic_check_type(prog, (TypeExpr *)type->args.data[i], known_types, generic_params, source_path);
        }
        semantic_check_type(prog, type->ret_type, known_types, generic_params, source_path);
        return;
    }
}

static void semantic_collect_external_type_names(TypeExpr *type, Vec_string8 *known_types, memops_arena *arena) {
    if (!type) return;
    if (type->kind == Type_Name) {
        if (!semantic_builtin_type_name(type->name) && !scope_has(known_types, type->name)) {
            Vec_string8_append(arena, known_types, type->name);
        }
        return;
    }
    if (type->kind == Type_Ptr || type->kind == Type_Array) {
        semantic_collect_external_type_names(type->elem, known_types, arena);
        return;
    }
    if (type->kind == Type_Generic) {
        if (!scope_has(known_types, type->name)) {
            Vec_string8_append(arena, known_types, type->name);
        }
        for (i32 i = 0; i < type->args.length; i++) {
            semantic_collect_external_type_names((TypeExpr *)type->args.data[i], known_types, arena);
        }
        return;
    }
    if (type->kind == Type_Proc) {
        for (i32 i = 0; i < type->args.length; i++) {
            semantic_collect_external_type_names((TypeExpr *)type->args.data[i], known_types, arena);
        }
        semantic_collect_external_type_names(type->ret_type, known_types, arena);
        return;
    }
}

static void semantic_collect_program_external_type_names(Program *prog, Vec_string8 *known_types, memops_arena *arena) {
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_external) continue;
        for (i32 j = 0; j < decl->fields.length; j++) {
            Field *field = (Field *)decl->fields.data[j];
            semantic_collect_external_type_names(field->type, known_types, arena);
        }
    }

    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        if (!decl->is_external) continue;
        semantic_collect_external_type_names(decl->ret_type, known_types, arena);
        for (i32 j = 0; j < decl->params.length; j++) {
            Param *param = (Param *)decl->params.data[j];
            semantic_collect_external_type_names(param->type, known_types, arena);
        }
    }

    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *decl = (Stmt *)prog->globals.data[i];
        if (decl->is_external) {
            semantic_collect_external_type_names(decl->type, known_types, arena);
        }
    }
}

static bool semantic_known_type_name(Vec_string8 *known_types, string8 name) {
    return semantic_intrinsic_type_name(name) || scope_has(known_types, name);
}

static void semantic_collect_angle_pattern_params(TypeExpr *type, Vec_string8 *known_types, string8 *param, i32 *count) {
    if (!type) return;
    if (type->kind == Type_Name) {
        if (!semantic_known_type_name(known_types, type->name)) {
            if (*count == 0) {
                *param = type->name;
                *count = 1;
            } else if (!string8_equals(param, &type->name)) {
                *count += 1;
                return;
            }
        }
        return;
    }
    if (type->kind == Type_Ptr || type->kind == Type_Array) {
        semantic_collect_angle_pattern_params(type->elem, known_types, param, count);
        return;
    }
    if (type->kind == Type_Generic) {
        if (!semantic_known_type_name(known_types, type->name)) {
            if (*count == 0) {
                *param = type->name;
                *count = 1;
            } else if (!string8_equals(param, &type->name)) {
                *count += 1;
                return;
            }
        }
        for (i32 i = 0; i < type->args.length; i++) {
            semantic_collect_angle_pattern_params((TypeExpr *)type->args.data[i], known_types, param, count);
        }
        return;
    }
    if (type->kind == Type_Proc) {
        semantic_collect_angle_pattern_params(type->ret_type, known_types, param, count);
        for (i32 i = 0; i < type->args.length; i++) {
            semantic_collect_angle_pattern_params((TypeExpr *)type->args.data[i], known_types, param, count);
        }
    }
}

static void semantic_resolve_proc_angle_types(Program *prog, Vec_string8 *known_types, memops_arena *arena) {
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];

        /* `proc<T, U, V>`: every name has to be a fresh parameter. Mixing a
           parameter with a concrete type would mean a partial specialisation,
           which has no lowering here, so it is rejected rather than guessed at. */
        if (decl->angle_types.length > 0) {
            bool all_params = true;
            for (i32 a = 0; a < decl->angle_types.length; a++) {
                TypeExpr *t = (TypeExpr *)decl->angle_types.data[a];
                if (t->kind != Type_Name || semantic_known_type_name(known_types, t->name)) {
                    all_params = false;
                    break;
                }
            }
            if (!all_params) {
                semantic_error_name_path(
                    "every name in a multi-parameter generic must be a new type parameter",
                    decl->name, decl->source_path, decl->line, decl->col);
                decl->angle_types.length = 0;
                continue;
            }
            decl->type_params = Vec_string8_reserve(arena, decl->angle_types.length);
            for (i32 a = 0; a < decl->angle_types.length; a++) {
                TypeExpr *t = (TypeExpr *)decl->angle_types.data[a];
                for (i32 b = 0; b < decl->type_params.length; b++) {
                    if (string8_equals(&decl->type_params.data[b], &t->name)) {
                        semantic_error_name_path("duplicate type parameter",
                            t->name, decl->source_path, decl->line, decl->col);
                    }
                }
                Vec_string8_append(arena, &decl->type_params, t->name);
            }
            decl->is_generic = true;
            decl->angle_types.length = 0;
            continue;
        }

        if (!decl->angle_type) continue;

        TypeExpr *angle = decl->angle_type;
        if (angle->kind == Type_Name && !semantic_known_type_name(known_types, angle->name)) {
            decl->type_params = Vec_string8_reserve(arena, 1);
            Vec_string8_append(arena, &decl->type_params, angle->name);
            decl->is_generic = true;
            decl->angle_type = null;
            continue;
        }

        string8 pattern_param = {0};
        i32 pattern_param_count = 0;
        semantic_collect_angle_pattern_params(angle, known_types, &pattern_param, &pattern_param_count);
        if (pattern_param_count == 1) {
            decl->type_params = Vec_string8_reserve(arena, 1);
            Vec_string8_append(arena, &decl->type_params, pattern_param);
            decl->is_generic = true;
            decl->generic_pattern = angle;
            decl->angle_type = null;
            semantic_check_type(prog, decl->generic_pattern, known_types, &decl->type_params, decl->source_path);
            continue;
        }

        semantic_check_type(prog, angle, known_types, null, decl->source_path);
        string8 suffix = type_mangle_concrete(arena, angle);
        string8 full_name = string8_reserve(arena, decl->name.length + 1 + suffix.length);
        string8_append_bytes(arena, &full_name, decl->name.data, decl->name.length);
        string8_append_cstr(arena, &full_name, "_");
        string8_append_bytes(arena, &full_name, suffix.data, suffix.length);
        decl->name = full_name;
        decl->is_generic = false;
        decl->angle_type = null;
    }
}

static string8 proc_semantic_key(memops_arena *arena, ProcDecl *decl) {
    if (decl && decl->is_generic && decl->generic_pattern) {
        string8 pattern = type_mangle_concrete(arena, decl->generic_pattern);
        string8 key = string8_reserve(arena, decl->name.length + pattern.length + 3);
        string8_append_bytes(arena, &key, decl->name.data, decl->name.length);
        string8_append_cstr(arena, &key, "<");
        string8_append_bytes(arena, &key, pattern.data, pattern.length);
        string8_append_cstr(arena, &key, ">");
        return key;
    }
    return decl ? decl->name : (string8){0};
}

/* Two modules that both use a C function must both declare it -- a `cinclude`
   brings no names into I. Forbidding that makes modules uncomposable: importing
   any two libraries that each declare `printf` would fail on a conflict neither
   author can see or resolve. C allows a compatible redeclaration for the same
   reason, so I does too, but only for `external` procs and only when the
   signatures are identical. A redeclaration that disagrees is still an error --
   that is a real conflict about what the C function looks like. */
static bool type_expr_identical(TypeExpr *a, TypeExpr *b) {
    if (a == b) return true;
    if (!a || !b) return false;
    if (a->kind != b->kind) return false;
    if (a->is_const != b->is_const) return false;
    if (a->is_volatile != b->is_volatile) return false;
    if (a->is_variadic != b->is_variadic) return false;
    if (!string8_equals(&a->name, &b->name)) return false;
    if (!string8_equals(&a->array_count, &b->array_count)) return false;
    if (!type_expr_identical(a->elem, b->elem)) return false;
    if (!type_expr_identical(a->ret_type, b->ret_type)) return false;
    if (a->args.length != b->args.length) return false;
    for (i32 i = 0; i < a->args.length; i++) {
        if (!type_expr_identical((TypeExpr *)a->args.data[i], (TypeExpr *)b->args.data[i])) return false;
    }
    return true;
}

static bool proc_signatures_identical(ProcDecl *a, ProcDecl *b) {
    if (!a || !b) return false;
    if (a->is_generic || b->is_generic) return false;
    if (a->is_variadic != b->is_variadic) return false;
    if (a->params.length != b->params.length) return false;
    if (!type_expr_identical(a->ret_type, b->ret_type)) return false;
    for (i32 i = 0; i < a->params.length; i++) {
        Param *pa = (Param *)a->params.data[i];
        Param *pb = (Param *)b->params.data[i];
        if (!pa || !pb) return false;
        if (!type_expr_identical(pa->type, pb->type)) return false;
    }
    return true;
}

static bool proc_is_compatible_external_redeclaration(Program *prog, i32 upto, ProcDecl *decl) {
    if (!decl || !decl->is_external) return false;
    for (i32 i = 0; i < upto; i++) {
        ProcDecl *prev = (ProcDecl *)prog->procs.data[i];
        if (!prev || !prev->is_external) continue;
        if (!string8_equals(&prev->name, &decl->name)) continue;
        return proc_signatures_identical(prev, decl);
    }
    return false;
}

static Vec_string8 semantic_collect_known_type_names(Program *prog, memops_arena *arena) {
    Vec_string8 known_types = Vec_string8_reserve(arena, prog->aliases.length + prog->structs.length + prog->enums.length + 8);
    for (i32 i = 0; i < prog->aliases.length; i++) {
        AliasDecl *decl = (AliasDecl *)prog->aliases.data[i];
        if (!scope_has(&known_types, decl->name)) Vec_string8_append(arena, &known_types, decl->name);
    }
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!scope_has(&known_types, decl->name)) Vec_string8_append(arena, &known_types, decl->name);
    }
    for (i32 i = 0; i < prog->enums.length; i++) {
        EnumDecl *decl = (EnumDecl *)prog->enums.data[i];
        if (!scope_has(&known_types, decl->name)) Vec_string8_append(arena, &known_types, decl->name);
    }
    return known_types;
}

static void collect_generic_struct_instances(Program *prog, StructDecl *decl, Vec_string8 *out, memops_arena *arena);

/* Flattens a struct's fields the way field access sees them: members of anonymous
   struct/union members belong to the enclosing type's name space. */
static void semantic_collect_named_fields(memops_arena *arena, StructDecl *decl, Vec_voidptr *out) {
    for (i32 i = 0; i < decl->fields.length; i++) {
        Field *field = (Field *)decl->fields.data[i];
        if (field->anon) {
            semantic_collect_named_fields(arena, field->anon, out);
            continue;
        }
        ptr_array_append(arena, out, field);
    }
}

static void semantic_check_program(Program *prog, memops_arena *arena) {
    Scope base = {0};
    base.globals = Vec_string8_reserve(arena, 64);
    base.procs = Vec_string8_reserve(arena, 64);
    base.enum_types = Vec_string8_reserve(arena, 64);
    Vec_string8 structs = Vec_string8_reserve(arena, 64);
    Vec_voidptr type_sites = ptr_array_reserve(arena, 64);
    Vec_voidptr proc_sites = ptr_array_reserve(arena, 64);
    Vec_voidptr global_sites = ptr_array_reserve(arena, 64);

    semantic_add_import_symbols(prog, &base, &structs, arena);

    for (i32 i = 0; i < prog->preprocessor_lines.length; i++) {
        string8 define_name = preprocessor_define_name(arena, prog->preprocessor_lines.data[i]);
        if (define_name.data) {
            Vec_string8_append(arena, &base.globals, define_name);
            semantic_decl_site_add_checked(arena, &global_sites, "duplicate global declaration", define_name, g_source_path, null, 0, 0);
        }
    }
    for (i32 i = 0; i < prog->defines.length; i++) {
        string8 macro_name = string_lit_inner(arena, prog->defines.data[i]);
        if (macro_name.data) {
            Vec_string8_append(arena, &base.globals, macro_name);
            semantic_decl_site_add_checked(arena, &global_sites, "duplicate global declaration", macro_name, g_source_path, null, 0, 0);
        }
    }

    for (i32 i = 0; i < prog->aliases.length; i++) {
        AliasDecl *decl = (AliasDecl *)prog->aliases.data[i];
        semantic_check_ident_available(decl->name, decl->line, decl->col);
        SemanticDeclSite *prev = semantic_decl_site_find(&type_sites, decl->name);
        if (prev) {
            semantic_error_name_dup_path("duplicate type alias", decl->name, decl->source_path, decl->import_chain, decl->line, decl->col, prev->path, prev->import_chain, prev->line, prev->col);
        }
        Vec_string8_append(arena, &structs, decl->name);
        semantic_decl_site_add(arena, &type_sites, decl->name, decl->source_path, decl->import_chain, decl->line, decl->col);
    }

    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        /* Type names go through the same reservation as values and procs.
           `sizeof: struct = {...}` emitted `structdef(sizeof) { ... }` and a
           reflect table named `sizeof_reflect`, none of which is valid C. */
        semantic_check_ident_available(decl->name, decl->line, decl->col);
        SemanticDeclSite *prev = semantic_decl_site_find(&type_sites, decl->name);
        if (prev) {
            semantic_error_name_dup_path("duplicate struct declaration", decl->name, decl->source_path, decl->import_chain, decl->line, decl->col, prev->path, prev->import_chain, prev->line, prev->col);
        }
        Vec_string8_append(arena, &structs, decl->name);
        semantic_decl_site_add(arena, &type_sites, decl->name, decl->source_path, decl->import_chain, decl->line, decl->col);

        /* Anonymous members put their fields in the owner's name space, so the
           flattened list is what has to be free of duplicates. */
        Vec_voidptr named_fields = ptr_array_reserve(arena, decl->fields.length);
        semantic_collect_named_fields(arena, decl, &named_fields);
        for (i32 j = 0; j < named_fields.length; j++) {
            Field *field = (Field *)named_fields.data[j];
            semantic_check_ident_available(field->name, field->line, field->col);
            for (i32 k = 0; k < j; k++) {
                Field *prev = (Field *)named_fields.data[k];
                if (!string8_equals(&prev->name, &field->name)) continue;
                semantic_error_name_dup_path(
                    "duplicate field",
                    field->name,
                    decl->source_path,
                    decl->import_chain,
                    field->line,
                    field->col,
                    decl->source_path,
                    decl->import_chain,
                    prev->line,
                    prev->col
                );
                break;
            }
        }

        string8 reflect_name = concat_name2(arena, decl->name, "_", string8_from_cstr(arena, "reflect"));
        Vec_string8_append(arena, &base.globals, reflect_name);
        semantic_decl_site_add_checked(arena, &global_sites, "duplicate generated global declaration", reflect_name, decl->source_path, decl->import_chain, decl->line, decl->col);
        if (decl->is_generic && !decl->is_external) {
            Vec_string8 instances = Vec_string8_reserve(arena, 4);
            collect_generic_struct_instances(prog, decl, &instances, arena);
            for (i32 j = 0; j < instances.length; j++) {
                string8 concrete_name = string8_reserve(arena, decl->name.length + 1 + instances.data[j].length + 8);
                string8_append_bytes(arena, &concrete_name, decl->name.data, decl->name.length);
                string8_append_cstr(arena, &concrete_name, "_");
                string8_append_bytes(arena, &concrete_name, instances.data[j].data, instances.data[j].length);
                string8_append_cstr(arena, &concrete_name, "_reflect");
                Vec_string8_append(arena, &base.globals, concrete_name);
                semantic_decl_site_add_checked(arena, &global_sites, "duplicate generated global declaration", concrete_name, decl->source_path, decl->import_chain, decl->line, decl->col);
            }
        }
    }

    for (i32 i = 0; i < prog->enums.length; i++) {
        EnumDecl *decl = (EnumDecl *)prog->enums.data[i];
        semantic_check_ident_available(decl->name, decl->line, decl->col);
        SemanticDeclSite *prev = semantic_decl_site_find(&type_sites, decl->name);
        if (prev) {
            semantic_error_name_dup_path("duplicate enum declaration", decl->name, decl->source_path, decl->import_chain, decl->line, decl->col, prev->path, prev->import_chain, prev->line, prev->col);
        }
        Vec_string8_append(arena, &structs, decl->name);
        if (!scope_has(&base.enum_types, decl->name)) {
            Vec_string8_append(arena, &base.enum_types, decl->name);
        }
        semantic_decl_site_add(arena, &type_sites, decl->name, decl->source_path, decl->import_chain, decl->line, decl->col);
        string8 reflect_name = concat_name2(arena, decl->name, "_", string8_from_cstr(arena, "reflect"));
        Vec_string8_append(arena, &base.globals, reflect_name);
        semantic_decl_site_add_checked(arena, &global_sites, "duplicate generated global declaration", reflect_name, decl->source_path, decl->import_chain, decl->line, decl->col);

        Vec_string8 enum_items = Vec_string8_reserve(arena, decl->items.length);
        for (i32 j = 0; j < decl->items.length; j++) {
            EnumItem *item = (EnumItem *)decl->items.data[j];
            if (scope_has(&enum_items, item->name)) {
                i32 prev_line = item->line;
                i32 prev_col = item->col;
                for (i32 k = 0; k < j; k++) {
                    EnumItem *prev = (EnumItem *)decl->items.data[k];
                    if (string8_equals(&prev->name, &item->name)) {
                        prev_line = prev->line;
                        prev_col = prev->col;
                        break;
                    }
                }
                semantic_error_name_dup_path(
                    "duplicate enum item",
                    item->name,
                    decl->source_path,
                    decl->import_chain,
                    item->line,
                    item->col,
                    decl->source_path,
                    decl->import_chain,
                    prev_line,
                    prev_col
                );
            }
            Vec_string8_append(arena, &enum_items, item->name);

            string8 c_name = concat_name2(arena, decl->name, "_", item->name);
            Vec_string8_append(arena, &base.globals, c_name);
            semantic_decl_site_add_checked(arena, &global_sites, "duplicate generated global declaration", c_name, decl->source_path, decl->import_chain, item->line, item->col);
        }
    }

    /* External signatures no longer register their own type names. Walking them
       and adding whatever they mentioned to known_types meant an external
       declaration *declared its types by using them*: a signature could name a
       type that exists nowhere and still check, which is the undeclared-call
       hole wearing a different hat. Foreign types are declared like any other
       now -- `X: struct[external] = {}` for an opaque one. */
    semantic_resolve_proc_angle_types(prog, &structs, arena);

    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        string8 proc_key = proc_semantic_key(arena, decl);
        SemanticDeclSite *prev = semantic_decl_site_find(&proc_sites, proc_key);
        bool compatible_external = prev && proc_is_compatible_external_redeclaration(prog, i, decl);
        if (prev && !compatible_external) {
            semantic_error_name_dup_path("duplicate proc declaration", decl->name, decl->source_path, decl->import_chain, decl->line, decl->col, prev->path, prev->import_chain, prev->line, prev->col);
        }
        if (compatible_external) {
            /* Already declared identically; one entry is enough. */
            continue;
        }
        Vec_string8_append(arena, &base.procs, decl->name);
        semantic_decl_site_add(arena, &proc_sites, proc_key, decl->source_path, decl->import_chain, decl->line, decl->col);
        semantic_decl_site_add_checked(arena, &global_sites, "duplicate proc declaration", proc_key, decl->source_path, decl->import_chain, decl->line, decl->col);
    }

    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *decl = (Stmt *)prog->globals.data[i];
        semantic_check_ident_available(decl->name, decl->line, decl->col);
        SemanticDeclSite *prev = semantic_decl_site_find(&global_sites, decl->name);
        if (prev) {
            semantic_error_name_dup_path("duplicate global declaration", decl->name, decl->source_path, decl->import_chain, decl->line, decl->col, prev->path, prev->import_chain, prev->line, prev->col);
        }
        Vec_string8_append(arena, &base.globals, decl->name);
        semantic_decl_site_add(arena, &global_sites, decl->name, decl->source_path, decl->import_chain, decl->line, decl->col);
    }

    /* Now that every struct and alias is registered, a record that contains
       itself by value can be reported here rather than by clang, which sees it
       as `field has incomplete type` in code the author never wrote. */
    semantic_check_type_cycles(prog, arena);
    semantic_check_enum_ranges(prog);

    /* External signatures no longer register their own type names. Walking them
       and adding whatever they mentioned to known_types meant an external
       declaration *declared its types by using them*: a signature could name a
       type that exists nowhere and still check, which is the undeclared-call
       hole wearing a different hat. Foreign types are declared like any other
       now -- `X: struct[external] = {}` for an opaque one. */

    for (i32 i = 0; i < prog->aliases.length; i++) {
        AliasDecl *decl = (AliasDecl *)prog->aliases.data[i];
        const char *prev_diag_source_path = null;
        const char *prev_diag_import_chain = null;
        diag_push_decl_context(decl->source_path, decl->import_chain, &prev_diag_source_path, &prev_diag_import_chain);
        semantic_check_type(prog, decl->type, &structs, null, decl->source_path);
        diag_pop_decl_context(prev_diag_source_path, prev_diag_import_chain);
    }

    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (decl->is_external) continue;
        const char *prev_diag_source_path = null;
        const char *prev_diag_import_chain = null;
        diag_push_decl_context(decl->source_path, decl->import_chain, &prev_diag_source_path, &prev_diag_import_chain);
        Vec_string8 struct_params = Vec_string8_reserve(arena, 1);
        if (decl->is_generic && decl->type_params.length > 0) {
            for (i32 tp = 0; tp < decl->type_params.length; tp++) { Vec_string8_append(arena, &struct_params, decl->type_params.data[tp]); }
        }
        for (i32 j = 0; j < decl->fields.length; j++) {
            Field *field = (Field *)decl->fields.data[j];
            semantic_check_type(prog, field->type, &structs, &struct_params, decl->source_path);
        }
        diag_pop_decl_context(prev_diag_source_path, prev_diag_import_chain);
    }

    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *decl = (Stmt *)prog->globals.data[i];
        const char *prev_diag_source_path = null;
        const char *prev_diag_import_chain = null;
        diag_push_decl_context(decl->source_path, decl->import_chain, &prev_diag_source_path, &prev_diag_import_chain);
        semantic_check_type(prog, decl->type, &structs, null, decl->source_path);
        if (decl->expr) {
            semantic_check_expr(decl->expr, &base, &structs, null);
        }
        diag_pop_decl_context(prev_diag_source_path, prev_diag_import_chain);
    }

    for (i32 i = 0; i < prog->procs.length; i++) {
        semantic_check_proc(prog, (ProcDecl *)prog->procs.data[i], &base, &structs, arena);
    }
}

typedef struct TypeSub {
    bool has;
    Vec_string8 params;
    Vec_voidptr args; // TypeExpr*
} TypeSub;

static Vec_voidptr *mangle_list_for(string8 mangle);
static TypeExpr *mangle_type_for(memops_arena *arena, string8 mangle);
static TypeSub type_sub_one(memops_arena *arena, string8 param, TypeExpr *arg);

/* The substitution a generic proc's body is lowered under for one instance.

   `bound` is the type the single parameter stands for. For a pattern overload
   like `json_read<Array<T>>` that is the T inside the pattern, not the whole
   `Array<i32>` the instance is named after -- binding the latter would lower the
   parameter `Array<T>` to `Array<Array<i32>>`. Only a proc with several
   parameters consults the list registry; every other shape keeps the path it
   always had. */
static TypeSub proc_sub_for_instance(memops_arena *arena, ProcDecl *decl, string8 mangle, TypeExpr *bound) {
    TypeSub sub = {0};
    if (!decl || decl->type_params.length == 0) return sub;
    if (decl->type_params.length > 1) {
        Vec_voidptr *types = mangle_list_for(mangle);
        if (types) {
            sub.has = true;
            sub.params = Vec_string8_reserve(arena, decl->type_params.length);
            sub.args = ptr_array_reserve(arena, decl->type_params.length);
            for (i32 i = 0; i < decl->type_params.length && i < types->length; i++) {
                Vec_string8_append(arena, &sub.params, decl->type_params.data[i]);
                ptr_array_append(arena, &sub.args, types->data[i]);
            }
            return sub;
        }
    }
    return type_sub_one(arena, decl->type_params.data[0],
                        bound ? bound : mangle_type_for(arena, mangle));
}

static TypeSub proc_call_sub(memops_arena *arena, ProcDecl *decl, Expr *call, TypeExpr *type_arg) {
    TypeSub sub = {0};
    if (!decl || !decl->is_generic) return sub;
    if (decl->type_params.length <= 1) {
        if (!type_arg || decl->type_params.length == 0) return sub;
        return type_sub_one(arena, decl->type_params.data[0], type_arg);
    }
    if (!call || call->type_args.length != decl->type_params.length) return sub;
    sub.has = true;
    sub.params = Vec_string8_reserve(arena, decl->type_params.length);
    sub.args = ptr_array_reserve(arena, decl->type_params.length);
    for (i32 i = 0; i < decl->type_params.length; i++) {
        Vec_string8_append(arena, &sub.params, decl->type_params.data[i]);
        ptr_array_append(arena, &sub.args, call->type_args.data[i]);
    }
    return sub;
}

/* Renders the parameters as written: `T`, or `T, U, V`. */
static void emit_proc_type_params(memops_arena *arena, string8 *out, ProcDecl *decl) {
    for (i32 i = 0; i < decl->type_params.length; i++) {
        if (i > 0) emit_cstr(arena, out, ", ");
        emit_string8(arena, out, decl->type_params.data[i]);
    }
}

static string8 proc_type_params_string(memops_arena *arena, ProcDecl *decl) {
    string8 out = string8_reserve(arena, 16);
    emit_proc_type_params(arena, &out, decl);
    return out;
}

static void emit_struct_type_params(memops_arena *arena, string8 *out, StructDecl *decl) {
    for (i32 i = 0; i < decl->type_params.length; i++) {
        if (i > 0) emit_cstr(arena, out, ", ");
        emit_string8(arena, out, decl->type_params.data[i]);
    }
}

static string8 struct_type_params_string(memops_arena *arena, StructDecl *decl) {
    string8 out = string8_reserve(arena, 16);
    emit_struct_type_params(arena, &out, decl);
    return out;
}

/* The substitution for one instantiation of a generic struct. The instance is
   named by the mangled arguments joined together, which for several parameters
   is what the list registry was recorded against. */
static TypeSub struct_sub_for_instance(memops_arena *arena, StructDecl *decl, string8 mangle) {
    TypeSub sub = {0};
    if (!decl || decl->type_params.length == 0) return sub;
    if (decl->type_params.length > 1) {
        Vec_voidptr *types = mangle_list_for(mangle);
        if (types) {
            sub.has = true;
            sub.params = Vec_string8_reserve(arena, decl->type_params.length);
            sub.args = ptr_array_reserve(arena, decl->type_params.length);
            for (i32 i = 0; i < decl->type_params.length && i < types->length; i++) {
                Vec_string8_append(arena, &sub.params, decl->type_params.data[i]);
                ptr_array_append(arena, &sub.args, types->data[i]);
            }
            return sub;
        }
    }
    return type_sub_one(arena, decl->type_params.data[0], mangle_type_for(arena, mangle));
}

/* Binds a written `Struct<A, B>` to the declaration's parameters, in order. */
static TypeSub struct_sub_from_args(memops_arena *arena, StructDecl *decl, TypeExpr *type) {
    TypeSub sub = {0};
    if (!decl || !type || decl->type_params.length == 0) return sub;
    if (type->args.length != decl->type_params.length) return sub;
    sub.has = true;
    sub.params = Vec_string8_reserve(arena, decl->type_params.length);
    sub.args = ptr_array_reserve(arena, decl->type_params.length);
    for (i32 i = 0; i < decl->type_params.length; i++) {
        Vec_string8_append(arena, &sub.params, decl->type_params.data[i]);
        ptr_array_append(arena, &sub.args, type->args.data[i]);
    }
    return sub;
}

/* The first type parameter of a generic proc, for paths that predate multiple
   parameters and can only be reached with one. */
static string8 proc_first_type_param(ProcDecl *decl) {
    string8 none = {0};
    if (!decl || decl->type_params.length == 0) return none;
    return decl->type_params.data[0];
}

/* The single-parameter case, which is still the overwhelming majority. */
static TypeSub type_sub_one(memops_arena *arena, string8 param, TypeExpr *arg) {
    TypeSub sub = {0};
    sub.has = true;
    sub.params = Vec_string8_reserve(arena, 1);
    sub.args = ptr_array_reserve(arena, 1);
    Vec_string8_append(arena, &sub.params, param);
    ptr_array_append(arena, &sub.args, arg);
    return sub;
}

/* The type a parameter name stands for under this substitution, or null when the
   name is not one of the parameters. */
static TypeExpr *type_sub_lookup(TypeSub sub, string8 name) {
    if (!sub.has) return null;
    for (i32 i = 0; i < sub.params.length && i < sub.args.length; i++) {
        if (string8_equals(&sub.params.data[i], &name)) {
            return (TypeExpr *)sub.args.data[i];
        }
    }
    return null;
}

/* The first substituted type. Only meaningful where exactly one parameter is
   possible, and asserted by the callers that use it. */
static TypeExpr *type_sub_first_arg(TypeSub sub) {
    if (!sub.has || sub.args.length == 0) return null;
    return (TypeExpr *)sub.args.data[0];
}

static bool string8_equals_name(string8 a, string8 b) {
    return string8_equals(&a, &b);
}

static bool string8_is_symbolic_type_name(string8 s) {
    if (!s.data || s.length == 0) return false;
    return s.length == 1 && (s.data[0] >= 'A' && s.data[0] <= 'Z');
}

static string8 type_mangle(memops_arena *arena, TypeExpr *type, TypeSub sub);
static string8 type_mangle_impl(memops_arena *arena, TypeExpr *type, TypeSub sub);
static void mangle_register(memops_arena *arena, string8 mangle, TypeExpr *type);
static TypeExpr *substitute_type_param(memops_arena *arena, TypeExpr *src, string8 param, TypeExpr *arg);
static TypeExpr *substitute_type_sub(memops_arena *arena, TypeExpr *src, TypeSub sub);

static void emit_cstr(memops_arena *arena, string8 *out, const char *cstr) {
    string8_append_cstr(arena, out, cstr);
}

static void emit_string8(memops_arena *arena, string8 *out, string8 s) {
    string8_append_bytes(arena, out, s.data, s.length);
}

static void emit_assign_op(memops_arena *arena, string8 *out, TokenKind op) {
    switch (op) {
        case Token_PlusEqual: emit_cstr(arena, out, " += "); return;
        case Token_MinusEqual: emit_cstr(arena, out, " -= "); return;
        case Token_StarEqual: emit_cstr(arena, out, " *= "); return;
        case Token_SlashEqual: emit_cstr(arena, out, " /= "); return;
        case Token_PercentEqual: emit_cstr(arena, out, " %= "); return;
        case Token_ShlEqual: emit_cstr(arena, out, " <<= "); return;
        case Token_ShrEqual: emit_cstr(arena, out, " >>= "); return;
        case Token_AmpersandEqual: emit_cstr(arena, out, " &= "); return;
        case Token_CaretEqual: emit_cstr(arena, out, " ^= "); return;
        case Token_PipeEqual: emit_cstr(arena, out, " |= "); return;
        default: emit_cstr(arena, out, " = "); return;
    }
}

static void emit_type_qualifiers(memops_arena *arena, string8 *out, TypeExpr *type) {
    if (type->is_const) emit_cstr(arena, out, "const ");
    if (type->is_volatile) emit_cstr(arena, out, "volatile ");
}

/* The reflect runtime's C names all carry an `i_` prefix, because reflection
   tables are emitted unconditionally and so this header's contents land in the C
   global namespace of every I program. `reflect` unprefixed is a plausible
   identifier for third-party C to claim -- it is a GLSL builtin, and any
   vector-math library an engine links is fair game.

   I source keeps the short spelling; this is where the two meet. It covers the
   record types, which C owns because the emitted tables are C initialisers of
   them, and the helpers and constants from std/reflect.i, which are ordinary I
   declarations that would otherwise land in C under their short names.

   The mapping is a closed list rather than a "starts with reflect" rule on
   purpose: a blanket rule would silently rewrite a user's own
   `reflect_normal: proc = { external; }` into `i_reflect_normal` and break the
   link against the C function they meant to bind. tests/run_tests.py asserts
   this list still covers std/reflect.h and std/reflect.i, so it cannot drift.

   The prefix is `i_`, not `__i_`: C reserves every identifier beginning with two
   underscores, or an underscore followed by an uppercase letter, to the
   implementation for any use. */
static const char *g_reflect_runtime_names[] = {
    "reflect",
    "reflect_attr_is_sep",
    "reflect_count_fields_with_attr",
    "reflect_count_fields_with_kind",
    "reflect_cstr_equal",
    "reflect_field",
    "reflect_field_at",
    "reflect_field_const_ptr",
    "reflect_field_copy",
    "reflect_field_copy_by_name",
    "reflect_field_count",
    "reflect_field_end_offset",
    "reflect_field_has_attr",
    "reflect_field_index",
    "reflect_field_info",
    "reflect_field_is_array",
    "reflect_field_is_generic",
    "reflect_field_is_pointer",
    "reflect_field_kind_array",
    "reflect_field_kind_generic",
    "reflect_field_kind_name",
    "reflect_field_kind_proc",
    "reflect_field_kind_ptr",
    "reflect_field_ptr",
    "reflect_field_zero",
    "reflect_field_zero_by_name",
    "reflect_fields",
    "reflect_find_field",
    "reflect_find_field_by_offset",
    "reflect_find_field_containing_offset",
    "reflect_find_field_index",
    "reflect_find_field_info",
    "reflect_find_field_with_attr",
    "reflect_find_field_with_kind",
    "reflect_find_value_by_name",
    "reflect_find_value_by_value",
    "reflect_is_enum",
    "reflect_is_struct",
    "reflect_is_union",
    "reflect_kind",
    "reflect_kind_enum",
    "reflect_kind_name",
    "reflect_kind_struct",
    "reflect_kind_union",
    "reflect_name_from_value",
    "reflect_next_field_with_attr",
    "reflect_next_field_with_kind",
    "reflect_type_kind",
    "reflect_type_kind_name",
    "reflect_value",
    "reflect_value_at",
    "reflect_value_count",
    "reflect_value_from_name",
    "reflect_values",
    "reflect_variant",
};

static bool is_reflect_runtime_name(string8 name) {
    for (u64 i = 0; i < sizeof(g_reflect_runtime_names) / sizeof(g_reflect_runtime_names[0]); i++) {
        if (string8_equals_cstr(&name, g_reflect_runtime_names[i])) return true;
    }
    return false;
}

/* The C spelling of an I-side name. Everything not owned by the reflect runtime
   passes through untouched, so a user's identifiers are never rewritten. */
static string8 reflect_runtime_c_name(memops_arena *arena, string8 name) {
    if (!is_reflect_runtime_name(name)) return name;
    string8 out = string8_reserve(arena, name.length + 2);
    string8_append_cstr(arena, &out, "i_");
    string8_append_bytes(arena, &out, name.data, name.length);
    return out;
}

static void emit_type(memops_arena *arena, string8 *out, TypeExpr *type, TypeSub sub) {
    if (type->kind == Type_Name) {
        TypeExpr *substituted = type_sub_lookup(sub, type->name);
        if (substituted) {
            emit_type_qualifiers(arena, out, type);
            emit_type(arena, out, substituted, (TypeSub){0});
            return;
        }
        emit_type_qualifiers(arena, out, type);
        emit_string8(arena, out, c_ident(arena, reflect_runtime_c_name(arena, type->name)));
        return;
    }
    if (type->kind == Type_Ptr) {
        emit_type(arena, out, type->elem, sub);
        /* Qualifiers on a pointer type apply to the pointer itself, so they land
           after the '*' the way C spells them. */
        emit_cstr(arena, out, " *");
        if (type->is_const) emit_cstr(arena, out, " const");
        if (type->is_volatile) emit_cstr(arena, out, " volatile");
        return;
    }
    if (type->kind == Type_Generic) {
        emit_type_qualifiers(arena, out, type);
        string8 mangle = type_mangle(arena, type, sub);
        emit_string8(arena, out, mangle);
        return;
    }
    if (type->kind == Type_Array) {
        emit_type(arena, out, type->elem, sub);
        return;
    }
    if (type->kind == Type_Proc) {
        if (type->ret_type) {
            emit_type(arena, out, type->ret_type, sub);
        } else {
            emit_cstr(arena, out, "void");
        }
        return;
    }
}

/* Recording the mangle here, where it is produced, means emission can always
   recover the real type behind a monomorph name. Doing it at the collection sites
   instead was too late: consumers looked up mangles before collection ran. */
static string8 type_mangle(memops_arena *arena, TypeExpr *type, TypeSub sub) {
    string8 mangle = type_mangle_impl(arena, type, sub);
    mangle_register(arena, mangle,
                    substitute_type_sub(arena, type, sub));
    return mangle;
}

static string8 type_mangle_impl(memops_arena *arena, TypeExpr *type, TypeSub sub) {
    string8 out = string8_reserve(arena, 64);
    if (type->is_const) {
        emit_cstr(arena, &out, "const_");
    }
    if (type->is_volatile) {
        emit_cstr(arena, &out, "volatile_");
    }
    if (type->kind == Type_Name) {
        TypeExpr *substituted = type_sub_lookup(sub, type->name);
        if (substituted) {
            return type_mangle(arena, substituted, (TypeSub){0});
        }
        emit_string8(arena, &out, type->name);
        return out;
    }
    if (type->kind == Type_Ptr) {
        emit_cstr(arena, &out, "ptr_");
        string8 inner = type_mangle(arena, type->elem, sub);
        emit_string8(arena, &out, inner);
        return out;
    }
    if (type->kind == Type_Generic) {
        emit_string8(arena, &out, type->name);
        emit_cstr(arena, &out, "_");
        for (i32 i = 0; i < type->args.length; i++) {
            if (i > 0) emit_cstr(arena, &out, "_");
            TypeExpr *arg = (TypeExpr *)type->args.data[i];
            string8 inner = type_mangle(arena, arg, sub);
            emit_string8(arena, &out, inner);
        }
        return out;
    }
    if (type->kind == Type_Array) {
        emit_cstr(arena, &out, "array_");
        emit_string8(arena, &out, type->array_count);
        emit_cstr(arena, &out, "_");
        string8 inner = type_mangle(arena, type->elem, sub);
        emit_string8(arena, &out, inner);
        return out;
    }
    if (type->kind == Type_Proc) {
        emit_cstr(arena, &out, "proc_");
        if (type->ret_type) {
            string8 inner = type_mangle(arena, type->ret_type, sub);
            emit_string8(arena, &out, inner);
        } else {
            emit_cstr(arena, &out, "void");
        }
        for (i32 i = 0; i < type->args.length; i++) {
            emit_cstr(arena, &out, "_");
            TypeExpr *arg = (TypeExpr *)type->args.data[i];
            string8 inner = type_mangle(arena, arg, sub);
            emit_string8(arena, &out, inner);
        }
        return out;
    }
    return out;
}

static void format_type_i(memops_arena *arena, string8 *out, TypeExpr *type, TypeSub sub) {
    if (!type) {
        emit_cstr(arena, out, "void");
        return;
    }
    if (type->is_const) {
        emit_cstr(arena, out, "const ");
    }
    if (type->is_volatile) {
        emit_cstr(arena, out, "volatile ");
    }
    if (type->kind == Type_Name) {
        TypeExpr *substituted = type_sub_lookup(sub, type->name);
        if (substituted) {
            format_type_i(arena, out, substituted, (TypeSub){0});
            return;
        }
        emit_c_ident(arena, out, type->name);
        return;
    }
    if (type->kind == Type_Ptr) {
        emit_cstr(arena, out, "*");
        format_type_i(arena, out, type->elem, sub);
        return;
    }
    if (type->kind == Type_Generic) {
        emit_c_ident(arena, out, type->name);
        emit_cstr(arena, out, "<");
        for (i32 i = 0; i < type->args.length; i++) {
            if (i > 0) emit_cstr(arena, out, ", ");
            format_type_i(arena, out, (TypeExpr *)type->args.data[i], sub);
        }
        emit_cstr(arena, out, ">");
        return;
    }
    if (type->kind == Type_Array) {
        emit_cstr(arena, out, "[");
        emit_string8(arena, out, type->array_count);
        emit_cstr(arena, out, "]");
        format_type_i(arena, out, type->elem, sub);
        return;
    }
    if (type->kind == Type_Proc) {
        emit_cstr(arena, out, "proc(");
        for (i32 i = 0; i < type->args.length; i++) {
            if (i > 0) emit_cstr(arena, out, ", ");
            string8 arg_name = {0};
            if (i < type->arg_names.length) arg_name = type->arg_names.data[i];
            if (arg_name.length > 0) {
                emit_string8(arena, out, arg_name);
                emit_cstr(arena, out, ":");
            }
            format_type_i(arena, out, (TypeExpr *)type->args.data[i], sub);
        }
        if (type->is_variadic) {
            if (type->args.length > 0) emit_cstr(arena, out, ", ");
            emit_cstr(arena, out, "...");
        }
        emit_cstr(arena, out, ")->");
        format_type_i(arena, out, type->ret_type, sub);
        return;
    }
}

static void symbol_json_print_string8(string8 s) {
    putchar('"');
    for (u64 i = 0; i < s.length; i++) {
        unsigned char c = s.data[i];
        switch (c) {
            case '\\': printf("\\\\"); break;
            case '"': printf("\\\""); break;
            case '\n': printf("\\n"); break;
            case '\r': printf("\\r"); break;
            case '\t': printf("\\t"); break;
            default:
                if (c < 0x20) printf("\\u%04x", c);
                else putchar(c);
                break;
        }
    }
    putchar('"');
}

static void symbol_json_emit_entry(
    bool *has_item,
    const char *kind,
    string8 name,
    const char *file,
    i32 line,
    i32 col,
    string8 detail,
    i32 source_len
) {
    if (*has_item) printf(",");
    *has_item = true;
    printf("{\"kind\":");
    diag_json_print_cstr(kind);
    printf(",\"name\":");
    symbol_json_print_string8(name);
    printf(",\"file\":");
    diag_json_print_cstr(file ? file : g_source_path);
    printf(",\"line\":%d,\"column\":%d,\"detail\":", line, col);
    symbol_json_print_string8(detail);
    printf(",\"source_len\":%d}", source_len);
}

static string8 symbol_type_i_string(memops_arena *arena, TypeExpr *type) {
    string8 out = string8_reserve(arena, 64);
    format_type_i(arena, &out, type, (TypeSub){0});
    return out;
}

static void symbol_json_emit_typed_entry(
    memops_arena *arena,
    bool *has_item,
    const char *kind,
    string8 name,
    const char *file,
    i32 line,
    i32 col,
    string8 detail,
    i32 source_len,
    TypeExpr *type
) {
    if (*has_item) printf(",");
    *has_item = true;
    printf("{\"kind\":");
    diag_json_print_cstr(kind);
    printf(",\"name\":");
    symbol_json_print_string8(name);
    printf(",\"file\":");
    diag_json_print_cstr(file ? file : g_source_path);
    printf(",\"line\":%d,\"column\":%d,\"detail\":", line, col);
    symbol_json_print_string8(detail);
    printf(",\"source_len\":%d,\"type\":", source_len);
    symbol_json_print_string8(symbol_type_i_string(arena, type));
    printf("}");
}

static void symbol_json_emit_scoped_typed_entry(
    memops_arena *arena,
    bool *has_item,
    const char *kind,
    string8 name,
    const char *file,
    i32 line,
    i32 col,
    string8 detail,
    i32 source_len,
    TypeExpr *type,
    string8 scope
) {
    if (*has_item) printf(",");
    *has_item = true;
    printf("{\"kind\":");
    diag_json_print_cstr(kind);
    printf(",\"name\":");
    symbol_json_print_string8(name);
    printf(",\"file\":");
    diag_json_print_cstr(file ? file : g_source_path);
    printf(",\"line\":%d,\"column\":%d,\"detail\":", line, col);
    symbol_json_print_string8(detail);
    printf(",\"source_len\":%d,\"type\":", source_len);
    symbol_json_print_string8(symbol_type_i_string(arena, type));
    printf(",\"scope\":");
    symbol_json_print_string8(scope);
    printf("}");
}

static void symbol_json_emit_field_entry(
    memops_arena *arena,
    bool *has_item,
    StructDecl *decl,
    Field *field,
    const char *file,
    string8 detail
) {
    if (*has_item) printf(",");
    *has_item = true;
    printf("{\"kind\":\"field\",\"owner\":");
    symbol_json_print_string8(decl->name);
    printf(",\"name\":");
    symbol_json_print_string8(field->name);
    printf(",\"file\":");
    diag_json_print_cstr(file ? file : g_source_path);
    printf(",\"line\":%d,\"column\":%d,\"detail\":", field->line, field->col);
    symbol_json_print_string8(detail);
    printf(",\"attrs\":");
    symbol_json_print_string8(field->attrs);
    printf(",\"type\":");
    symbol_json_print_string8(symbol_type_i_string(arena, field->type));
    printf(",\"type_param\":");
    symbol_json_print_string8(struct_type_params_string(arena, decl));
    printf(",\"source_len\":%d}", (i32)field->name.length);
}

static void symbol_json_emit_struct_entry(
    memops_arena *arena,
    bool *has_item,
    StructDecl *decl,
    string8 detail
) {
    if (*has_item) printf(",");
    *has_item = true;
    printf("{\"kind\":");
    diag_json_print_cstr(decl->is_union ? "union" : "struct");
    printf(",\"name\":");
    symbol_json_print_string8(decl->name);
    printf(",\"file\":");
    diag_json_print_cstr(decl->source_path ? decl->source_path : g_source_path);
    printf(",\"line\":%d,\"column\":%d,\"detail\":", decl->line, decl->col);
    symbol_json_print_string8(detail);
    printf(",\"source_len\":%d,\"type_param\":", (i32)decl->name.length);
    symbol_json_print_string8(struct_type_params_string(arena, decl));
    printf("}");
}

static void symbol_json_emit_alias_entry(
    memops_arena *arena,
    bool *has_item,
    AliasDecl *decl,
    string8 detail
) {
    if (*has_item) printf(",");
    *has_item = true;
    printf("{\"kind\":\"alias\",\"name\":");
    symbol_json_print_string8(decl->name);
    printf(",\"file\":");
    diag_json_print_cstr(decl->source_path ? decl->source_path : g_source_path);
    printf(",\"line\":%d,\"column\":%d,\"detail\":", decl->line, decl->col);
    symbol_json_print_string8(detail);
    printf(",\"source_len\":%d,\"target_type\":", (i32)decl->name.length);
    symbol_json_print_string8(symbol_type_i_string(arena, decl->type));
    TypeExpr *proc_type = null;
    if (decl->type && decl->type->kind == Type_Proc) {
        proc_type = decl->type;
    } else if (decl->type && decl->type->kind == Type_Ptr && decl->type->elem && decl->type->elem->kind == Type_Proc) {
        proc_type = decl->type->elem;
    }
    if (proc_type) {
        printf(",\"params\":[");
        for (i32 i = 0; i < proc_type->args.length; i++) {
            if (i > 0) printf(",");
            TypeExpr *arg_type = (TypeExpr *)proc_type->args.data[i];
            string8 arg_name = {0};
            if (i < proc_type->arg_names.length) {
                arg_name = proc_type->arg_names.data[i];
            }
            char fallback_name[32];
            if (arg_name.length == 0) {
                snprintf(fallback_name, sizeof(fallback_name), "arg%d", i);
                arg_name = string8_from_cstr(arena, fallback_name);
            }
            printf("{\"name\":");
            symbol_json_print_string8(arg_name);
            printf(",\"type\":");
            symbol_json_print_string8(symbol_type_i_string(arena, arg_type));
            printf("}");
        }
        printf("],\"return_type\":");
        symbol_json_print_string8(symbol_type_i_string(arena, proc_type->ret_type));
        printf(",\"variadic\":%s", proc_type->is_variadic ? "true" : "false");
    }
    printf("}");
}

static void symbol_json_emit_enum_member_entry(
    bool *has_item,
    EnumDecl *decl,
    EnumItem *item,
    string8 generated,
    string8 detail
) {
    if (*has_item) printf(",");
    *has_item = true;
    printf("{\"kind\":\"enumMember\",\"name\":");
    symbol_json_print_string8(generated);
    printf(",\"file\":");
    diag_json_print_cstr(decl->source_path ? decl->source_path : g_source_path);
    printf(",\"line\":%d,\"column\":%d,\"detail\":", item->line, item->col);
    symbol_json_print_string8(detail);
    printf(",\"source_len\":%d,\"owner\":", (i32)item->name.length);
    symbol_json_print_string8(decl->name);
    printf(",\"item\":");
    symbol_json_print_string8(item->name);
    printf("}");
}

static void symbol_json_emit_proc_entry(
    memops_arena *arena,
    bool *has_item,
    ProcDecl *decl,
    string8 display_name,
    string8 detail
) {
    if (*has_item) printf(",");
    *has_item = true;
    printf("{\"kind\":\"proc\",\"name\":");
    symbol_json_print_string8(display_name);
    printf(",\"file\":");
    diag_json_print_cstr(decl->source_path ? decl->source_path : g_source_path);
    printf(",\"line\":%d,\"column\":%d,\"detail\":", decl->line, decl->col);
    symbol_json_print_string8(detail);
    printf(",\"source_len\":%d", (i32)display_name.length);
    printf(",\"params\":[");
    for (i32 i = 0; i < decl->params.length; i++) {
        if (i > 0) printf(",");
        Param *param = (Param *)decl->params.data[i];
        string8 param_type = symbol_type_i_string(arena, param->type);
        printf("{\"name\":");
        symbol_json_print_string8(param->name);
        printf(",\"type\":");
        symbol_json_print_string8(param_type);
        printf("}");
    }
    printf("],\"return_type\":");
    symbol_json_print_string8(symbol_type_i_string(arena, decl->ret_type));
    printf(",\"variadic\":%s", decl->is_variadic ? "true" : "false");
    printf(",\"type_param\":");
    symbol_json_print_string8(proc_type_params_string(arena, decl));
    printf(",\"generic_pattern\":");
    if (decl->generic_pattern) {
        symbol_json_print_string8(symbol_type_i_string(arena, decl->generic_pattern));
    } else {
        symbol_json_print_string8((string8){0});
    }
    printf(",\"callconv\":");
    symbol_json_print_string8(decl->callconv);
    printf("}");
}

static string8 symbol_detail_struct(memops_arena *arena, StructDecl *decl) {
    string8 out = string8_reserve(arena, 128);
    emit_string8(arena, &out, decl->name);
    emit_cstr(arena, &out, decl->is_union ? ":union" : ":struct");
    if (decl->is_generic) {
        emit_cstr(arena, &out, "<");
        emit_struct_type_params(arena, &out, decl);
        emit_cstr(arena, &out, ">");
    }
    emit_cstr(arena, &out, " = {");
    return out;
}

static string8 symbol_detail_field(memops_arena *arena, StructDecl *decl, Field *field) {
    string8 out = string8_reserve(arena, 128);
    emit_string8(arena, &out, decl->name);
    emit_cstr(arena, &out, ".");
    emit_string8(arena, &out, field->name);
    emit_cstr(arena, &out, ": ");
    format_type_i(arena, &out, field->type, (TypeSub){0});
    return out;
}

static string8 symbol_detail_enum(memops_arena *arena, EnumDecl *decl) {
    string8 out = string8_reserve(arena, 128);
    emit_string8(arena, &out, decl->name);
    emit_cstr(arena, &out, ":enum = {");
    return out;
}

static string8 symbol_detail_alias(memops_arena *arena, AliasDecl *decl) {
    string8 out = string8_reserve(arena, 128);
    emit_string8(arena, &out, decl->name);
    emit_cstr(arena, &out, ":alias = ");
    format_type_i(arena, &out, decl->type, (TypeSub){0});
    emit_cstr(arena, &out, ";");
    return out;
}

static string8 proc_display_name(memops_arena *arena, ProcDecl *decl) {
    string8 owner = {0};
    string8 member = {0};
    if (!split_qualified_name(decl->name, &owner, &member)) {
        return decl->name;
    }
    string8 out = string8_reserve(arena, decl->name.length + 32);
    emit_string8(arena, &out, owner);
    if (decl->is_generic) {
        emit_cstr(arena, &out, "<");
        emit_proc_type_params(arena, &out, decl);
        emit_cstr(arena, &out, ">");
    }
    emit_string8(arena, &out, member);
    return out;
}

static string8 symbol_detail_proc(memops_arena *arena, ProcDecl *decl) {
    string8 out = string8_reserve(arena, 256);
    emit_string8(arena, &out, proc_display_name(arena, decl));
    emit_cstr(arena, &out, ":proc");
    if (decl->callconv.length > 0) {
        emit_cstr(arena, &out, "[");
        emit_string8(arena, &out, decl->callconv);
        emit_cstr(arena, &out, "]");
    }
    if (decl->is_generic) {
        emit_cstr(arena, &out, "<");
        emit_proc_type_params(arena, &out, decl);
        emit_cstr(arena, &out, ">");
    }
    emit_cstr(arena, &out, "(");
    for (i32 i = 0; i < decl->params.length; i++) {
        if (i > 0) emit_cstr(arena, &out, ", ");
        Param *param = (Param *)decl->params.data[i];
        emit_string8(arena, &out, param->name);
        emit_cstr(arena, &out, ":");
        format_type_i(arena, &out, param->type, (TypeSub){0});
    }
    if (decl->is_variadic) {
        if (decl->params.length > 0) emit_cstr(arena, &out, ", ");
        emit_cstr(arena, &out, "...");
    }
    emit_cstr(arena, &out, ")->");
    format_type_i(arena, &out, decl->ret_type, (TypeSub){0});
    return out;
}

static string8 symbol_detail_global(memops_arena *arena, Stmt *stmt) {
    string8 out = string8_reserve(arena, 128);
    emit_string8(arena, &out, stmt->name);
    emit_cstr(arena, &out, ": ");
    format_type_i(arena, &out, stmt->type, (TypeSub){0});
    return out;
}

static string8 symbol_detail_named_type(memops_arena *arena, string8 name, TypeExpr *type) {
    string8 out = string8_reserve(arena, 128);
    emit_string8(arena, &out, name);
    emit_cstr(arena, &out, ": ");
    format_type_i(arena, &out, type, (TypeSub){0});
    return out;
}

static void emit_symbol_json_stmt_vars(memops_arena *arena, bool *has_item, Stmt *stmt, const char *fallback_path, string8 scope);

static void emit_symbol_json_stmt_list_vars(memops_arena *arena, bool *has_item, Vec_voidptr *stmts, const char *fallback_path, string8 scope) {
    for (i32 i = 0; i < stmts->length; i++) {
        emit_symbol_json_stmt_vars(arena, has_item, (Stmt *)stmts->data[i], fallback_path, scope);
    }
}

static void emit_symbol_json_stmt_vars(memops_arena *arena, bool *has_item, Stmt *stmt, const char *fallback_path, string8 scope) {
    if (!stmt) return;
    const char *path = stmt->source_path ? stmt->source_path : fallback_path;
    if (stmt->kind == Stmt_Var) {
        symbol_json_emit_scoped_typed_entry(
            arena,
            has_item,
            "variable",
            stmt->name,
            path,
            stmt->line,
            stmt->col,
            symbol_detail_named_type(arena, stmt->name, stmt->type),
            (i32)stmt->name.length,
            stmt->type,
            scope
        );
    }
    if (stmt->for_init) {
        emit_symbol_json_stmt_vars(arena, has_item, stmt->for_init, fallback_path, scope);
    }
    if (stmt->for_step) {
        emit_symbol_json_stmt_vars(arena, has_item, stmt->for_step, fallback_path, scope);
    }
    emit_symbol_json_stmt_list_vars(arena, has_item, &stmt->for_body, fallback_path, scope);
    emit_symbol_json_stmt_list_vars(arena, has_item, &stmt->while_body, fallback_path, scope);
    emit_symbol_json_stmt_list_vars(arena, has_item, &stmt->if_then_body, fallback_path, scope);
    emit_symbol_json_stmt_list_vars(arena, has_item, &stmt->if_else_body, fallback_path, scope);
    if (stmt->if_else_if) {
        emit_symbol_json_stmt_vars(arena, has_item, stmt->if_else_if, fallback_path, scope);
    }
    for (i32 i = 0; i < stmt->switch_cases.length; i++) {
        SwitchCase *sc = (SwitchCase *)stmt->switch_cases.data[i];
        emit_symbol_json_stmt_list_vars(arena, has_item, &sc->body, fallback_path, scope);
    }
    emit_symbol_json_stmt_list_vars(arena, has_item, &stmt->switch_default_body, fallback_path, scope);
}

static void emit_symbols_json_items(memops_arena *arena, Program *prog, bool *has_item) {
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        symbol_json_emit_struct_entry(arena, has_item, decl, symbol_detail_struct(arena, decl));
        for (i32 j = 0; j < decl->fields.length; j++) {
            Field *field = (Field *)decl->fields.data[j];
            symbol_json_emit_field_entry(arena, has_item, decl, field, decl->source_path, symbol_detail_field(arena, decl, field));
        }
    }
    for (i32 i = 0; i < prog->enums.length; i++) {
        EnumDecl *decl = (EnumDecl *)prog->enums.data[i];
        symbol_json_emit_entry(has_item, "enum", decl->name, decl->source_path, decl->line, decl->col, symbol_detail_enum(arena, decl), (i32)decl->name.length);
        for (i32 j = 0; j < decl->items.length; j++) {
            EnumItem *item = (EnumItem *)decl->items.data[j];
            string8 generated = string8_reserve(arena, decl->name.length + 1 + item->name.length);
            emit_string8(arena, &generated, decl->name);
            emit_cstr(arena, &generated, "_");
            emit_string8(arena, &generated, item->name);
            string8 detail = string8_reserve(arena, decl->name.length + item->name.length + 16);
            emit_string8(arena, &detail, decl->name);
            emit_cstr(arena, &detail, ".");
            emit_string8(arena, &detail, item->name);
            emit_cstr(arena, &detail, ": enum member");
            symbol_json_emit_enum_member_entry(has_item, decl, item, generated, detail);
        }
    }
    for (i32 i = 0; i < prog->aliases.length; i++) {
        AliasDecl *decl = (AliasDecl *)prog->aliases.data[i];
        symbol_json_emit_alias_entry(arena, has_item, decl, symbol_detail_alias(arena, decl));
    }
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        string8 display_name = proc_display_name(arena, decl);
        symbol_json_emit_proc_entry(arena, has_item, decl, display_name, symbol_detail_proc(arena, decl));
        for (i32 j = 0; j < decl->params.length; j++) {
            Param *param = (Param *)decl->params.data[j];
            symbol_json_emit_scoped_typed_entry(
                arena,
                has_item,
                "parameter",
                param->name,
                decl->source_path,
                param->line,
                param->col,
                symbol_detail_named_type(arena, param->name, param->type),
                (i32)param->name.length,
                param->type,
                display_name
            );
        }
        emit_symbol_json_stmt_list_vars(arena, has_item, &decl->body, decl->source_path, display_name);
    }
    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *stmt = (Stmt *)prog->globals.data[i];
        symbol_json_emit_typed_entry(arena, has_item, "global", stmt->name, stmt->source_path, stmt->line, stmt->col, symbol_detail_global(arena, stmt), (i32)stmt->name.length, stmt->type);
    }
}

static void emit_symbols_json(memops_arena *arena, Program *prog) {
    bool has_item = false;
    printf("[");
    emit_symbols_json_items(arena, prog, &has_item);
    printf("]\n");
}

static void emit_lsp_json(memops_arena *arena, Program *prog) {
    bool has_item = false;
    printf("{\"diagnostics\":[],\"symbols\":[");
    emit_symbols_json_items(arena, prog, &has_item);
    printf("]}\n");
}

static bool array_string8_contains(Vec_string8 *arr, string8 value) {
    for (i32 i = 0; i < arr->length; i++) {
        if (string8_equals(&arr->data[i], &value)) {
            return true;
        }
    }
    return false;
}

static i32 array_string8_index(Vec_string8 *arr, string8 value) {
    for (i32 i = 0; i < arr->length; i++) {
        if (string8_equals(&arr->data[i], &value)) {
            return i;
        }
    }
    return -1;
}

static bool type_is_concrete_under_sub(TypeExpr *type, TypeSub sub);

/* Monomorph instances are tracked by mangled name, but emitting a substituted body
   needs the type the mangle came from: 'Array<*i32>' must emit 'i32 *' for its
   field, not the mangled spelling 'ptr_i32', which is not a C type name. Every
   instance mangle is recorded here as it is produced so emission can recover the
   real type instead of fabricating a Type_Name from the mangle. */
typedef struct MangleBinding {
    string8 mangle;
    TypeExpr *type;
} MangleBinding;

static Vec_voidptr g_mangle_bindings;
static bool g_mangle_bindings_ready = false;
static TypeExpr *clone_type_expr(memops_arena *arena, TypeExpr *src);
static TypeExpr *substitute_type_param(memops_arena *arena, TypeExpr *src, string8 param, TypeExpr *arg);
static TypeExpr *substitute_type_sub(memops_arena *arena, TypeExpr *src, TypeSub sub);

typedef struct MangleListBinding {
    string8 mangle;
    Vec_voidptr types; // TypeExpr*
} MangleListBinding;

static Vec_voidptr g_mangle_list_bindings;
static bool g_mangle_list_ready;

static void mangle_register_list(memops_arena *arena, string8 mangle, Vec_voidptr *types) {
    if (!mangle.data || !types) return;
    if (!g_mangle_list_ready) {
        g_mangle_list_bindings = ptr_array_reserve(arena, 16);
        g_mangle_list_ready = true;
    }
    for (i32 i = 0; i < g_mangle_list_bindings.length; i++) {
        MangleListBinding *b = (MangleListBinding *)g_mangle_list_bindings.data[i];
        if (string8_equals(&b->mangle, &mangle)) return;
    }
    MangleListBinding *entry = memops_arena_push_struct(arena, MangleListBinding);
    entry->mangle = mangle;
    entry->types = ptr_array_reserve(arena, types->length);
    for (i32 i = 0; i < types->length; i++) {
        ptr_array_append(arena, &entry->types, clone_type_expr(arena, (TypeExpr *)types->data[i]));
    }
    ptr_array_append(arena, &g_mangle_list_bindings, entry);
}

static Vec_voidptr *mangle_list_for(string8 mangle) {
    if (!g_mangle_list_ready) return null;
    for (i32 i = 0; i < g_mangle_list_bindings.length; i++) {
        MangleListBinding *b = (MangleListBinding *)g_mangle_list_bindings.data[i];
        if (string8_equals(&b->mangle, &mangle)) return &b->types;
    }
    return null;
}

static void mangle_register(memops_arena *arena, string8 mangle, TypeExpr *type) {
    if (!type || !mangle.data) return;
    if (!g_mangle_bindings_ready) {
        g_mangle_bindings = ptr_array_reserve(arena, 32);
        g_mangle_bindings_ready = true;
    }
    for (i32 i = 0; i < g_mangle_bindings.length; i++) {
        MangleBinding *binding = (MangleBinding *)g_mangle_bindings.data[i];
        if (string8_equals(&binding->mangle, &mangle)) return;
    }
    MangleBinding *entry = memops_arena_push_struct(arena, MangleBinding);
    entry->mangle = mangle;
    entry->type = clone_type_expr(arena, type);
    ptr_array_append(arena, &g_mangle_bindings, entry);
}

/* Falls back to a plain name so anything unregistered behaves as it always has. */
static TypeExpr *mangle_type_for(memops_arena *arena, string8 mangle) {
    if (g_mangle_bindings_ready) {
        for (i32 i = 0; i < g_mangle_bindings.length; i++) {
            MangleBinding *binding = (MangleBinding *)g_mangle_bindings.data[i];
            if (string8_equals(&binding->mangle, &mangle)) {
                return clone_type_expr(arena, binding->type);
            }
        }
    }
    TypeExpr *fallback = type_new(arena, Type_Name);
    fallback->name = mangle;
    return fallback;
}

static void collect_type_instances(TypeExpr *type, string8 base, Vec_string8 *out, memops_arena *arena) {
    if (!type) return;
    if (type->kind == Type_Generic && string8_equals_name(type->name, base)) {
        if (type->args.length >= 1) {
            Vec_voidptr resolved = ptr_array_reserve(arena, type->args.length);
            string8 mangle = string8_reserve(arena, 32);
            bool concrete = true;
            for (i32 i = 0; i < type->args.length; i++) {
                TypeExpr *arg = (TypeExpr *)type->args.data[i];
                if (!type_is_concrete_under_sub(arg, (TypeSub){0})) { concrete = false; break; }
                ptr_array_append(arena, &resolved, arg);
                if (i > 0) string8_append_cstr(arena, &mangle, "_");
                string8 inner = type_mangle(arena, arg, (TypeSub){0});
                string8_append_bytes(arena, &mangle, inner.data, inner.length);
            }
            if (!concrete) return;
            if (type->args.length > 1) {
                mangle_register_list(arena, mangle, &resolved);
            }
            if (!array_string8_contains(out, mangle)) {
                Vec_string8_append(arena, out, mangle);
            }
        }
    }

    if (type->kind == Type_Ptr) {
        collect_type_instances(type->elem, base, out, arena);
    }
    if (type->kind == Type_Array) {
        collect_type_instances(type->elem, base, out, arena);
    }
    if (type->kind == Type_Generic) {
        for (i32 i = 0; i < type->args.length; i++) {
            collect_type_instances((TypeExpr *)type->args.data[i], base, out, arena);
        }
    }
    if (type->kind == Type_Proc) {
        collect_type_instances(type->ret_type, base, out, arena);
        for (i32 i = 0; i < type->args.length; i++) {
            collect_type_instances((TypeExpr *)type->args.data[i], base, out, arena);
        }
    }
}

static void collect_type_instances_sub(TypeExpr *type, string8 base, Vec_string8 *out, memops_arena *arena, TypeSub sub) {
    if (!type) return;
    if (type->kind == Type_Generic && string8_equals_name(type->name, base)) {
        if (type->args.length == 1) {
            TypeExpr *arg = (TypeExpr *)type->args.data[0];
            if (!type_is_concrete_under_sub(arg, sub)) {
                return;
            }
            string8 mangle = type_mangle(arena, arg, sub);
            if (!array_string8_contains(out, mangle)) {
                Vec_string8_append(arena, out, mangle);
            }
        }
    }

    if (type->kind == Type_Ptr) {
        collect_type_instances_sub(type->elem, base, out, arena, sub);
    }
    if (type->kind == Type_Array) {
        collect_type_instances_sub(type->elem, base, out, arena, sub);
    }
    if (type->kind == Type_Generic) {
        for (i32 i = 0; i < type->args.length; i++) {
            collect_type_instances_sub((TypeExpr *)type->args.data[i], base, out, arena, sub);
        }
    }
    if (type->kind == Type_Proc) {
        collect_type_instances_sub(type->ret_type, base, out, arena, sub);
        for (i32 i = 0; i < type->args.length; i++) {
            collect_type_instances_sub((TypeExpr *)type->args.data[i], base, out, arena, sub);
        }
    }
}

static void collect_type_instances_from_stmt(Stmt *s, string8 base, Vec_string8 *out, memops_arena *arena);
static void collect_type_instances_from_expr(Expr *e, string8 base, Vec_string8 *out, memops_arena *arena);
static void collect_type_instances_from_stmt_sub(Stmt *s, string8 base, Vec_string8 *out, memops_arena *arena, TypeSub sub);
static void collect_type_instances_from_expr_sub(Expr *e, string8 base, Vec_string8 *out, memops_arena *arena, TypeSub sub);
static bool type_is_concrete_under_sub(TypeExpr *type, TypeSub sub);

static void collect_type_instances_from_stmt(Stmt *s, string8 base, Vec_string8 *out, memops_arena *arena) {
    if (!s) return;
    if (s->kind == Stmt_Var) {
        collect_type_instances(s->type, base, out, arena);
        collect_type_instances_from_expr(s->expr, base, out, arena);
    } else if (s->kind == Stmt_Assign) {
        collect_type_instances_from_expr(s->lhs, base, out, arena);
        collect_type_instances_from_expr(s->expr, base, out, arena);
    } else if (s->kind == Stmt_Return) {
        collect_type_instances_from_expr(s->expr, base, out, arena);
    } else if (s->kind == Stmt_Expr) {
        collect_type_instances_from_expr(s->expr, base, out, arena);
    } else if (s->kind == Stmt_For) {
        collect_type_instances_from_stmt(s->for_init, base, out, arena);
        collect_type_instances_from_expr(s->for_cond, base, out, arena);
        collect_type_instances_from_stmt(s->for_step, base, out, arena);
        for (i32 i = 0; i < s->for_body.length; i++) {
            collect_type_instances_from_stmt((Stmt *)s->for_body.data[i], base, out, arena);
        }
    } else if (s->kind == Stmt_If) {
        collect_type_instances_from_expr(s->if_cond, base, out, arena);
        for (i32 i = 0; i < s->if_then_body.length; i++) {
            collect_type_instances_from_stmt((Stmt *)s->if_then_body.data[i], base, out, arena);
        }
        if (s->if_else_if) {
            collect_type_instances_from_stmt(s->if_else_if, base, out, arena);
        } else {
            for (i32 i = 0; i < s->if_else_body.length; i++) {
                collect_type_instances_from_stmt((Stmt *)s->if_else_body.data[i], base, out, arena);
            }
        }
    } else if (s->kind == Stmt_While) {
        collect_type_instances_from_expr(s->while_cond, base, out, arena);
        for (i32 i = 0; i < s->while_body.length; i++) {
            collect_type_instances_from_stmt((Stmt *)s->while_body.data[i], base, out, arena);
        }
    } else if (s->kind == Stmt_DoWhile || s->kind == Stmt_Label) {
        for (i32 i = 0; i < s->while_body.length; i++) {
            collect_type_instances_from_stmt((Stmt *)s->while_body.data[i], base, out, arena);
        }
        collect_type_instances_from_expr(s->while_cond, base, out, arena);
    } else if (s->kind == Stmt_Switch) {
        collect_type_instances_from_expr(s->switch_expr, base, out, arena);
        for (i32 i = 0; i < s->switch_cases.length; i++) {
            SwitchCase *sc = (SwitchCase *)s->switch_cases.data[i];
            collect_type_instances_from_expr(sc->expr, base, out, arena);
            for (i32 j = 0; j < sc->body.length; j++) {
                collect_type_instances_from_stmt((Stmt *)sc->body.data[j], base, out, arena);
            }
        }
        for (i32 i = 0; i < s->switch_default_body.length; i++) {
            collect_type_instances_from_stmt((Stmt *)s->switch_default_body.data[i], base, out, arena);
        }
    }
}

static void collect_type_instances_from_expr(Expr *e, string8 base, Vec_string8 *out, memops_arena *arena) {
    if (!e) return;
    if (e->kind == Expr_Call) {
        string8 owner = {0};
        string8 member = {0};
        if (split_qualified_name(e->name, &owner, &member) &&
            string8_equals(&owner, &base) &&
            e->type_args.length == 1) {
            TypeExpr *arg = (TypeExpr *)e->type_args.data[0];
            if (type_is_concrete_under_sub(arg, (TypeSub){0})) {
                string8 mangle = type_mangle(arena, arg, (TypeSub){0});
                if (!array_string8_contains(out, mangle)) {
                    Vec_string8_append(arena, out, mangle);
                }
            }
        }
        for (i32 i = 0; i < e->type_args.length; i++) {
            collect_type_instances((TypeExpr *)e->type_args.data[i], base, out, arena);
        }
        for (i32 i = 0; i < e->args.length; i++) {
            collect_type_instances_from_expr((Expr *)e->args.data[i], base, out, arena);
        }
    } else if (e->kind == Expr_Field) {
        collect_type_instances_from_expr(e->base, base, out, arena);
    } else if (e->kind == Expr_SizeofType || e->kind == Expr_AlignofType) {
        if (e->cast_type) collect_type_instances(e->cast_type, base, out, arena);
        else collect_type_instances_from_expr(e->inner, base, out, arena);
    } else if (e->kind == Expr_ZeroInit) {
        collect_type_instances(e->cast_type, base, out, arena);
    } else if (e->kind == Expr_InitList) {
        collect_type_instances(e->cast_type, base, out, arena);
        for (i32 i = 0; i < e->args.length; i++) {
            if (e->designator_kinds.data[i] == InitDesignator_Index) {
                collect_type_instances_from_expr((Expr *)e->designators.data[i], base, out, arena);
            }
            collect_type_instances_from_expr((Expr *)e->args.data[i], base, out, arena);
        }
    } else if (e->kind == Expr_CompoundInit) {
        collect_type_instances(e->cast_type, base, out, arena);
        collect_type_instances_from_expr(e->inner, base, out, arena);
    } else if (e->kind == Expr_Addr) {
        collect_type_instances_from_expr(e->inner, base, out, arena);
    } else if (e->kind == Expr_Unary) {
        collect_type_instances_from_expr(e->inner, base, out, arena);
    } else if (e->kind == Expr_Binary) {
        collect_type_instances_from_expr(e->left, base, out, arena);
        collect_type_instances_from_expr(e->right, base, out, arena);
    } else if (e->kind == Expr_Ternary) {
        collect_type_instances_from_expr(e->left, base, out, arena);
        collect_type_instances_from_expr(e->right, base, out, arena);
        collect_type_instances_from_expr(e->third, base, out, arena);
    } else if (e->kind == Expr_Index) {
        collect_type_instances_from_expr(e->base, base, out, arena);
        collect_type_instances_from_expr(e->index_expr, base, out, arena);
    } else if (e->kind == Expr_Cast) {
        collect_type_instances(e->cast_type, base, out, arena);
        collect_type_instances_from_expr(e->inner, base, out, arena);
    }
}

static void collect_type_instances_from_stmt_sub(Stmt *s, string8 base, Vec_string8 *out, memops_arena *arena, TypeSub sub) {
    if (!s) return;
    if (s->kind == Stmt_Var) {
        collect_type_instances_sub(s->type, base, out, arena, sub);
        collect_type_instances_from_expr_sub(s->expr, base, out, arena, sub);
    } else if (s->kind == Stmt_Assign) {
        collect_type_instances_from_expr_sub(s->lhs, base, out, arena, sub);
        collect_type_instances_from_expr_sub(s->expr, base, out, arena, sub);
    } else if (s->kind == Stmt_Return) {
        collect_type_instances_from_expr_sub(s->expr, base, out, arena, sub);
    } else if (s->kind == Stmt_Expr) {
        collect_type_instances_from_expr_sub(s->expr, base, out, arena, sub);
    } else if (s->kind == Stmt_For) {
        collect_type_instances_from_stmt_sub(s->for_init, base, out, arena, sub);
        collect_type_instances_from_expr_sub(s->for_cond, base, out, arena, sub);
        collect_type_instances_from_stmt_sub(s->for_step, base, out, arena, sub);
        for (i32 i = 0; i < s->for_body.length; i++) {
            collect_type_instances_from_stmt_sub((Stmt *)s->for_body.data[i], base, out, arena, sub);
        }
    } else if (s->kind == Stmt_If) {
        collect_type_instances_from_expr_sub(s->if_cond, base, out, arena, sub);
        for (i32 i = 0; i < s->if_then_body.length; i++) {
            collect_type_instances_from_stmt_sub((Stmt *)s->if_then_body.data[i], base, out, arena, sub);
        }
        if (s->if_else_if) {
            collect_type_instances_from_stmt_sub(s->if_else_if, base, out, arena, sub);
        } else {
            for (i32 i = 0; i < s->if_else_body.length; i++) {
                collect_type_instances_from_stmt_sub((Stmt *)s->if_else_body.data[i], base, out, arena, sub);
            }
        }
    } else if (s->kind == Stmt_While) {
        collect_type_instances_from_expr_sub(s->while_cond, base, out, arena, sub);
        for (i32 i = 0; i < s->while_body.length; i++) {
            collect_type_instances_from_stmt_sub((Stmt *)s->while_body.data[i], base, out, arena, sub);
        }
    } else if (s->kind == Stmt_DoWhile || s->kind == Stmt_Label) {
        for (i32 i = 0; i < s->while_body.length; i++) {
            collect_type_instances_from_stmt_sub((Stmt *)s->while_body.data[i], base, out, arena, sub);
        }
        collect_type_instances_from_expr_sub(s->while_cond, base, out, arena, sub);
    } else if (s->kind == Stmt_Switch) {
        collect_type_instances_from_expr_sub(s->switch_expr, base, out, arena, sub);
        for (i32 i = 0; i < s->switch_cases.length; i++) {
            SwitchCase *sc = (SwitchCase *)s->switch_cases.data[i];
            collect_type_instances_from_expr_sub(sc->expr, base, out, arena, sub);
            for (i32 j = 0; j < sc->body.length; j++) {
                collect_type_instances_from_stmt_sub((Stmt *)sc->body.data[j], base, out, arena, sub);
            }
        }
        for (i32 i = 0; i < s->switch_default_body.length; i++) {
            collect_type_instances_from_stmt_sub((Stmt *)s->switch_default_body.data[i], base, out, arena, sub);
        }
    }
}

static void collect_type_instances_from_expr_sub(Expr *e, string8 base, Vec_string8 *out, memops_arena *arena, TypeSub sub) {
    if (!e) return;
    if (e->kind == Expr_Call) {
        string8 owner = {0};
        string8 member = {0};
        if (split_qualified_name(e->name, &owner, &member) &&
            string8_equals(&owner, &base) &&
            e->type_args.length == 1) {
            TypeExpr *arg = (TypeExpr *)e->type_args.data[0];
            if (type_is_concrete_under_sub(arg, sub)) {
                string8 mangle = type_mangle(arena, arg, sub);
                if (!array_string8_contains(out, mangle)) {
                    Vec_string8_append(arena, out, mangle);
                }
            }
        }
        for (i32 i = 0; i < e->type_args.length; i++) {
            collect_type_instances_sub((TypeExpr *)e->type_args.data[i], base, out, arena, sub);
        }
        for (i32 i = 0; i < e->args.length; i++) {
            collect_type_instances_from_expr_sub((Expr *)e->args.data[i], base, out, arena, sub);
        }
    } else if (e->kind == Expr_Field) {
        collect_type_instances_from_expr_sub(e->base, base, out, arena, sub);
    } else if (e->kind == Expr_SizeofType || e->kind == Expr_AlignofType) {
        if (e->cast_type) collect_type_instances_sub(e->cast_type, base, out, arena, sub);
        else collect_type_instances_from_expr_sub(e->inner, base, out, arena, sub);
    } else if (e->kind == Expr_ZeroInit) {
        collect_type_instances_sub(e->cast_type, base, out, arena, sub);
    } else if (e->kind == Expr_InitList) {
        collect_type_instances_sub(e->cast_type, base, out, arena, sub);
        for (i32 i = 0; i < e->args.length; i++) {
            if (e->designator_kinds.data[i] == InitDesignator_Index) {
                collect_type_instances_from_expr_sub((Expr *)e->designators.data[i], base, out, arena, sub);
            }
            collect_type_instances_from_expr_sub((Expr *)e->args.data[i], base, out, arena, sub);
        }
    } else if (e->kind == Expr_CompoundInit) {
        collect_type_instances_sub(e->cast_type, base, out, arena, sub);
        collect_type_instances_from_expr_sub(e->inner, base, out, arena, sub);
    } else if (e->kind == Expr_Addr) {
        collect_type_instances_from_expr_sub(e->inner, base, out, arena, sub);
    } else if (e->kind == Expr_Unary) {
        collect_type_instances_from_expr_sub(e->inner, base, out, arena, sub);
    } else if (e->kind == Expr_Binary) {
        collect_type_instances_from_expr_sub(e->left, base, out, arena, sub);
        collect_type_instances_from_expr_sub(e->right, base, out, arena, sub);
    } else if (e->kind == Expr_Ternary) {
        collect_type_instances_from_expr_sub(e->left, base, out, arena, sub);
        collect_type_instances_from_expr_sub(e->right, base, out, arena, sub);
        collect_type_instances_from_expr_sub(e->third, base, out, arena, sub);
    } else if (e->kind == Expr_Index) {
        collect_type_instances_from_expr_sub(e->base, base, out, arena, sub);
        collect_type_instances_from_expr_sub(e->index_expr, base, out, arena, sub);
    } else if (e->kind == Expr_Cast) {
        collect_type_instances_sub(e->cast_type, base, out, arena, sub);
        collect_type_instances_from_expr_sub(e->inner, base, out, arena, sub);
    }
}

typedef struct GenericInstanceSite GenericInstanceSite;
static void collect_generic_proc_instances_with_sites(
    Program *prog,
    ProcDecl *decl,
    Vec_string8 *out,
    Vec_string8 *out_subs,
    Vec_voidptr *constraint_sites,
    memops_arena *arena
);

static void collect_generic_struct_instances(Program *prog, StructDecl *decl, Vec_string8 *out, memops_arena *arena) {
    static bool collecting_nested_dependencies = false;
    for (i32 i = 0; i < prog->aliases.length; i++) {
        AliasDecl *a = (AliasDecl *)prog->aliases.data[i];
        collect_type_instances(a->type, decl->name, out, arena);
    }

    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *s = (Stmt *)prog->globals.data[i];
        collect_type_instances(s->type, decl->name, out, arena);
    }

    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *s = (StructDecl *)prog->structs.data[i];
        for (i32 j = 0; j < s->fields.length; j++) {
            Field *f = (Field *)s->fields.data[j];
            collect_type_instances(f->type, decl->name, out, arena);
        }
        if (s->is_generic && !string8_equals(&s->name, &decl->name) && !collecting_nested_dependencies) {
            Vec_string8 owner_instances = Vec_string8_reserve(arena, 4);
            collecting_nested_dependencies = true;
            collect_generic_struct_instances(prog, s, &owner_instances, arena);
            collecting_nested_dependencies = false;
            for (i32 j = 0; j < owner_instances.length; j++) {

                TypeSub sub = {0};
                sub = struct_sub_for_instance(arena, s, owner_instances.data[j]);
                for (i32 k = 0; k < s->fields.length; k++) {
                    Field *f = (Field *)s->fields.data[k];
                    collect_type_instances_sub(f->type, decl->name, out, arena, sub);
                }
            }
        }
    }

    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *p = (ProcDecl *)prog->procs.data[i];
        for (i32 j = 0; j < p->params.length; j++) {
            Param *param = (Param *)p->params.data[j];
            collect_type_instances(param->type, decl->name, out, arena);
        }
        collect_type_instances(p->ret_type, decl->name, out, arena);
        for (i32 j = 0; j < p->body.length; j++) {
            Stmt *s = (Stmt *)p->body.data[j];
            collect_type_instances_from_stmt(s, decl->name, out, arena);
        }
        if (p->is_generic) {
            Vec_string8 proc_instances = Vec_string8_reserve(arena, 4);
            Vec_voidptr instance_sites = ptr_array_reserve(arena, 4);
            collect_generic_proc_instances_with_sites(prog, p, &proc_instances, null, &instance_sites, arena);
            for (i32 j = 0; j < proc_instances.length; j++) {
                TypeExpr *arg = mangle_type_for(arena, proc_instances.data[j]);
                TypeSub sub = proc_sub_for_instance(arena, p, proc_instances.data[j], arg);
                for (i32 k = 0; k < p->params.length; k++) {
                    Param *param = (Param *)p->params.data[k];
                    collect_type_instances_sub(param->type, decl->name, out, arena, sub);
                }
                collect_type_instances_sub(p->ret_type, decl->name, out, arena, sub);
                for (i32 k = 0; k < p->body.length; k++) {
                    Stmt *s = (Stmt *)p->body.data[k];
                    collect_type_instances_from_stmt_sub(s, decl->name, out, arena, sub);
                }
            }
        }
    }
}

typedef struct GenericProcEntry {
    ProcDecl *decl;
    Vec_string8 instances; // mangled concrete type args
    Vec_string8 sub_instances; // mangled type params used inside the generic body
} GenericProcEntry;

typedef struct GenericInstanceSite {
    string8 proc_name;
    string8 mangle;
    const char *source_path;
    i32 line;
    i32 col;
} GenericInstanceSite;

static GenericInstanceSite *generic_instance_site_new(
    memops_arena *arena,
    string8 proc_name,
    string8 mangle,
    const char *source_path,
    i32 line,
    i32 col
) {
    GenericInstanceSite *site = memops_arena_push_struct(arena, GenericInstanceSite);
    memset(site, 0, sizeof(*site));
    site->proc_name = proc_name;
    site->mangle = mangle;
    site->source_path = source_path;
    site->line = line;
    site->col = col;
    return site;
}

static GenericInstanceSite *generic_instance_site_find(Vec_voidptr *sites, string8 proc_name, string8 mangle) {
    for (i32 i = 0; i < sites->length; i++) {
        GenericInstanceSite *site = (GenericInstanceSite *)sites->data[i];
        if (string8_equals(&site->proc_name, &proc_name) && string8_equals(&site->mangle, &mangle)) {
            return site;
        }
    }
    return null;
}

static GenericProcEntry *generic_entry_from_decl(Vec_voidptr *entries, ProcDecl *decl) {
    for (i32 i = 0; i < entries->length; i++) {
        GenericProcEntry *e = (GenericProcEntry *)entries->data[i];
        if (e->decl == decl) return e;
    }
    return null;
}

static bool generic_pattern_bind(memops_arena *arena, ProcDecl *decl, TypeExpr *pattern, TypeExpr *arg, TypeExpr **bound) {
    if (!decl || !pattern || !arg) return false;
    if (pattern->kind == Type_Name && string8_equals_name(pattern->name, proc_first_type_param(decl))) {
        if (!*bound) {
            *bound = arg;
            return true;
        }
        string8 a = type_mangle(arena, *bound, (TypeSub){0});
        string8 b = type_mangle(arena, arg, (TypeSub){0});
        return string8_equals(&a, &b);
    }
    if (pattern->kind != arg->kind) return false;
    if (pattern->kind == Type_Name) {
        return string8_equals(&pattern->name, &arg->name);
    }
    if (pattern->kind == Type_Ptr || pattern->kind == Type_Array) {
        return generic_pattern_bind(arena, decl, pattern->elem, arg->elem, bound);
    }
    if (pattern->kind == Type_Generic) {
        if (!string8_equals(&pattern->name, &arg->name) || pattern->args.length != arg->args.length) return false;
        for (i32 i = 0; i < pattern->args.length; i++) {
            if (!generic_pattern_bind(arena, decl, (TypeExpr *)pattern->args.data[i], (TypeExpr *)arg->args.data[i], bound)) return false;
        }
        return true;
    }
    if (pattern->kind == Type_Proc) {
        if (pattern->args.length != arg->args.length || pattern->is_variadic != arg->is_variadic) return false;
        if (!generic_pattern_bind(arena, decl, pattern->ret_type, arg->ret_type, bound)) return false;
        for (i32 i = 0; i < pattern->args.length; i++) {
            if (!generic_pattern_bind(arena, decl, (TypeExpr *)pattern->args.data[i], (TypeExpr *)arg->args.data[i], bound)) return false;
        }
        return true;
    }
    return false;
}

static bool generic_proc_match_type_arg(memops_arena *arena, ProcDecl *decl, TypeExpr *arg, TypeExpr **bound) {
    if (!decl || !arg) return false;
    if (!decl->generic_pattern) {
        if (bound) *bound = arg;
        return true;
    }
    TypeExpr *match = null;
    if (!generic_pattern_bind(arena, decl, decl->generic_pattern, arg, &match) || !match) return false;
    if (bound) *bound = match;
    return true;
}

static TypeExpr *substitute_type_param(memops_arena *arena, TypeExpr *src, string8 param, TypeExpr *arg);
static TypeExpr *substitute_type_sub(memops_arena *arena, TypeExpr *src, TypeSub sub);

static GenericProcEntry *generic_entry_from_call(
    Vec_voidptr *entries,
    string8 name,
    TypeExpr *arg,
    TypeSub sub,
    TypeExpr **out_bound,
    memops_arena *arena
) {
    if (out_bound) *out_bound = null;
    TypeExpr *resolved_arg = substitute_type_sub(arena, arg, sub);
    for (i32 i = 0; i < entries->length; i++) {
        GenericProcEntry *e = (GenericProcEntry *)entries->data[i];
        if (!string8_equals(&e->decl->name, &name)) continue;
        TypeExpr *bound = null;
        if (generic_proc_match_type_arg(arena, e->decl, resolved_arg, &bound)) {
            if (out_bound) *out_bound = bound;
            return e;
        }
    }
    return null;
}

static bool type_is_concrete_under_sub(TypeExpr *type, TypeSub sub) {
    if (!type) return false;
    if (type->kind == Type_Name) {
        TypeExpr *substituted = type_sub_lookup(sub, type->name);
        if (substituted) {
            return type_is_concrete_under_sub(substituted, (TypeSub){0});
        }
        return !string8_is_symbolic_type_name(type->name);
    }
    if (type->kind == Type_Ptr) {
        return type_is_concrete_under_sub(type->elem, sub);
    }
    if (type->kind == Type_Array) {
        return type_is_concrete_under_sub(type->elem, sub);
    }
    if (type->kind == Type_Generic) {
        for (i32 i = 0; i < type->args.length; i++) {
            if (!type_is_concrete_under_sub((TypeExpr *)type->args.data[i], sub)) return false;
        }
        return true;
    }
    if (type->kind == Type_Proc) {
        if (type->ret_type && !type_is_concrete_under_sub(type->ret_type, sub)) return false;
        for (i32 i = 0; i < type->args.length; i++) {
            if (!type_is_concrete_under_sub((TypeExpr *)type->args.data[i], sub)) return false;
        }
        return true;
    }
    return false;
}

static bool generic_entry_add_instance(memops_arena *arena, GenericProcEntry *entry, string8 mangle, string8 sub_mangle) {
    if (array_string8_contains(&entry->instances, mangle)) return false;
    Vec_string8_append(arena, &entry->instances, mangle);
    Vec_string8_append(arena, &entry->sub_instances, sub_mangle);
    return true;
}

static bool collect_generic_calls_from_expr(
    Expr *e,
    TypeSub sub,
    Vec_voidptr *entries,
    Vec_voidptr *constraint_sites,
    const char *source_path,
    memops_arena *arena
) {
    bool changed = false;
    if (!e) return false;
    if (e->kind == Expr_Call) {
        if (e->type_args.length > 1) {
            /* Several type arguments: the instance is keyed by all of them
               joined, which is also the suffix the symbol takes. The types are
               registered against that key so the substitution can be rebuilt
               wherever only the mangle travelled. */
            GenericProcEntry *target = null;
            for (i32 t = 0; t < entries->length; t++) {
                GenericProcEntry *entry = (GenericProcEntry *)entries->data[t];
                if (string8_equals(&entry->decl->name, &e->name) &&
                    entry->decl->type_params.length == e->type_args.length) {
                    target = entry;
                    break;
                }
            }
            if (target) {
                bool all_concrete = true;
                Vec_voidptr resolved = ptr_array_reserve(arena, e->type_args.length);
                string8 joined = string8_reserve(arena, 32);
                for (i32 t = 0; t < e->type_args.length; t++) {
                    TypeExpr *arg = (TypeExpr *)e->type_args.data[t];
                    if (!type_is_concrete_under_sub(arg, sub)) { all_concrete = false; break; }
                    TypeExpr *concrete = substitute_type_sub(arena, arg, sub);
                    ptr_array_append(arena, &resolved, concrete);
                    if (t > 0) string8_append_cstr(arena, &joined, "_");
                    string8 m = type_mangle(arena, arg, sub);
                    string8_append_bytes(arena, &joined, m.data, m.length);
                }
                if (all_concrete) {
                    mangle_register_list(arena, joined, &resolved);
                    if (generic_entry_add_instance(arena, target, joined, joined)) {
                        changed = true;
                    }
                    if (!generic_instance_site_find(constraint_sites, target->decl->name, joined)) {
                        ptr_array_append(
                            arena,
                            constraint_sites,
                            generic_instance_site_new(arena, target->decl->name, joined, source_path, e->line, e->col)
                        );
                    }
                }
            }
        }
        if (e->type_args.length == 1) {
            TypeExpr *bound = null;
            GenericProcEntry *target = generic_entry_from_call(entries, e->name, (TypeExpr *)e->type_args.data[0], sub, &bound, arena);
            if (target) {
                TypeExpr *arg = (TypeExpr *)e->type_args.data[0];
                if (type_is_concrete_under_sub(arg, sub)) {
                    string8 mangle = type_mangle(arena, arg, sub);
                    string8 sub_mangle = type_mangle(arena, bound ? bound : arg, (TypeSub){0});
                    if (generic_entry_add_instance(arena, target, mangle, sub_mangle)) {
                        changed = true;
                    }
                    if (!generic_instance_site_find(constraint_sites, target->decl->name, mangle)) {
                        ptr_array_append(
                            arena,
                            constraint_sites,
                            generic_instance_site_new(arena, target->decl->name, mangle, source_path, e->line, e->col)
                        );
                    }
                }
            }
        }
        for (i32 i = 0; i < e->args.length; i++) {
            if (collect_generic_calls_from_expr((Expr *)e->args.data[i], sub, entries, constraint_sites, source_path, arena)) changed = true;
        }
        return changed;
    }
    if (e->kind == Expr_Addr || e->kind == Expr_Cast || e->kind == Expr_Unary) {
        return collect_generic_calls_from_expr(e->inner, sub, entries, constraint_sites, source_path, arena);
    }
    if (e->kind == Expr_InitList) {
        for (i32 i = 0; i < e->args.length; i++) {
            if (e->designator_kinds.data[i] == InitDesignator_Index) {
                if (collect_generic_calls_from_expr((Expr *)e->designators.data[i], sub, entries, constraint_sites, source_path, arena)) changed = true;
            }
            if (collect_generic_calls_from_expr((Expr *)e->args.data[i], sub, entries, constraint_sites, source_path, arena)) changed = true;
        }
        return changed;
    }
    if (e->kind == Expr_CompoundInit) {
        return collect_generic_calls_from_expr(e->inner, sub, entries, constraint_sites, source_path, arena);
    }
    if (e->kind == Expr_Field) {
        return collect_generic_calls_from_expr(e->base, sub, entries, constraint_sites, source_path, arena);
    }
    if (e->kind == Expr_Index) {
        bool c0 = collect_generic_calls_from_expr(e->base, sub, entries, constraint_sites, source_path, arena);
        bool c1 = collect_generic_calls_from_expr(e->index_expr, sub, entries, constraint_sites, source_path, arena);
        return c0 || c1;
    }
    if (e->kind == Expr_Binary) {
        bool c0 = collect_generic_calls_from_expr(e->left, sub, entries, constraint_sites, source_path, arena);
        bool c1 = collect_generic_calls_from_expr(e->right, sub, entries, constraint_sites, source_path, arena);
        return c0 || c1;
    }
    if (e->kind == Expr_Ternary) {
        bool c0 = collect_generic_calls_from_expr(e->left, sub, entries, constraint_sites, source_path, arena);
        bool c1 = collect_generic_calls_from_expr(e->right, sub, entries, constraint_sites, source_path, arena);
        bool c2 = collect_generic_calls_from_expr(e->third, sub, entries, constraint_sites, source_path, arena);
        return c0 || c1 || c2;
    }
    return false;
}

static bool collect_generic_calls_from_stmt(
    Stmt *s,
    TypeSub sub,
    Vec_voidptr *entries,
    Vec_voidptr *constraint_sites,
    memops_arena *arena
) {
    if (!s) return false;
    bool changed = false;
    const char *source_path = s->source_path;
    if (s->kind == Stmt_Var) {
        if (collect_generic_calls_from_expr(s->expr, sub, entries, constraint_sites, source_path, arena)) changed = true;
        return changed;
    }
    if (s->kind == Stmt_Assign) {
        if (collect_generic_calls_from_expr(s->lhs, sub, entries, constraint_sites, source_path, arena)) changed = true;
        if (collect_generic_calls_from_expr(s->expr, sub, entries, constraint_sites, source_path, arena)) changed = true;
        return changed;
    }
    if (s->kind == Stmt_Return || s->kind == Stmt_Expr) {
        return collect_generic_calls_from_expr(s->expr, sub, entries, constraint_sites, source_path, arena);
    }
    if (s->kind == Stmt_For) {
        if (collect_generic_calls_from_stmt(s->for_init, sub, entries, constraint_sites, arena)) changed = true;
        if (collect_generic_calls_from_expr(s->for_cond, sub, entries, constraint_sites, source_path, arena)) changed = true;
        if (collect_generic_calls_from_stmt(s->for_step, sub, entries, constraint_sites, arena)) changed = true;
        for (i32 i = 0; i < s->for_body.length; i++) {
            if (collect_generic_calls_from_stmt((Stmt *)s->for_body.data[i], sub, entries, constraint_sites, arena)) changed = true;
        }
        return changed;
    }
    if (s->kind == Stmt_If) {
        if (collect_generic_calls_from_expr(s->if_cond, sub, entries, constraint_sites, source_path, arena)) changed = true;
        for (i32 i = 0; i < s->if_then_body.length; i++) {
            if (collect_generic_calls_from_stmt((Stmt *)s->if_then_body.data[i], sub, entries, constraint_sites, arena)) changed = true;
        }
        if (s->if_else_if) {
            if (collect_generic_calls_from_stmt(s->if_else_if, sub, entries, constraint_sites, arena)) changed = true;
        } else {
            for (i32 i = 0; i < s->if_else_body.length; i++) {
                if (collect_generic_calls_from_stmt((Stmt *)s->if_else_body.data[i], sub, entries, constraint_sites, arena)) changed = true;
            }
        }
        return changed;
    }
    if (s->kind == Stmt_While) {
        if (collect_generic_calls_from_expr(s->while_cond, sub, entries, constraint_sites, source_path, arena)) changed = true;
        for (i32 i = 0; i < s->while_body.length; i++) {
            if (collect_generic_calls_from_stmt((Stmt *)s->while_body.data[i], sub, entries, constraint_sites, arena)) changed = true;
        }
        return changed;
    }
    if (s->kind == Stmt_DoWhile || s->kind == Stmt_Label) {
        for (i32 i = 0; i < s->while_body.length; i++) {
            if (collect_generic_calls_from_stmt((Stmt *)s->while_body.data[i], sub, entries, constraint_sites, arena)) changed = true;
        }
        if (collect_generic_calls_from_expr(s->while_cond, sub, entries, constraint_sites, source_path, arena)) changed = true;
        return changed;
    }
    if (s->kind == Stmt_Switch) {
        if (collect_generic_calls_from_expr(s->switch_expr, sub, entries, constraint_sites, source_path, arena)) changed = true;
        for (i32 i = 0; i < s->switch_cases.length; i++) {
            SwitchCase *sc = (SwitchCase *)s->switch_cases.data[i];
            if (collect_generic_calls_from_expr(sc->expr, sub, entries, constraint_sites, source_path, arena)) changed = true;
            for (i32 j = 0; j < sc->body.length; j++) {
                if (collect_generic_calls_from_stmt((Stmt *)sc->body.data[j], sub, entries, constraint_sites, arena)) changed = true;
            }
        }
        for (i32 i = 0; i < s->switch_default_body.length; i++) {
            if (collect_generic_calls_from_stmt((Stmt *)s->switch_default_body.data[i], sub, entries, constraint_sites, arena)) changed = true;
        }
        return changed;
    }
    return false;
}

/* The instantiation closure is a property of the whole program, not of the decl
   being asked about: decl only selects which results come back at the end. Running
   it per decl meant repeating a whole-program fixpoint once per generic proc, which
   on a project pulling in the std containers is 135 times per pass. It is computed
   once and shared instead.

   The cache is keyed on anything that can change the answer: the proc list growing
   (imports) and the printfmt pass rewriting bodies, which can introduce new generic
   calls. */
static Vec_voidptr g_generic_entries;
static Vec_voidptr g_generic_sites;
static bool g_generic_cache_ready = false;
static i32 g_generic_cache_procs = -1;
static u64 g_generic_cache_generation = 0;
static u64 g_program_generation = 0;

static void collect_generic_proc_instances_build(Program *prog, memops_arena *arena);

static Vec_voidptr *generic_entries_get(Program *prog, memops_arena *arena) {
    if (!g_generic_cache_ready ||
        g_generic_cache_procs != prog->procs.length ||
        g_generic_cache_generation != g_program_generation) {
        collect_generic_proc_instances_build(prog, arena);
        g_generic_cache_ready = true;
        g_generic_cache_procs = prog->procs.length;
        g_generic_cache_generation = g_program_generation;
    }
    return &g_generic_entries;
}

static void collect_generic_proc_instances_with_sites(
    Program *prog,
    ProcDecl *decl,
    Vec_string8 *out,
    Vec_string8 *out_subs,
    Vec_voidptr *constraint_sites,
    memops_arena *arena
) {
    Vec_voidptr *cached = generic_entries_get(prog, arena);
    if (constraint_sites) {
        for (i32 i = 0; i < g_generic_sites.length; i++) {
            ptr_array_append(arena, constraint_sites, g_generic_sites.data[i]);
        }
    }
    GenericProcEntry *target = generic_entry_from_decl(cached, decl);
    if (!target) return;
    for (i32 i = 0; i < target->instances.length; i++) {
        string8 mangle = target->instances.data[i];
        if (!array_string8_contains(out, mangle)) {
            Vec_string8_append(arena, out, mangle);
            if (out_subs) {
                Vec_string8_append(arena, out_subs, target->sub_instances.data[i]);
            }
        }
    }
}

static void collect_generic_proc_instances_build(Program *prog, memops_arena *arena) {
    Vec_voidptr *constraint_sites = &g_generic_sites;
    *constraint_sites = ptr_array_reserve(arena, 16);
    Vec_voidptr entries = ptr_array_reserve(arena, 32);
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *p = (ProcDecl *)prog->procs.data[i];
        if (!p->is_generic) continue;
        GenericProcEntry *entry = memops_arena_push_struct(arena, GenericProcEntry);
        memset(entry, 0, sizeof(*entry));
        entry->decl = p;
        entry->instances = Vec_string8_reserve(arena, 4);
        entry->sub_instances = Vec_string8_reserve(arena, 4);
        ptr_array_append(arena, &entries, entry);
    }

    // Seed from non-generic contexts only.
    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *s = (Stmt *)prog->globals.data[i];
        collect_generic_calls_from_stmt(s, (TypeSub){0}, &entries, constraint_sites, arena);
    }
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *p = (ProcDecl *)prog->procs.data[i];
        if (p->is_generic) continue;
        for (i32 j = 0; j < p->body.length; j++) {
            Stmt *s = (Stmt *)p->body.data[j];
            collect_generic_calls_from_stmt(s, (TypeSub){0}, &entries, constraint_sites, arena);
        }
    }

    // Closure: discovered generic instances can induce further generic calls.
    bool changed = true;
    while (changed) {
        changed = false;
        for (i32 i = 0; i < entries.length; i++) {
            GenericProcEntry *entry = (GenericProcEntry *)entries.data[i];
            for (i32 j = 0; j < entry->instances.length; j++) {
                TypeExpr *bound = mangle_type_for(arena, entry->sub_instances.data[j]);
                TypeSub sub = proc_sub_for_instance(arena, entry->decl, entry->instances.data[j], bound);
                for (i32 k = 0; k < entry->decl->body.length; k++) {
                    Stmt *s = (Stmt *)entry->decl->body.data[k];
                    if (collect_generic_calls_from_stmt(s, sub, &entries, constraint_sites, arena)) {
                        changed = true;
                    }
                }
            }
        }
    }

    g_generic_entries = entries;
}

static bool type_expr_equal_resolved(Program *prog, TypeExpr *a, TypeExpr *b);

static ProcDecl *program_find_proc_named(Program *prog, string8 name) {
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *p = (ProcDecl *)prog->procs.data[i];
        if (string8_equals(&p->name, &name)) {
            return p;
        }
    }
    return null;
}

typedef struct RequirementCheck {
    bool ok;
    bool has_bad_signature;
    ProcDecl *bad_proc;
    string8 required;
    string8 base;
} RequirementCheck;

static TypeExpr *type_name_expr_from_string(memops_arena *arena, string8 name) {
    TypeExpr *t = type_new(arena, Type_Name);
    t->name = name;
    return t;
}

static bool hash_requirement_proc_signature_matches(Program *prog, memops_arena *arena, ProcDecl *proc, string8 type_mangle) {
    if (!proc || proc->is_variadic || proc->params.length != 1) return false;
    Param *param = (Param *)proc->params.data[0];
    TypeExpr *expected_type = type_name_expr_from_string(arena, type_mangle);
    TypeExpr *expected_ret = type_name_expr_from_string(arena, string8_from_cstr(arena, "u64"));
    return type_expr_equal_resolved(prog, param->type, expected_type) &&
           type_expr_equal_resolved(prog, proc->ret_type, expected_ret);
}

static RequirementCheck check_requirement_for_instance(Program *prog, memops_arena *arena, string8 requirement, string8 type_mangle) {
    RequirementCheck check = {0};
    string8 base = requirement;
    if (string8_equals_cstr(&requirement, "hashable")) {
        base = string8_from_cstr(arena, "hash");
    }
    check.base = base;

    string8 required = string8_reserve(arena, base.length + 1 + type_mangle.length);
    string8_append_bytes(arena, &required, base.data, base.length);
    string8_append_cstr(arena, &required, "_");
    string8_append_bytes(arena, &required, type_mangle.data, type_mangle.length);
    check.required = required;

    ProcDecl *concrete = program_find_proc_named(prog, required);
    if (concrete) {
        if (string8_equals_cstr(&requirement, "hashable") &&
            !hash_requirement_proc_signature_matches(prog, arena, concrete, type_mangle)) {
            check.has_bad_signature = true;
            check.bad_proc = concrete;
            return check;
        }
        check.ok = true;
        return check;
    }
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *p = (ProcDecl *)prog->procs.data[i];
        if (p->is_generic && string8_equals(&p->name, &base)) {
            check.ok = true;
            return check;
        }
    }

    return check;
}

static void validate_generic_constraints(Program *prog, memops_arena *arena) {
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        if (!decl->is_generic || !decl->constraint.data || decl->constraint.length == 0) {
            continue;
        }

        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        Vec_voidptr constraint_sites = ptr_array_reserve(arena, 4);
        collect_generic_proc_instances_with_sites(prog, decl, &instances, null, &constraint_sites, arena);
        for (i32 j = 0; j < instances.length; j++) {
            string8 mangle = instances.data[j];

            RequirementCheck check = check_requirement_for_instance(prog, arena, decl->constraint, mangle);
            if (!check.ok) {
                GenericInstanceSite *site = generic_instance_site_find(&constraint_sites, decl->name, mangle);
                const char *site_path = site && site->source_path ? site->source_path : (decl->source_path ? decl->source_path : g_source_path);
                i32 site_line = site ? site->line : decl->line;
                i32 site_col = site ? site->col : decl->col;
                const char *decl_path = decl->source_path ? decl->source_path : g_source_path;
                if (g_diag_json) {
                    char message[1024];
                    if (check.has_bad_signature) {
                        snprintf(
                            message,
                            sizeof(message),
                            "proc '%.*s' requires '%.*s' for type '%.*s' (function '%.*s' has incompatible signature)",
                            (int)decl->name.length, decl->name.data,
                            (int)decl->constraint.length, decl->constraint.data,
                            (int)mangle.length, mangle.data,
                            (int)check.required.length, check.required.data
                        );
                    } else {
                        snprintf(
                            message,
                            sizeof(message),
                            "proc '%.*s' requires '%.*s' for type '%.*s' (missing function '%.*s')",
                            (int)decl->name.length, decl->name.data,
                            (int)decl->constraint.length, decl->constraint.data,
                            (int)mangle.length, mangle.data,
                            (int)check.required.length, check.required.data
                        );
                    }

                    bool has_note = false;
                    diag_json_open(site_path, site_line, site_col, "requirement", message);
                    diag_json_note_import_chain(&has_note);
                    if (check.has_bad_signature) {
                        char expected_note[512];
                        snprintf(
                            expected_note,
                            sizeof(expected_note),
                            "expected signature: %.*s(value:%.*s)->u64",
                            (int)check.required.length, check.required.data,
                            (int)mangle.length, mangle.data
                        );
                        diag_json_note_cstr(&has_note, site_path, site_line, site_col, expected_note);
                        if (check.bad_proc) {
                            const char *bad_path = check.bad_proc->source_path ? check.bad_proc->source_path : g_source_path;
                            char bad_note[512];
                            snprintf(
                                bad_note,
                                sizeof(bad_note),
                                "function '%.*s' declared here",
                                (int)check.bad_proc->name.length,
                                check.bad_proc->name.data
                            );
                            diag_json_note_cstr(&has_note, bad_path, check.bad_proc->line, check.bad_proc->col, bad_note);
                        }
                    }
                    char instantiated_note[512];
                    snprintf(
                        instantiated_note,
                        sizeof(instantiated_note),
                        "generic '%.*s' instantiated here with type '%.*s'",
                        (int)decl->name.length, decl->name.data,
                        (int)mangle.length, mangle.data
                    );
                    diag_json_note_cstr(&has_note, site_path, site_line, site_col, instantiated_note);
                    char declared_note[512];
                    snprintf(
                        declared_note,
                        sizeof(declared_note),
                        "generic declared here with requirement '%.*s'",
                        (int)decl->constraint.length,
                        decl->constraint.data
                    );
                    diag_json_note_cstr(&has_note, decl_path, decl->line, decl->col, declared_note);
                    diag_json_close();
                    diag_record_error();
                } else {
                if (check.has_bad_signature) {
                    printf(
                        "%s:%d:%d: requirement error: proc '%.*s' requires '%.*s' for type '%.*s' (function '%.*s' has incompatible signature)\n",
                        site_path,
                        site_line,
                        site_col,
                        (int)decl->name.length, decl->name.data,
                        (int)decl->constraint.length, decl->constraint.data,
                        (int)mangle.length, mangle.data,
                        (int)check.required.length, check.required.data
                    );
                } else {
                    printf(
                        "%s:%d:%d: requirement error: proc '%.*s' requires '%.*s' for type '%.*s' (missing function '%.*s')\n",
                        site_path,
                        site_line,
                        site_col,
                        (int)decl->name.length, decl->name.data,
                        (int)decl->constraint.length, decl->constraint.data,
                        (int)mangle.length, mangle.data,
                        (int)check.required.length, check.required.data
                    );
                }
                diag_print_file_context(site_path, site_line, site_col);
                if (check.has_bad_signature) {
                    printf("%s:%d:%d: note: expected signature: %.*s(value:%.*s)->u64\n",
                           site_path,
                           site_line,
                           site_col,
                           (int)check.required.length, check.required.data,
                           (int)mangle.length, mangle.data);
                    if (check.bad_proc) {
                        const char *bad_path = check.bad_proc->source_path ? check.bad_proc->source_path : g_source_path;
                        printf("%s:%d:%d: note: function '%.*s' declared here\n",
                               bad_path,
                               check.bad_proc->line,
                               check.bad_proc->col,
                               (int)check.bad_proc->name.length,
                               check.bad_proc->name.data);
                        diag_print_file_context(bad_path, check.bad_proc->line, check.bad_proc->col);
                    }
                }
                printf("%s:%d:%d: note: generic '%.*s' instantiated here with type '%.*s'\n",
                       site_path,
                       site_line,
                       site_col,
                       (int)decl->name.length, decl->name.data,
                       (int)mangle.length, mangle.data);
                printf("%s:%d:%d: note: generic declared here with requirement '%.*s'\n",
                       decl_path,
                       decl->line,
                       decl->col,
                       (int)decl->constraint.length,
                       decl->constraint.data);
                diag_print_file_context(decl_path, decl->line, decl->col);
                diag_record_error();
                }
            }
        }
    }
}

/* Scopes chain to their enclosing scope rather than copying it. Copying meant every
   proc and every nested block duplicated the whole outer scope, which on a real
   project came to ~1.9M element copies because the outermost scope already holds a
   reflection global per struct. Lookup walks outward, so shadowing still works:
   the innermost match wins. A child never outlives its parent; every child is a
   local in a frame nested inside the parent's. */
typedef struct TypeScope TypeScope;
struct TypeScope {
    Vec_string8 names;
    Vec_voidptr types; // TypeExpr*
    TypeSub sub;
    TypeScope *parent;
};

static TypeExpr *type_name_expr_const(memops_arena *arena, const char *name);
static TypeExpr *substitute_type_param(memops_arena *arena, TypeExpr *src, string8 param, TypeExpr *arg);
static TypeExpr *substitute_type_sub(memops_arena *arena, TypeExpr *src, TypeSub sub);

static TypeScope type_scope_make(memops_arena *arena, i32 cap) {
    TypeScope s = {0};
    s.names = Vec_string8_reserve(arena, cap);
    s.types = ptr_array_reserve(arena, cap);
    return s;
}

static TypeScope type_scope_copy(memops_arena *arena, TypeScope *src) {
    TypeScope dst = type_scope_make(arena, 8);
    dst.sub = src->sub;
    dst.parent = src;
    return dst;
}

static TypeExpr *type_scope_apply_sub(TypeScope *scope, memops_arena *arena, TypeExpr *type) {
    if (!scope || !scope->sub.has || !type) return type;
    return substitute_type_sub(arena, type, scope->sub);
}

static void type_scope_add(memops_arena *arena, TypeScope *s, string8 name, TypeExpr *type) {
    Vec_string8_append(arena, &s->names, name);
    ptr_array_append(arena, &s->types, type);
}

static TypeExpr *reflect_global_type(memops_arena *arena, const char *type_name) {
    return type_name_expr_const(arena, type_name);
}

static void type_scope_add_reflection_globals(memops_arena *arena, TypeScope *scope, Program *prog) {
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (decl->is_external) continue;
        string8 reflect_name = concat_name2(arena, decl->name, "_", string8_from_cstr(arena, "reflect"));
        type_scope_add(arena, scope, reflect_name, reflect_global_type(arena, "reflect"));
        if (decl->is_generic) {
            Vec_string8 instances = Vec_string8_reserve(arena, 4);
            collect_generic_struct_instances(prog, decl, &instances, arena);
            for (i32 j = 0; j < instances.length; j++) {
                string8 concrete_name = string8_reserve(arena, decl->name.length + 1 + instances.data[j].length + 8);
                string8_append_bytes(arena, &concrete_name, decl->name.data, decl->name.length);
                string8_append_cstr(arena, &concrete_name, "_");
                string8_append_bytes(arena, &concrete_name, instances.data[j].data, instances.data[j].length);
                string8_append_cstr(arena, &concrete_name, "_reflect");
                type_scope_add(arena, scope, concrete_name, reflect_global_type(arena, "reflect"));
            }
        }
    }

    for (i32 i = 0; i < prog->enums.length; i++) {
        EnumDecl *decl = (EnumDecl *)prog->enums.data[i];
        if (decl->is_external) continue;
        string8 reflect_name = concat_name2(arena, decl->name, "_", string8_from_cstr(arena, "reflect"));
        type_scope_add(arena, scope, reflect_name, reflect_global_type(arena, "reflect"));
    }
}

static TypeExpr *type_scope_lookup(TypeScope *s, string8 name) {
    for (TypeScope *level = s; level; level = level->parent) {
        for (i32 i = level->names.length - 1; i >= 0; i--) {
            if (string8_equals(&level->names.data[i], &name)) {
                return (TypeExpr *)level->types.data[i];
            }
        }
    }
    return null;
}

static TypeExpr *type_name_expr(memops_arena *arena, const char *name) {
    TypeExpr *t = type_new(arena, Type_Name);
    t->name = string8_from_cstr(arena, name);
    return t;
}

static TypeExpr *type_name_expr_const(memops_arena *arena, const char *name) {
    TypeExpr *t = type_name_expr(arena, name);
    t->is_const = true;
    return t;
}

static TypeExpr *type_ptr_to(memops_arena *arena, TypeExpr *elem) {
    TypeExpr *ptr = type_new(arena, Type_Ptr);
    ptr->elem = elem;
    return ptr;
}

static TypeExpr *type_ptr_to_const_name(memops_arena *arena, const char *name) {
    return type_ptr_to(arena, type_name_expr_const(arena, name));
}

static TypeExpr *clone_type_expr(memops_arena *arena, TypeExpr *src) {
    if (!src) return null;
    TypeExpr *dst = type_new(arena, src->kind);
    dst->name = src->name;
    dst->array_count = src->array_count;
    dst->is_const = src->is_const;
    dst->is_volatile = src->is_volatile;
    dst->is_variadic = src->is_variadic;
    if (src->elem) {
        dst->elem = clone_type_expr(arena, src->elem);
    }
    if (src->ret_type) {
        dst->ret_type = clone_type_expr(arena, src->ret_type);
    }
    if (src->args.length > 0) {
        dst->args = ptr_array_reserve(arena, src->args.length);
        for (i32 i = 0; i < src->args.length; i++) {
            ptr_array_append(arena, &dst->args, clone_type_expr(arena, (TypeExpr *)src->args.data[i]));
        }
    }
    if (src->arg_names.length > 0) {
        dst->arg_names = Vec_string8_reserve(arena, src->arg_names.length);
        for (i32 i = 0; i < src->arg_names.length; i++) {
            Vec_string8_append(arena, &dst->arg_names, src->arg_names.data[i]);
        }
    }
    return dst;
}

/* Applies every substitution in one pass, so a body written over <T, U> is
   rewritten in a single walk rather than once per parameter. */
static TypeExpr *substitute_type_sub(memops_arena *arena, TypeExpr *src, TypeSub sub) {
    if (!sub.has || !src) return src;
    TypeExpr *out = src;
    for (i32 i = 0; i < sub.params.length && i < sub.args.length; i++) {
        out = substitute_type_param(arena, out, sub.params.data[i], (TypeExpr *)sub.args.data[i]);
    }
    return out;
}

static TypeExpr *substitute_type_param(memops_arena *arena, TypeExpr *src, string8 param, TypeExpr *arg) {
    if (!src) return null;
    if (src->kind == Type_Name && string8_equals(&src->name, &param)) {
        return clone_type_expr(arena, arg);
    }
    TypeExpr *dst = type_new(arena, src->kind);
    dst->name = src->name;
    dst->array_count = src->array_count;
    dst->is_const = src->is_const;
    dst->is_volatile = src->is_volatile;
    dst->is_variadic = src->is_variadic;
    if (src->elem) {
        dst->elem = substitute_type_param(arena, src->elem, param, arg);
    }
    if (src->ret_type) {
        dst->ret_type = substitute_type_param(arena, src->ret_type, param, arg);
    }
    if (src->args.length > 0) {
        dst->args = ptr_array_reserve(arena, src->args.length);
        for (i32 i = 0; i < src->args.length; i++) {
            TypeExpr *in = (TypeExpr *)src->args.data[i];
            ptr_array_append(arena, &dst->args, substitute_type_param(arena, in, param, arg));
        }
    }
    if (src->arg_names.length > 0) {
        dst->arg_names = Vec_string8_reserve(arena, src->arg_names.length);
        for (i32 i = 0; i < src->arg_names.length; i++) {
            Vec_string8_append(arena, &dst->arg_names, src->arg_names.data[i]);
        }
    }
    return dst;
}

static TypeExpr *resolve_alias_type(Program *prog, TypeExpr *type);
static bool type_expr_equal_resolved(Program *prog, TypeExpr *a, TypeExpr *b);

static TypeExpr *reflect_builtin_field_type(TypeExpr *base_type, string8 field_name, memops_arena *arena) {
    if (!base_type || base_type->kind != Type_Name) return null;
    string8 base_name = base_type->name;

    if (string8_equals_cstr(&base_name, "reflect_field")) {
        if (string8_equals_cstr(&field_name, "name")) return type_ptr_to_const_name(arena, "c8");
        if (string8_equals_cstr(&field_name, "type")) return type_ptr_to_const_name(arena, "c8");
        if (string8_equals_cstr(&field_name, "attrs")) return type_ptr_to_const_name(arena, "c8");
        if (string8_equals_cstr(&field_name, "offset")) return type_name_expr(arena, "u64");
        if (string8_equals_cstr(&field_name, "size")) return type_name_expr(arena, "u64");
        if (string8_equals_cstr(&field_name, "align")) return type_name_expr(arena, "u64");
        if (string8_equals_cstr(&field_name, "kind")) return type_name_expr(arena, "i32");
        if (string8_equals_cstr(&field_name, "array_count")) return type_name_expr(arena, "u64");
        if (string8_equals_cstr(&field_name, "pointer_depth")) return type_name_expr(arena, "u64");
        if (string8_equals_cstr(&field_name, "base_type")) return type_ptr_to_const_name(arena, "c8");
        if (string8_equals_cstr(&field_name, "elem_type")) return type_ptr_to_const_name(arena, "c8");
        if (string8_equals_cstr(&field_name, "generic_arg_type")) return type_ptr_to_const_name(arena, "c8");
        if (string8_equals_cstr(&field_name, "is_const")) return type_name_expr(arena, "u64");
        if (string8_equals_cstr(&field_name, "info")) return type_ptr_to(arena, type_name_expr_const(arena, "reflect"));
        return null;
    }

    if (string8_equals_cstr(&base_name, "reflect_value")) {
        if (string8_equals_cstr(&field_name, "name")) return type_ptr_to_const_name(arena, "c8");
        if (string8_equals_cstr(&field_name, "value")) return type_name_expr(arena, "i32");
        return null;
    }

    /* The variant is a plain union, so both arms are always accessible and the
       kind is what says which one is live. reflect_fields/reflect_values in
       std/reflect.h are the checked way in. */
    if (string8_equals_cstr(&base_name, "reflect_variant")) {
        if (string8_equals_cstr(&field_name, "fields")) return type_ptr_to(arena, type_name_expr_const(arena, "reflect_field"));
        if (string8_equals_cstr(&field_name, "values")) return type_ptr_to(arena, type_name_expr_const(arena, "reflect_value"));
        return null;
    }

    if (string8_equals_cstr(&base_name, "reflect")) {
        if (string8_equals_cstr(&field_name, "name")) return type_ptr_to_const_name(arena, "c8");
        if (string8_equals_cstr(&field_name, "size")) return type_name_expr(arena, "u64");
        if (string8_equals_cstr(&field_name, "align")) return type_name_expr(arena, "u64");
        if (string8_equals_cstr(&field_name, "kind")) return type_name_expr(arena, "i32");
        if (string8_equals_cstr(&field_name, "count")) return type_name_expr(arena, "u64");
        if (string8_equals_cstr(&field_name, "variant")) return type_name_expr_const(arena, "reflect_variant");
        return null;
    }

    return null;
}

/* Members of an anonymous struct/union are named directly on the owner, so the
   search descends into them as if their fields were declared inline. */
static Field *struct_find_field(StructDecl *decl, string8 field_name) {
    for (i32 f = 0; f < decl->fields.length; f++) {
        Field *field = (Field *)decl->fields.data[f];
        if (field->anon) {
            Field *nested = struct_find_field(field->anon, field_name);
            if (nested) return nested;
            continue;
        }
        if (string8_equals(&field->name, &field_name)) return field;
    }
    return null;
}

static TypeExpr *lookup_field_type(Program *prog, TypeExpr *base_type, string8 field_name, memops_arena *arena) {
    if (!base_type) return null;
    base_type = resolve_alias_type(prog, base_type);
    TypeExpr *reflect_field = reflect_builtin_field_type(base_type, field_name, arena);
    if (reflect_field) return reflect_field;
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (base_type->kind == Type_Name && !decl->is_generic) {
            if (!string8_equals(&decl->name, &base_type->name)) continue;
            Field *field = struct_find_field(decl, field_name);
            if (field) return clone_type_expr(arena, field->type);
        }
        if (base_type->kind == Type_Generic && decl->is_generic) {
            if (!string8_equals(&decl->name, &base_type->name)) continue;
            if (base_type->args.length != decl->type_params.length) return null;
            Field *field = struct_find_field(decl, field_name);
            if (field) {
                return substitute_type_sub(arena, field->type, struct_sub_from_args(arena, decl, base_type));
            }
        }
    }
    return null;
}

static bool type_is_declared_aggregate(Program *prog, TypeExpr *type) {
    if (!type) return false;
    type = resolve_alias_type(prog, type);
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (type->kind == Type_Name && !decl->is_generic && string8_equals(&decl->name, &type->name)) {
            return !decl->is_external || decl->fields.length > 0;
        }
        if (type->kind == Type_Generic && decl->is_generic && string8_equals(&decl->name, &type->name)) {
            return !decl->is_external || decl->fields.length > 0;
        }
    }
    return false;
}

/* An external struct that declares no fields. The compiler has nothing to check
   a field access against, so before this existed `handle[0].anything` compiled
   and was handed straight to C -- every reflection accessor in a real program
   was riding that path. Declaring the fields alongside `external` opts the type
   back into checking; leaving them off is still fine for a genuinely opaque
   handle, which is never field-accessed anyway. */
static StructDecl *lookup_opaque_external_struct(Program *prog, TypeExpr *type) {
    if (!type) {
        return null;
    }
    type = resolve_alias_type(prog, type);
    if (type->kind != Type_Name) {
        return null;
    }
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (decl->is_generic || !decl->is_external || decl->fields.length > 0) {
            continue;
        }
        if (string8_equals(&decl->name, &type->name)) {
            return decl;
        }
    }
    return null;
}

static void type_error_opaque_field_access(StructDecl *decl, string8 field_name, i32 line, i32 col) {
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "cannot read field '%.*s': type '%.*s' is external and declares no fields",
            (int)field_name.length, field_name.data,
            (int)decl->name.length, decl->name.data
        );
        diag_json_error(diag_current_path(), line, col, "type", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: cannot read field '%.*s': type '%.*s' is external and declares no fields\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line, col,
        (int)field_name.length, field_name.data,
        (int)decl->name.length, decl->name.data
    );
    printf(
        "%s:%d:%d: note: list the fields alongside `external;` so they can be checked\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        decl->line, decl->col
    );
    diag_finish_at(line, col);
}

/* `Point<>` parses to a plain name, `Point_reflect`, so a chain like
   `Point<>.variant.values` roots at that name. Walking back to it recovers which
   type the reflect table belongs to. Anything else -- a parameter, an index, a
   call -- roots at something with no static kind, and is left alone. */
static string8 reflect_owner_name_from_expr(Expr *base) {
    string8 none = {0};
    while (base && base->kind == Expr_Field) {
        base = base->base;
    }
    if (!base || base->kind != Expr_Name) return none;
    if (!string8_ends_with_cstr(base->name, "_reflect")) return none;
    string8 owner = base->name;
    owner.length -= 8; /* strlen("_reflect") */
    if (owner.length == 0) return none;
    return owner;
}

/* Which variant arm is live for the type that owns this table, or null when the
   compiler cannot say. A monomorphised name like `Boxed_i32_reflect` has no
   StructDecl under that name, so it lands here as unknown and is not diagnosed:
   the check only fires where the kind is positively known. */
static const char *reflect_live_arm_for_owner(Program *prog, string8 owner) {
    if (owner.length == 0) return null;
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (decl->is_generic || decl->is_external) continue;
        if (string8_equals(&decl->name, &owner)) {
            return decl->is_union ? "union" : "struct";
        }
    }
    for (i32 i = 0; i < prog->enums.length; i++) {
        EnumDecl *decl = (EnumDecl *)prog->enums.data[i];
        if (decl->is_external) continue;
        if (string8_equals(&decl->name, &owner)) return "enum";
    }
    return null;
}

static void type_error_reflect_variant_arm(
    string8 owner,
    const char *owner_kind,
    string8 arm,
    const char *live_arm,
    const char *helper,
    i32 line,
    i32 col
) {
    const char *article = owner_kind[0] == 'e' ? "an" : "a";
    if (g_diag_json) {
        char message[1024];
        char note[1024];
        snprintf(
            message,
            sizeof(message),
            "'%.*s' is %s %s, so its reflect variant holds '%s', not '%.*s'",
            (int)owner.length, owner.data, article, owner_kind, live_arm,
            (int)arm.length, arm.data
        );
        /* The editor sees the same fix the terminal does. Without this the
           message says what is wrong but not what to write instead. */
        snprintf(
            note,
            sizeof(note),
            "reading the other arm reinterprets the pointer; use '%.*s<>.variant.%s', "
            "or %s from std/reflect.h where the kind is only known at run time",
            (int)owner.length, owner.data, live_arm, helper
        );
        diag_json_error_with_note(
            diag_current_path(), line, col, "type", message,
            diag_current_path(), line, col, note
        );
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: '%.*s' is %s %s, so its reflect variant holds '%s', not '%.*s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line, col,
        (int)owner.length, owner.data, article, owner_kind, live_arm,
        (int)arm.length, arm.data
    );
    printf(
        "%s:%d:%d: note: reading the other arm reinterprets the pointer; use '%.*s<>.variant.%s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line, col,
        (int)owner.length, owner.data, live_arm
    );
    printf(
        "%s:%d:%d: note: where the kind is only known at run time, use %s from std/reflect.h\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line, col, helper
    );
    diag_finish_at(line, col);
}

/* The two reflect records collapsed into one, which cost a type error: a struct
   table and an enum table are both `reflect` now, so nothing stopped
   `Point<>.variant.values`. That does not fail loudly -- both arms begin with a
   `const char *name`, so it reinterprets the fields pointer and yields a
   plausible wrong string. This puts the diagnostic back wherever the kind is
   statically known, which is every `Type<>` site. */
static bool type_is_reflect_runtime_record(Program *prog, TypeExpr *type) {
    if (!type) return false;
    type = resolve_alias_type(prog, type);
    if (type->kind != Type_Name) return false;
    return string8_equals_cstr(&type->name, "reflect") ||
           string8_equals_cstr(&type->name, "reflect_field") ||
           string8_equals_cstr(&type->name, "reflect_value") ||
           string8_equals_cstr(&type->name, "reflect_variant");
}

static void check_reflect_variant_arm(
    Program *prog,
    Expr *e,
    TypeExpr *base_type,
    memops_arena *arena
) {
    if (!base_type) return;
    base_type = resolve_alias_type(prog, base_type);
    if (base_type->kind != Type_Name) return;
    if (!string8_equals_cstr(&base_type->name, "reflect_variant")) return;

    bool wants_fields = string8_equals_cstr(&e->name, "fields");
    bool wants_values = string8_equals_cstr(&e->name, "values");
    if (!wants_fields && !wants_values) return;

    string8 owner = reflect_owner_name_from_expr(e->base);
    const char *owner_kind = reflect_live_arm_for_owner(prog, owner);
    if (!owner_kind) return;

    bool is_enum = owner_kind[0] == 'e';
    if (is_enum == wants_values) return;

    type_error_reflect_variant_arm(
        owner,
        owner_kind,
        e->name,
        is_enum ? "values" : "fields",
        is_enum ? "reflect_values()" : "reflect_fields()",
        e->line,
        e->col
    );
    (void)arena;
}

static StructDecl *lookup_aggregate_decl(Program *prog, TypeExpr *type, TypeSub *sub, memops_arena *arena) {
    if (sub) *sub = (TypeSub){0};
    if (!type) return null;
    type = resolve_alias_type(prog, type);
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (type->kind == Type_Name && !decl->is_generic && string8_equals(&decl->name, &type->name)) {
            if (decl->is_external && decl->fields.length == 0) return null;
            return decl;
        }
        if (type->kind == Type_Generic && decl->is_generic && string8_equals(&decl->name, &type->name)) {
            if (decl->is_external && decl->fields.length == 0) return null;
            if (sub && type->args.length == decl->type_params.length) {
                *sub = struct_sub_from_args(arena, decl, type);
            }
            return decl;
        }
    }
    return null;
}

/* Proc lookup by name runs once per call site across three whole-program passes.
   As a linear scan that is quadratic in program size: a 771-proc project spent
   ~5M string comparisons here. The table is rebuilt whenever the proc list grows,
   which is the only way it changes, and keeps first-declaration-wins so behaviour
   matches the scan it replaces. */
static string8 *g_proc_index_names = null;
static ProcDecl **g_proc_index_decls = null;
static i32 g_proc_index_cap = 0;
static i32 g_proc_index_built_for = -1;

static u64 name_hash(string8 name) {
    u64 h = 1469598103934665603ull;
    for (u64 i = 0; i < name.length; i++) {
        h ^= (u64)name.data[i];
        h *= 1099511628211ull;
    }
    return h;
}

static void proc_index_build(memops_arena *arena, Program *prog) {
    i32 cap = 64;
    while (cap < prog->procs.length * 2) cap *= 2;
    g_proc_index_cap = cap;
    g_proc_index_names = memops_arena_push_array_zero(arena, string8, (u64)cap);
    g_proc_index_decls = memops_arena_push_array_zero(arena, ProcDecl *, (u64)cap);

    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        u64 slot = name_hash(decl->name) & (u64)(cap - 1);
        bool duplicate = false;
        while (g_proc_index_decls[slot]) {
            if (string8_equals(&g_proc_index_names[slot], &decl->name)) {
                duplicate = true; // keep the first declaration, as the scan did
                break;
            }
            slot = (slot + 1) & (u64)(cap - 1);
        }
        if (duplicate) continue;
        g_proc_index_names[slot] = decl->name;
        g_proc_index_decls[slot] = decl;
    }
    g_proc_index_built_for = prog->procs.length;
}

static ProcDecl *lookup_proc_decl(Program *prog, string8 name) {
    if (g_proc_index_built_for != prog->procs.length) {
        proc_index_build(g_index_arena, prog);
    }
    u64 slot = name_hash(name) & (u64)(g_proc_index_cap - 1);
    while (g_proc_index_decls[slot]) {
        if (string8_equals(&g_proc_index_names[slot], &name)) {
            return g_proc_index_decls[slot];
        }
        slot = (slot + 1) & (u64)(g_proc_index_cap - 1);
    }
    return null;
}

static EnumDecl *lookup_enum_decl(Program *prog, string8 name) {
    for (i32 i = 0; i < prog->enums.length; i++) {
        EnumDecl *decl = (EnumDecl *)prog->enums.data[i];
        if (string8_equals(&decl->name, &name)) return decl;
    }
    return null;
}

static EnumItem *lookup_enum_item(EnumDecl *decl, string8 name) {
    if (!decl) return null;
    for (i32 i = 0; i < decl->items.length; i++) {
        EnumItem *item = (EnumItem *)decl->items.data[i];
        if (string8_equals(&item->name, &name)) return item;
    }
    return null;
}

static string8 enum_item_c_name(memops_arena *arena, EnumDecl *decl, EnumItem *item) {
    if (!decl || !item) return (string8){0};
    /* An external enum is defined by the C header, so its items already carry
       their real C names. Prefixing them would invent a symbol that header never
       declared. Only enums I emits itself get the generated prefix. */
    if (decl->is_external) return item->name;
    return concat_name2(arena, c_ident(arena, decl->name), "_", c_ident(arena, item->name));
}

/* Rewrites `[Enum.Member]` array counts to the member's C name, now that every
   import has been merged and all enums are visible. A bare identifier is left
   alone: it names a C constant or macro the generated code refers to directly. */
static void resolve_array_count_constants(memops_arena *arena, Program *prog) {
    for (i32 i = 0; i < prog->pending_array_counts.length; i++) {
        TypeExpr *type = (TypeExpr *)prog->pending_array_counts.data[i];
        if (!type) continue;

        i32 dot = -1;
        for (i32 c = 0; c < type->array_count.length; c++) {
            if (type->array_count.data[c] == '.') { dot = c; break; }
        }
        if (dot < 0) continue;

        /* `Enum<>.count` is known once the enum is parsed, so it becomes a
           literal here rather than a reflection lookup the C array size could
           not use. */
        if (dot >= 2 && type->array_count.data[dot - 2] == '<' && type->array_count.data[dot - 1] == '>') {
            string8 reflect_enum_name = { type->array_count.data, dot - 2 };
            string8 reflect_member = { type->array_count.data + dot + 1, type->array_count.length - dot - 1 };
            EnumDecl *reflect_decl = lookup_enum_decl(prog, reflect_enum_name);
            if (!reflect_decl) {
                semantic_error_name("unknown enum in array count", reflect_enum_name, type->line, type->col);
                continue;
            }
            if (!string8_equals_cstr(&reflect_member, "count")) {
                semantic_error_name("only 'count' can size an array", reflect_member, type->line, type->col);
                continue;
            }
            char count_buf[32];
            snprintf(count_buf, sizeof(count_buf), "%d", (i32)reflect_decl->items.length);
            type->array_count = string8_from_cstr(arena, count_buf);
            continue;
        }

        string8 enum_name = { type->array_count.data, dot };
        string8 member_name = { type->array_count.data + dot + 1, type->array_count.length - dot - 1 };

        EnumDecl *decl = lookup_enum_decl(prog, enum_name);
        if (!decl) {
            semantic_error_name("unknown enum in array count", enum_name, type->line, type->col);
            continue;
        }
        EnumItem *item = lookup_enum_item(decl, member_name);
        if (!item) {
            semantic_error_name("unknown enum member in array count", type->array_count, type->line, type->col);
            continue;
        }
        type->array_count = enum_item_c_name(arena, decl, item);
    }
}

static bool resolve_enum_member_expr(Program *prog, Expr *e, EnumDecl **out_decl, EnumItem **out_item) {
    if (out_decl) *out_decl = null;
    if (out_item) *out_item = null;
    if (!e || e->kind != Expr_Field || !e->base || e->base->kind != Expr_Name) return false;

    EnumDecl *decl = lookup_enum_decl(prog, e->base->name);
    if (!decl) return false;
    if (out_decl) *out_decl = decl;
    if (out_item) *out_item = lookup_enum_item(decl, e->name);
    return true;
}

static EnumDecl *lookup_enum_constant_decl(memops_arena *arena, Program *prog, string8 name) {
    for (i32 i = 0; i < prog->enums.length; i++) {
        EnumDecl *decl = (EnumDecl *)prog->enums.data[i];
        for (i32 j = 0; j < decl->items.length; j++) {
            EnumItem *item = (EnumItem *)decl->items.data[j];
            string8 c_name = enum_item_c_name(arena, decl, item);
            if (string8_equals(&c_name, &name)) return decl;
        }
    }
    return null;
}

static TypeExpr *generic_call_type_arg(Expr *call) {
    if (!call || call->type_args.length != 1) return null;
    return (TypeExpr *)call->type_args.data[0];
}

static ProcDecl *lookup_call_proc_decl(
    Program *prog,
    Expr *call,
    TypeScope *scope,
    memops_arena *arena,
    TypeExpr **out_type_arg,
    bool *out_concrete_specialization
) {
    if (out_type_arg) *out_type_arg = null;
    if (out_concrete_specialization) *out_concrete_specialization = false;
    if (!call) return null;

    TypeExpr *type_arg = generic_call_type_arg(call);
    if (type_arg) {
        TypeExpr *resolved_arg = type_scope_apply_sub(scope, arena, type_arg);
        if (out_type_arg) *out_type_arg = resolved_arg;

        string8 mangle = type_mangle(arena, resolved_arg, (TypeSub){0});
        string8 concrete_name = mono_proc_name_from_mangle(arena, call->name, mangle);
        ProcDecl *concrete = lookup_proc_decl(prog, concrete_name);
        if (concrete) {
            if (out_concrete_specialization) *out_concrete_specialization = true;
            return concrete;
        }

        for (i32 i = 0; i < prog->procs.length; i++) {
            ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
            if (!decl->is_generic || !string8_equals(&decl->name, &call->name)) continue;
            TypeExpr *bound = null;
            if (generic_proc_match_type_arg(arena, decl, resolved_arg, &bound)) {
                if (out_type_arg) *out_type_arg = bound ? bound : resolved_arg;
                return decl;
            }
        }
    }

    return lookup_proc_decl(prog, call->name);
}

static bool type_compatible(Program *prog, TypeExpr *dst, TypeExpr *src);
static void collect_generic_struct_instances(Program *prog, StructDecl *decl, Vec_string8 *out, memops_arena *arena);

static TypeExpr *proc_decl_pointer_type(memops_arena *arena, ProcDecl *decl) {
    if (!decl) return null;
    TypeExpr *proc_t = type_new(arena, Type_Proc);
    proc_t->name = decl->callconv;
    proc_t->ret_type = decl->ret_type;
    proc_t->is_variadic = decl->is_variadic;
    proc_t->args = ptr_array_reserve(arena, decl->params.length);
    proc_t->arg_names = Vec_string8_reserve(arena, decl->params.length);
    for (i32 i = 0; i < decl->params.length; i++) {
        Param *param = (Param *)decl->params.data[i];
        ptr_array_append(arena, &proc_t->args, param->type);
        Vec_string8_append(arena, &proc_t->arg_names, param->name);
    }

    TypeExpr *ptr_t = type_new(arena, Type_Ptr);
    ptr_t->elem = proc_t;
    return ptr_t;
}

static TypeExpr *type_proc_from_callable_type(Program *prog, TypeExpr *type) {
    type = resolve_alias_type(prog, type);
    if (!type) return null;
    if (type->kind == Type_Proc) return type;
    if (type->kind == Type_Ptr) {
        TypeExpr *elem = resolve_alias_type(prog, type->elem);
        if (elem && elem->kind == Type_Proc) return elem;
    }
    return null;
}

static TypeExpr *infer_expr_type(Expr *e, TypeScope *scope, Program *prog, memops_arena *arena) {
    if (!e) return null;
    if (e->kind == Expr_Name) {
        if (string8_equals_cstr(&e->name, "null")) {
            TypeExpr *void_t = type_name_expr(arena, "void");
            TypeExpr *ptr_t = type_new(arena, Type_Ptr);
            ptr_t->elem = void_t;
            return ptr_t;
        }
        /* `x<>` where x is a value, not a type. The parser writes every `<>` as
           `<name>_reflect` because it cannot tell the two apart; here the type
           is known, so a value's `<>` becomes its type's record. Guarded on the
           written name being absent, which leaves `Type<>` alone -- that name
           already resolves. Cleared afterwards so the rewrite happens once. */
        if (e->reflect_base.data && !type_scope_lookup(scope, e->name)) {
            TypeExpr *base = type_scope_lookup(scope, e->reflect_base);
            if (base) {
                base = type_scope_apply_sub(scope, arena, base);
            }
            if (base && (base->kind == Type_Name || base->kind == Type_Generic)) {
                /* A generic's record is under its monomorphised name --
                   `Box<i32>` reflects as `Box_i32_reflect` -- and type_mangle
                   is what produces that name everywhere else. This is the only
                   convenient way to reach it: `Box<i32><>` is not a spelling,
                   so before this the mangled global had to be written by hand. */
                string8 owner = base->kind == Type_Generic
                                    ? type_mangle(arena, base, (TypeSub){0})
                                    : base->name;
                e->name = concat_name2(arena, owner, "_",
                                       string8_from_cstr(arena, "reflect"));
                e->reflect_base = (string8){0};
            }
        }
        TypeExpr *scope_type = type_scope_lookup(scope, e->name);
        if (scope_type) return type_scope_apply_sub(scope, arena, scope_type);
        EnumDecl *enum_decl = lookup_enum_constant_decl(arena, prog, e->name);
        if (enum_decl) return type_name_expr_from_string(arena, enum_decl->name);
        return proc_decl_pointer_type(arena, lookup_proc_decl(prog, e->name));
    }
    if (e->kind == Expr_Number) {
        string8 n = e->number;
        for (u64 i = 0; i < n.length; i++) {
            if (n.data[i] == '.') {
                return type_name_expr(arena, "f32");
            }
        }
        if (n.length > 3 &&
            n.data[n.length - 3] == 'u' &&
            n.data[n.length - 2] == '6' &&
            n.data[n.length - 1] == '4') {
            return type_name_expr(arena, "u64");
        }
        return type_name_expr(arena, "i32");
    }
    if (e->kind == Expr_String) {
        TypeExpr *char_t = type_name_expr(arena, "c8");
        TypeExpr *ptr_t = type_new(arena, Type_Ptr);
        ptr_t->elem = char_t;
        return ptr_t;
    }
    if (e->kind == Expr_Char) {
        return type_name_expr(arena, "c8");
    }
    if (e->kind == Expr_SizeofType || e->kind == Expr_AlignofType) {
        return type_name_expr(arena, "usize");
    }
    if ((e->kind == Expr_ZeroInit || e->kind == Expr_InitList) && e->cast_type) {
        return e->cast_type;
    }
    if (e->kind == Expr_Addr) {
        TypeExpr *inner = infer_expr_type(e->inner, scope, prog, arena);
        if (!inner) return null;
        TypeExpr *ptr_t = type_new(arena, Type_Ptr);
        ptr_t->elem = inner;
        return ptr_t;
    }
    if (e->kind == Expr_Unary) {
        return infer_expr_type(e->inner, scope, prog, arena);
    }
    if (e->kind == Expr_Cast) {
        return type_scope_apply_sub(scope, arena, e->cast_type);
    }
    if (e->kind == Expr_CompoundInit) {
        return type_scope_apply_sub(scope, arena, e->cast_type);
    }
    if (e->kind == Expr_Index) {
        TypeExpr *base = resolve_alias_type(prog, infer_expr_type(e->base, scope, prog, arena));
        if (base && base->kind == Type_Ptr) return base->elem;
        if (base && base->kind == Type_Array) return base->elem;
        return null;
    }
    if (e->kind == Expr_Field) {
        EnumDecl *enum_decl = null;
        EnumItem *enum_item = null;
        if (resolve_enum_member_expr(prog, e, &enum_decl, &enum_item)) {
            return enum_item ? type_name_expr_from_string(arena, enum_decl->name) : null;
        }
        TypeExpr *base = infer_expr_type(e->base, scope, prog, arena);
        return lookup_field_type(prog, base, e->name, arena);
    }
    if (e->kind == Expr_Ternary) {
        TypeExpr *right = infer_expr_type(e->right, scope, prog, arena);
        TypeExpr *third = infer_expr_type(e->third, scope, prog, arena);
        if (right && third && type_compatible(prog, right, third)) return right;
        if (right && third && type_compatible(prog, third, right)) return third;
        return right;
    }
    if (e->kind == Expr_Call) {
        /* Indirect call: the result is whatever the callee's proc type returns.
           The callee is normally a pointer to a proc, so step through it. */
        if (e->base) {
            TypeExpr *callee = infer_expr_type(e->base, scope, prog, arena);
            TypeExpr *proc_type = callee ? type_proc_from_callable_type(prog, callee) : null;
            return proc_type ? proc_type->ret_type : null;
        }
        TypeExpr *type_arg = null;
        ProcDecl *decl = lookup_call_proc_decl(prog, e, scope, arena, &type_arg, null);
        if (decl) {
            if (decl->is_generic) {
                TypeSub call_sub = proc_call_sub(arena, decl, e, type_arg);
                if (call_sub.has) return substitute_type_sub(arena, decl->ret_type, call_sub);
            }
            if (decl->is_generic) {
                /* The type argument is missing or wrong, which is reported at the
                   call. Returning unknown keeps the unsubstituted type param from
                   leaking into surrounding checks as a second, confusing error. */
                return null;
            }
            return type_scope_apply_sub(scope, arena, decl->ret_type);
        }
        TypeExpr *callee_type = type_scope_lookup(scope, e->name);
        TypeExpr *proc_type = type_proc_from_callable_type(prog, callee_type);
        if (proc_type) {
            return proc_type->ret_type;
        }
    }
    if (e->kind == Expr_Binary) {
        TypeExpr *left = infer_expr_type(e->left, scope, prog, arena);
        TypeExpr *right = infer_expr_type(e->right, scope, prog, arena);
        if (e->op == Token_LAngle || e->op == Token_RAngle ||
            e->op == Token_LessEqual || e->op == Token_GreaterEqual ||
            e->op == Token_EqualEqual || e->op == Token_BangEqual ||
            e->op == Token_Keyword_And || e->op == Token_Keyword_Or) {
            return type_name_expr(arena, "b32");
        }
        if (e->op == Token_Minus && left && right && left->kind == Type_Ptr && right->kind == Type_Ptr) {
            return type_name_expr(arena, "long");
        }
        if (e->op == Token_Minus && left && right && left->kind == Type_Ptr && right->kind == Type_Array && type_expr_equal_resolved(prog, left->elem, right->elem)) {
            return type_name_expr(arena, "long");
        }
        if (e->op == Token_Minus && left && right && left->kind == Type_Array && right->kind == Type_Ptr && type_expr_equal_resolved(prog, left->elem, right->elem)) {
            return type_name_expr(arena, "long");
        }
        return infer_expr_type(e->left, scope, prog, arena);
    }
    return null;
}

static bool type_is_integer_name(string8 name) {
    return string8_equals_cstr(&name, "bool") ||
           string8_equals_cstr(&name, "char") ||
           string8_equals_cstr(&name, "BOOL") ||
           string8_equals_cstr(&name, "BOOLEAN") ||
           string8_equals_cstr(&name, "ATOM") ||
           string8_equals_cstr(&name, "BYTE") ||
           string8_equals_cstr(&name, "CHAR") ||
           string8_equals_cstr(&name, "DWORD") ||
           string8_equals_cstr(&name, "HRESULT") ||
           string8_equals_cstr(&name, "INT") ||
           string8_equals_cstr(&name, "INT8") ||
           string8_equals_cstr(&name, "INT16") ||
           string8_equals_cstr(&name, "INT32") ||
           string8_equals_cstr(&name, "INT64") ||
           string8_equals_cstr(&name, "LONG") ||
           string8_equals_cstr(&name, "LPARAM") ||
           string8_equals_cstr(&name, "LRESULT") ||
           string8_equals_cstr(&name, "SHORT") ||
           string8_equals_cstr(&name, "UINT") ||
           string8_equals_cstr(&name, "UINT8") ||
           string8_equals_cstr(&name, "UINT16") ||
           string8_equals_cstr(&name, "UINT32") ||
           string8_equals_cstr(&name, "UINT64") ||
           string8_equals_cstr(&name, "ULONG") ||
           string8_equals_cstr(&name, "USHORT") ||
           string8_equals_cstr(&name, "WPARAM") ||
           string8_equals_cstr(&name, "WORD") ||
           string8_equals_cstr(&name, "int") ||
           string8_equals_cstr(&name, "long") ||
           string8_equals_cstr(&name, "ma_bool32") ||
           string8_equals_cstr(&name, "ma_channel") ||
           string8_equals_cstr(&name, "ma_format") ||
           string8_equals_cstr(&name, "ma_int8") ||
           string8_equals_cstr(&name, "ma_int16") ||
           string8_equals_cstr(&name, "ma_int32") ||
           string8_equals_cstr(&name, "ma_int64") ||
           string8_equals_cstr(&name, "ma_result") ||
           string8_equals_cstr(&name, "ma_uint8") ||
           string8_equals_cstr(&name, "ma_uint16") ||
           string8_equals_cstr(&name, "ma_uint32") ||
           string8_equals_cstr(&name, "ma_uint64") ||
           string8_equals_cstr(&name, "short") ||
           string8_equals_cstr(&name, "u8") ||
           string8_equals_cstr(&name, "u16") ||
           string8_equals_cstr(&name, "u32") ||
           string8_equals_cstr(&name, "u64") ||
           string8_equals_cstr(&name, "usize") ||
           string8_equals_cstr(&name, "i8") ||
           string8_equals_cstr(&name, "i16") ||
           string8_equals_cstr(&name, "i32") ||
           string8_equals_cstr(&name, "i64") ||
           string8_equals_cstr(&name, "c8") ||
           string8_equals_cstr(&name, "b8") ||
           string8_equals_cstr(&name, "b16") ||
           string8_equals_cstr(&name, "b64") ||
           string8_equals_cstr(&name, "intptr") ||
           string8_equals_cstr(&name, "uintptr") ||
           string8_equals_cstr(&name, "ptrdiff") ||
           string8_equals_cstr(&name, "intmax") ||
           string8_equals_cstr(&name, "uintmax") ||
           string8_equals_cstr(&name, "b32");
}

static bool type_is_float_name(string8 name) {
    return string8_equals_cstr(&name, "DOUBLE") ||
           string8_equals_cstr(&name, "FLOAT") ||
           string8_equals_cstr(&name, "double") ||
           string8_equals_cstr(&name, "f32") ||
           string8_equals_cstr(&name, "f64") ||
           string8_equals_cstr(&name, "float");
}

static bool type_is_numeric(TypeExpr *type) {
    if (!type || type->kind != Type_Name) return false;
    return type_is_integer_name(type->name) || type_is_float_name(type->name);
}

static bool type_is_integer(TypeExpr *type) {
    if (!type || type->kind != Type_Name) return false;
    return type_is_integer_name(type->name);
}

static bool type_is_float(TypeExpr *type) {
    if (!type || type->kind != Type_Name) return false;
    return type_is_float_name(type->name);
}

static bool type_is_boolish(TypeExpr *type) {
    if (!type || type->kind != Type_Name) return false;
    return string8_equals_cstr(&type->name, "b32") ||  // the default
           string8_equals_cstr(&type->name, "b8") ||
           string8_equals_cstr(&type->name, "b16") ||
           string8_equals_cstr(&type->name, "b64");
}

static bool type_is_void_type(Program *prog, TypeExpr *type) {
    type = resolve_alias_type(prog, type);
    return type &&
           type->kind == Type_Name &&
           string8_equals_cstr(&type->name, "void");
}

static void type_error_index_base(TypeExpr *base, i32 line, i32 col, memops_arena *arena) {
    string8 base_name = type_mangle(arena, base, (TypeSub){0});
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "cannot index non-array/non-pointer type '%.*s'",
            (int)base_name.length,
            base_name.data
        );
        diag_json_error(diag_current_path(), line, col, "type", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: cannot index non-array/non-pointer type '%.*s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        (int)base_name.length,
        base_name.data
    );
    diag_finish_at(line, col);
}

static void type_error_index_value(TypeExpr *index_type, i32 line, i32 col, memops_arena *arena) {
    string8 index_name = type_mangle(arena, index_type, (TypeSub){0});
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "index expression must be numeric, got '%.*s'",
            (int)index_name.length,
            index_name.data
        );
        diag_json_error(diag_current_path(), line, col, "type", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: index expression must be numeric, got '%.*s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        (int)index_name.length,
        index_name.data
    );
    diag_finish_at(line, col);
}

static bool type_is_void_pointer(TypeExpr *type) {
    return type &&
           type->kind == Type_Ptr &&
           type->elem &&
           type->elem->kind == Type_Name &&
           string8_equals_cstr(&type->elem->name, "void");
}

static bool type_pointer_like_pointee_is_const(Program *prog, TypeExpr *type) {
    type = resolve_alias_type(prog, type);
    if (!type) return false;
    if (type->kind == Type_Array) {
        if (type->is_const) return true;
        TypeExpr *elem = resolve_alias_type(prog, type->elem);
        return elem && elem->is_const;
    }
    if (type->kind != Type_Ptr || !type->elem) return false;
    TypeExpr *elem = resolve_alias_type(prog, type->elem);
    return elem && elem->is_const;
}

static bool type_void_pointer_const_compatible(Program *prog, TypeExpr *dst, TypeExpr *src) {
    if (!dst || !src) return false;
    dst = resolve_alias_type(prog, dst);
    src = resolve_alias_type(prog, src);
    if (!dst || !src) return false;
    if (!type_is_void_pointer(dst) && !type_is_void_pointer(src)) return false;
    return !type_pointer_like_pointee_is_const(prog, src) ||
           type_pointer_like_pointee_is_const(prog, dst);
}

static bool type_is_c_opaque_pointer_name(string8 name) {
    return string8_equals_cstr(&name, "HANDLE") ||
           string8_equals_cstr(&name, "HBRUSH") ||
           string8_equals_cstr(&name, "HCURSOR") ||
           string8_equals_cstr(&name, "HDC") ||
           string8_equals_cstr(&name, "HICON") ||
           string8_equals_cstr(&name, "HINSTANCE") ||
           string8_equals_cstr(&name, "HMODULE") ||
           string8_equals_cstr(&name, "HMENU") ||
           string8_equals_cstr(&name, "HWND");
}

static bool type_is_c_opaque_proc_pointer_name(string8 name) {
    return string8_equals_cstr(&name, "FARPROC") ||
           string8_equals_cstr(&name, "PROC");
}

static TypeExpr *resolve_alias_type(Program *prog, TypeExpr *type) {
    if (!prog || !type || type->kind != Type_Name) return type;
    TypeExpr *current = type;
    for (i32 depth = 0; depth < 32 && current && current->kind == Type_Name; depth++) {
        bool changed = false;
        for (i32 i = 0; i < prog->aliases.length; i++) {
            AliasDecl *decl = (AliasDecl *)prog->aliases.data[i];
            if (string8_equals(&decl->name, &current->name)) {
                current = decl->type;
                changed = true;
                break;
            }
        }
        if (!changed) break;
    }
    return current ? current : type;
}

static bool type_expr_equal_resolved(Program *prog, TypeExpr *a, TypeExpr *b) {
    if (!a || !b) return false;
    a = resolve_alias_type(prog, a);
    b = resolve_alias_type(prog, b);
    if (!a || !b) return false;
    if (a->kind != b->kind) return false;
    if (a->is_const != b->is_const) return false;
    if (a->kind == Type_Name) {
        return string8_equals(&a->name, &b->name);
    }
    if (a->kind == Type_Ptr) {
        return type_expr_equal_resolved(prog, a->elem, b->elem);
    }
    if (a->kind == Type_Array) {
        return string8_equals(&a->array_count, &b->array_count) && type_expr_equal_resolved(prog, a->elem, b->elem);
    }
    if (a->kind == Type_Generic) {
        if (!string8_equals(&a->name, &b->name) || a->args.length != b->args.length) return false;
        for (i32 i = 0; i < a->args.length; i++) {
            if (!type_expr_equal_resolved(prog, (TypeExpr *)a->args.data[i], (TypeExpr *)b->args.data[i])) return false;
        }
        return true;
    }
    if (a->kind == Type_Proc) {
        if (!string8_equals(&a->name, &b->name)) return false;
        if (a->is_variadic != b->is_variadic) return false;
        if (!type_expr_equal_resolved(prog, a->ret_type, b->ret_type) || a->args.length != b->args.length) return false;
        for (i32 i = 0; i < a->args.length; i++) {
            if (!type_expr_equal_resolved(prog, (TypeExpr *)a->args.data[i], (TypeExpr *)b->args.data[i])) return false;
        }
        return true;
    }
    return false;
}

static bool type_expr_assignable_qualified(Program *prog, TypeExpr *dst, TypeExpr *src, bool nested) {
    if (!dst || !src) return true;
    dst = resolve_alias_type(prog, dst);
    src = resolve_alias_type(prog, src);
    if (!dst || !src) return true;
    if (nested && src->is_const && !dst->is_const) return false;
    if (dst->kind != src->kind) return false;
    if (dst->kind == Type_Name) {
        return string8_equals(&dst->name, &src->name);
    }
    if (dst->kind == Type_Ptr) {
        return type_expr_assignable_qualified(prog, dst->elem, src->elem, true);
    }
    if (dst->kind == Type_Array) {
        return string8_equals(&dst->array_count, &src->array_count) &&
               type_expr_assignable_qualified(prog, dst->elem, src->elem, true);
    }
    if (dst->kind == Type_Generic) {
        if (!string8_equals(&dst->name, &src->name) || dst->args.length != src->args.length) return false;
        for (i32 i = 0; i < dst->args.length; i++) {
            TypeExpr *dst_arg = (TypeExpr *)dst->args.data[i];
            TypeExpr *src_arg = (TypeExpr *)src->args.data[i];
            if (!type_expr_assignable_qualified(prog, dst_arg, src_arg, true)) return false;
        }
        return true;
    }
    if (dst->kind == Type_Proc) {
        if (!string8_equals(&dst->name, &src->name)) return false;
        if (dst->is_variadic != src->is_variadic) return false;
        if (!type_expr_equal_resolved(prog, dst->ret_type, src->ret_type) || dst->args.length != src->args.length) {
            return false;
        }
        for (i32 i = 0; i < dst->args.length; i++) {
            TypeExpr *dst_arg = (TypeExpr *)dst->args.data[i];
            TypeExpr *src_arg = (TypeExpr *)src->args.data[i];
            if (!type_expr_equal_resolved(prog, dst_arg, src_arg)) return false;
        }
        return true;
    }
    return false;
}

static bool type_is_program_enum(Program *prog, TypeExpr *type) {
    if (!prog || !type || type->kind != Type_Name) return false;
    for (i32 i = 0; i < prog->enums.length; i++) {
        EnumDecl *decl = (EnumDecl *)prog->enums.data[i];
        if (string8_equals(&decl->name, &type->name)) {
            return true;
        }
    }
    return false;
}

static bool type_is_numeric_or_enum(Program *prog, TypeExpr *type) {
    type = resolve_alias_type(prog, type);
    return type_is_numeric(type) || type_is_program_enum(prog, type);
}

static bool type_is_integer_or_enum(Program *prog, TypeExpr *type) {
    type = resolve_alias_type(prog, type);
    return type_is_integer(type) || type_is_program_enum(prog, type);
}

static bool type_is_truthy(Program *prog, TypeExpr *type) {
    type = resolve_alias_type(prog, type);
    return type_is_numeric(type) ||
           type_is_program_enum(prog, type) ||
           (type && type->kind == Type_Ptr) ||
           (type && type->kind == Type_Array);
}

static bool type_compatible(Program *prog, TypeExpr *dst, TypeExpr *src) {
    if (!dst || !src) return true;
    dst = resolve_alias_type(prog, dst);
    src = resolve_alias_type(prog, src);
    if (type_expr_equal_resolved(prog, dst, src)) return true;
    if (type_expr_assignable_qualified(prog, dst, src, false)) return true;
    if (dst->kind == Type_Proc && src->kind == Type_Ptr && src->elem && type_expr_assignable_qualified(prog, dst, src->elem, false)) return true;
    if (src->kind == Type_Proc && dst->kind == Type_Ptr && dst->elem && type_expr_assignable_qualified(prog, dst->elem, src, false)) return true;
    if (type_is_numeric(dst) && type_is_numeric(src)) return true;
    if (type_is_program_enum(prog, src) && (type_is_integer(dst) || type_is_boolish(dst))) return true;
    if (type_is_boolish(dst) && src->kind == Type_Ptr) return true;
    if (type_is_void_pointer(src) && (dst->kind == Type_Ptr || dst->kind == Type_Proc)) {
        return type_void_pointer_const_compatible(prog, dst, src);
    }
    if (type_is_void_pointer(src) && dst->kind == Type_Name && type_is_c_opaque_pointer_name(dst->name)) return true;
    if (dst->kind == Type_Ptr && src->kind == Type_Array) {
        if (type_is_void_pointer(dst)) return type_void_pointer_const_compatible(prog, dst, src);
        if (type_expr_assignable_qualified(prog, dst->elem, src->elem, true)) return true;
    }
    if (dst->kind == Type_Ptr && src->kind == Type_Ptr) {
        if (type_is_void_pointer(src)) return type_void_pointer_const_compatible(prog, dst, src);
        if (type_is_void_pointer(dst)) return type_void_pointer_const_compatible(prog, dst, src);
    }
    return false;
}

static bool type_allows_compound_assign(Program *prog, TokenKind op, TypeExpr *dst, TypeExpr *src) {
    if (op == Token_Equal) return type_compatible(prog, dst, src);
    if (!dst || !src) return true;
    dst = resolve_alias_type(prog, dst);
    src = resolve_alias_type(prog, src);
    bool dst_enum = type_is_program_enum(prog, dst);
    bool src_enum = type_is_program_enum(prog, src);
    bool integer_or_same_enum = type_is_integer_or_enum(prog, dst) &&
                                type_is_integer_or_enum(prog, src) &&
                                (!(dst_enum && src_enum) || type_expr_equal_resolved(prog, dst, src));
    if ((op == Token_PlusEqual || op == Token_MinusEqual) && dst->kind == Type_Ptr && type_is_integer_or_enum(prog, src)) {
        return true;
    }
    if (op == Token_PercentEqual ||
        op == Token_ShlEqual ||
        op == Token_ShrEqual ||
        op == Token_AmpersandEqual ||
        op == Token_CaretEqual ||
        op == Token_PipeEqual) {
        return integer_or_same_enum;
    }
    if ((op == Token_PlusEqual ||
         op == Token_MinusEqual ||
         op == Token_StarEqual ||
         op == Token_SlashEqual) &&
        type_is_numeric(dst) && type_is_numeric(src)) {
        return true;
    }
    return type_compatible(prog, dst, src);
}

static bool type_allows_cast(Program *prog, TypeExpr *dst, TypeExpr *src) {
    if (!dst || !src) return true;
    TypeExpr *raw_dst = dst;
    TypeExpr *raw_src = src;
    dst = resolve_alias_type(prog, dst);
    src = resolve_alias_type(prog, src);
    if (type_compatible(prog, dst, src)) return true;
    if (dst->kind == Type_Array || src->kind == Type_Array) return false;
    if (type_compatible(prog, src, dst)) return true;
    if (type_is_declared_aggregate(prog, dst) || type_is_declared_aggregate(prog, src)) return false;
    TypeExpr *dst_proc = type_proc_from_callable_type(prog, dst);
    TypeExpr *src_proc = type_proc_from_callable_type(prog, src);
    if (dst_proc || src_proc) {
        if (raw_dst && raw_dst->kind == Type_Name && type_is_c_opaque_proc_pointer_name(raw_dst->name)) return true;
        if (raw_src && raw_src->kind == Type_Name && type_is_c_opaque_proc_pointer_name(raw_src->name)) return true;
        return dst_proc && src_proc && type_expr_equal_resolved(prog, dst_proc, src_proc);
    }
    if ((dst->kind == Type_Ptr && type_is_float(src)) ||
        (src->kind == Type_Ptr && type_is_float(dst))) {
        return false;
    }
    if ((dst->kind == Type_Ptr || type_is_void_pointer(dst)) && (src->kind == Type_Ptr || type_is_integer(src))) return true;
    if ((src->kind == Type_Ptr || type_is_void_pointer(src)) && (dst->kind == Type_Ptr || type_is_integer(dst))) return true;
    if ((type_is_numeric(dst) || type_is_program_enum(prog, dst) || type_is_boolish(dst)) &&
        (type_is_numeric(src) || type_is_program_enum(prog, src) || type_is_boolish(src))) {
        return true;
    }
    return true;
}

static void type_note_array_pointer_mismatch(
    Program *prog,
    TypeExpr *expected,
    TypeExpr *actual,
    memops_arena *arena
);

static void type_json_note_mismatch_details(
    bool *has_note,
    Program *prog,
    TypeExpr *expected,
    TypeExpr *actual,
    memops_arena *arena
);

static void type_json_note_proc_signature_mismatch(
    bool *has_note,
    Program *prog,
    TypeExpr *expected,
    TypeExpr *actual,
    memops_arena *arena
);

static void type_note_pointer_value_mismatch(
    Program *prog,
    TypeExpr *expected,
    TypeExpr *actual
);

static void type_note_proc_signature(
    const char *label,
    TypeExpr *proc_type,
    memops_arena *arena
) {
    if (!proc_type || proc_type->kind != Type_Proc) return;
    printf("%s:0:0: note: %s proc signature: (", g_diag_source_path ? g_diag_source_path : g_source_path, label);
    for (i32 i = 0; i < proc_type->args.length; i++) {
        TypeExpr *arg_type = (TypeExpr *)proc_type->args.data[i];
        string8 arg_name = type_mangle(arena, arg_type, (TypeSub){0});
        if (i > 0) printf(", ");
        printf("arg%d:%.*s", i, (int)arg_name.length, arg_name.data);
    }
    if (proc_type->is_variadic) {
        if (proc_type->args.length > 0) printf(", ");
        printf("...");
    }
    string8 ret_name = type_mangle(arena, proc_type->ret_type, (TypeSub){0});
    printf(")->%.*s\n", (int)ret_name.length, ret_name.data);
}

static void type_note_proc_signature_mismatch(
    Program *prog,
    TypeExpr *expected,
    TypeExpr *actual,
    memops_arena *arena
) {
    TypeExpr *expected_proc = type_proc_from_callable_type(prog, expected);
    TypeExpr *actual_proc = type_proc_from_callable_type(prog, actual);
    if (!expected_proc || !actual_proc) return;
    type_note_proc_signature("expected", expected_proc, arena);
    type_note_proc_signature("actual", actual_proc, arena);
}

static void type_json_note_array_pointer_mismatch(
    bool *has_note,
    Program *prog,
    TypeExpr *expected,
    TypeExpr *actual,
    memops_arena *arena
) {
    expected = resolve_alias_type(prog, expected);
    actual = resolve_alias_type(prog, actual);
    if (!expected || !actual || expected->kind != Type_Ptr || actual->kind != Type_Array) return;
    if (!expected->elem || !actual->elem || type_expr_equal_resolved(prog, expected->elem, actual->elem)) return;

    string8 expected_elem = type_mangle(arena, expected->elem, (TypeSub){0});
    string8 actual_elem = type_mangle(arena, actual->elem, (TypeSub){0});
    char note[1024];
    snprintf(
        note,
        sizeof(note),
        "fixed array can decay to pointer only when element types match; expected element '%.*s', got '%.*s'",
        (int)expected_elem.length,
        expected_elem.data,
        (int)actual_elem.length,
        actual_elem.data
    );
    diag_json_note_cstr(has_note, diag_current_path(), 0, 0, note);
}

static void type_json_note_proc_signature(
    bool *has_note,
    const char *label,
    TypeExpr *proc_type,
    memops_arena *arena
) {
    if (!proc_type || proc_type->kind != Type_Proc) return;
    string8 note = string8_reserve(arena, 256);
    string8_append_cstr(arena, &note, label);
    string8_append_cstr(arena, &note, " proc signature: (");
    for (i32 i = 0; i < proc_type->args.length; i++) {
        TypeExpr *arg_type = (TypeExpr *)proc_type->args.data[i];
        string8 arg_name = type_mangle(arena, arg_type, (TypeSub){0});
        if (i > 0) string8_append_cstr(arena, &note, ", ");
        string8_append_cstr(arena, &note, "arg");
        char index_buf[32];
        snprintf(index_buf, sizeof(index_buf), "%d", i);
        string8_append_cstr(arena, &note, index_buf);
        string8_append_cstr(arena, &note, ":");
        string8_append_bytes(arena, &note, arg_name.data, arg_name.length);
    }
    if (proc_type->is_variadic) {
        if (proc_type->args.length > 0) string8_append_cstr(arena, &note, ", ");
        string8_append_cstr(arena, &note, "...");
    }
    string8 ret_name = type_mangle(arena, proc_type->ret_type, (TypeSub){0});
    string8_append_cstr(arena, &note, ")->");
    string8_append_bytes(arena, &note, ret_name.data, ret_name.length);
    string8_append_byte(arena, &note, 0);
    diag_json_note_cstr(has_note, diag_current_path(), 0, 0, (const char *)note.data);
}

static void type_json_note_proc_signature_mismatch(
    bool *has_note,
    Program *prog,
    TypeExpr *expected,
    TypeExpr *actual,
    memops_arena *arena
) {
    TypeExpr *expected_proc = type_proc_from_callable_type(prog, expected);
    TypeExpr *actual_proc = type_proc_from_callable_type(prog, actual);
    if (!expected_proc || !actual_proc) return;
    type_json_note_proc_signature(has_note, "expected", expected_proc, arena);
    type_json_note_proc_signature(has_note, "actual", actual_proc, arena);
}

static void type_error_incompatible(
    const char *context,
    Program *prog,
    TypeExpr *dst,
    TypeExpr *src,
    i32 line,
    i32 col,
    memops_arena *arena
) {
    string8 dst_name = type_mangle(arena, dst, (TypeSub){0});
    string8 src_name = type_mangle(arena, src, (TypeSub){0});
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "%s expected '%.*s', got '%.*s'",
            context,
            (int)dst_name.length,
            dst_name.data,
            (int)src_name.length,
            src_name.data
        );
        bool has_note = false;
        diag_json_open(diag_current_path(), line, col, "type", message);
        diag_json_note_import_chain(&has_note);
        type_json_note_mismatch_details(&has_note, prog, dst, src, arena);
        diag_json_close();
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: %s expected '%.*s', got '%.*s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        context,
        (int)dst_name.length,
        dst_name.data,
        (int)src_name.length,
        src_name.data
    );
    type_note_array_pointer_mismatch(prog, dst, src, arena);
    type_note_proc_signature_mismatch(prog, dst, src, arena);
    type_note_pointer_value_mismatch(prog, dst, src);
    diag_finish_at(line, col);
}

static void type_error_compound_assignment(
    TokenKind op,
    Program *prog,
    TypeExpr *dst,
    TypeExpr *src,
    i32 line,
    i32 col,
    memops_arena *arena
) {
    string8 dst_name = type_mangle(arena, resolve_alias_type(prog, dst), (TypeSub){0});
    string8 src_name = type_mangle(arena, resolve_alias_type(prog, src), (TypeSub){0});
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "operator %s cannot be applied to '%.*s' and '%.*s'",
            token_kind_name(op),
            (int)dst_name.length,
            dst_name.data,
            (int)src_name.length,
            src_name.data
        );
        bool has_note = false;
        diag_json_open(diag_current_path(), line, col, "type", message);
        diag_json_note_import_chain(&has_note);
        type_json_note_mismatch_details(&has_note, prog, dst, src, arena);
        diag_json_close();
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: operator %s cannot be applied to '%.*s' and '%.*s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        token_kind_name(op),
        (int)dst_name.length,
        dst_name.data,
        (int)src_name.length,
        src_name.data
    );
    type_note_array_pointer_mismatch(prog, dst, src, arena);
    type_note_proc_signature_mismatch(prog, dst, src, arena);
    type_note_pointer_value_mismatch(prog, dst, src);
    diag_finish_at(line, col);
}

static void type_error_cast(
    Program *prog,
    TypeExpr *dst,
    TypeExpr *src,
    i32 line,
    i32 col,
    memops_arena *arena
) {
    string8 dst_name = type_mangle(arena, resolve_alias_type(prog, dst), (TypeSub){0});
    string8 src_name = type_mangle(arena, resolve_alias_type(prog, src), (TypeSub){0});
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "cannot cast '%.*s' to '%.*s'",
            (int)src_name.length,
            src_name.data,
            (int)dst_name.length,
            dst_name.data
        );
        bool has_note = false;
        diag_json_open(diag_current_path(), line, col, "type", message);
        diag_json_note_import_chain(&has_note);
        type_json_note_proc_signature_mismatch(&has_note, prog, dst, src, arena);
        diag_json_close();
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: cannot cast '%.*s' to '%.*s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        (int)src_name.length,
        src_name.data,
        (int)dst_name.length,
        dst_name.data
    );
    type_note_proc_signature_mismatch(prog, dst, src, arena);
    diag_finish_at(line, col);
}

static void type_note_array_pointer_mismatch(
    Program *prog,
    TypeExpr *expected,
    TypeExpr *actual,
    memops_arena *arena
) {
    expected = resolve_alias_type(prog, expected);
    actual = resolve_alias_type(prog, actual);
    if (!expected || !actual || expected->kind != Type_Ptr || actual->kind != Type_Array) return;
    if (!expected->elem || !actual->elem || type_expr_equal_resolved(prog, expected->elem, actual->elem)) return;

    string8 expected_elem = type_mangle(arena, expected->elem, (TypeSub){0});
    string8 actual_elem = type_mangle(arena, actual->elem, (TypeSub){0});
    printf(
        "%s:0:0: note: fixed array can decay to pointer only when element types match; expected element '%.*s', got '%.*s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        (int)expected_elem.length,
        expected_elem.data,
        (int)actual_elem.length,
        actual_elem.data
    );
}

static bool type_expr_equal_ignoring_top_const(Program *prog, TypeExpr *a, TypeExpr *b) {
    a = resolve_alias_type(prog, a);
    b = resolve_alias_type(prog, b);
    if (!a || !b) return false;
    bool a_const = a->is_const;
    bool b_const = b->is_const;
    a->is_const = false;
    b->is_const = false;
    bool equal = type_expr_equal_resolved(prog, a, b);
    a->is_const = a_const;
    b->is_const = b_const;
    return equal;
}

static void type_note_pointer_value_mismatch(
    Program *prog,
    TypeExpr *expected,
    TypeExpr *actual
) {
    expected = resolve_alias_type(prog, expected);
    actual = resolve_alias_type(prog, actual);
    if (!expected || !actual) return;

    if (expected->kind == Type_Ptr && actual->kind != Type_Ptr && actual->kind != Type_Array) {
        if (type_expr_equal_ignoring_top_const(prog, expected->elem, actual)) {
            printf(
                "%s:0:0: note: expected a pointer; use '.&' to take the value address\n",
                g_diag_source_path ? g_diag_source_path : g_source_path
            );
        }
        return;
    }

    if (actual->kind == Type_Ptr && expected->kind != Type_Ptr && expected->kind != Type_Array) {
        if (type_expr_equal_ignoring_top_const(prog, expected, actual->elem)) {
            printf(
                "%s:0:0: note: got a pointer; use '[0]' to access the pointed value\n",
                g_diag_source_path ? g_diag_source_path : g_source_path
            );
        }
    }
}

static void type_json_note_pointer_value_mismatch(
    bool *has_note,
    Program *prog,
    TypeExpr *expected,
    TypeExpr *actual
) {
    expected = resolve_alias_type(prog, expected);
    actual = resolve_alias_type(prog, actual);
    if (!expected || !actual) return;

    if (expected->kind == Type_Ptr && actual->kind != Type_Ptr && actual->kind != Type_Array) {
        if (type_expr_equal_ignoring_top_const(prog, expected->elem, actual)) {
            diag_json_note_cstr(has_note, diag_current_path(), 0, 0, "expected a pointer; use '.&' to take the value address");
        }
        return;
    }

    if (actual->kind == Type_Ptr && expected->kind != Type_Ptr && expected->kind != Type_Array) {
        if (type_expr_equal_ignoring_top_const(prog, expected, actual->elem)) {
            diag_json_note_cstr(has_note, diag_current_path(), 0, 0, "got a pointer; use '[0]' to access the pointed value");
        }
    }
}

static void type_json_note_mismatch_details(
    bool *has_note,
    Program *prog,
    TypeExpr *expected,
    TypeExpr *actual,
    memops_arena *arena
) {
    type_json_note_array_pointer_mismatch(has_note, prog, expected, actual, arena);
    type_json_note_proc_signature_mismatch(has_note, prog, expected, actual, arena);
    type_json_note_pointer_value_mismatch(has_note, prog, expected, actual);
}

static const char *expr_kind_name(ExprKind kind) {
    if (kind == Expr_Name) return "name";
    if (kind == Expr_Number) return "number";
    if (kind == Expr_String) return "string";
    if (kind == Expr_Char) return "character literal";
    if (kind == Expr_Call) return "call";
    if (kind == Expr_Addr) return "address expression";
    if (kind == Expr_Binary) return "binary expression";
    if (kind == Expr_Index) return "index expression";
    if (kind == Expr_Field) return "field expression";
    if (kind == Expr_SizeofType) return "sizeof";
    if (kind == Expr_AlignofType) return "alignof";
    if (kind == Expr_ZeroInit) return "zero initializer";
    if (kind == Expr_InitList) return "initializer list";
    if (kind == Expr_CompoundInit) return "compound initializer";
    if (kind == Expr_Cast) return "cast";
    if (kind == Expr_Unary) return "unary expression";
    if (kind == Expr_Ternary) return "ternary expression";
    return "expression";
}

static bool expr_is_assignment_target(Expr *e) {
    if (!e) return false;
    return e->kind == Expr_Name ||
           e->kind == Expr_Index ||
           e->kind == Expr_Field;
}

static void type_error_assignment_target(Expr *lhs) {
    i32 line = lhs ? lhs->line : 0;
    i32 col = lhs ? lhs->col : 0;
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "assignment target must be a name, field, or indexed element; got %s",
            lhs ? expr_kind_name(lhs->kind) : "expression"
        );
        diag_json_error(diag_current_path(), line, col, "type", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: assignment target must be a name, field, or indexed element; got %s\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        lhs ? expr_kind_name(lhs->kind) : "expression"
    );
    diag_finish_at(line, col);
}

static bool type_is_const_lvalue_type(Program *prog, TypeExpr *type) {
    if (!type) return false;
    if (type->is_const) return true;
    TypeExpr *resolved = resolve_alias_type(prog, type);
    return resolved && resolved->is_const;
}

static TypeExpr *expr_const_lvalue_source_type(Expr *lhs, TypeScope *scope, Program *prog, memops_arena *arena) {
    if (!lhs) return null;
    TypeExpr *type = infer_expr_type(lhs, scope, prog, arena);
    if (type_is_const_lvalue_type(prog, type)) return type;

    if (lhs->kind == Expr_Index) {
        TypeExpr *base = infer_expr_type(lhs->base, scope, prog, arena);
        TypeExpr *resolved = resolve_alias_type(prog, base);
        if (resolved && resolved->kind == Type_Array && resolved->is_const) return base;
        return null;
    }

    if (lhs->kind == Expr_Field) {
        TypeExpr *base = infer_expr_type(lhs->base, scope, prog, arena);
        if (type_is_const_lvalue_type(prog, base)) return base;
        return null;
    }

    return null;
}

static void type_error_const_assignment(Program *prog, Expr *lhs, TypeExpr *target, TypeExpr *const_source, memops_arena *arena) {
    i32 line = lhs ? lhs->line : 0;
    i32 col = lhs ? lhs->col : 0;
    string8 target_name = type_mangle_concrete(arena, target);
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "cannot assign to const target of type '%.*s'",
            (int)target_name.length,
            target_name.data
        );
        if (const_source && !type_expr_equal_resolved(prog, const_source, target)) {
            string8 source_name = type_mangle_concrete(arena, const_source);
            char note[1024];
            snprintf(
                note,
                sizeof(note),
                "constness comes from lvalue base type '%.*s'",
                (int)source_name.length,
                source_name.data
            );
            diag_json_error_with_note(diag_current_path(), line, col, "type", message, diag_current_path(), 0, 0, note);
        } else {
            diag_json_error(diag_current_path(), line, col, "type", message);
        }
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: cannot assign to const target of type '%.*s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        (int)target_name.length,
        target_name.data
    );
    if (const_source && !type_expr_equal_resolved(prog, const_source, target)) {
        string8 source_name = type_mangle_concrete(arena, const_source);
        printf(
            "%s:0:0: note: constness comes from lvalue base type '%.*s'\n",
            diag_current_path(),
            (int)source_name.length,
            source_name.data
        );
    }
    diag_finish_at(line, col);
}

static void type_error_address_target(Expr *inner, i32 line, i32 col) {
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "address target must be a name, field, or indexed element; got %s",
            inner ? expr_kind_name(inner->kind) : "expression"
        );
        diag_json_error(diag_current_path(), line, col, "type", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: address target must be a name, field, or indexed element; got %s\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        inner ? expr_kind_name(inner->kind) : "expression"
    );
    diag_note_import_chain();
    diag_record_error();
    return;
}

static void type_note_proc_decl(ProcDecl *decl);

static void type_error_return_value_presence(
    Program *prog,
    TypeExpr *return_type,
    ProcDecl *proc,
    bool has_value,
    i32 line,
    i32 col,
    memops_arena *arena
) {
    if (has_value) {
        if (g_diag_json) {
            diag_json_error_with_note(
                diag_current_path(),
                line,
                col,
                "type",
                "void proc should not return a value",
                proc && proc->source_path ? proc->source_path : g_source_path,
                proc ? proc->line : 0,
                proc ? proc->col : 0,
                "proc declared here"
            );
            diag_record_error();
            return;
        }
        printf(
            "%s:%d:%d: type error: void proc should not return a value\n",
            g_diag_source_path ? g_diag_source_path : g_source_path,
            line,
            col
        );
    } else {
        string8 return_name = type_mangle(arena, resolve_alias_type(prog, return_type), (TypeSub){0});
        if (g_diag_json) {
            char message[1024];
            snprintf(
                message,
                sizeof(message),
                "non-void proc must return a value of type '%.*s'",
                (int)return_name.length,
                return_name.data
            );
            diag_json_error_with_note(
                diag_current_path(),
                line,
                col,
                "type",
                message,
                proc && proc->source_path ? proc->source_path : g_source_path,
                proc ? proc->line : 0,
                proc ? proc->col : 0,
                "proc declared here"
            );
            diag_record_error();
            return;
        }
        printf(
            "%s:%d:%d: type error: non-void proc must return a value of type '%.*s'\n",
            g_diag_source_path ? g_diag_source_path : g_source_path,
            line,
            col,
            (int)return_name.length,
            return_name.data
        );
    }
    diag_print_file_context(diag_current_path(), line, col);
    diag_note_import_chain();
    type_note_proc_decl(proc);
    diag_record_error();
    return;
}

static void type_note_proc_decl(ProcDecl *decl) {
    if (!decl) return;
    printf(
        "%s:%d:%d: note: proc '%.*s' declared here\n",
        decl->source_path ? decl->source_path : g_source_path,
        decl->line,
        decl->col,
        (int)decl->name.length,
        decl->name.data
    );
}

static void type_json_note_proc_expected_params(bool *has_note, ProcDecl *decl, memops_arena *arena) {
    if (!decl || !arena) return;
    char note[4096];
    size_t used = 0;
    diag_appendf(note, sizeof(note), &used, "expected params: ");
    for (i32 i = 0; i < decl->params.length; i++) {
        Param *param = (Param *)decl->params.data[i];
        string8 param_type = type_mangle(arena, param->type, (TypeSub){0});
        if (i > 0) diag_appendf(note, sizeof(note), &used, ", ");
        diag_appendf(
            note,
            sizeof(note),
            &used,
            "%.*s:%.*s",
            (int)param->name.length,
            param->name.data,
            (int)param_type.length,
            param_type.data
        );
    }
    if (decl->is_variadic) {
        if (decl->params.length > 0) diag_appendf(note, sizeof(note), &used, ", ");
        diag_appendf(note, sizeof(note), &used, "...");
    }
    diag_json_note_cstr(has_note, decl->source_path ? decl->source_path : g_source_path, 0, 0, note);
}

static void type_json_note_proc_pointer_expected_params(bool *has_note, TypeExpr *proc_type, memops_arena *arena) {
    if (!proc_type || !arena) return;
    char note[4096];
    size_t used = 0;
    diag_appendf(note, sizeof(note), &used, "expected params: ");
    for (i32 i = 0; i < proc_type->args.length; i++) {
        TypeExpr *arg_type = (TypeExpr *)proc_type->args.data[i];
        string8 arg_name = type_mangle(arena, arg_type, (TypeSub){0});
        string8 param_name = {0};
        if (i < proc_type->arg_names.length) param_name = proc_type->arg_names.data[i];
        if (i > 0) diag_appendf(note, sizeof(note), &used, ", ");
        if (param_name.length > 0) {
            diag_appendf(
                note,
                sizeof(note),
                &used,
                "%.*s:%.*s",
                (int)param_name.length,
                param_name.data,
                (int)arg_name.length,
                arg_name.data
            );
        } else {
            diag_appendf(note, sizeof(note), &used, "arg%d:%.*s", i, (int)arg_name.length, arg_name.data);
        }
    }
    if (proc_type->is_variadic) {
        if (proc_type->args.length > 0) diag_appendf(note, sizeof(note), &used, ", ");
        diag_appendf(note, sizeof(note), &used, "...");
    }
    diag_json_note_cstr(has_note, diag_current_path(), 0, 0, note);
}

static void type_error_proc_type_arg_count(
    ProcDecl *decl,
    i32 got,
    i32 line,
    i32 col
) {
    if (decl->is_generic) {
        if (g_diag_json) {
            char message[1024];
            snprintf(
                message,
                sizeof(message),
                "generic proc '%.*s' expects %d type arg%s, got %d",
                (int)decl->name.length,
                decl->name.data,
                (int)(decl->type_params.length > 0 ? decl->type_params.length : 1),
                (decl->type_params.length == 1 || decl->type_params.length == 0) ? "" : "s",
                got
            );
            diag_json_error_with_note(
                diag_current_path(),
                line,
                col,
                "type",
                message,
                decl->source_path ? decl->source_path : g_source_path,
                decl->line,
                decl->col,
                "proc declared here"
            );
            diag_record_error();
            return;
        }
        printf(
            "%s:%d:%d: type error: generic proc '%.*s' expects %d type arg%s, got %d\n",
            g_diag_source_path ? g_diag_source_path : g_source_path,
            line,
            col,
            (int)decl->name.length,
            decl->name.data,
            (int)(decl->type_params.length > 0 ? decl->type_params.length : 1),
            (decl->type_params.length == 1 || decl->type_params.length == 0) ? "" : "s",
            got
        );
    } else {
        if (g_diag_json) {
            char message[1024];
            snprintf(
                message,
                sizeof(message),
                "proc '%.*s' is not generic; got %d type arg%s",
                (int)decl->name.length,
                decl->name.data,
                got,
                got == 1 ? "" : "s"
            );
            diag_json_error_with_note(
                diag_current_path(),
                line,
                col,
                "type",
                message,
                decl->source_path ? decl->source_path : g_source_path,
                decl->line,
                decl->col,
                "proc declared here"
            );
            diag_record_error();
            return;
        }
        printf(
            "%s:%d:%d: type error: proc '%.*s' is not generic; got %d type arg%s\n",
            g_diag_source_path ? g_diag_source_path : g_source_path,
            line,
            col,
            (int)decl->name.length,
            decl->name.data,
            got,
            got == 1 ? "" : "s"
        );
    }
    diag_print_file_context(diag_current_path(), line, col);
    diag_note_import_chain();
    type_note_proc_decl(decl);
    diag_record_error();
    return;
}

static void type_error_proc_arg_count(
    ProcDecl *decl,
    i32 got,
    i32 line,
    i32 col,
    memops_arena *arena
) {
    if (decl->is_variadic) {
        if (g_diag_json) {
            char message[1024];
            snprintf(
                message,
                sizeof(message),
                "proc '%.*s' expects at least %d args, got %d",
                (int)decl->name.length,
                decl->name.data,
                (int)decl->params.length,
                got
            );
            bool has_note = false;
            diag_json_open(diag_current_path(), line, col, "type", message);
            diag_json_note_import_chain(&has_note);
            type_json_note_proc_expected_params(&has_note, decl, arena);
            diag_json_note_cstr(
                &has_note,
                decl->source_path ? decl->source_path : g_source_path,
                decl->line,
                decl->col,
                "proc declared here"
            );
            diag_json_close();
            diag_record_error();
            return;
        }
        printf(
            "%s:%d:%d: type error: proc '%.*s' expects at least %d args, got %d\n",
            g_diag_source_path ? g_diag_source_path : g_source_path,
            line,
            col,
            (int)decl->name.length,
            decl->name.data,
            (int)decl->params.length,
            got
        );
    } else {
        if (g_diag_json) {
            char message[1024];
            snprintf(
                message,
                sizeof(message),
                "proc '%.*s' expects %d args, got %d",
                (int)decl->name.length,
                decl->name.data,
                (int)decl->params.length,
                got
            );
            bool has_note = false;
            diag_json_open(diag_current_path(), line, col, "type", message);
            diag_json_note_import_chain(&has_note);
            type_json_note_proc_expected_params(&has_note, decl, arena);
            diag_json_note_cstr(
                &has_note,
                decl->source_path ? decl->source_path : g_source_path,
                decl->line,
                decl->col,
                "proc declared here"
            );
            diag_json_close();
            diag_record_error();
            return;
        }
        printf(
            "%s:%d:%d: type error: proc '%.*s' expects %d args, got %d\n",
            g_diag_source_path ? g_diag_source_path : g_source_path,
            line,
            col,
            (int)decl->name.length,
            decl->name.data,
            (int)decl->params.length,
            got
        );
    }
    diag_print_file_context(diag_current_path(), line, col);
    printf("%s:0:0: note: expected params: ", decl->source_path ? decl->source_path : g_source_path);
    for (i32 i = 0; i < decl->params.length; i++) {
        Param *param = (Param *)decl->params.data[i];
        string8 param_type = type_mangle(arena, param->type, (TypeSub){0});
        if (i > 0) printf(", ");
        printf(
            "%.*s:%.*s",
            (int)param->name.length,
            param->name.data,
            (int)param_type.length,
            param_type.data
        );
    }
    if (decl->is_variadic) {
        if (decl->params.length > 0) printf(", ");
        printf("...");
    }
    printf("\n");
    diag_note_import_chain();
    type_note_proc_decl(decl);
    diag_record_error();
    return;
}

static void type_error_proc_argument(
    Program *prog,
    ProcDecl *decl,
    Param *param,
    i32 arg_index,
    TypeExpr *generic_arg,
    TypeExpr *expected,
    TypeExpr *actual,
    i32 line,
    i32 col,
    memops_arena *arena
) {
    string8 expected_name = type_mangle(arena, expected, (TypeSub){0});
    string8 actual_name = type_mangle(arena, actual, (TypeSub){0});
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "proc '%.*s' argument %d '%.*s' expected '%.*s', got '%.*s'",
            (int)decl->name.length,
            decl->name.data,
            arg_index + 1,
            (int)param->name.length,
            param->name.data,
            (int)expected_name.length,
            expected_name.data,
            (int)actual_name.length,
            actual_name.data
        );
        bool has_note = false;
        diag_json_open(diag_current_path(), line, col, "type", message);
        diag_json_note_import_chain(&has_note);
        type_json_note_mismatch_details(&has_note, prog, expected, actual, arena);
        if (decl->is_generic && generic_arg) {
            string8 generic_arg_name = type_mangle(arena, generic_arg, (TypeSub){0});
            char note[512];
            snprintf(
                note,
                sizeof(note),
                "generic '%.*s' instantiated here with type '%.*s'",
                (int)decl->name.length,
                decl->name.data,
                (int)generic_arg_name.length,
                generic_arg_name.data
            );
            diag_json_note_cstr(&has_note, diag_current_path(), line, col, note);
        }
        diag_json_note_cstr(
            &has_note,
            decl->source_path ? decl->source_path : g_source_path,
            param->line,
            param->col,
            "parameter declared here"
        );
        diag_json_close();
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: proc '%.*s' argument %d '%.*s' expected '%.*s', got '%.*s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        (int)decl->name.length,
        decl->name.data,
        arg_index + 1,
        (int)param->name.length,
        param->name.data,
        (int)expected_name.length,
        expected_name.data,
        (int)actual_name.length,
        actual_name.data
    );
    type_note_array_pointer_mismatch(prog, expected, actual, arena);
    type_note_pointer_value_mismatch(prog, expected, actual);
    diag_print_file_context(diag_current_path(), line, col);
    diag_note_import_chain();
    if (decl->is_generic && generic_arg) {
        string8 generic_arg_name = type_mangle(arena, generic_arg, (TypeSub){0});
        printf(
            "%s:%d:%d: note: generic '%.*s' instantiated here with type '%.*s'\n",
            diag_current_path(),
            line,
            col,
            (int)decl->name.length,
            decl->name.data,
            (int)generic_arg_name.length,
            generic_arg_name.data
        );
    }
    printf(
        "%s:%d:%d: note: parameter '%.*s' declared here\n",
        decl->source_path ? decl->source_path : g_source_path,
        param->line,
        param->col,
        (int)param->name.length,
        param->name.data
    );
    type_note_proc_decl(decl);
    diag_record_error();
    return;
}

/* A call whose name resolves to nothing at all. This used to return quietly,
   which is how `sops_skin_state_deinit`, `pacops_character_name` and
   `guiops_layout_content_height` -- none of which exist -- passed `i: checked`
   and were caught only by clang, pointing at generated code. A `cinclude` brings
   no declarations with it: C functions are named in I or they are not callable.
   See docs/name-resolution.md. */
/* Compiler builtins that are spelled like calls. `printfmt` never reaches the
   backend as a call at all -- it is lowered to a printf during emission -- so it
   has no declaration to find and must not be reported as undeclared. `sizeof`
   and `alignof` used to belong on this list; they are now their own node kind
   and cannot reach the call path. */
static bool call_name_is_builtin(string8 name) {
    return string8_equals_cstr(&name, "printfmt");
}

static void type_error_call_undeclared(
    string8 name,
    i32 line,
    i32 col
) {
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "call to undeclared proc '%.*s'; declare it, or add "
            "`%.*s: proc[external](...) -> T = {}` if it comes from C",
            (int)name.length,
            name.data,
            (int)name.length,
            name.data
        );
        diag_json_error(diag_current_path(), line, col, "type", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: call to undeclared proc '%.*s'; declare it, or add "
        "`%.*s: proc[external](...) -> T = {}` if it comes from C\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        (int)name.length,
        name.data,
        (int)name.length,
        name.data
    );
    diag_finish_at(line, col);
}

static void type_error_call_non_proc(
    string8 name,
    TypeExpr *callee,
    i32 line,
    i32 col,
    memops_arena *arena
) {
    /* `callee` is null whenever the callee expression has no inferable type: an
       undeclared receiver (`nosuch.method()`), a field that does not exist on a
       primitive (`n.g()` with `n: i32`), an index into an undeclared name, or a
       void-valued call used as a base. Mangling null dereferences it, so name
       the type only when there is one. type_mangle itself stays strict, because
       a null reaching it from a real mangling context is a compiler bug that
       should stay loud rather than emit a bogus symbol. */
    string8 callee_name = callee ? type_mangle(arena, callee, (TypeSub){0})
                                 : string8_from_cstr(arena, "unknown");
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "cannot call non-proc symbol '%.*s' of type '%.*s'",
            (int)name.length,
            name.data,
            (int)callee_name.length,
            callee_name.data
        );
        diag_json_error(diag_current_path(), line, col, "type", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: cannot call non-proc symbol '%.*s' of type '%.*s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        (int)name.length,
        name.data,
        (int)callee_name.length,
        callee_name.data
    );
    diag_finish_at(line, col);
}

static void type_error_proc_pointer_type_arg_count(
    string8 name,
    i32 got,
    i32 line,
    i32 col
) {
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "proc pointer '%.*s' is not generic; got %d type arg%s",
            (int)name.length,
            name.data,
            got,
            got == 1 ? "" : "s"
        );
        diag_json_error(diag_current_path(), line, col, "type", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: proc pointer '%.*s' is not generic; got %d type arg%s\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        (int)name.length,
        name.data,
        got,
        got == 1 ? "" : "s"
    );
    diag_finish_at(line, col);
}

static void type_error_proc_pointer_arg_count(
    string8 name,
    TypeExpr *proc_type,
    i32 got,
    i32 line,
    i32 col,
    memops_arena *arena
) {
    if (proc_type->is_variadic) {
        if (g_diag_json) {
            char message[1024];
            snprintf(
                message,
                sizeof(message),
                "proc pointer '%.*s' expects at least %d args, got %d",
                (int)name.length,
                name.data,
                (int)proc_type->args.length,
                got
            );
            bool has_note = false;
            diag_json_open(diag_current_path(), line, col, "type", message);
            diag_json_note_import_chain(&has_note);
            type_json_note_proc_pointer_expected_params(&has_note, proc_type, arena);
            diag_json_close();
            diag_record_error();
            return;
        }
        printf(
            "%s:%d:%d: type error: proc pointer '%.*s' expects at least %d args, got %d\n",
            g_diag_source_path ? g_diag_source_path : g_source_path,
            line,
            col,
            (int)name.length,
            name.data,
            (int)proc_type->args.length,
            got
        );
    } else {
        if (g_diag_json) {
            char message[1024];
            snprintf(
                message,
                sizeof(message),
                "proc pointer '%.*s' expects %d args, got %d",
                (int)name.length,
                name.data,
                (int)proc_type->args.length,
                got
            );
            bool has_note = false;
            diag_json_open(diag_current_path(), line, col, "type", message);
            diag_json_note_import_chain(&has_note);
            type_json_note_proc_pointer_expected_params(&has_note, proc_type, arena);
            diag_json_close();
            diag_record_error();
            return;
        }
        printf(
            "%s:%d:%d: type error: proc pointer '%.*s' expects %d args, got %d\n",
            g_diag_source_path ? g_diag_source_path : g_source_path,
            line,
            col,
            (int)name.length,
            name.data,
            (int)proc_type->args.length,
            got
        );
    }
    printf("%s:0:0: note: expected params: ", diag_current_path());
    for (i32 i = 0; i < proc_type->args.length; i++) {
        TypeExpr *arg_type = (TypeExpr *)proc_type->args.data[i];
        string8 arg_name = type_mangle(arena, arg_type, (TypeSub){0});
        string8 param_name = {0};
        if (i < proc_type->arg_names.length) param_name = proc_type->arg_names.data[i];
        if (i > 0) printf(", ");
        if (param_name.length > 0) {
            printf("%.*s:%.*s", (int)param_name.length, param_name.data, (int)arg_name.length, arg_name.data);
        } else {
            printf("arg%d:%.*s", i, (int)arg_name.length, arg_name.data);
        }
    }
    if (proc_type->is_variadic) {
        if (proc_type->args.length > 0) printf(", ");
        printf("...");
    }
    printf("\n");
    diag_finish_at(line, col);
}

static void type_error_proc_pointer_argument(
    Program *prog,
    string8 name,
    TypeExpr *proc_type,
    i32 arg_index,
    TypeExpr *expected,
    TypeExpr *actual,
    i32 line,
    i32 col,
    memops_arena *arena
) {
    string8 expected_name = type_mangle(arena, expected, (TypeSub){0});
    string8 actual_name = type_mangle(arena, actual, (TypeSub){0});
    string8 param_name = {0};
    if (arg_index >= 0 && arg_index < proc_type->arg_names.length) {
        param_name = proc_type->arg_names.data[arg_index];
    }
    const char *param_text = param_name.data ? (const char *)param_name.data : "";
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "proc pointer '%.*s' argument %d%s%.*s%s expected '%.*s', got '%.*s'",
            (int)name.length,
            name.data,
            arg_index + 1,
            param_name.length > 0 ? " '" : "",
            (int)param_name.length,
            param_text,
            param_name.length > 0 ? "'" : "",
            (int)expected_name.length,
            expected_name.data,
            (int)actual_name.length,
            actual_name.data
        );
        bool has_note = false;
        diag_json_open(diag_current_path(), line, col, "type", message);
        diag_json_note_import_chain(&has_note);
        type_json_note_mismatch_details(&has_note, prog, expected, actual, arena);
        type_json_note_proc_pointer_expected_params(&has_note, proc_type, arena);
        diag_json_close();
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: proc pointer '%.*s' argument %d%s%.*s%s expected '%.*s', got '%.*s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        (int)name.length,
        name.data,
        arg_index + 1,
        param_name.length > 0 ? " '" : "",
        (int)param_name.length,
        param_text,
        param_name.length > 0 ? "'" : "",
        (int)expected_name.length,
        expected_name.data,
        (int)actual_name.length,
        actual_name.data
    );
    type_note_array_pointer_mismatch(prog, expected, actual, arena);
    type_note_pointer_value_mismatch(prog, expected, actual);
    printf("%s:0:0: note: expected params: ", diag_current_path());
    for (i32 i = 0; i < proc_type->args.length; i++) {
        TypeExpr *arg_type = (TypeExpr *)proc_type->args.data[i];
        string8 arg_name = type_mangle(arena, arg_type, (TypeSub){0});
        string8 expected_param_name = {0};
        if (i < proc_type->arg_names.length) expected_param_name = proc_type->arg_names.data[i];
        if (i > 0) printf(", ");
        if (expected_param_name.length > 0) {
            printf("%.*s:%.*s", (int)expected_param_name.length, expected_param_name.data, (int)arg_name.length, arg_name.data);
        } else {
            printf("arg%d:%.*s", i, (int)arg_name.length, arg_name.data);
        }
    }
    if (proc_type->is_variadic) {
        if (proc_type->args.length > 0) printf(", ");
        printf("...");
    }
    printf("\n");
    diag_finish_at(line, col);
}

static void type_error_binary_op(TokenKind op, TypeExpr *left, TypeExpr *right, i32 line, i32 col, memops_arena *arena) {
    string8 left_name = type_mangle(arena, left, (TypeSub){0});
    string8 right_name = type_mangle(arena, right, (TypeSub){0});
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "operator %s cannot be applied to '%.*s' and '%.*s'",
            token_kind_name(op),
            (int)left_name.length,
            left_name.data,
            (int)right_name.length,
            right_name.data
        );
        diag_json_error(diag_current_path(), line, col, "type", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: operator %s cannot be applied to '%.*s' and '%.*s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        token_kind_name(op),
        (int)left_name.length,
        left_name.data,
        (int)right_name.length,
        right_name.data
    );
    diag_finish_at(line, col);
}

static void type_error_ternary_condition(TypeExpr *cond, i32 line, i32 col, memops_arena *arena) {
    string8 cond_name = type_mangle(arena, cond, (TypeSub){0});
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "ternary condition must be scalar/pointer, got '%.*s'",
            (int)cond_name.length,
            cond_name.data
        );
        diag_json_error(diag_current_path(), line, col, "type", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: ternary condition must be scalar/pointer, got '%.*s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        (int)cond_name.length,
        cond_name.data
    );
    diag_finish_at(line, col);
}

static void type_error_condition(const char *context, TypeExpr *cond, i32 line, i32 col, memops_arena *arena) {
    string8 cond_name = type_mangle(arena, cond, (TypeSub){0});
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "%s condition must be scalar/pointer, got '%.*s'",
            context,
            (int)cond_name.length,
            cond_name.data
        );
        diag_json_error(diag_current_path(), line, col, "type", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: %s condition must be scalar/pointer, got '%.*s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        context,
        (int)cond_name.length,
        cond_name.data
    );
    diag_finish_at(line, col);
}

static void type_error_ternary_arms(TypeExpr *right, TypeExpr *third, i32 line, i32 col, memops_arena *arena) {
    string8 right_name = type_mangle(arena, right, (TypeSub){0});
    string8 third_name = type_mangle(arena, third, (TypeSub){0});
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "ternary arms cannot mix '%.*s' and '%.*s'",
            (int)right_name.length,
            right_name.data,
            (int)third_name.length,
            third_name.data
        );
        diag_json_error(diag_current_path(), line, col, "type", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: ternary arms cannot mix '%.*s' and '%.*s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        (int)right_name.length,
        right_name.data,
        (int)third_name.length,
        third_name.data
    );
    diag_finish_at(line, col);
}

static void type_error_enum_member(EnumDecl *decl, string8 member_name, i32 line, i32 col) {
    if (!decl) return;
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "enum '%.*s' has no member '%.*s'",
            (int)decl->name.length,
            decl->name.data,
            (int)member_name.length,
            member_name.data
        );
        diag_json_error_range(diag_current_path(), line, col, line, col + (i32)member_name.length + 1, "type", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: enum '%.*s' has no member '%.*s'\n",
        diag_current_path(),
        line,
        col,
        (int)decl->name.length,
        decl->name.data,
        (int)member_name.length,
        member_name.data
    );
    diag_finish_range(line, col, (i32)member_name.length + 1);
}

/* Types that provably cannot have a field, so `.name` on them is always wrong.

   Deliberately a closed list of I's own scalars plus arrays rather than
   "anything not a declared aggregate". A type name the compiler does not know
   is a foreign C type arriving through a `cinclude` -- D3D11_RASTERIZER_DESC
   and friends are declared in `d3d11.h` and nowhere in I -- and its fields are
   genuinely unknown here, so reporting on those would be a false positive.
   That silence is the type-level twin of the undeclared-call bargain in
   `docs/name-resolution.md`, and it goes away with the same fix. */
static bool type_never_has_fields(TypeExpr *type) {
    if (!type) return false;
    if (type->kind == Type_Array) return true;
    if (type->kind != Type_Name) return false;
    string8 n = type->name;
    return string8_equals_cstr(&n, "i8") ||
           string8_equals_cstr(&n, "i16") ||
           string8_equals_cstr(&n, "i32") ||
           string8_equals_cstr(&n, "i64") ||
           string8_equals_cstr(&n, "u8") ||
           string8_equals_cstr(&n, "u16") ||
           string8_equals_cstr(&n, "u32") ||
           string8_equals_cstr(&n, "u64") ||
           string8_equals_cstr(&n, "f32") ||
           string8_equals_cstr(&n, "f64") ||
           string8_equals_cstr(&n, "usize") ||
           string8_equals_cstr(&n, "b32") ||
           string8_equals_cstr(&n, "c8") ||
           string8_equals_cstr(&n, "b8") ||
           string8_equals_cstr(&n, "b16") ||
           string8_equals_cstr(&n, "b64") ||
           string8_equals_cstr(&n, "intptr") ||
           string8_equals_cstr(&n, "uintptr") ||
           string8_equals_cstr(&n, "ptrdiff") ||
           string8_equals_cstr(&n, "intmax") ||
           string8_equals_cstr(&n, "uintmax") ||
           string8_equals_cstr(&n, "void");
}

static void type_error_field_access(
    Program *prog,
    Expr *base_expr,
    TypeExpr *base,
    string8 field_name,
    i32 line,
    i32 col,
    memops_arena *arena
) {
    if (!base) {
        if (g_diag_json) {
            char message[1024];
            snprintf(
                message,
                sizeof(message),
                "cannot resolve base type for field '%.*s'",
                (int)field_name.length,
                field_name.data
            );
            diag_json_error(diag_current_path(), line, col, "type", message);
            diag_record_error();
            return;
        }
        printf(
            "%s:%d:%d: type error: cannot resolve base type for field '%.*s'\n",
            g_diag_source_path ? g_diag_source_path : g_source_path,
            line,
            col,
            (int)field_name.length,
            field_name.data
        );
        diag_finish_at(line, col);
        return;
    }

    TypeExpr *resolved = resolve_alias_type(prog, base);
    string8 base_name = type_mangle(arena, resolved, (TypeSub){0});
    if (resolved->kind == Type_Ptr) {
        if (g_diag_json) {
            char message[1024];
            if (base_expr && base_expr->kind == Expr_Name) {
                snprintf(
                    message,
                    sizeof(message),
                    "field '%.*s' cannot be accessed on pointer type '%.*s'; use %.*s[0].%.*s",
                    (int)field_name.length,
                    field_name.data,
                    (int)base_name.length,
                    base_name.data,
                    (int)base_expr->name.length,
                    base_expr->name.data,
                    (int)field_name.length,
                    field_name.data
                );
            } else {
                snprintf(
                    message,
                    sizeof(message),
                    "field '%.*s' cannot be accessed on pointer type '%.*s'; index the pointer before accessing '.%.*s'",
                    (int)field_name.length,
                    field_name.data,
                    (int)base_name.length,
                    base_name.data,
                    (int)field_name.length,
                    field_name.data
                );
            }
            diag_json_error(diag_current_path(), line, col, "type", message);
            diag_record_error();
            return;
        }
        if (base_expr && base_expr->kind == Expr_Name) {
            printf(
                "%s:%d:%d: type error: field '%.*s' cannot be accessed on pointer type '%.*s'; use %.*s[0].%.*s\n",
                g_diag_source_path ? g_diag_source_path : g_source_path,
                line,
                col,
                (int)field_name.length,
                field_name.data,
                (int)base_name.length,
                base_name.data,
                (int)base_expr->name.length,
                base_expr->name.data,
                (int)field_name.length,
                field_name.data
            );
        } else {
            printf(
                "%s:%d:%d: type error: field '%.*s' cannot be accessed on pointer type '%.*s'; index the pointer before accessing '.%.*s'\n",
                g_diag_source_path ? g_diag_source_path : g_source_path,
                line,
                col,
                (int)field_name.length,
                field_name.data,
                (int)base_name.length,
                base_name.data,
                (int)field_name.length,
                field_name.data
            );
        }
        diag_finish_at(line, col);
        return;
    }

    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "type '%.*s' has no field '%.*s'",
            (int)base_name.length,
            base_name.data,
            (int)field_name.length,
            field_name.data
        );
        diag_json_error(diag_current_path(), line, col, "type", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: type '%.*s' has no field '%.*s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        (int)base_name.length,
        base_name.data,
        (int)field_name.length,
        field_name.data
    );
    diag_finish_at(line, col);
}

static void type_error_initializer_field(
    TypeExpr *target,
    string8 field_name,
    i32 line,
    i32 col,
    memops_arena *arena
) {
    string8 target_name = type_mangle(arena, target, (TypeSub){0});
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "initializer for type '%.*s' has no field '%.*s'",
            (int)target_name.length,
            target_name.data,
            (int)field_name.length,
            field_name.data
        );
        diag_json_error(diag_current_path(), line, col, "type", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: initializer for type '%.*s' has no field '%.*s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        (int)target_name.length,
        target_name.data,
        (int)field_name.length,
        field_name.data
    );
    diag_finish_at(line, col);
}

static void type_error_initializer_count(
    TypeExpr *target,
    i32 line,
    i32 col,
    memops_arena *arena
) {
    string8 target_name = type_mangle(arena, target, (TypeSub){0});
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "too many positional initializer values for type '%.*s'",
            (int)target_name.length,
            target_name.data
        );
        diag_json_error(diag_current_path(), line, col, "type", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: too many positional initializer values for type '%.*s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        (int)target_name.length,
        target_name.data
    );
    diag_finish_at(line, col);
}

static void type_error_initializer_duplicate_field(
    string8 field_name,
    i32 line,
    i32 col,
    i32 prev_line,
    i32 prev_col
) {
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "duplicate initializer for field '%.*s'",
            (int)field_name.length,
            field_name.data
        );
        diag_json_error_with_note(diag_current_path(), line, col, "type", message, diag_current_path(), prev_line, prev_col, "previous initializer here");
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: duplicate initializer for field '%.*s' (previous at %d:%d)\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        (int)field_name.length,
        field_name.data,
        prev_line,
        prev_col
    );
    diag_finish_at(line, col);
}

static void type_error_initializer_duplicate_index(
    string8 index_name,
    i32 line,
    i32 col,
    i32 prev_line,
    i32 prev_col
) {
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "duplicate initializer for array index '%.*s'",
            (int)index_name.length,
            index_name.data
        );
        diag_json_error_with_note(diag_current_path(), line, col, "type", message, diag_current_path(), prev_line, prev_col, "previous initializer here");
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: duplicate initializer for array index '%.*s' (previous at %d:%d)\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        (int)index_name.length,
        index_name.data,
        prev_line,
        prev_col
    );
    diag_finish_at(line, col);
}

static void type_error_initializer_index_bounds(
    TypeExpr *target,
    string8 index_name,
    i32 line,
    i32 col,
    memops_arena *arena
) {
    string8 target_name = type_mangle(arena, target, (TypeSub){0});
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "initializer index '%.*s' is out of bounds for type '%.*s'",
            (int)index_name.length,
            index_name.data,
            (int)target_name.length,
            target_name.data
        );
        diag_json_error(diag_current_path(), line, col, "type", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: initializer index '%.*s' is out of bounds for type '%.*s'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        (int)index_name.length,
        index_name.data,
        (int)target_name.length,
        target_name.data
    );
    diag_finish_at(line, col);
}

static void type_error_initializer_index_integer(
    string8 index_name,
    i32 line,
    i32 col
) {
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "initializer index '%.*s' must be a non-negative integer literal",
            (int)index_name.length,
            index_name.data
        );
        diag_json_error(diag_current_path(), line, col, "type", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: initializer index '%.*s' must be a non-negative integer literal\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        line,
        col,
        (int)index_name.length,
        index_name.data
    );
    diag_finish_at(line, col);
}

static i64 parse_nonnegative_i64(string8 text) {
    if (text.length == 0) return -1;
    i64 value = 0;
    for (u64 i = 0; i < text.length; i++) {
        u8 c = text.data[i];
        if (c < '0' || c > '9') return -1;
        value = value * 10 + (i64)(c - '0');
    }
    return value;
}

static i64 type_array_count_value(TypeExpr *type) {
    if (!type || type->kind != Type_Array) return -1;
    return parse_nonnegative_i64(type->array_count);
}

static string8 string8_from_i64(memops_arena *arena, i64 value) {
    char buf[32];
    snprintf(buf, sizeof(buf), "%lld", (long long)value);
    return string8_copy_from_slice(arena, (u8 *)buf, (u64)strlen(buf));
}

static void check_assignment_compatible(
    const char *context,
    Program *prog,
    TypeExpr *dst,
    TypeExpr *src,
    i32 line,
    i32 col,
    memops_arena *arena
) {
    if (!dst || !src) return;
    if (!type_compatible(prog, dst, src)) {
        type_error_incompatible(context, prog, dst, src, line, col, arena);
    }
}

static void type_check_expr(Expr *e, TypeScope *scope, Program *prog, memops_arena *arena);
static void type_check_binary_op(Expr *e, TypeScope *scope, Program *prog, memops_arena *arena);
static void type_check_ternary(Expr *e, TypeScope *scope, Program *prog, memops_arena *arena);

static void type_check_initializer_against(
    Expr *init,
    TypeExpr *expected,
    TypeScope *scope,
    Program *prog,
    memops_arena *arena
) {
    if (!init || !expected) return;
    expected = type_scope_apply_sub(scope, arena, expected);
    if (init->kind == Expr_ZeroInit) {
        init->cast_type = expected;
        return;
    }
    if (init->kind != Expr_InitList) return;
    init->cast_type = expected;

    expected = resolve_alias_type(prog, expected);
    if (expected && expected->kind == Type_Array) {
        i64 array_count = type_array_count_value(expected);
        i64 positional_index = 0;
        Vec_string8 initialized_indices = Vec_string8_reserve(arena, init->args.length);
        Vec_i32 initialized_index_lines = Vec_i32_reserve(arena, init->args.length);
        Vec_i32 initialized_index_cols = Vec_i32_reserve(arena, init->args.length);
        for (i32 i = 0; i < init->args.length; i++) {
            string8 index_name = {0};
            i32 index_line = init->line;
            i32 index_col = init->col;
            if (init->designator_kinds.data[i] == InitDesignator_Index) {
                Expr *designator = (Expr *)init->designators.data[i];
                type_check_expr(designator, scope, prog, arena);
                if (designator && designator->kind == Expr_Number) {
                    index_name = designator->number;
                    index_line = designator->line;
                    index_col = designator->col;
                    i64 index_value = parse_nonnegative_i64(index_name);
                    if (index_value < 0) {
                        type_error_initializer_index_integer(index_name, index_line, index_col);
                    }
                    if (array_count >= 0 && index_value >= array_count) {
                        type_error_initializer_index_bounds(expected, index_name, index_line, index_col, arena);
                    }
                }
            } else if (init->designator_kinds.data[i] == InitDesignator_None) {
                if (array_count >= 0 && positional_index >= array_count) {
                    type_error_initializer_count(expected, init->line, init->col, arena);
                }
                index_name = string8_from_i64(arena, positional_index);
                positional_index += 1;
            } else {
                type_error_initializer_count(expected, init->line, init->col, arena);
            }
            if (index_name.data) {
                i32 prev_index = string8_vec_find(&initialized_indices, index_name);
                if (prev_index >= 0) {
                    type_error_initializer_duplicate_index(
                        index_name,
                        index_line,
                        index_col,
                        initialized_index_lines.data[prev_index],
                        initialized_index_cols.data[prev_index]
                    );
                }
            }
            if (index_name.data) {
                Vec_string8_append(arena, &initialized_indices, index_name);
                Vec_i32_append(arena, &initialized_index_lines, index_line);
                Vec_i32_append(arena, &initialized_index_cols, index_col);
            }
            Expr *value = (Expr *)init->args.data[i];
            type_check_expr(value, scope, prog, arena);
            type_check_initializer_against(value, expected->elem, scope, prog, arena);
            TypeExpr *actual = infer_expr_type(value, scope, prog, arena);
            check_assignment_compatible("array initializer", prog, expected->elem, actual, init->line, init->col, arena);
        }
        return;
    }

    TypeSub sub = {0};
    StructDecl *decl = lookup_aggregate_decl(prog, expected, &sub, arena);
    if (!decl) return;

    i32 positional_index = 0;
    Vec_string8 initialized_fields = Vec_string8_reserve(arena, decl->fields.length);
    Vec_i32 initialized_field_lines = Vec_i32_reserve(arena, decl->fields.length);
    Vec_i32 initialized_field_cols = Vec_i32_reserve(arena, decl->fields.length);
    for (i32 i = 0; i < init->args.length; i++) {
        InitDesignatorKind designator_kind = (InitDesignatorKind)init->designator_kinds.data[i];
        Expr *designator = (Expr *)init->designators.data[i];
        Field *field = null;
        Expr *value = (Expr *)init->args.data[i];
        i32 init_line = init->line;
        i32 init_col = init->col;

        if (designator_kind == InitDesignator_Field) {
            for (i32 f = 0; f < decl->fields.length; f++) {
                Field *candidate = (Field *)decl->fields.data[f];
                if (string8_equals(&candidate->name, &designator->name)) {
                    field = candidate;
                    break;
                }
            }
            if (!field) {
                type_error_initializer_field(expected, designator->name, designator->line, designator->col, arena);
                continue; // no field to check this value against
            }
            init_line = designator->line;
            init_col = designator->col;
        } else if (designator_kind == InitDesignator_None) {
            if (positional_index >= decl->fields.length) {
                type_error_initializer_count(expected, init->line, init->col, arena);
                continue; // reading past the field list would be out of bounds
            }
            field = (Field *)decl->fields.data[positional_index];
            positional_index += 1;
            if (value) {
                init_line = value->line;
                init_col = value->col;
            }
        } else {
            type_error_initializer_count(expected, init->line, init->col, arena);
        }

        if (field) {
            i32 prev_index = string8_vec_find(&initialized_fields, field->name);
            if (prev_index >= 0) {
                type_error_initializer_duplicate_field(
                    field->name,
                    init_line,
                    init_col,
                    initialized_field_lines.data[prev_index],
                    initialized_field_cols.data[prev_index]
                );
            }
        }
        if (field) {
            Vec_string8_append(arena, &initialized_fields, field->name);
            Vec_i32_append(arena, &initialized_field_lines, init_line);
            Vec_i32_append(arena, &initialized_field_cols, init_col);
        }

        type_check_expr(value, scope, prog, arena);
        TypeExpr *field_type = substitute_type_sub(arena, field->type, sub);
        type_check_initializer_against(value, field_type, scope, prog, arena);
        TypeExpr *actual = infer_expr_type(value, scope, prog, arena);
        check_assignment_compatible("field initializer", prog, field_type, actual, init->line, init->col, arena);
    }
}

static void type_error_missing_type_operation(Expr *call, TypeExpr *type_arg, memops_arena *arena) {
    string8 mangle = type_arg ? type_mangle(arena, type_arg, (TypeSub){0}) : string8_from_cstr(arena, "unknown");
    string8 missing = mono_proc_name_from_mangle(arena, call->name, mangle);
    if (g_diag_json) {
        char message[1024];
        snprintf(
            message,
            sizeof(message),
            "missing type operation proc '%.*s' for call '%.*s<%.*s>'",
            (int)missing.length,
            missing.data,
            (int)call->name.length,
            call->name.data,
            (int)mangle.length,
            mangle.data
        );
        diag_json_error(diag_current_path(), call->line, call->col, "type", message);
        diag_record_error();
        return;
    }
    printf(
        "%s:%d:%d: type error: missing type operation proc '%.*s' for call '%.*s<%.*s>'\n",
        g_diag_source_path ? g_diag_source_path : g_source_path,
        call->line,
        call->col,
        (int)missing.length,
        missing.data,
        (int)call->name.length,
        call->name.data,
        (int)mangle.length,
        mangle.data
    );
    diag_print_file_context(g_diag_source_path ? g_diag_source_path : g_source_path, call->line, call->col);
    diag_note_import_chain();
    diag_record_error();
    return;
}

static void type_check_call(Expr *call, TypeScope *scope, Program *prog, memops_arena *arena) {
    /* Indirect call through an expression -- a proc pointer in a field, an array
       element, and so on. Argument checking is the same as for a proc-pointer
       variable; only finding the callee's type differs. */
    if (call->base) {
        type_check_expr(call->base, scope, prog, arena);
        TypeExpr *callee_type = infer_expr_type(call->base, scope, prog, arena);
        TypeExpr *proc_type = callee_type ? type_proc_from_callable_type(prog, callee_type) : null;
        string8 callee_name = string8_from_cstr(arena, "callee expression");
        if (!proc_type) {
            type_error_call_non_proc(callee_name, callee_type, call->line, call->col, arena);
            for (i32 i = 0; i < call->args.length; i++) {
                type_check_expr((Expr *)call->args.data[i], scope, prog, arena);
            }
            return;
        }
        if (proc_type->is_variadic) {
            if (call->args.length < proc_type->args.length) {
                type_error_proc_pointer_arg_count(callee_name, proc_type, call->args.length, call->line, call->col, arena);
            }
        } else if (call->args.length != proc_type->args.length) {
            type_error_proc_pointer_arg_count(callee_name, proc_type, call->args.length, call->line, call->col, arena);
        }
        for (i32 i = 0; i < call->args.length && i < proc_type->args.length; i++) {
            TypeExpr *expected = (TypeExpr *)proc_type->args.data[i];
            Expr *arg = (Expr *)call->args.data[i];
            type_check_initializer_against(arg, expected, scope, prog, arena);
            type_check_expr(arg, scope, prog, arena);
            TypeExpr *actual = infer_expr_type(arg, scope, prog, arena);
            if (expected && actual && !type_compatible(prog, expected, actual)) {
                type_error_proc_pointer_argument(prog, callee_name, proc_type, i, expected, actual,
                                                 arg ? arg->line : call->line, arg ? arg->col : call->col, arena);
            }
        }
        if (proc_type->is_variadic) {
            for (i32 i = proc_type->args.length; i < call->args.length; i++) {
                type_check_expr((Expr *)call->args.data[i], scope, prog, arena);
            }
        }
        return;
    }

    TypeExpr *type_arg = null;
    bool concrete_specialization = false;
    ProcDecl *decl = lookup_call_proc_decl(prog, call, scope, arena, &type_arg, &concrete_specialization);

    /* C resolves a name to its nearest binding, so a local or a parameter
       shadows a proc of the same name. The proc table used to win regardless,
       which meant I read `helper(3)` as a call to the proc while C read the same
       text as calling the `i32` local next to it -- one program, two meanings,
       and a clang error about generated code the author never wrote.

       Procs are never entered into the TypeScope; it holds locals, parameters,
       globals, function-like macros and reflection globals. So a hit here is by
       construction a nearer binding than the top-level proc, and preferring it
       is exactly C's rule. Explicit type arguments are left alone: `f<i32>(x)`
       is unambiguously a generic proc call, never a variable. */
    if (decl && call->type_args.length == 0) {
        TypeExpr *shadowing = type_scope_lookup(scope, call->name);
        if (shadowing) {
            if (!type_proc_from_callable_type(prog, shadowing)) {
                type_error_call_non_proc(call->name, shadowing, call->line, call->col, arena);
                for (i32 i = 0; i < call->args.length; i++) {
                    type_check_expr((Expr *)call->args.data[i], scope, prog, arena);
                }
                return;
            }
            /* Callable after all -- a proc-pointer local, say. Hand it to the
               indirect path below, which already checks against a proc type. */
            decl = null;
        }
    }

    if (!decl) {
        if (call->type_args.length > 0) {
            type_error_missing_type_operation(call, type_arg, arena);
        }
        TypeExpr *callee_type = type_scope_lookup(scope, call->name);
        if (!callee_type) {
            if (!call_name_is_builtin(call->name)) {
                type_error_call_undeclared(call->name, call->line, call->col);
            }
            /* Still check the arguments: their own mistakes are independent of
               the callee being unknown, and reporting them together beats
               making the author fix one call at a time. */
            for (i32 i = 0; i < call->args.length; i++) {
                type_check_expr((Expr *)call->args.data[i], scope, prog, arena);
            }
            return;
        }

        TypeExpr *proc_type = type_proc_from_callable_type(prog, callee_type);
        if (!proc_type) {
            type_error_call_non_proc(call->name, callee_type, call->line, call->col, arena);
            for (i32 i = 0; i < call->args.length; i++) {
                type_check_expr((Expr *)call->args.data[i], scope, prog, arena);
            }
            return; // nothing below can describe a callee that is not callable
        }
        if (call->type_args.length > 0) {
            type_error_proc_pointer_type_arg_count(call->name, call->type_args.length, call->line, call->col);
        }
        if (proc_type->is_variadic) {
            if (call->args.length < proc_type->args.length) {
                type_error_proc_pointer_arg_count(call->name, proc_type, call->args.length, call->line, call->col, arena);
            }
        } else if (call->args.length != proc_type->args.length) {
            type_error_proc_pointer_arg_count(call->name, proc_type, call->args.length, call->line, call->col, arena);
        }
        for (i32 i = 0; i < call->args.length && i < proc_type->args.length; i++) {
            TypeExpr *expected = (TypeExpr *)proc_type->args.data[i];
            Expr *arg = (Expr *)call->args.data[i];
            type_check_initializer_against(arg, expected, scope, prog, arena);
            type_check_expr(arg, scope, prog, arena);
            TypeExpr *actual = infer_expr_type(arg, scope, prog, arena);
            if (expected && actual && !type_compatible(prog, expected, actual)) {
                type_error_proc_pointer_argument(prog, call->name, proc_type, i, expected, actual, arg ? arg->line : call->line, arg ? arg->col : call->col, arena);
            }
        }
        if (proc_type->is_variadic) {
            for (i32 i = proc_type->args.length; i < call->args.length; i++) {
                type_check_expr((Expr *)call->args.data[i], scope, prog, arena);
            }
        }
        return;
    }
    if (decl->is_generic) {
        i32 wanted = decl->type_params.length > 0 ? decl->type_params.length : 1;
        if (call->type_args.length != wanted) {
            type_error_proc_type_arg_count(decl, call->type_args.length, call->line, call->col);
            /* Parameter types still mention the unsubstituted type param, so
               checking arguments against them would only invent follow-on errors. */
            for (i32 i = 0; i < call->args.length; i++) {
                type_check_expr((Expr *)call->args.data[i], scope, prog, arena);
            }
            return;
        }
    } else if (call->type_args.length > 0 && !concrete_specialization) {
        type_error_proc_type_arg_count(decl, call->type_args.length, call->line, call->col);
    }
    if (decl->is_variadic) {
        if (call->args.length < decl->params.length) {
            type_error_proc_arg_count(decl, call->args.length, call->line, call->col, arena);
        }
    } else if (call->args.length != decl->params.length) {
        type_error_proc_arg_count(decl, call->args.length, call->line, call->col, arena);
    }

    for (i32 i = 0; i < call->args.length && i < decl->params.length; i++) {
        Param *param = (Param *)decl->params.data[i];
        TypeExpr *expected = param->type;
        if (decl->is_generic) {
            TypeSub call_sub = proc_call_sub(arena, decl, call, type_arg);
            if (call_sub.has) expected = substitute_type_sub(arena, param->type, call_sub);
        }
        Expr *arg = (Expr *)call->args.data[i];
        type_check_initializer_against(arg, expected, scope, prog, arena);
        type_check_expr(arg, scope, prog, arena);
        TypeExpr *actual = infer_expr_type(arg, scope, prog, arena);
        if (expected && actual && !type_compatible(prog, expected, actual)) {
            type_error_proc_argument(prog, decl, param, i, type_arg, expected, actual, arg ? arg->line : call->line, arg ? arg->col : call->col, arena);
        }
    }
    if (decl->is_variadic) {
        for (i32 i = decl->params.length; i < call->args.length; i++) {
            type_check_expr((Expr *)call->args.data[i], scope, prog, arena);
        }
    }
}

static void type_check_expr(Expr *e, TypeScope *scope, Program *prog, memops_arena *arena) {
    if (!e) return;
    if (e->kind == Expr_Call) {
        type_check_call(e, scope, prog, arena);
        return;
    }
    if (e->kind == Expr_Binary) {
        type_check_expr(e->left, scope, prog, arena);
        type_check_expr(e->right, scope, prog, arena);
        type_check_binary_op(e, scope, prog, arena);
        return;
    }
    if (e->kind == Expr_Ternary) {
        type_check_expr(e->left, scope, prog, arena);
        type_check_expr(e->right, scope, prog, arena);
        type_check_expr(e->third, scope, prog, arena);
        type_check_ternary(e, scope, prog, arena);
        return;
    }
    if (e->kind == Expr_Addr) {
        if (!expr_is_assignment_target(e->inner)) {
            type_error_address_target(e->inner, e->line, e->col);
        }
        type_check_expr(e->inner, scope, prog, arena);
        return;
    }
    if (e->kind == Expr_Cast) {
        type_check_expr(e->inner, scope, prog, arena);
        TypeExpr *actual = infer_expr_type(e->inner, scope, prog, arena);
        TypeExpr *cast_type = type_scope_apply_sub(scope, arena, e->cast_type);
        if (!type_allows_cast(prog, cast_type, actual)) {
            type_error_cast(prog, cast_type, actual, e->line, e->col, arena);
        }
        return;
    }
    if (e->kind == Expr_CompoundInit) {
        TypeExpr *compound_type = type_scope_apply_sub(scope, arena, e->cast_type);
        type_check_initializer_against(e->inner, compound_type, scope, prog, arena);
        if (e->inner && e->inner->kind != Expr_InitList && e->inner->kind != Expr_ZeroInit) {
            type_check_expr(e->inner, scope, prog, arena);
        }
        return;
    }
    if (e->kind == Expr_Unary) {
        type_check_expr(e->inner, scope, prog, arena);
        return;
    }
    if (e->kind == Expr_Index) {
        type_check_expr(e->base, scope, prog, arena);
        type_check_expr(e->index_expr, scope, prog, arena);
        TypeExpr *base = resolve_alias_type(prog, infer_expr_type(e->base, scope, prog, arena));
        if (base && base->kind != Type_Ptr && base->kind != Type_Array) {
            type_error_index_base(base, e->line, e->col, arena);
        }
        TypeExpr *index_type = resolve_alias_type(prog, infer_expr_type(e->index_expr, scope, prog, arena));
        if (index_type && !type_is_numeric(index_type) && !type_is_program_enum(prog, index_type)) {
            type_error_index_value(index_type, e->index_expr->line, e->index_expr->col, arena);
        }
        return;
    }
    if (e->kind == Expr_Field) {
        EnumDecl *enum_decl = null;
        EnumItem *enum_item = null;
        if (resolve_enum_member_expr(prog, e, &enum_decl, &enum_item)) {
            if (!enum_item) {
                type_error_enum_member(enum_decl, e->name, e->line, e->col);
            }
            e->kind = Expr_Name;
            e->name = enum_item_c_name(arena, enum_decl, enum_item);
            e->base = null;
            return;
        }
        type_check_expr(e->base, scope, prog, arena);
        TypeExpr *base = infer_expr_type(e->base, scope, prog, arena);
        TypeExpr *field = lookup_field_type(prog, base, e->name, arena);
        TypeExpr *resolved = resolve_alias_type(prog, base);
        check_reflect_variant_arm(prog, e, base, arena);
        if (!field && resolved) {
            if (resolved->kind == Type_Ptr ||
                type_is_declared_aggregate(prog, resolved) ||
                type_is_reflect_runtime_record(prog, resolved)) {
                type_error_field_access(prog, e->base, base, e->name, e->line, e->col, arena);
            } else {
                StructDecl *opaque = lookup_opaque_external_struct(prog, resolved);
                if (opaque) {
                    type_error_opaque_field_access(opaque, e->name, e->line, e->col);
                } else if (type_never_has_fields(resolved)) {
                    type_error_field_access(prog, e->base, base, e->name, e->line, e->col, arena);
                }
            }
        }
        return;
    }
    if (e->kind == Expr_InitList) {
        for (i32 i = 0; i < e->args.length; i++) {
            if (e->designator_kinds.data[i] == InitDesignator_Index) {
                type_check_expr((Expr *)e->designators.data[i], scope, prog, arena);
            }
            type_check_expr((Expr *)e->args.data[i], scope, prog, arena);
        }
        return;
    }
}

static bool type_pointer_subtract_compatible(Program *prog, TypeExpr *left, TypeExpr *right) {
    if (!left || !right) return false;
    left = resolve_alias_type(prog, left);
    right = resolve_alias_type(prog, right);
    if (!left || !right) return false;

    if (left->kind == Type_Ptr && right->kind == Type_Ptr) {
        return type_expr_assignable_qualified(prog, left->elem, right->elem, true) ||
               type_expr_assignable_qualified(prog, right->elem, left->elem, true);
    }
    if (left->kind == Type_Ptr && right->kind == Type_Array) {
        return type_expr_assignable_qualified(prog, left->elem, right->elem, true) ||
               type_expr_assignable_qualified(prog, right->elem, left->elem, true);
    }
    if (left->kind == Type_Array && right->kind == Type_Ptr) {
        return type_expr_assignable_qualified(prog, left->elem, right->elem, true) ||
               type_expr_assignable_qualified(prog, right->elem, left->elem, true);
    }
    return false;
}

static bool type_pointer_compare_compatible(Program *prog, TypeExpr *left, TypeExpr *right) {
    if (!left || !right) return false;
    left = resolve_alias_type(prog, left);
    right = resolve_alias_type(prog, right);
    if (!left || !right) return false;
    if (left->kind != Type_Ptr || right->kind != Type_Ptr) return false;

    return type_expr_assignable_qualified(prog, left->elem, right->elem, true) ||
           type_expr_assignable_qualified(prog, right->elem, left->elem, true);
}

static void type_check_binary_op(Expr *e, TypeScope *scope, Program *prog, memops_arena *arena) {
    TypeExpr *left = resolve_alias_type(prog, infer_expr_type(e->left, scope, prog, arena));
    TypeExpr *right = resolve_alias_type(prog, infer_expr_type(e->right, scope, prog, arena));
    if (!left || !right) return;

    bool left_enum = type_is_program_enum(prog, left);
    bool right_enum = type_is_program_enum(prog, right);
    bool numeric_or_same_enum = type_is_numeric_or_enum(prog, left) &&
                                type_is_numeric_or_enum(prog, right) &&
                                (!(left_enum && right_enum) || type_expr_equal_resolved(prog, left, right));
    bool integer_or_same_enum = type_is_integer_or_enum(prog, left) &&
                                type_is_integer_or_enum(prog, right) &&
                                (!(left_enum && right_enum) || type_expr_equal_resolved(prog, left, right));

    bool ok = false;
    if (e->op == Token_Star || e->op == Token_Slash) {
        ok = numeric_or_same_enum;
    } else if (e->op == Token_Percent) {
        ok = integer_or_same_enum;
    } else if (e->op == Token_Plus) {
        ok = numeric_or_same_enum ||
             (left->kind == Type_Ptr && type_is_integer_or_enum(prog, right)) ||
             (right->kind == Type_Ptr && type_is_integer_or_enum(prog, left)) ||
             (left->kind == Type_Array && type_is_integer_or_enum(prog, right)) ||
             (right->kind == Type_Array && type_is_integer_or_enum(prog, left));
    } else if (e->op == Token_Minus) {
        ok = numeric_or_same_enum ||
             (left->kind == Type_Ptr && type_is_integer_or_enum(prog, right)) ||
             (left->kind == Type_Array && type_is_integer_or_enum(prog, right)) ||
             type_pointer_subtract_compatible(prog, left, right);
    } else if (e->op == Token_Keyword_Shl || e->op == Token_Keyword_Shr ||
               e->op == Token_Ampersand || e->op == Token_Caret || e->op == Token_Pipe) {
        ok = integer_or_same_enum;
    } else if (e->op == Token_LAngle || e->op == Token_RAngle ||
               e->op == Token_LessEqual || e->op == Token_GreaterEqual) {
        ok = numeric_or_same_enum ||
             type_pointer_compare_compatible(prog, left, right);
    } else if (e->op == Token_EqualEqual || e->op == Token_BangEqual) {
        ok = type_compatible(prog, left, right) ||
             type_compatible(prog, right, left);
    } else if (e->op == Token_Keyword_And || e->op == Token_Keyword_Or) {
        ok = type_is_truthy(prog, left) && type_is_truthy(prog, right);
    } else {
        ok = true;
    }

    if (!ok) {
        type_error_binary_op(e->op, left, right, e->line, e->col, arena);
    }
}

static void type_check_ternary(Expr *e, TypeScope *scope, Program *prog, memops_arena *arena) {
    TypeExpr *cond = resolve_alias_type(prog, infer_expr_type(e->left, scope, prog, arena));
    TypeExpr *right = resolve_alias_type(prog, infer_expr_type(e->right, scope, prog, arena));
    TypeExpr *third = resolve_alias_type(prog, infer_expr_type(e->third, scope, prog, arena));
    if (cond && !type_is_truthy(prog, cond)) {
        type_error_ternary_condition(cond, e->left->line, e->left->col, arena);
    }
    if (!right || !third) return;
    if (!type_compatible(prog, right, third) && !type_compatible(prog, third, right)) {
        type_error_ternary_arms(right, third, e->line, e->col, arena);
    }
}

static void type_check_condition(
    const char *context,
    Expr *cond_expr,
    TypeScope *scope,
    Program *prog,
    memops_arena *arena
) {
    if (!cond_expr) return;
    type_check_expr(cond_expr, scope, prog, arena);
    TypeExpr *cond = resolve_alias_type(prog, infer_expr_type(cond_expr, scope, prog, arena));
    if (cond && !type_is_truthy(prog, cond)) {
        type_error_condition(context, cond, cond_expr->line, cond_expr->col, arena);
    }
}

static void type_check_stmt(
    Stmt *s,
    TypeScope *scope,
    Program *prog,
    TypeExpr *return_type,
    ProcDecl *current_proc,
    memops_arena *arena
);

static void type_check_stmt(
    Stmt *s,
    TypeScope *scope,
    Program *prog,
    TypeExpr *return_type,
    ProcDecl *current_proc,
    memops_arena *arena
) {
    if (!s) return;
    const char *prev_diag_source_path = g_diag_source_path;
    const char *prev_diag_import_chain = g_diag_import_chain;
    if (s->source_path) {
        g_diag_source_path = s->source_path;
    }
    g_diag_import_chain = s->import_chain;
    if (s->kind == Stmt_Var) {
        TypeExpr *local_type = type_scope_apply_sub(scope, arena, s->type);
        type_check_expr(s->expr, scope, prog, arena);
        type_check_initializer_against(s->expr, local_type, scope, prog, arena);
        TypeExpr *actual = infer_expr_type(s->expr, scope, prog, arena);
        check_assignment_compatible("initializer", prog, local_type, actual, s->line, s->col, arena);
        type_scope_add(arena, scope, s->name, local_type);
        g_diag_source_path = prev_diag_source_path;
        g_diag_import_chain = prev_diag_import_chain;
        return;
    }
    if (s->kind == Stmt_Assign) {
        if (!expr_is_assignment_target(s->lhs)) {
            type_error_assignment_target(s->lhs);
        }
        type_check_expr(s->lhs, scope, prog, arena);
        type_check_expr(s->expr, scope, prog, arena);
        TypeExpr *expected = infer_expr_type(s->lhs, scope, prog, arena);
        TypeExpr *const_source = expr_const_lvalue_source_type(s->lhs, scope, prog, arena);
        if (const_source) {
            type_error_const_assignment(prog, s->lhs, expected, const_source, arena);
        }
        type_check_initializer_against(s->expr, expected, scope, prog, arena);
        TypeExpr *actual = infer_expr_type(s->expr, scope, prog, arena);
        if (!type_allows_compound_assign(prog, s->assign_op, expected, actual)) {
            if (s->assign_op == Token_Equal) {
                type_error_incompatible("assignment", prog, expected, actual, s->line, s->col, arena);
            } else {
                type_error_compound_assignment(s->assign_op, prog, expected, actual, s->line, s->col, arena);
            }
        }
        g_diag_source_path = prev_diag_source_path;
        g_diag_import_chain = prev_diag_import_chain;
        return;
    }
    if (s->kind == Stmt_Return) {
        type_check_expr(s->expr, scope, prog, arena);
        bool returns_void = type_is_void_type(prog, return_type);
        if (s->expr && returns_void) {
            type_error_return_value_presence(prog, return_type, current_proc, true, s->line, s->col, arena);
        }
        if (!s->expr && !returns_void) {
            type_error_return_value_presence(prog, return_type, current_proc, false, s->line, s->col, arena);
        }
        if (s->expr) {
            type_check_initializer_against(s->expr, return_type, scope, prog, arena);
            TypeExpr *actual = infer_expr_type(s->expr, scope, prog, arena);
            check_assignment_compatible("return", prog, return_type, actual, s->line, s->col, arena);
        }
        g_diag_source_path = prev_diag_source_path;
        g_diag_import_chain = prev_diag_import_chain;
        return;
    }
    if (s->kind == Stmt_Expr) {
        type_check_expr(s->expr, scope, prog, arena);
        g_diag_source_path = prev_diag_source_path;
        g_diag_import_chain = prev_diag_import_chain;
        return;
    }
    if (s->kind == Stmt_For) {
        TypeScope loop_scope = type_scope_copy(arena, scope);
        if (s->for_init) type_check_stmt(s->for_init, &loop_scope, prog, return_type, current_proc, arena);
        type_check_condition("for", s->for_cond, &loop_scope, prog, arena);
        if (s->for_step) type_check_stmt(s->for_step, &loop_scope, prog, return_type, current_proc, arena);
        for (i32 i = 0; i < s->for_body.length; i++) {
            type_check_stmt((Stmt *)s->for_body.data[i], &loop_scope, prog, return_type, current_proc, arena);
        }
        g_diag_source_path = prev_diag_source_path;
        g_diag_import_chain = prev_diag_import_chain;
        return;
    }
    if (s->kind == Stmt_If) {
        type_check_condition("if", s->if_cond, scope, prog, arena);
        TypeScope then_scope = type_scope_copy(arena, scope);
        for (i32 i = 0; i < s->if_then_body.length; i++) {
            type_check_stmt((Stmt *)s->if_then_body.data[i], &then_scope, prog, return_type, current_proc, arena);
        }
        if (s->if_else_if) {
            TypeScope else_if_scope = type_scope_copy(arena, scope);
            type_check_stmt(s->if_else_if, &else_if_scope, prog, return_type, current_proc, arena);
        } else {
            TypeScope else_scope = type_scope_copy(arena, scope);
            for (i32 i = 0; i < s->if_else_body.length; i++) {
                type_check_stmt((Stmt *)s->if_else_body.data[i], &else_scope, prog, return_type, current_proc, arena);
            }
        }
        g_diag_source_path = prev_diag_source_path;
        g_diag_import_chain = prev_diag_import_chain;
        return;
    }
    if (s->kind == Stmt_While) {
        type_check_condition("while", s->while_cond, scope, prog, arena);
        TypeScope loop_scope = type_scope_copy(arena, scope);
        for (i32 i = 0; i < s->while_body.length; i++) {
            type_check_stmt((Stmt *)s->while_body.data[i], &loop_scope, prog, return_type, current_proc, arena);
        }
        g_diag_source_path = prev_diag_source_path;
        g_diag_import_chain = prev_diag_import_chain;
        return;
    }
    if (s->kind == Stmt_DoWhile || s->kind == Stmt_Label) {
        TypeScope loop_scope = type_scope_copy(arena, scope);
        for (i32 i = 0; i < s->while_body.length; i++) {
            type_check_stmt((Stmt *)s->while_body.data[i], &loop_scope, prog, return_type, current_proc, arena);
        }
        if (s->kind == Stmt_DoWhile) {
            type_check_condition("do while", s->while_cond, &loop_scope, prog, arena);
        }
        g_diag_source_path = prev_diag_source_path;
        g_diag_import_chain = prev_diag_import_chain;
        return;
    }
    if (s->kind == Stmt_Switch) {
        type_check_expr(s->switch_expr, scope, prog, arena);
        TypeExpr *switch_type = resolve_alias_type(prog, infer_expr_type(s->switch_expr, scope, prog, arena));
        if (switch_type && !type_is_numeric_or_enum(prog, switch_type)) {
            type_error_incompatible("switch expression", prog, type_name_expr(arena, "integer_or_enum"), switch_type, s->line, s->col, arena);
        }
        for (i32 i = 0; i < s->switch_cases.length; i++) {
            SwitchCase *sc = (SwitchCase *)s->switch_cases.data[i];
            type_check_expr(sc->expr, scope, prog, arena);
            TypeExpr *case_type = infer_expr_type(sc->expr, scope, prog, arena);
            if (switch_type && case_type) {
                bool case_ok = type_compatible(prog, switch_type, case_type) ||
                               type_compatible(prog, case_type, switch_type) ||
                               (type_is_program_enum(prog, switch_type) && type_is_integer(case_type));
                if (!case_ok) {
                    type_error_incompatible("switch case", prog, switch_type, case_type, sc->expr->line, sc->expr->col, arena);
                }
            }
            TypeScope case_scope = type_scope_copy(arena, scope);
            for (i32 j = 0; j < sc->body.length; j++) {
                type_check_stmt((Stmt *)sc->body.data[j], &case_scope, prog, return_type, current_proc, arena);
            }
        }
        TypeScope default_scope = type_scope_copy(arena, scope);
        for (i32 i = 0; i < s->switch_default_body.length; i++) {
            type_check_stmt((Stmt *)s->switch_default_body.data[i], &default_scope, prog, return_type, current_proc, arena);
        }
        g_diag_source_path = prev_diag_source_path;
        g_diag_import_chain = prev_diag_import_chain;
        return;
    }
    g_diag_source_path = prev_diag_source_path;
    g_diag_import_chain = prev_diag_import_chain;
}

static void type_check_proc_body(
    Program *prog,
    ProcDecl *p,
    TypeScope *globals,
    TypeSub sub,
    memops_arena *arena
) {
    const char *prev_diag_source_path = g_diag_source_path;
    const char *prev_diag_import_chain = g_diag_import_chain;
    if (p->source_path) {
        g_diag_source_path = p->source_path;
    }
    g_diag_import_chain = p->import_chain;

    TypeScope scope = type_scope_copy(arena, globals);
    scope.sub = sub;
    for (i32 j = 0; j < p->params.length; j++) {
        Param *param = (Param *)p->params.data[j];
        TypeExpr *param_type = type_scope_apply_sub(&scope, arena, param->type);
        type_scope_add(arena, &scope, param->name, param_type);
    }

    TypeExpr *return_type = type_scope_apply_sub(&scope, arena, p->ret_type);
    for (i32 j = 0; j < p->body.length; j++) {
        type_check_stmt((Stmt *)p->body.data[j], &scope, prog, return_type, p, arena);
    }

    g_diag_source_path = prev_diag_source_path;
    g_diag_import_chain = prev_diag_import_chain;
}

static void type_check_generic_proc_instances(
    Program *prog,
    TypeScope *globals,
    memops_arena *arena
) {
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *p = (ProcDecl *)prog->procs.data[i];
        if (!p->is_generic || p->is_external) continue;

        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        Vec_string8 sub_instances = Vec_string8_reserve(arena, 4);
        Vec_voidptr instance_sites = ptr_array_reserve(arena, 4);
        collect_generic_proc_instances_with_sites(prog, p, &instances, &sub_instances, &instance_sites, arena);
        for (i32 j = 0; j < instances.length; j++) {
            TypeExpr *arg = mangle_type_for(arena, sub_instances.data[j]);
            TypeSub sub = proc_sub_for_instance(arena, p, instances.data[j], arg);
            type_check_proc_body(prog, p, globals, sub, arena);
        }
    }
}

static void type_scope_add_function_like_macros(memops_arena *arena, TypeScope *scope, Program *prog) {
    for (i32 i = 0; i < prog->preprocessor_lines.length; i++) {
        string8 line = prog->preprocessor_lines.data[i];
        if (!preprocessor_define_is_function_like(line)) continue;
        string8 name = preprocessor_define_name(arena, line);
        if (!name.data) continue;
        TypeExpr *macro = type_new(arena, Type_Proc);
        macro->args = ptr_array_reserve(arena, 1);
        /* No return type on purpose. cpp substitutes tokens and has no types, so
           a macro's result is genuinely unknown -- `#define TWICE(x) ((x)*2)`
           yields a number while `gin_require(...)` yields nothing at all. A null
           return type is how the checker already spells "unknown", and it makes
           both usable without inventing a type for either. */
        macro->ret_type = null;
        macro->is_variadic = true;
        type_scope_add(arena, scope, name, macro);
    }
}

static void type_check_program(Program *prog, memops_arena *arena) {
    TypeScope globals = type_scope_make(arena, 64);
    type_scope_add_reflection_globals(arena, &globals, prog);
    type_scope_add_function_like_macros(arena, &globals, prog);
    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *g = (Stmt *)prog->globals.data[i];
        const char *prev_diag_source_path = g_diag_source_path;
        const char *prev_diag_import_chain = g_diag_import_chain;
        if (g->source_path) {
            g_diag_source_path = g->source_path;
        }
        g_diag_import_chain = g->import_chain;
        type_check_expr(g->expr, &globals, prog, arena);
        type_check_initializer_against(g->expr, g->type, &globals, prog, arena);
        TypeExpr *actual = infer_expr_type(g->expr, &globals, prog, arena);
        check_assignment_compatible("global initializer", prog, g->type, actual, g->line, g->col, arena);
        type_scope_add(arena, &globals, g->name, g->type);
        g_diag_source_path = prev_diag_source_path;
        g_diag_import_chain = prev_diag_import_chain;
    }

    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *p = (ProcDecl *)prog->procs.data[i];
        if (p->is_generic) continue;
        type_check_proc_body(prog, p, &globals, (TypeSub){0}, arena);
    }

    type_check_generic_proc_instances(prog, &globals, arena);
}

static const char *printfmt_path(const char *path) {
    return path ? path : g_source_path;
}

static void printfmt_error_at(const char *path, i32 line, i32 col, const char *message) {
    if (g_diag_json) {
        diag_json_error(printfmt_path(path), line, col, "format", message);
        diag_record_error();
        return;
    }
    printf("%s:%d:%d: format error: %s\n", printfmt_path(path), line, col, message);
    diag_record_error();
    return;
}

static bool expr_is_printfmt_call(Expr *e) {
    return e && e->kind == Expr_Call && string8_equals_cstr(&e->name, "printfmt");
}

static i32 printfmt_count_placeholders(string8 lit) {
    if (lit.length < 2 || lit.data[0] != '"' || lit.data[lit.length - 1] != '"') return -1;
    i32 count = 0;
    for (u64 i = 1; i + 1 < lit.length; i++) {
        if (lit.data[i] == '{' && (i + 1) < (lit.length - 1) && lit.data[i + 1] == '}') {
            count++;
            i++;
        }
    }
    return count;
}

static Expr *printfmt_string_expr(memops_arena *arena, u8 *data, u64 length, i32 line, i32 col) {
    Expr *e = expr_new(arena, Expr_String);
    e->string_lit = string8_reserve(arena, length + 2);
    string8_append_byte(arena, &e->string_lit, '"');
    string8_append_bytes(arena, &e->string_lit, data, length);
    string8_append_byte(arena, &e->string_lit, '"');
    e->line = line;
    e->col = col;
    return e;
}

static Expr *printfmt_call_expr(memops_arena *arena, const char *name, i32 line, i32 col) {
    Expr *call = expr_new(arena, Expr_Call);
    call->name = string8_from_cstr(arena, name);
    call->args = ptr_array_reserve(arena, 2);
    call->type_args = ptr_array_reserve(arena, 1);
    call->line = line;
    call->col = col;
    return call;
}

static Stmt *printfmt_expr_stmt(memops_arena *arena, Expr *expr, Stmt *source) {
    Stmt *stmt = memops_arena_push_struct(arena, Stmt);
    memset(stmt, 0, sizeof(Stmt));
    stmt->kind = Stmt_Expr;
    stmt->expr = expr;
    stmt->source_path = source ? source->source_path : null;
    stmt->import_chain = source ? source->import_chain : null;
    stmt->line = expr ? expr->line : (source ? source->line : 0);
    stmt->col = expr ? expr->col : (source ? source->col : 0);
    return stmt;
}

static void printfmt_append_print_cstr(memops_arena *arena, Vec_voidptr *out, Stmt *source, u8 *data, u64 length, i32 line, i32 col) {
    if (length == 0) return;
    Expr *arg = printfmt_string_expr(arena, data, length, line, col);
    Expr *call = printfmt_call_expr(arena, "print_cstr", line, col);
    ptr_array_append(arena, &call->args, arg);
    ptr_array_append(arena, out, printfmt_expr_stmt(arena, call, source));
}

/* The printf conversion that matches what std/Print.i's print<T> would emit for
   this type, or null when there is no single spec that does.

   `bool` and `b32` return null on purpose: print<bool> writes "true"/"false",
   which no conversion produces, so folding them would change what the program
   prints. Anything else without an entry -- a user's own print<Payload> -- lands
   here too and keeps the per-argument call. */
static const char *printfmt_spec_for_type(Program *prog, TypeExpr *type) {
    if (!type) return null;
    type = resolve_alias_type(prog, type);
    if (type->kind == Type_Ptr) {
        TypeExpr *elem = type->elem ? resolve_alias_type(prog, type->elem) : null;
        if (elem && elem->kind == Type_Name && string8_equals_cstr(&elem->name, "c8")) {
            return "%s";
        }
        return null;
    }
    if (type->kind != Type_Name) return null;
    string8 n = type->name;
    if (string8_equals_cstr(&n, "i8")) return "%d";
    if (string8_equals_cstr(&n, "i16")) return "%d";
    if (string8_equals_cstr(&n, "i32")) return "%d";
    if (string8_equals_cstr(&n, "i64")) return "%lld";
    if (string8_equals_cstr(&n, "u8")) return "%u";
    if (string8_equals_cstr(&n, "u16")) return "%u";
    if (string8_equals_cstr(&n, "u32")) return "%u";
    if (string8_equals_cstr(&n, "u64")) return "%llu";
    if (string8_equals_cstr(&n, "usize")) return "%zu";
    if (string8_equals_cstr(&n, "f32")) return "%f";
    if (string8_equals_cstr(&n, "f64")) return "%f";
    if (string8_equals_cstr(&n, "c8")) return "%c";
    if (string8_equals_cstr(&n, "b8")) return "%d";
    if (string8_equals_cstr(&n, "b16")) return "%d";
    if (string8_equals_cstr(&n, "b32")) return "%d";
    if (string8_equals_cstr(&n, "b64")) return "%lld";
    if (string8_equals_cstr(&n, "intptr")) return "%lld";
    if (string8_equals_cstr(&n, "uintptr")) return "%llu";
    if (string8_equals_cstr(&n, "ptrdiff")) return "%lld";
    if (string8_equals_cstr(&n, "intmax")) return "%lld";
    if (string8_equals_cstr(&n, "uintmax")) return "%llu";
    return null;
}

static bool printfmt_spec_is_cstr(const char *spec) {
    return spec && spec[0] == '%' && spec[1] == 's' && spec[2] == 0;
}

/* A `%` in the author's text is literal, so it has to be doubled on the way into
   a real printf format. */
static void printfmt_append_escaped(memops_arena *arena, string8 *out, u8 *data, u64 length) {
    for (u64 i = 0; i < length; i++) {
        if (data[i] == '%') string8_append_byte(arena, out, '%');
        string8_append_byte(arena, out, data[i]);
    }
}

static void printfmt_append_print_arg(memops_arena *arena, Vec_voidptr *out, Stmt *source, Expr *arg, i32 arg_index, TypeScope *scope, Program *prog) {
    TypeExpr *type = infer_expr_type(arg, scope, prog, arena);
    if (!type) {
        char message[256];
        snprintf(message, sizeof(message), "cannot infer printfmt arg %d type", (int)arg_index);
        printfmt_error_at(source ? source->source_path : null, arg->line, arg->col, message);
    }
    Expr *call = printfmt_call_expr(arena, "print", arg->line, arg->col);
    ptr_array_append(arena, &call->args, arg);
    ptr_array_append(arena, &call->type_args, clone_type_expr(arena, type));
    ptr_array_append(arena, out, printfmt_expr_stmt(arena, call, source));
}

static void rewrite_printfmt_in_expr(Expr *e, const char *path);
static void rewrite_printfmt_stmt_in_place(Stmt *s, TypeScope *scope, Program *prog, memops_arena *arena);
static void rewrite_printfmt_body(Vec_voidptr *body, TypeScope *scope, Program *prog, memops_arena *arena);

static void rewrite_printfmt_in_expr(Expr *e, const char *path) {
    if (!e) return;
    if (expr_is_printfmt_call(e)) {
        printfmt_error_at(path, e->line, e->col, "printfmt can only be used as a statement");
    }
    if (e->kind == Expr_Call || e->kind == Expr_InitList || e->kind == Expr_CompoundInit) {
        for (i32 i = 0; i < e->args.length; i++) {
            rewrite_printfmt_in_expr((Expr *)e->args.data[i], path);
        }
        for (i32 i = 0; i < e->designators.length; i++) {
            rewrite_printfmt_in_expr((Expr *)e->designators.data[i], path);
        }
        return;
    }
    if (e->kind == Expr_Binary) {
        rewrite_printfmt_in_expr(e->left, path);
        rewrite_printfmt_in_expr(e->right, path);
        return;
    }
    if (e->kind == Expr_Ternary) {
        rewrite_printfmt_in_expr(e->left, path);
        rewrite_printfmt_in_expr(e->right, path);
        rewrite_printfmt_in_expr(e->third, path);
        return;
    }
    if (e->kind == Expr_Addr || e->kind == Expr_Cast || e->kind == Expr_Unary) {
        rewrite_printfmt_in_expr(e->inner, path);
        return;
    }
    if (e->kind == Expr_Index) {
        rewrite_printfmt_in_expr(e->base, path);
        rewrite_printfmt_in_expr(e->index_expr, path);
        return;
    }
    if (e->kind == Expr_Field) {
        rewrite_printfmt_in_expr(e->base, path);
        return;
    }
}

/* Collapse the whole printfmt into one printf.

   The per-piece expansion below is correct but emits a call per literal run and
   per argument -- nine calls for a four-placeholder line -- and each one re-enters
   the C formatter and takes the stream lock. Measured at 1.65x the cost of the
   equivalent hand-written printf. Since the format is a literal and every
   argument's type is already known here, the conversions can be worked out now
   and the whole thing emitted as a single call.

   Returns false when any argument has no matching conversion, in which case the
   caller falls back and nothing is lost. */
static bool printfmt_try_single_call(
    memops_arena *arena,
    Vec_voidptr *out,
    Stmt *stmt,
    Expr *call,
    Expr *fmt,
    string8 lit,
    TypeScope *scope,
    Program *prog
) {
    i32 value_count = call->args.length - 1;
    for (i32 i = 0; i < value_count; i++) {
        Expr *arg = (Expr *)call->args.data[i + 1];
        TypeExpr *type = infer_expr_type(arg, scope, prog, arena);
        if (!printfmt_spec_for_type(prog, type)) return false;
    }

    string8 built = string8_reserve(arena, lit.length + (u64)value_count * 4 + 4);
    string8_append_byte(arena, &built, '"');

    Expr *printf_call = printfmt_call_expr(arena, "printf", fmt->line, fmt->col);
    Expr *fmt_arg = expr_new(arena, Expr_String);
    fmt_arg->line = fmt->line;
    fmt_arg->col = fmt->col;
    ptr_array_append(arena, &printf_call->args, fmt_arg);

    u64 chunk_start = 1;
    i32 arg_index = 0;
    for (u64 i = 1; i + 1 < lit.length; i++) {
        if (lit.data[i] == '{' && (i + 1) < (lit.length - 1) && lit.data[i + 1] == '}') {
            printfmt_append_escaped(arena, &built, lit.data + chunk_start, i - chunk_start);

            Expr *arg = (Expr *)call->args.data[arg_index + 1];
            TypeExpr *type = infer_expr_type(arg, scope, prog, arena);
            const char *spec = printfmt_spec_for_type(prog, type);
            string8_append_cstr(arena, &built, spec);

            /* print_cstr prints "(null)" for a null pointer; printf with %s would
               be undefined. The guard keeps the behaviour without costing a call. */
            if (printfmt_spec_is_cstr(spec)) {
                Expr *guard = expr_new(arena, Expr_Ternary);
                Expr *cmp = expr_new(arena, Expr_Binary);
                Expr *nul = expr_new(arena, Expr_Name);
                Expr *fallback = expr_new(arena, Expr_String);
                nul->name = string8_from_cstr(arena, "null");
                nul->line = arg->line;
                nul->col = arg->col;
                cmp->op = Token_BangEqual;
                cmp->left = arg;
                cmp->right = nul;
                cmp->line = arg->line;
                cmp->col = arg->col;
                fallback->string_lit = string8_from_cstr(arena, "\"(null)\"");
                fallback->line = arg->line;
                fallback->col = arg->col;
                guard->left = cmp;
                guard->right = arg;
                guard->third = fallback;
                guard->line = arg->line;
                guard->col = arg->col;
                ptr_array_append(arena, &printf_call->args, guard);
            } else {
                ptr_array_append(arena, &printf_call->args, arg);
            }

            arg_index++;
            i++;
            chunk_start = i + 1;
        }
    }
    printfmt_append_escaped(arena, &built, lit.data + chunk_start, (lit.length - 1) - chunk_start);
    string8_append_byte(arena, &built, '"');
    fmt_arg->string_lit = built;

    ptr_array_append(arena, out, printfmt_expr_stmt(arena, printf_call, stmt));
    return true;
}

static void rewrite_printfmt_call_stmt(Stmt *stmt, TypeScope *scope, Program *prog, memops_arena *arena, Vec_voidptr *out) {
    /* Expanding a printfmt replaces statements and can introduce generic print
       calls, so anything cached about instantiation has to be recomputed. */
    g_program_generation++;
    Expr *call = stmt->expr;
    if (call->args.length < 1) {
        printfmt_error_at(stmt->source_path, call->line, call->col, "printfmt expects a string literal format");
        return;
    }
    Expr *fmt = (Expr *)call->args.data[0];
    if (!fmt || fmt->kind != Expr_String) {
        printfmt_error_at(stmt->source_path, call->line, call->col, "printfmt expects a string literal format");
        return; // string_lit is meaningless on any other expression
    }

    string8 lit = fmt->string_lit;
    i32 placeholder_count = printfmt_count_placeholders(lit);
    if (placeholder_count < 0) {
        printfmt_error_at(stmt->source_path, fmt->line, fmt->col, "printfmt expects a normal string literal format");
        return;
    }
    i32 value_count = call->args.length - 1;
    if (placeholder_count != value_count) {
        char message[256];
        snprintf(message, sizeof(message), "printfmt placeholder count (%d) does not match arg count (%d)", placeholder_count, value_count);
        printfmt_error_at(stmt->source_path, fmt->line, fmt->col, message);
        return; // expanding would read past the supplied arguments
    }

    if (printfmt_try_single_call(arena, out, stmt, call, fmt, lit, scope, prog)) {
        return;
    }

    u64 chunk_start = 1;
    i32 arg_index = 0;
    for (u64 i = 1; i + 1 < lit.length; i++) {
        if (lit.data[i] == '{' && (i + 1) < (lit.length - 1) && lit.data[i + 1] == '}') {
            printfmt_append_print_cstr(arena, out, stmt, lit.data + chunk_start, i - chunk_start, fmt->line, fmt->col);
            Expr *arg = (Expr *)call->args.data[arg_index + 1];
            printfmt_append_print_arg(arena, out, stmt, arg, arg_index + 1, scope, prog);
            arg_index++;
            i++;
            chunk_start = i + 1;
        }
    }
    printfmt_append_print_cstr(arena, out, stmt, lit.data + chunk_start, (lit.length - 1) - chunk_start, fmt->line, fmt->col);
}

static void rewrite_printfmt_stmt_in_place(Stmt *s, TypeScope *scope, Program *prog, memops_arena *arena) {
    if (!s) return;
    const char *path = s->source_path;
    if (s->kind == Stmt_Var) {
        rewrite_printfmt_in_expr(s->expr, path);
        type_scope_add(arena, scope, s->name, s->type);
        return;
    }
    if (s->kind == Stmt_Expr && expr_is_printfmt_call(s->expr)) {
        printfmt_error_at(path, s->line, s->col, "printfmt cannot be used in this statement position");
    }
    if (s->kind == Stmt_Assign || s->kind == Stmt_Expr || s->kind == Stmt_Return) {
        rewrite_printfmt_in_expr(s->expr, path);
        return;
    }
    if (s->kind == Stmt_For) {
        TypeScope loop_scope = type_scope_copy(arena, scope);
        if (s->for_init) rewrite_printfmt_stmt_in_place(s->for_init, &loop_scope, prog, arena);
        if (s->for_cond) rewrite_printfmt_in_expr(s->for_cond, path);
        if (s->for_step) rewrite_printfmt_stmt_in_place(s->for_step, &loop_scope, prog, arena);
        rewrite_printfmt_body(&s->for_body, &loop_scope, prog, arena);
        return;
    }
    if (s->kind == Stmt_If) {
        rewrite_printfmt_in_expr(s->if_cond, path);
        TypeScope then_scope = type_scope_copy(arena, scope);
        rewrite_printfmt_body(&s->if_then_body, &then_scope, prog, arena);
        if (s->if_else_if) {
            TypeScope else_if_scope = type_scope_copy(arena, scope);
            rewrite_printfmt_stmt_in_place(s->if_else_if, &else_if_scope, prog, arena);
        } else {
            TypeScope else_scope = type_scope_copy(arena, scope);
            rewrite_printfmt_body(&s->if_else_body, &else_scope, prog, arena);
        }
        return;
    }
    if (s->kind == Stmt_While) {
        rewrite_printfmt_in_expr(s->while_cond, path);
        TypeScope loop_scope = type_scope_copy(arena, scope);
        rewrite_printfmt_body(&s->while_body, &loop_scope, prog, arena);
        return;
    }
    if (s->kind == Stmt_DoWhile || s->kind == Stmt_Label) {
        TypeScope loop_scope = type_scope_copy(arena, scope);
        rewrite_printfmt_body(&s->while_body, &loop_scope, prog, arena);
        rewrite_printfmt_in_expr(s->while_cond, path);
        return;
    }
    if (s->kind == Stmt_Switch) {
        rewrite_printfmt_in_expr(s->switch_expr, path);
        for (i32 i = 0; i < s->switch_cases.length; i++) {
            SwitchCase *sc = (SwitchCase *)s->switch_cases.data[i];
            rewrite_printfmt_in_expr(sc->expr, path);
            TypeScope case_scope = type_scope_copy(arena, scope);
            rewrite_printfmt_body(&sc->body, &case_scope, prog, arena);
        }
        TypeScope default_scope = type_scope_copy(arena, scope);
        rewrite_printfmt_body(&s->switch_default_body, &default_scope, prog, arena);
        return;
    }
}

static void rewrite_printfmt_body(Vec_voidptr *body, TypeScope *scope, Program *prog, memops_arena *arena) {
    Vec_voidptr rewritten = ptr_array_reserve(arena, body->length + 8);
    for (i32 i = 0; i < body->length; i++) {
        Stmt *s = (Stmt *)body->data[i];
        if (s && s->kind == Stmt_Expr && expr_is_printfmt_call(s->expr)) {
            rewrite_printfmt_call_stmt(s, scope, prog, arena, &rewritten);
            continue;
        }
        rewrite_printfmt_stmt_in_place(s, scope, prog, arena);
        ptr_array_append(arena, &rewritten, s);
    }
    *body = rewritten;
}

static void rewrite_printfmt_formats(Program *prog, memops_arena *arena) {
    TypeScope globals = type_scope_make(arena, 64);
    type_scope_add_reflection_globals(arena, &globals, prog);
    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *g = (Stmt *)prog->globals.data[i];
        rewrite_printfmt_in_expr(g->expr, g->source_path);
        type_scope_add(arena, &globals, g->name, g->type);
    }

    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *p = (ProcDecl *)prog->procs.data[i];
        TypeScope scope = type_scope_copy(arena, &globals);
        for (i32 j = 0; j < p->params.length; j++) {
            Param *param = (Param *)p->params.data[j];
            type_scope_add(arena, &scope, param->name, param->type);
        }
        rewrite_printfmt_body(&p->body, &scope, prog, arena);
    }
}

static void emit_pre_directives(memops_arena *arena, string8 *out, Vec_string8 *lines);
static void emit_expr(memops_arena *arena, string8 *out, Expr *e, TypeSub sub, string8 generic_name);
static void emit_expr_value(memops_arena *arena, string8 *out, Expr *e, TypeSub sub, string8 generic_name);
static void emit_expr_condition(memops_arena *arena, string8 *out, Expr *e, TypeSub sub, string8 generic_name);
static void emit_stmt(memops_arena *arena, string8 *out, Stmt *s, TypeSub sub, string8 generic_name);

/* True when control cannot run off the end of the list, so an appended break
   would be unreachable. An if/else counts only when both arms jump. */
static bool stmt_list_ends_in_jump(Vec_voidptr *body) {
    if (body->length == 0) {
        return false;
    }
    Stmt *last = (Stmt *)body->data[body->length - 1];
    if (last->kind == Stmt_Return || last->kind == Stmt_Break ||
        last->kind == Stmt_Continue || last->kind == Stmt_Goto) {
        return true;
    }
    if (last->kind == Stmt_If && last->if_else_body.length > 0 && !last->if_else_if) {
        return stmt_list_ends_in_jump(&last->if_then_body) &&
               stmt_list_ends_in_jump(&last->if_else_body);
    }
    return false;
}

static void emit_proc_monomorph_comment(memops_arena *arena, string8 *out, ProcDecl *decl, string8 type_mangled, GenericInstanceSite *site);
static void emit_line_directive_path(memops_arena *arena, string8 *out, const char *path, i32 line);
static void emit_generated_line_directive(memops_arena *arena, string8 *out);

static void emit_decl_type_prefix(memops_arena *arena, string8 *out, TypeExpr *type, TypeSub sub) {
    while (type && type->kind == Type_Array) {
        type = type->elem;
    }
    emit_type(arena, out, type, sub);
}

static void emit_decl_array_suffix(memops_arena *arena, string8 *out, TypeExpr *type) {
    if (!type) return;
    if (type->kind == Type_Array) {
        emit_cstr(arena, out, "[");
        emit_string8(arena, out, type->array_count);
        emit_cstr(arena, out, "]");
        emit_decl_array_suffix(arena, out, type->elem);
    }
}

static void emit_decl(memops_arena *arena, string8 *out, TypeExpr *type, string8 name, TypeSub sub) {
    /* The reflect runtime's I-side constants are declared here as well as read,
       so the declaration has to agree with the reads. */
    name = c_ident(arena, reflect_runtime_c_name(arena, name));
    TypeExpr *proc_type = null;
    if (type && type->kind == Type_Proc) {
        proc_type = type;
    } else if (type && type->kind == Type_Ptr && type->elem && type->elem->kind == Type_Proc) {
        proc_type = type->elem;
    }
    if (proc_type) {
        if (proc_type->ret_type) emit_type(arena, out, proc_type->ret_type, sub);
        else emit_cstr(arena, out, "void");
        emit_cstr(arena, out, " (");
        if (proc_type->name.data) {
            emit_c_ident(arena, out, proc_type->name);
            emit_cstr(arena, out, " ");
        }
        emit_cstr(arena, out, "*");
        emit_string8(arena, out, name);
        emit_cstr(arena, out, ")(");
        if (proc_type->args.length == 0 && !proc_type->is_variadic) {
            emit_cstr(arena, out, "void");
        } else {
            for (i32 i = 0; i < proc_type->args.length; i++) {
                if (i > 0) emit_cstr(arena, out, ", ");
                emit_type(arena, out, (TypeExpr *)proc_type->args.data[i], sub);
                if (i < proc_type->arg_names.length && proc_type->arg_names.data[i].length > 0) {
                    emit_cstr(arena, out, " ");
                    emit_string8(arena, out, proc_type->arg_names.data[i]);
                }
            }
            if (proc_type->is_variadic) {
                if (proc_type->args.length > 0) emit_cstr(arena, out, ", ");
                emit_cstr(arena, out, "...");
            }
        }
        emit_cstr(arena, out, ")");
        return;
    }
    emit_decl_type_prefix(arena, out, type, sub);
    emit_cstr(arena, out, " ");
    emit_string8(arena, out, name);
    emit_decl_array_suffix(arena, out, type);
}

static void emit_compound_literal_type(memops_arena *arena, string8 *out, TypeExpr *type, TypeSub sub) {
    emit_decl_type_prefix(arena, out, type, sub);
    emit_decl_array_suffix(arena, out, type);
}

static void emit_if_stmt(memops_arena *arena, string8 *out, Stmt *s, TypeSub sub, string8 generic_name) {
    emit_cstr(arena, out, "if (");
    emit_expr_condition(arena, out, s->if_cond, sub, generic_name);
    emit_cstr(arena, out, ") {\n");
    for (i32 i = 0; i < s->if_then_body.length; i++) {
        emit_cstr(arena, out, "        ");
        emit_stmt(arena, out, (Stmt *)s->if_then_body.data[i], sub, generic_name);
    }
    emit_cstr(arena, out, "    }");
    if (s->if_else_if) {
        emit_cstr(arena, out, " else ");
        emit_if_stmt(arena, out, s->if_else_if, sub, generic_name);
        return;
    }
    if (s->if_else_body.length > 0) {
        emit_cstr(arena, out, " else {\n");
        for (i32 i = 0; i < s->if_else_body.length; i++) {
            emit_cstr(arena, out, "        ");
            emit_stmt(arena, out, (Stmt *)s->if_else_body.data[i], sub, generic_name);
        }
        emit_cstr(arena, out, "    }\n");
        return;
    }
    emit_cstr(arena, out, "\n");
}

static void emit_expr_value(memops_arena *arena, string8 *out, Expr *e, TypeSub sub, string8 generic_name) {
    if (e && (e->kind == Expr_InitList || e->kind == Expr_ZeroInit) && e->cast_type) {
        emit_cstr(arena, out, "((");
        emit_compound_literal_type(arena, out, e->cast_type, sub);
        emit_cstr(arena, out, ")");
        emit_expr(arena, out, e, sub, generic_name);
        emit_cstr(arena, out, ")");
        return;
    }
    emit_expr(arena, out, e, sub, generic_name);
}

static void emit_binary_op(memops_arena *arena, string8 *out, TokenKind op) {
    if (op == Token_Plus) {
        emit_cstr(arena, out, " + ");
    } else if (op == Token_Minus) {
        emit_cstr(arena, out, " - ");
    } else if (op == Token_Star) {
        emit_cstr(arena, out, " * ");
    } else if (op == Token_Slash) {
        emit_cstr(arena, out, " / ");
    } else if (op == Token_Percent) {
        emit_cstr(arena, out, " % ");
    } else if (op == Token_LAngle) {
        emit_cstr(arena, out, " < ");
    } else if (op == Token_RAngle) {
        emit_cstr(arena, out, " > ");
    } else if (op == Token_LessEqual) {
        emit_cstr(arena, out, " <= ");
    } else if (op == Token_GreaterEqual) {
        emit_cstr(arena, out, " >= ");
    } else if (op == Token_EqualEqual) {
        emit_cstr(arena, out, " == ");
    } else if (op == Token_BangEqual) {
        emit_cstr(arena, out, " != ");
    } else if (op == Token_Ampersand) {
        emit_cstr(arena, out, " & ");
    } else if (op == Token_Caret) {
        emit_cstr(arena, out, " ^ ");
    } else if (op == Token_Pipe) {
        emit_cstr(arena, out, " | ");
    } else if (op == Token_Keyword_And) {
        emit_cstr(arena, out, " && ");
    } else if (op == Token_Keyword_Or) {
        emit_cstr(arena, out, " || ");
    } else if (op == Token_Keyword_Shl) {
        emit_cstr(arena, out, " << ");
    } else if (op == Token_Keyword_Shr) {
        emit_cstr(arena, out, " >> ");
    } else {
        emit_cstr(arena, out, " /* unsupported op */ ");
    }
}

static void emit_binary_expr(memops_arena *arena, string8 *out, Expr *e, TypeSub sub, string8 generic_name, bool wrap) {
    if (wrap) emit_cstr(arena, out, "(");
    emit_expr(arena, out, e->left, sub, generic_name);
    emit_binary_op(arena, out, e->op);
    emit_expr(arena, out, e->right, sub, generic_name);
    if (wrap) emit_cstr(arena, out, ")");
}

static void emit_expr_condition(memops_arena *arena, string8 *out, Expr *e, TypeSub sub, string8 generic_name) {
    if (e && e->kind == Expr_Binary) {
        emit_binary_expr(arena, out, e, sub, generic_name, false);
        return;
    }
    emit_expr(arena, out, e, sub, generic_name);
}

static void emit_expr(memops_arena *arena, string8 *out, Expr *e, TypeSub sub, string8 generic_name) {
    if (!e) return;
    if (e->kind == Expr_Number) {
        string8 n = e->number;
        if (n.length > 3 &&
            n.data[n.length - 3] == 'u' &&
            n.data[n.length - 2] == '6' &&
            n.data[n.length - 1] == '4') {
            emit_cstr(arena, out, "((u64)");
            string8 digits = string8_copy_from_slice(arena, n.data, n.length - 3);
            emit_string8(arena, out, digits);
            emit_cstr(arena, out, ")");
        } else {
            emit_string8(arena, out, n);
        }
        return;
    }
    if (e->kind == Expr_String || e->kind == Expr_Char) {
        emit_string8(arena, out, e->string_lit);
        return;
    }
    if (e->kind == Expr_SizeofType || e->kind == Expr_AlignofType) {
        emit_cstr(arena, out, e->kind == Expr_SizeofType ? "sizeof(" : "_Alignof(");
        if (e->cast_type) {
            emit_type(arena, out, e->cast_type, sub);
        } else {
            emit_expr(arena, out, e->inner, sub, generic_name);
        }
        emit_cstr(arena, out, ")");
        return;
    }
    if (e->kind == Expr_ZeroInit) {
        emit_cstr(arena, out, "{}");
        return;
    }
    if (e->kind == Expr_InitList) {
        bool guarded = false;
        for (i32 i = 0; i < e->arg_directives.length; i++) {
            if (e->arg_directives.data[i]) guarded = true;
        }
        emit_cstr(arena, out, "{");
        for (i32 i = 0; i < e->args.length; i++) {
            if (i < e->arg_directives.length) {
                emit_pre_directives(arena, out, (Vec_string8 *)e->arg_directives.data[i]);
            }
            if (!guarded && i > 0) emit_cstr(arena, out, ", ");
            Expr *designator = (Expr *)e->designators.data[i];
            if (designator) {
                if (e->designator_kinds.data[i] == InitDesignator_Field) {
                    emit_cstr(arena, out, ".");
                    emit_c_ident(arena, out, designator->name);
                    emit_cstr(arena, out, " = ");
                } else {
                    emit_cstr(arena, out, "[");
                    emit_expr(arena, out, designator, sub, generic_name);
                    emit_cstr(arena, out, "] = ");
                }
            }
            emit_expr(arena, out, (Expr *)e->args.data[i], sub, generic_name);
            if (guarded) emit_cstr(arena, out, ", ");
        }
        if (e->args.length < e->arg_directives.length) {
            emit_pre_directives(arena, out, (Vec_string8 *)e->arg_directives.data[e->args.length]);
        }
        emit_cstr(arena, out, "}");
        return;
    }
    if (e->kind == Expr_CompoundInit) {
        emit_cstr(arena, out, "((");
        emit_compound_literal_type(arena, out, e->cast_type, sub);
        emit_cstr(arena, out, ")");
        emit_expr(arena, out, e->inner, sub, generic_name);
        emit_cstr(arena, out, ")");
        return;
    }
    if (e->kind == Expr_Name) {
        if (string8_equals_cstr(&e->name, "null")) {
            emit_cstr(arena, out, "0");
            return;
        }
        emit_string8(arena, out, c_ident(arena, reflect_runtime_c_name(arena, e->name)));
        return;
    }
    if (e->kind == Expr_Cast) {
        emit_cstr(arena, out, "((");
        emit_type(arena, out, e->cast_type, sub);
        emit_cstr(arena, out, ")(");
        emit_expr(arena, out, e->inner, sub, generic_name);
        emit_cstr(arena, out, "))");
        return;
    }
    if (e->kind == Expr_Addr) {
        emit_cstr(arena, out, "&(");
        emit_expr(arena, out, e->inner, sub, generic_name);
        emit_cstr(arena, out, ")");
        return;
    }
    if (e->kind == Expr_Unary) {
        if (e->op == Token_Bang) emit_cstr(arena, out, "!");
        else if (e->op == Token_Minus) emit_cstr(arena, out, "-");
        else if (e->op == Token_Tilde) emit_cstr(arena, out, "~");
        emit_expr(arena, out, e->inner, sub, generic_name);
        return;
    }
    if (e->kind == Expr_Index) {
        emit_expr(arena, out, e->base, sub, generic_name);
        emit_cstr(arena, out, "[");
        emit_expr(arena, out, e->index_expr, sub, generic_name);
        emit_cstr(arena, out, "]");
        return;
    }
    if (e->kind == Expr_Field) {
        emit_expr(arena, out, e->base, sub, generic_name);
        emit_cstr(arena, out, ".");
        emit_c_ident(arena, out, e->name);
        return;
    }
    if (e->kind == Expr_Binary) {
        emit_binary_expr(arena, out, e, sub, generic_name, true);
        return;
    }
    if (e->kind == Expr_Ternary) {
        emit_cstr(arena, out, "(");
        emit_expr(arena, out, e->left, sub, generic_name);
        emit_cstr(arena, out, " ? ");
        emit_expr(arena, out, e->right, sub, generic_name);
        emit_cstr(arena, out, " : ");
        emit_expr(arena, out, e->third, sub, generic_name);
        emit_cstr(arena, out, ")");
        return;
    }
    if (e->kind == Expr_Call) {
        if (e->base) {
            /* Indirect call: the callee is an expression, not a name, so there is
               no monomorph mangling to apply. */
            emit_expr_value(arena, out, e->base, sub, generic_name);
            emit_cstr(arena, out, "(");
            for (i32 i = 0; i < e->args.length; i++) {
                if (i > 0) emit_cstr(arena, out, ", ");
                emit_expr_value(arena, out, (Expr *)e->args.data[i], sub, generic_name);
            }
            emit_cstr(arena, out, ")");
            return;
        }
        if (e->type_args.length > 1) {
            /* Several type arguments join into one suffix, so `first<i32, f32>`
               calls `first_i32_f32` -- the same name the instance was recorded
               and emitted under. */
            string8 joined = string8_reserve(arena, 32);
            for (i32 t = 0; t < e->type_args.length; t++) {
                if (t > 0) string8_append_cstr(arena, &joined, "_");
                string8 m = type_mangle(arena, (TypeExpr *)e->type_args.data[t], sub);
                string8_append_bytes(arena, &joined, m.data, m.length);
            }
            emit_mono_proc_name(arena, out, e->name, joined);
        } else if (e->type_args.length == 1) {
            TypeExpr *arg = (TypeExpr *)e->type_args.data[0];
            string8 mangle = type_mangle(arena, arg, sub);
            emit_mono_proc_name(arena, out, e->name, mangle);
        } else if (generic_name.data && string8_equals_name(e->name, generic_name) && sub.has) {
            emit_c_ident(arena, out, e->name);
            emit_cstr(arena, out, "_");
            string8 mangle = type_mangle(arena, type_sub_first_arg(sub), (TypeSub){0});
            emit_string8(arena, out, mangle);
        } else {
            emit_string8(arena, out, c_ident(arena, reflect_runtime_c_name(arena, e->name)));
        }

        emit_cstr(arena, out, "(");
        for (i32 i = 0; i < e->args.length; i++) {
            if (i > 0) emit_cstr(arena, out, ", ");
            emit_expr_value(arena, out, (Expr *)e->args.data[i], sub, generic_name);
        }
        emit_cstr(arena, out, ")");
        return;
    }
}

static void emit_stmt(memops_arena *arena, string8 *out, Stmt *s, TypeSub sub, string8 generic_name) {
    if (!s) return;
    emit_line_directive_path(arena, out, s->source_path, s->line);
    if (s->kind == Stmt_Var) {
        if (s->align.data && s->align.length > 0) {
            emit_cstr(arena, out, "_Alignas(");
            emit_string8(arena, out, s->align);
            emit_cstr(arena, out, ") ");
        }
        if (s->is_static) emit_cstr(arena, out, "static ");
        emit_decl(arena, out, s->type, s->name, sub);
        /* '= ?' lowers to a plain C declaration, so it costs exactly what the
           equivalent C would: nothing. */
        if (s->expr && !s->is_uninitialized) {
            emit_cstr(arena, out, " = ");
            emit_expr(arena, out, s->expr, sub, generic_name);
        }
        emit_cstr(arena, out, ";\n");
        return;
    }
    if (s->kind == Stmt_Return) {
        emit_cstr(arena, out, "return");
        if (s->expr) {
            emit_cstr(arena, out, " ");
            emit_expr_value(arena, out, s->expr, sub, generic_name);
        }
        emit_cstr(arena, out, ";\n");
        return;
    }
    if (s->kind == Stmt_Directive) {
        /* Verbatim, and on a line of its own -- the caller has already indented,
           which C allows before '#', but the directive must still end the line. */
        emit_c_ident(arena, out, s->name);
        emit_cstr(arena, out, "\n");
        return;
    }
    if (s->kind == Stmt_Goto) {
        emit_cstr(arena, out, "goto ");
        emit_c_ident(arena, out, s->name);
        emit_cstr(arena, out, ";\n");
        return;
    }
    if (s->kind == Stmt_Label) {
        emit_c_ident(arena, out, s->name);
        emit_cstr(arena, out, ": {\n");
        for (i32 i = 0; i < s->while_body.length; i++) {
            emit_cstr(arena, out, "    ");
            emit_stmt(arena, out, (Stmt *)s->while_body.data[i], sub, generic_name);
        }
        emit_cstr(arena, out, "}\n");
        return;
    }
    if (s->kind == Stmt_Assign) {
        if (s->lhs) emit_expr(arena, out, s->lhs, sub, generic_name);
        else emit_c_ident(arena, out, s->name);
        emit_assign_op(arena, out, s->assign_op);
        emit_expr_value(arena, out, s->expr, sub, generic_name);
        emit_cstr(arena, out, ";\n");
        return;
    }
    if (s->kind == Stmt_Expr) {
        emit_expr_value(arena, out, s->expr, sub, generic_name);
        emit_cstr(arena, out, ";\n");
        return;
    }
    if (s->kind == Stmt_For) {
        emit_cstr(arena, out, "for (");
        if (s->for_init) {
            if (s->for_init->kind == Stmt_Var) {
                emit_decl(arena, out, s->for_init->type, s->for_init->name, sub);
                if (s->for_init->expr) {
                    emit_cstr(arena, out, " = ");
                    emit_expr(arena, out, s->for_init->expr, sub, generic_name);
                }
            } else if (s->for_init->kind == Stmt_Assign) {
                emit_string8(arena, out, s->for_init->name);
                emit_cstr(arena, out, " = ");
                emit_expr_value(arena, out, s->for_init->expr, sub, generic_name);
            } else if (s->for_init->kind == Stmt_Expr) {
                emit_expr(arena, out, s->for_init->expr, sub, generic_name);
            }
        }
        emit_cstr(arena, out, "; ");
        if (s->for_cond) {
            emit_expr_condition(arena, out, s->for_cond, sub, generic_name);
        }
        emit_cstr(arena, out, "; ");
        if (s->for_step) {
            if (s->for_step->kind == Stmt_Assign) {
                if (s->for_step->lhs) emit_expr(arena, out, s->for_step->lhs, sub, generic_name);
                else emit_string8(arena, out, s->for_step->name);
                emit_assign_op(arena, out, s->for_step->assign_op);
                emit_expr_value(arena, out, s->for_step->expr, sub, generic_name);
            } else if (s->for_step->kind == Stmt_Expr) {
                emit_expr_value(arena, out, s->for_step->expr, sub, generic_name);
            }
        }
        emit_cstr(arena, out, ") {\n");
        for (i32 i = 0; i < s->for_body.length; i++) {
            emit_cstr(arena, out, "        ");
            emit_stmt(arena, out, (Stmt *)s->for_body.data[i], sub, generic_name);
        }
        emit_cstr(arena, out, "    }\n");
        return;
    }
    if (s->kind == Stmt_While) {
        emit_cstr(arena, out, "while (");
        emit_expr_condition(arena, out, s->while_cond, sub, generic_name);
        emit_cstr(arena, out, ") {\n");
        for (i32 i = 0; i < s->while_body.length; i++) {
            emit_cstr(arena, out, "        ");
            emit_stmt(arena, out, (Stmt *)s->while_body.data[i], sub, generic_name);
        }
        emit_cstr(arena, out, "    }\n");
        return;
    }
    if (s->kind == Stmt_DoWhile) {
        emit_cstr(arena, out, "do {\n");
        for (i32 i = 0; i < s->while_body.length; i++) {
            emit_cstr(arena, out, "        ");
            emit_stmt(arena, out, (Stmt *)s->while_body.data[i], sub, generic_name);
        }
        emit_cstr(arena, out, "    } while (");
        emit_expr_condition(arena, out, s->while_cond, sub, generic_name);
        emit_cstr(arena, out, ");\n");
        return;
    }
    if (s->kind == Stmt_Break) {
        emit_cstr(arena, out, "break;\n");
        return;
    }
    if (s->kind == Stmt_Continue) {
        emit_cstr(arena, out, "continue;\n");
        return;
    }
    if (s->kind == Stmt_Switch) {
        emit_cstr(arena, out, "switch (");
        emit_expr(arena, out, s->switch_expr, sub, generic_name);
        emit_cstr(arena, out, ") {\n");
        for (i32 i = 0; i < s->switch_cases.length; i++) {
            SwitchCase *sc = (SwitchCase *)s->switch_cases.data[i];
            emit_cstr(arena, out, "    case ");
            emit_expr(arena, out, sc->expr, sub, generic_name);
            /* The block is real in C too, so locals in one case cannot collide
               with locals in another. */
            emit_cstr(arena, out, ": {\n");
            for (i32 j = 0; j < sc->body.length; j++) {
                emit_cstr(arena, out, "        ");
                emit_stmt(arena, out, (Stmt *)sc->body.data[j], sub, generic_name);
            }
            emit_cstr(arena, out, "    }\n");
            /* A case takes a block, so it reads as self-contained and does not
               fall through. Without this the C fell into the next case: an
               approaching enemy ran the approach case, fell into retreat, and
               walked directly away from its target. Skip it when the body
               already ends in a jump so the C stays warning-clean. */
            if (!stmt_list_ends_in_jump(&sc->body)) {
                emit_cstr(arena, out, "    break;\n");
            }
        }
        if (s->has_switch_default) {
            emit_cstr(arena, out, "    default: {\n");
            for (i32 i = 0; i < s->switch_default_body.length; i++) {
                emit_cstr(arena, out, "        ");
                emit_stmt(arena, out, (Stmt *)s->switch_default_body.data[i], sub, generic_name);
            }
            emit_cstr(arena, out, "    }\n");
        }
        emit_cstr(arena, out, "    }\n");
        return;
    }
    if (s->kind == Stmt_If) {
        emit_if_stmt(arena, out, s, sub, generic_name);
        return;
    }
}

static void emit_c_ident(memops_arena *arena, string8 *out, string8 name) {
    emit_string8(arena, out, c_ident(arena, name));
}

static void emit_pre_directives(memops_arena *arena, string8 *out, Vec_string8 *lines) {
    if (!lines) return;
    for (i32 i = 0; i < lines->length; i++) {
        emit_cstr(arena, out, "\n");
        emit_string8(arena, out, lines->data[i]);
        emit_cstr(arena, out, "\n");
    }
}

static void emit_struct_fields(
    memops_arena *arena,
    string8 *out,
    StructDecl *decl,
    TypeSub sub,
    const char *indent
) {
    for (i32 i = 0; i < decl->fields.length; i++) {
        Field *f = (Field *)decl->fields.data[i];
        emit_pre_directives(arena, out, &f->pre_directives);
        emit_line_directive_path(arena, out, decl->source_path, f->line);
        emit_cstr(arena, out, indent);
        if (f->align.data && f->align.length > 0) {
            emit_cstr(arena, out, "_Alignas(");
            emit_string8(arena, out, f->align);
            emit_cstr(arena, out, ") ");
        }
        if (f->anon) {
            emit_cstr(arena, out, f->anon->is_union ? "union {\n" : "struct {\n");
            emit_struct_fields(arena, out, f->anon, sub, "        ");
            emit_cstr(arena, out, indent);
            emit_cstr(arena, out, "};\n");
            continue;
        }
        emit_decl(arena, out, f->type, f->name, sub);
        if (f->bit_width.data) {
            emit_cstr(arena, out, " : ");
            emit_string8(arena, out, f->bit_width);
        }
        emit_cstr(arena, out, ";\n");
    }
    emit_pre_directives(arena, out, &decl->tail_directives);
}

/* `packed` becomes a pragma pair around the definition and `align(N)` an
   attribute on the tag. Both spellings are accepted by clang and clang-cl,
   which is the whole toolchain this backend targets. The layout checks in
   emit_struct_layout_check compare the result against C either way, so a
   declared layout that these do not actually produce still fails the build. */
static void emit_struct_decl(memops_arena *arena, string8 *out, StructDecl *decl) {
    emit_line_directive_path(arena, out, decl->source_path, decl->line);
    if (decl->packed) {
        emit_cstr(arena, out, "#pragma pack(push, 1)\n");
    }
    if (decl->align.data && decl->align.length > 0) {
        /* The alignment attribute has to sit between `struct` and the tag, and
           structdef() has already pasted those together by the time it is
           expanded. So an aligned record spells out what that macro would have
           produced, with the attribute in the one place C accepts it. */
        emit_cstr(arena, out, "typedef ");
        emit_cstr(arena, out, decl->is_union ? "union " : "struct ");
        emit_c_ident(arena, out, decl->name);
        emit_cstr(arena, out, " ");
        emit_c_ident(arena, out, decl->name);
        emit_cstr(arena, out, ";\n");
        emit_cstr(arena, out, decl->is_union ? "union " : "struct ");
        emit_cstr(arena, out, "__attribute__((aligned(");
        emit_string8(arena, out, decl->align);
        emit_cstr(arena, out, "))) ");
        emit_c_ident(arena, out, decl->name);
        emit_cstr(arena, out, " ");
    } else {
        emit_cstr(arena, out, decl->is_union ? "uniondef(" : "structdef(");
        emit_c_ident(arena, out, decl->name);
        emit_cstr(arena, out, ") ");
    }
    emit_cstr(arena, out, "{\n");
    emit_struct_fields(arena, out, decl, (TypeSub){0}, "    ");
    emit_cstr(arena, out, "};\n");
    if (decl->packed) {
        emit_cstr(arena, out, "#pragma pack(pop)\n");
    }
    emit_cstr(arena, out, "\n");
}

static void emit_alias_decl(memops_arena *arena, string8 *out, AliasDecl *decl) {
    emit_line_directive_path(arena, out, decl->source_path, decl->line);
    emit_cstr(arena, out, "typedef ");
    emit_decl(arena, out, decl->type, decl->name, (TypeSub){0});
    emit_cstr(arena, out, ";\n\n");
}

static void emit_enum_decl(memops_arena *arena, string8 *out, EnumDecl *decl) {
    if (decl->is_external) return;
    emit_line_directive_path(arena, out, decl->source_path, decl->line);
    emit_cstr(arena, out, "typedef enum ");
    emit_c_ident(arena, out, decl->name);
    /* C leaves an enum's underlying type to the implementation, which is what
       made shape.md 2.6 a question at all. Stating it pins the width and the
       signedness, so a member that does not fit is a C error here instead of a
       value that reads differently depending on how it is cast. */
    emit_cstr(arena, out, " {\n");
    for (i32 i = 0; i < decl->items.length; i++) {
        EnumItem *item = (EnumItem *)decl->items.data[i];
        emit_pre_directives(arena, out, &item->pre_directives);
        emit_line_directive_path(arena, out, decl->source_path, item->line);
        emit_cstr(arena, out, "    ");
        emit_c_ident(arena, out, decl->name);
        emit_cstr(arena, out, "_");
        emit_c_ident(arena, out, item->name);
        if (item->value_expr) {
            emit_cstr(arena, out, " = ");
            emit_expr(arena, out, item->value_expr, (TypeSub){0}, (string8){0});
        } else if (item->value.data) {
            emit_cstr(arena, out, " = ");
            emit_string8(arena, out, item->value);
        }
        emit_cstr(arena, out, ",\n");
    }
    emit_pre_directives(arena, out, &decl->tail_directives);
    emit_cstr(arena, out, "} ");
    emit_c_ident(arena, out, decl->name);
    emit_cstr(arena, out, ";\n");

    /* The underlying type, asserted rather than dictated. An unattributed
       enum is i32; see shape.md 2.6. */
    emit_cstr(arena, out, "static_assert(sizeof(");
    emit_c_ident(arena, out, decl->name);
    emit_cstr(arena, out, ") == sizeof(");
    if (decl->underlying.data && decl->underlying.length > 0) {
        emit_string8(arena, out, decl->underlying);
    } else {
        emit_cstr(arena, out, "i32");
    }
    emit_cstr(arena, out, "), \"enum ");
    emit_string8(arena, out, decl->name);
    emit_cstr(arena, out, ": underlying type is not the declared width\");\n\n");
}

static void emit_struct_fwd_decl(memops_arena *arena, string8 *out, string8 name, bool is_union) {
    if (is_union) {
        emit_cstr(arena, out, "typedef union ");
        emit_string8(arena, out, name);
        emit_cstr(arena, out, " ");
        emit_string8(arena, out, name);
        emit_cstr(arena, out, ";\n");
        return;
    }
    emit_cstr(arena, out, "structdecl(");
    emit_string8(arena, out, name);
    emit_cstr(arena, out, ");\n");
}

static void emit_struct_monomorph_comment(memops_arena *arena, string8 *out, StructDecl *decl, string8 type_mangled) {
    emit_generated_line_directive(arena, out);
    emit_cstr(arena, out, "/* I monomorph: ");
    emit_cstr(arena, out, decl->is_union ? "union " : "struct ");
    emit_c_ident(arena, out, decl->name);
    emit_cstr(arena, out, "<");
    emit_struct_type_params(arena, out, decl);
    emit_cstr(arena, out, "> -> ");
    emit_c_ident(arena, out, decl->name);
    emit_cstr(arena, out, "_");
    emit_string8(arena, out, type_mangled);
    emit_cstr(arena, out, "; declared at ");
    emit_cstr(arena, out, decl->source_path ? decl->source_path : g_source_path);
    char loc_buf[64];
    snprintf(loc_buf, sizeof(loc_buf), ":%d:%d", decl->line, decl->col);
    emit_cstr(arena, out, loc_buf);
    emit_cstr(arena, out, " */\n");
}

static void emit_struct_decl_mono(memops_arena *arena, string8 *out, StructDecl *decl, string8 type_mangled, TypeExpr *arg) {
    TypeSub sub = {0};
    sub = struct_sub_for_instance(arena, decl, type_mangled);

    emit_struct_monomorph_comment(arena, out, decl, type_mangled);
    emit_line_directive_path(arena, out, decl->source_path, decl->line);
    emit_cstr(arena, out, decl->is_union ? "uniondef(" : "structdef(");
    emit_c_ident(arena, out, decl->name);
    emit_cstr(arena, out, "_");
    emit_string8(arena, out, type_mangled);
    emit_cstr(arena, out, ") {\n");
    emit_struct_fields(arena, out, decl, sub, "    ");
    emit_cstr(arena, out, "};\n\n");
}

typedef struct ConcreteStructDef {
    StructDecl *decl;
    bool is_mono;
    string8 name;
    string8 mangle;
    TypeExpr *arg;
    TypeSub sub;
    bool emitted;
    bool visiting;
} ConcreteStructDef;

static ConcreteStructDef *concrete_struct_def_new(
    memops_arena *arena,
    StructDecl *decl,
    bool is_mono,
    string8 name,
    string8 mangle,
    TypeExpr *arg
) {
    ConcreteStructDef *def = memops_arena_push_struct(arena, ConcreteStructDef);
    memset(def, 0, sizeof(ConcreteStructDef));
    def->decl = decl;
    def->is_mono = is_mono;
    def->name = name;
    def->mangle = mangle;
    def->arg = arg;
    if (is_mono) {
        def->sub = struct_sub_for_instance(arena, decl, mangle);
    }
    return def;
}

static string8 concrete_struct_mono_name(memops_arena *arena, StructDecl *decl, string8 mangle) {
    string8 name = string8_reserve(arena, decl->name.length + 1 + mangle.length);
    emit_string8(arena, &name, decl->name);
    emit_cstr(arena, &name, "_");
    emit_string8(arena, &name, mangle);
    return name;
}

static Vec_voidptr collect_concrete_struct_defs(memops_arena *arena, Program *prog) {
    Vec_voidptr defs = ptr_array_reserve(arena, 16);
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (decl->is_external) continue;
        if (!decl->is_generic) {
            ptr_array_append(arena, &defs, concrete_struct_def_new(arena, decl, false, decl->name, (string8){0}, null));
            continue;
        }

        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        collect_generic_struct_instances(prog, decl, &instances, arena);
        for (i32 j = 0; j < instances.length; j++) {
            TypeExpr *arg = mangle_type_for(arena, instances.data[j]);
            string8 name = concrete_struct_mono_name(arena, decl, instances.data[j]);
            ptr_array_append(arena, &defs, concrete_struct_def_new(arena, decl, true, name, instances.data[j], arg));
        }
    }
    return defs;
}

static ConcreteStructDef *concrete_struct_def_find(Vec_voidptr *defs, string8 name) {
    if (!name.data) return null;
    for (i32 i = 0; i < defs->length; i++) {
        ConcreteStructDef *def = (ConcreteStructDef *)defs->data[i];
        if (string8_equals(&def->name, &name)) {
            return def;
        }
    }
    return null;
}

static string8 concrete_value_dependency_name(Program *prog, memops_arena *arena, TypeExpr *type, TypeSub sub) {
    if (!type) return (string8){0};
    if (type->kind == Type_Ptr || type->kind == Type_Proc) {
        return (string8){0};
    }
    if (type->kind == Type_Array) {
        return concrete_value_dependency_name(prog, arena, type->elem, sub);
    }
    if (type->kind == Type_Name) {
        TypeExpr *substituted = type_sub_lookup(sub, type->name);
        if (substituted) {
            return concrete_value_dependency_name(prog, arena, substituted, (TypeSub){0});
        }
    }
    TypeExpr *resolved = resolve_alias_type(prog, type);
    if (resolved && resolved != type) {
        return concrete_value_dependency_name(prog, arena, resolved, sub);
    }
    if (type->kind == Type_Name || type->kind == Type_Generic) {
        return type_mangle(arena, type, sub);
    }
    return (string8){0};
}

static void emit_concrete_struct_def_sorted(memops_arena *arena, string8 *out, Program *prog, Vec_voidptr *defs, ConcreteStructDef *def) {
    if (!def || def->emitted) return;
    if (def->visiting) return;
    def->visiting = true;
    for (i32 i = 0; i < def->decl->fields.length; i++) {
        Field *field = (Field *)def->decl->fields.data[i];
        string8 dep_name = concrete_value_dependency_name(prog, arena, field->type, def->sub);
        if (!dep_name.data || string8_equals(&dep_name, &def->name)) {
            continue;
        }
        ConcreteStructDef *dep = concrete_struct_def_find(defs, dep_name);
        if (dep) {
            emit_concrete_struct_def_sorted(arena, out, prog, defs, dep);
        }
    }
    def->visiting = false;
    def->emitted = true;
    if (def->is_mono) {
        emit_struct_decl_mono(arena, out, def->decl, def->mangle, def->arg);
    } else {
        emit_struct_decl(arena, out, def->decl);
    }
}

static void emit_concrete_struct_defs_sorted(memops_arena *arena, string8 *out, Program *prog) {
    Vec_voidptr defs = collect_concrete_struct_defs(arena, prog);
    for (i32 i = 0; i < defs.length; i++) {
        emit_concrete_struct_def_sorted(arena, out, prog, &defs, (ConcreteStructDef *)defs.data[i]);
    }
}

static void emit_proc_params(memops_arena *arena, string8 *out, ProcDecl *decl, TypeSub sub) {
    if (decl->params.length == 0 && !decl->is_variadic) {
        emit_cstr(arena, out, "void");
    } else {
        for (i32 i = 0; i < decl->params.length; i++) {
            if (i > 0) emit_cstr(arena, out, ", ");
            Param *p = (Param *)decl->params.data[i];
            if (p->line != decl->line) {
                emit_cstr(arena, out, "\n");
                emit_line_directive_path(arena, out, decl->source_path, p->line);
            }
            emit_decl(arena, out, p->type, p->name, sub);
        }
    }
    if (decl->is_variadic) {
        if (decl->params.length > 0) emit_cstr(arena, out, ", ");
        emit_cstr(arena, out, "...");
    }
}

static void emit_proc_decl(memops_arena *arena, string8 *out, ProcDecl *decl) {
    if (decl->is_external) return;
    emit_line_directive_path(arena, out, decl->source_path, decl->line);
    if (decl->is_static) emit_cstr(arena, out, "static ");
    emit_type(arena, out, decl->ret_type, (TypeSub){0});
    emit_cstr(arena, out, " ");
    if (decl->callconv.data) {
        emit_string8(arena, out, decl->callconv);
        emit_cstr(arena, out, " ");
    }
    emit_string8(arena, out, c_ident(arena, reflect_runtime_c_name(arena, decl->name)));
    emit_cstr(arena, out, "(");
    emit_proc_params(arena, out, decl, (TypeSub){0});
    emit_cstr(arena, out, ") {\n");
    for (i32 i = 0; i < decl->body.length; i++) {
        emit_cstr(arena, out, "    ");
        emit_stmt(arena, out, (Stmt *)decl->body.data[i], (TypeSub){0}, (string8){0});
    }
    emit_cstr(arena, out, "}\n\n");
}

static void emit_proc_proto(memops_arena *arena, string8 *out, ProcDecl *decl) {
    if (decl->is_external && !decl->emit_external_proto) return;
    emit_line_directive_path(arena, out, decl->source_path, decl->line);
    if (decl->is_static) emit_cstr(arena, out, "static ");
    emit_type(arena, out, decl->ret_type, (TypeSub){0});
    emit_cstr(arena, out, " ");
    if (decl->callconv.data) {
        emit_string8(arena, out, decl->callconv);
        emit_cstr(arena, out, " ");
    }
    emit_string8(arena, out, c_ident(arena, reflect_runtime_c_name(arena, decl->name)));
    emit_cstr(arena, out, "(");
    emit_proc_params(arena, out, decl, (TypeSub){0});
    emit_cstr(arena, out, ");\n");
}

static void emit_proc_decl_mono(memops_arena *arena, string8 *out, ProcDecl *decl, string8 type_mangled, TypeExpr *arg, GenericInstanceSite *site) {
    if (decl->is_external) return;
    TypeSub sub = proc_sub_for_instance(arena, decl, type_mangled, arg);

    emit_proc_monomorph_comment(arena, out, decl, type_mangled, site);
    emit_line_directive_path(arena, out, decl->source_path, decl->line);
    emit_type(arena, out, decl->ret_type, sub);
    emit_cstr(arena, out, " ");
    if (decl->callconv.data) {
        emit_string8(arena, out, decl->callconv);
        emit_cstr(arena, out, " ");
    }
    emit_mono_proc_name(arena, out, decl->name, type_mangled);
    emit_cstr(arena, out, "(");
    emit_proc_params(arena, out, decl, sub);
    emit_cstr(arena, out, ") {\n");
    for (i32 i = 0; i < decl->body.length; i++) {
        emit_cstr(arena, out, "    ");
        emit_stmt(arena, out, (Stmt *)decl->body.data[i], sub, decl->name);
    }
    emit_cstr(arena, out, "}\n\n");
}

static void emit_proc_proto_mono(memops_arena *arena, string8 *out, ProcDecl *decl, string8 type_mangled, TypeExpr *arg, GenericInstanceSite *site) {
    if (decl->is_external && !decl->emit_external_proto) return;
    TypeSub sub = proc_sub_for_instance(arena, decl, type_mangled, arg);

    emit_proc_monomorph_comment(arena, out, decl, type_mangled, site);
    emit_line_directive_path(arena, out, decl->source_path, decl->line);
    emit_type(arena, out, decl->ret_type, sub);
    emit_cstr(arena, out, " ");
    if (decl->callconv.data) {
        emit_string8(arena, out, decl->callconv);
        emit_cstr(arena, out, " ");
    }
    emit_mono_proc_name(arena, out, decl->name, type_mangled);
    emit_cstr(arena, out, "(");
    emit_proc_params(arena, out, decl, sub);
    emit_cstr(arena, out, ");\n");
}

static const char *reflect_type_kind_name(TypeExpr *type) {
    if (!type) return "I_Reflect_Type_Name";
    switch (type->kind) {
        case Type_Name: return "I_Reflect_Type_Name";
        case Type_Ptr: return "I_Reflect_Type_Ptr";
        case Type_Generic: return "I_Reflect_Type_Generic";
        case Type_Array: return "I_Reflect_Type_Array";
        case Type_Proc: return "I_Reflect_Type_Proc";
    }
    return "I_Reflect_Type_Name";
}

static u64 reflect_pointer_depth(TypeExpr *type) {
    u64 depth = 0;
    while (type && type->kind == Type_Ptr) {
        depth++;
        type = type->elem;
    }
    return depth;
}

static bool reflect_type_has_const(TypeExpr *type) {
    if (!type) return false;
    if (type->is_const) return true;
    if (type->kind == Type_Ptr || type->kind == Type_Array) {
        return reflect_type_has_const(type->elem);
    }
    if (type->kind == Type_Generic) {
        for (i32 i = 0; i < type->args.length; i++) {
            if (reflect_type_has_const((TypeExpr *)type->args.data[i])) return true;
        }
    }
    if (type->kind == Type_Proc) {
        if (reflect_type_has_const(type->ret_type)) return true;
        for (i32 i = 0; i < type->args.length; i++) {
            if (reflect_type_has_const((TypeExpr *)type->args.data[i])) return true;
        }
    }
    return false;
}

static void emit_reflect_array_count(memops_arena *arena, string8 *out, TypeExpr *type) {
    if (!type || type->kind != Type_Array) {
        emit_cstr(arena, out, "0");
        return;
    }
    if (type->array_count.length > 0) {
        emit_string8(arena, out, type->array_count);
        return;
    }
    emit_cstr(arena, out, "0");
}

static void emit_string8_as_c_string(memops_arena *arena, string8 *out, string8 s);

static void emit_reflect_base_type(memops_arena *arena, string8 *out, TypeExpr *type) {
    if (type && (type->kind == Type_Name || type->kind == Type_Generic)) {
        emit_string8_as_c_string(arena, out, type->name);
        return;
    }
    emit_cstr(arena, out, "\"\"");
}

static void emit_reflect_elem_type(memops_arena *arena, string8 *out, TypeExpr *type) {
    if (type && (type->kind == Type_Ptr || type->kind == Type_Array) && type->elem) {
        string8 elem_name = type_mangle(arena, type->elem, (TypeSub){0});
        emit_string8_as_c_string(arena, out, elem_name);
        return;
    }
    emit_cstr(arena, out, "\"\"");
}

static void emit_reflect_generic_arg_type(memops_arena *arena, string8 *out, TypeExpr *type) {
    if (type && type->kind == Type_Generic && type->args.length > 0) {
        TypeExpr *arg = (TypeExpr *)type->args.data[0];
        string8 arg_name = type_mangle(arena, arg, (TypeSub){0});
        emit_string8_as_c_string(arena, out, arg_name);
        return;
    }
    emit_cstr(arena, out, "\"\"");
}

static void emit_string8_as_c_string(memops_arena *arena, string8 *out, string8 s) {
    emit_cstr(arena, out, "\"");
    for (u64 i = 0; i < s.length; i++) {
        u8 c = s.data[i];
        if (c == '\\' || c == '"') {
            string8_append_byte(arena, out, '\\');
            string8_append_byte(arena, out, c);
        } else if (c == '\n') {
            emit_cstr(arena, out, "\\n");
        } else if (c == '\r') {
            emit_cstr(arena, out, "\\r");
        } else if (c == '\t') {
            emit_cstr(arena, out, "\\t");
        } else {
            string8_append_byte(arena, out, c);
        }
    }
    emit_cstr(arena, out, "\"");
}

static void emit_c_string_literal_from_cstr(memops_arena *arena, string8 *out, const char *s) {
    emit_cstr(arena, out, "\"");
    for (const char *p = s; *p; p++) {
        if (*p == '\\' || *p == '"') {
            string8_append_byte(arena, out, '\\');
        }
        string8_append_byte(arena, out, (u8)*p);
    }
    emit_cstr(arena, out, "\"");
}

/* A '#line N "f"' directive renumbers everything after it, so the emitter tracks
   where the last one landed and how many lines have been written since. A new
   directive is only needed when the implied position has drifted from the wanted
   one, which keeps mappings exact while dropping the redundant majority. */
static bool g_emit_all_line_directives = false; // --emit-all-line-directives, for the mapping test
static string8 *g_line_last_out = null;
static const char *g_line_last_path = null;
static i32 g_line_last_value = 0;
static u64 g_line_last_offset = 0;

static void emit_line_directive_reset(void) {
    g_line_last_out = null;
    g_line_last_path = null;
    g_line_last_value = 0;
    g_line_last_offset = 0;
}

static i32 emit_count_newlines_since(string8 *out, u64 from) {
    i32 count = 0;
    for (u64 i = from; i < out->length; i++) {
        if (out->data[i] == '\n') count++;
    }
    return count;
}

static void emit_line_directive_path(memops_arena *arena, string8 *out, const char *path, i32 line) {
    const char *use_path = path ? path : g_source_path;
    if (!g_emit_all_line_directives &&
        g_line_last_out == out &&
        g_line_last_path &&
        cstr_equals(g_line_last_path, use_path) &&
        out->length >= g_line_last_offset &&
        g_line_last_value + emit_count_newlines_since(out, g_line_last_offset) == line) {
        return;
    }
    char line_buf[64];
    snprintf(line_buf, sizeof(line_buf), "#line %d ", line);
    emit_cstr(arena, out, line_buf);
    emit_c_string_literal_from_cstr(arena, out, use_path);
    emit_cstr(arena, out, "\n");
    g_line_last_out = out;
    g_line_last_path = use_path;
    g_line_last_value = line;
    g_line_last_offset = out->length;
}

static void emit_generated_line_directive(memops_arena *arena, string8 *out) {
    emit_line_directive_path(arena, out, "<generated>", 1);
}

static void emit_proc_monomorph_comment(
    memops_arena *arena,
    string8 *out,
    ProcDecl *decl,
    string8 type_mangled,
    GenericInstanceSite *site
) {
    emit_generated_line_directive(arena, out);
    emit_cstr(arena, out, "/* I monomorph: proc ");
    string8 owner = {0};
    string8 member = {0};
    if (split_qualified_name(decl->name, &owner, &member)) {
        emit_string8(arena, out, owner);
        emit_cstr(arena, out, "<");
        emit_proc_type_params(arena, out, decl);
        emit_cstr(arena, out, ">");
        emit_string8(arena, out, member);
    } else {
        emit_c_ident(arena, out, decl->name);
        emit_cstr(arena, out, "<");
        emit_proc_type_params(arena, out, decl);
        emit_cstr(arena, out, ">");
    }
    emit_cstr(arena, out, " -> ");
    emit_mono_proc_name(arena, out, decl->name, type_mangled);
    emit_cstr(arena, out, "; declared at ");
    emit_cstr(arena, out, decl->source_path ? decl->source_path : g_source_path);
    char loc_buf[64];
    snprintf(loc_buf, sizeof(loc_buf), ":%d:%d", decl->line, decl->col);
    emit_cstr(arena, out, loc_buf);
    if (site) {
        emit_cstr(arena, out, "; instantiated at ");
        emit_cstr(arena, out, site->source_path ? site->source_path : g_source_path);
        snprintf(loc_buf, sizeof(loc_buf), ":%d:%d", site->line, site->col);
        emit_cstr(arena, out, loc_buf);
    }
    emit_cstr(arena, out, " */\n");
}

/* Anonymous members are flattened into the owner's field list, because C lets
   offsetof reach their members directly. Bitfields have no address, so their
   offset/size/alignment are reported as zero rather than emitting invalid C. */
/* The name whose reflect table describes this field's own type: the type itself
   for a plain name, the element type through a pointer or array, and the
   monomorphised name for a generic. Empty when there is nothing to link to. */
static string8 reflect_info_target_name(memops_arena *arena, TypeExpr *type) {
    string8 empty = {0};
    if (!type) return empty;
    while (type->kind == Type_Ptr || type->kind == Type_Array) {
        if (!type->elem) return empty;
        type = type->elem;
    }
    if (type->kind == Type_Name || type->kind == Type_Generic) {
        return type_mangle(arena, type, (TypeSub){0});
    }
    return empty;
}

/* Emits `&Name_reflect` when that name has a table, `0` otherwise. Builtins,
   external types and procs have none, so their fields link to null rather than
   to a symbol that was never emitted. */
static void emit_reflect_info_link(memops_arena *arena, string8 *out, Program *prog, TypeExpr *type) {
    string8 target = reflect_info_target_name(arena, type);
    if (prog && target.length > 0) {
        for (i32 i = 0; i < prog->structs.length; i++) {
            StructDecl *decl = (StructDecl *)prog->structs.data[i];
            if (decl->is_external) continue;
            /* A generic's table exists per instantiation, under the mangled name,
               so match that rather than the template name. */
            if (decl->is_generic) continue;
            if (string8_equals(&decl->name, &target)) {
                emit_cstr(arena, out, "&");
                emit_string8(arena, out, target);
                emit_cstr(arena, out, "_reflect");
                return;
            }
        }
        for (i32 i = 0; i < prog->enums.length; i++) {
            EnumDecl *decl = (EnumDecl *)prog->enums.data[i];
            if (decl->is_external) continue;
            if (string8_equals(&decl->name, &target)) {
                emit_cstr(arena, out, "&");
                emit_string8(arena, out, target);
                emit_cstr(arena, out, "_reflect");
                return;
            }
        }
    }
    emit_cstr(arena, out, "0");
}

static void emit_struct_reflection_fields(
    memops_arena *arena,
    string8 *out,
    Program *prog,
    StructDecl *decl,
    string8 concrete_name,
    TypeSub sub
) {
    for (i32 i = 0; i < decl->fields.length; i++) {
        Field *f = (Field *)decl->fields.data[i];
        emit_pre_directives(arena, out, &f->pre_directives);
        if (f->anon) {
            emit_struct_reflection_fields(arena, out, prog, f->anon, concrete_name, sub);
            continue;
        }
        TypeExpr *field_type = substitute_type_sub(arena, f->type, sub);
        string8 type_name = type_mangle(arena, field_type, (TypeSub){0});
        bool is_bitfield = f->bit_width.data != null;
        emit_cstr(arena, out, "    {");
        emit_string8_as_c_string(arena, out, f->name);
        emit_cstr(arena, out, ", ");
        emit_string8_as_c_string(arena, out, type_name);
        emit_cstr(arena, out, ", ");
        emit_string8_as_c_string(arena, out, f->attrs);
        if (is_bitfield) {
            emit_cstr(arena, out, ", 0, 0, 0, ");
        } else {
            emit_cstr(arena, out, ", (u64)offsetof(");
            emit_string8(arena, out, concrete_name);
            emit_cstr(arena, out, ", ");
            emit_c_ident(arena, out, f->name);
            emit_cstr(arena, out, "), (u64)sizeof(((");
            emit_string8(arena, out, concrete_name);
            emit_cstr(arena, out, " *)0)->");
            emit_c_ident(arena, out, f->name);
            emit_cstr(arena, out, "), (u64)_Alignof(");
            emit_compound_literal_type(arena, out, field_type, (TypeSub){0});
            emit_cstr(arena, out, "), ");
        }
        emit_cstr(arena, out, reflect_type_kind_name(field_type));
        emit_cstr(arena, out, ", ");
        emit_reflect_array_count(arena, out, field_type);
        emit_cstr(arena, out, ", ");
        char depth_buf[32];
        snprintf(depth_buf, sizeof(depth_buf), "%llu", (unsigned long long)reflect_pointer_depth(field_type));
        emit_cstr(arena, out, depth_buf);
        emit_cstr(arena, out, ", ");
        emit_reflect_base_type(arena, out, field_type);
        emit_cstr(arena, out, ", ");
        emit_reflect_elem_type(arena, out, field_type);
        emit_cstr(arena, out, ", ");
        emit_reflect_generic_arg_type(arena, out, field_type);
        emit_cstr(arena, out, ", ");
        emit_cstr(arena, out, reflect_type_has_const(field_type) ? "1" : "0");
        emit_cstr(arena, out, ", ");
        emit_reflect_info_link(arena, out, prog, field_type);
        emit_cstr(arena, out, "},\n");
    }
    emit_pre_directives(arena, out, &decl->tail_directives);
}

/* A union reports as its own kind rather than as a struct with a flag, so a
   consumer that only handles structs cannot silently walk overlapping members
   as though they were adjacent; §3 of docs/reflection-issues.md. */
/* Layout verification for `external` records.

   An `external` struct is a claim about a type C owns: these fields, these
   types, this order. Nothing checked that claim. If a header reordered a member,
   or a `long` was the wrong width on this target, I type-checked field access
   happily against a layout that was a lie, and the result was wrong bytes rather
   than a diagnostic. Procs at least have a prototype C can compare against;
   records had nothing.

   The compiler cannot assert `sizeof(X) == 24` because it does not compute C
   layouts -- it defers to C for sizes everywhere, including reflection. So it
   emits a *shadow* record built from what I was told, and asks C to compare the
   two. C computes both with the same rules, so any disagreement about order,
   member type, padding or alignment fails at compile time, at zero runtime cost
   and with no effect on codegen.

   Skipped: bitfields and anonymous members, which `offsetof` cannot address;
   generic records, which have no single C type; and records with no field list,
   which claim nothing to check. Emitted once into the .c rather than the header,
   because one translation unit proving the layout proves it for all thirty. */
static bool struct_layout_checkable_depth(Program *prog, StructDecl *decl, i32 depth) {
    if (!decl || !decl->is_external || decl->is_generic) return false;
    if (decl->no_layout_check) return false;
    if (decl->fields.length == 0) return false;
    if (depth > 16) return false;   /* cyclic or absurdly deep; do not guess */
    for (i32 i = 0; i < decl->fields.length; i++) {
        Field *f = (Field *)decl->fields.data[i];
        if (!f || f->anon || f->bit_width.data) return false;
        if (!f->name.data || f->name.length == 0) return false;
        /* A by-value field of an unnameable record makes this record unnameable
           too: the shadow would mention a type C does not have. Pointers are
           exempt -- a pointer is a pointer whatever it points at. */
        TypeExpr *t = f->type;
        while (t && t->kind == Type_Array) t = t->elem;
        if (!t || t->kind != Type_Name) continue;
        for (i32 j = 0; j < prog->structs.length; j++) {
            StructDecl *other = (StructDecl *)prog->structs.data[j];
            if (!other || !string8_equals(&other->name, &t->name)) continue;
            if (other == decl) break;
            if (other->is_external && !struct_layout_checkable_depth(prog, other, depth + 1)) {
                return false;
            }
            break;
        }
    }
    return true;
}

static bool struct_layout_checkable(Program *prog, StructDecl *decl) {
    return struct_layout_checkable_depth(prog, decl, 0);
}

static void emit_struct_layout_check(memops_arena *arena, string8 *out, Program *prog, StructDecl *decl) {
    if (!struct_layout_checkable(prog, decl)) return;

    /* The reflect runtime's records are spelled `i_reflect_field` and friends in
       C while I keeps the short name, so the check has to name the C one. */
    string8 c_name = c_ident(arena, reflect_runtime_c_name(arena, decl->name));
    string8 shadow = concat_name2(arena, string8_from_cstr(arena, "i_layout"), "_", decl->name);

    emit_cstr(arena, out, "typedef ");
    emit_cstr(arena, out, decl->is_union ? "union {\n" : "struct {\n");
    emit_struct_fields(arena, out, decl, (TypeSub){0}, "    ");
    emit_cstr(arena, out, "} ");
    emit_string8(arena, out, shadow);
    emit_cstr(arena, out, ";\n");

    emit_cstr(arena, out, "static_assert(sizeof(");
    emit_string8(arena, out, c_name);
    emit_cstr(arena, out, ") == sizeof(");
    emit_string8(arena, out, shadow);
    emit_cstr(arena, out, "), \"external layout mismatch: ");
    emit_c_ident(arena, out, decl->name);
    emit_cstr(arena, out, " size\");\n");

    emit_cstr(arena, out, "static_assert(_Alignof(");
    emit_string8(arena, out, c_name);
    emit_cstr(arena, out, ") == _Alignof(");
    emit_string8(arena, out, shadow);
    emit_cstr(arena, out, "), \"external layout mismatch: ");
    emit_c_ident(arena, out, decl->name);
    emit_cstr(arena, out, " alignment\");\n");

    for (i32 i = 0; i < decl->fields.length; i++) {
        Field *f = (Field *)decl->fields.data[i];
        emit_pre_directives(arena, out, &f->pre_directives);
        emit_cstr(arena, out, "static_assert(offsetof(");
        emit_string8(arena, out, c_name);
        emit_cstr(arena, out, ", ");
        emit_c_ident(arena, out, f->name);
        emit_cstr(arena, out, ") == offsetof(");
        emit_string8(arena, out, shadow);
        emit_cstr(arena, out, ", ");
        emit_c_ident(arena, out, f->name);
        emit_cstr(arena, out, "), \"external layout mismatch: ");
        emit_c_ident(arena, out, decl->name);
        emit_cstr(arena, out, ".");
        emit_c_ident(arena, out, f->name);
        emit_cstr(arena, out, " offset\");\n");

        emit_cstr(arena, out, "static_assert(sizeof(((");
        emit_string8(arena, out, c_name);
        emit_cstr(arena, out, " *)0)->");
        emit_c_ident(arena, out, f->name);
        emit_cstr(arena, out, ") == sizeof(((");
        emit_string8(arena, out, shadow);
        emit_cstr(arena, out, " *)0)->");
        emit_c_ident(arena, out, f->name);
        emit_cstr(arena, out, "), \"external layout mismatch: ");
        emit_c_ident(arena, out, decl->name);
        emit_cstr(arena, out, ".");
        emit_c_ident(arena, out, f->name);
        emit_cstr(arena, out, " type\");\n");
    }
    emit_pre_directives(arena, out, &decl->tail_directives);
    emit_cstr(arena, out, "\n");
}

static void emit_external_layout_checks(memops_arena *arena, string8 *out, Program *prog) {
    bool any = false;
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!struct_layout_checkable(prog, decl)) continue;
        if (!any) {
            emit_line_directive_reset();
            emit_cstr(arena, out,
                      "/* Layout checks for external records: each declared layout is compared\n"
                      "   against the one C actually has. Frontend-only, no codegen. */\n");
            any = true;
        }
        emit_struct_layout_check(arena, out, prog, decl);
    }
}

static void emit_struct_reflection(
    memops_arena *arena,
    string8 *out,
    Program *prog,
    StructDecl *decl,
    string8 concrete_name,
    TypeSub sub
) {
    emit_generated_line_directive(arena, out);
    emit_cstr(arena, out, "static const i_reflect_field i_reflect_fields_");
    emit_string8(arena, out, concrete_name);
    emit_cstr(arena, out, "[] = {\n");
    emit_struct_reflection_fields(arena, out, prog, decl, concrete_name, sub);
    emit_cstr(arena, out, "};\n");
    emit_cstr(arena, out, "const i_reflect ");
    emit_string8(arena, out, concrete_name);
    emit_cstr(arena, out, "_reflect = {");
    emit_string8_as_c_string(arena, out, concrete_name);
    emit_cstr(arena, out, ", (u64)sizeof(");
    emit_string8(arena, out, concrete_name);
    emit_cstr(arena, out, "), ");
    emit_cstr(arena, out, "(u64)_Alignof(");
    emit_string8(arena, out, concrete_name);
    emit_cstr(arena, out, "), ");
    emit_cstr(arena, out, decl->is_union ? "I_Reflect_Union" : "I_Reflect_Struct");
    emit_cstr(arena, out, ", ");
    emit_cstr(arena, out, "(u64)(sizeof(i_reflect_fields_");
    emit_string8(arena, out, concrete_name);
    emit_cstr(arena, out, ") / sizeof(i_reflect_fields_");
    emit_string8(arena, out, concrete_name);
    emit_cstr(arena, out, "[0]))");
    emit_cstr(arena, out, ", {.fields = i_reflect_fields_");
    emit_string8(arena, out, concrete_name);
    emit_cstr(arena, out, "}};\n\n");
}

/* Nested `info` links point at other types' tables, and a table can be defined
   later in the file than the one that links to it, so every table is declared up
   front. The single-translation-unit path has no header to carry these. */
static void emit_reflect_table_fwd_decls(memops_arena *arena, Program *prog, string8 *out) {
    emit_generated_line_directive(arena, out);
    for (i32 i = 0; i < prog->enums.length; i++) {
        EnumDecl *decl = (EnumDecl *)prog->enums.data[i];
        if (decl->is_external) continue;
        emit_cstr(arena, out, "extern const i_reflect ");
        emit_c_ident(arena, out, decl->name);
        emit_cstr(arena, out, "_reflect;\n");
    }
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (decl->is_generic || decl->is_external) continue;
        emit_cstr(arena, out, "extern const i_reflect ");
        emit_c_ident(arena, out, decl->name);
        emit_cstr(arena, out, "_reflect;\n");
    }
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic || decl->is_external) continue;
        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        collect_generic_struct_instances(prog, decl, &instances, arena);
        for (i32 j = 0; j < instances.length; j++) {
            emit_cstr(arena, out, "extern const i_reflect ");
            emit_c_ident(arena, out, decl->name);
            emit_cstr(arena, out, "_");
            emit_string8(arena, out, instances.data[j]);
            emit_cstr(arena, out, "_reflect;\n");
        }
    }
    emit_cstr(arena, out, "\n");
}

static void emit_struct_reflection_extern(memops_arena *arena, string8 *out, string8 concrete_name) {
    emit_generated_line_directive(arena, out);
    emit_cstr(arena, out, "extern const i_reflect ");
    emit_string8(arena, out, concrete_name);
    emit_cstr(arena, out, "_reflect;\n");
    emit_cstr(arena, out, "#define ");
    emit_string8(arena, out, concrete_name);
    emit_cstr(arena, out, "_reflected ");
    emit_string8(arena, out, concrete_name);
    emit_cstr(arena, out, "_reflect\n");
}

static void emit_enum_reflection(memops_arena *arena, string8 *out, EnumDecl *decl) {
    if (decl->is_external) return;
    emit_generated_line_directive(arena, out);
    emit_cstr(arena, out, "static const i_reflect_value i_reflect_values_");
    emit_c_ident(arena, out, decl->name);
    emit_cstr(arena, out, "[] = {\n");
    for (i32 i = 0; i < decl->items.length; i++) {
        EnumItem *item = (EnumItem *)decl->items.data[i];
        emit_pre_directives(arena, out, &item->pre_directives);
        emit_cstr(arena, out, "    {");
        emit_string8_as_c_string(arena, out, item->name);
        emit_cstr(arena, out, ", ");
        emit_c_ident(arena, out, decl->name);
        emit_cstr(arena, out, "_");
        emit_c_ident(arena, out, item->name);
        emit_cstr(arena, out, "},\n");
    }
    emit_pre_directives(arena, out, &decl->tail_directives);
    emit_cstr(arena, out, "};\n");
    emit_cstr(arena, out, "const i_reflect ");
    emit_c_ident(arena, out, decl->name);
    emit_cstr(arena, out, "_reflect = {");
    emit_string8_as_c_string(arena, out, decl->name);
    emit_cstr(arena, out, ", (u64)sizeof(");
    emit_c_ident(arena, out, decl->name);
    emit_cstr(arena, out, "), ");
    emit_cstr(arena, out, "(u64)_Alignof(");
    emit_c_ident(arena, out, decl->name);
    emit_cstr(arena, out, "), I_Reflect_Enum, ");
    emit_cstr(arena, out, "(u64)(sizeof(i_reflect_values_");
    emit_c_ident(arena, out, decl->name);
    emit_cstr(arena, out, ") / sizeof(i_reflect_values_");
    emit_c_ident(arena, out, decl->name);
    emit_cstr(arena, out, "[0]))");
    emit_cstr(arena, out, ", {.values = i_reflect_values_");
    emit_c_ident(arena, out, decl->name);
    emit_cstr(arena, out, "}};\n\n");
}

static void emit_enum_reflection_extern(memops_arena *arena, string8 *out, EnumDecl *decl) {
    emit_generated_line_directive(arena, out);
    emit_cstr(arena, out, "extern const i_reflect ");
    emit_c_ident(arena, out, decl->name);
    emit_cstr(arena, out, "_reflect;\n");
    emit_cstr(arena, out, "#define ");
    emit_c_ident(arena, out, decl->name);
    emit_cstr(arena, out, "_reflected ");
    emit_c_ident(arena, out, decl->name);
    emit_cstr(arena, out, "_reflect\n");
}

static void emit_native_monomorph_umbrella_includes(memops_arena *arena, Program *prog, string8 *out) {
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic || !decl->is_external) continue;

        Vec_string8 instances = Vec_string8_reserve(arena, 8);
        collect_generic_struct_instances(prog, decl, &instances, arena);
        if (instances.length == 0) continue;

        emit_cstr(arena, out, "#include \"");
        emit_c_ident(arena, out, decl->name);
        emit_cstr(arena, out, ".h\"\n");
    }
}

static void emit_generated_file_banner(memops_arena *arena, string8 *out, const char *kind) {
    emit_cstr(arena, out, "/* Generated by ilang from ");
    emit_cstr(arena, out, g_source_path ? g_source_path : "<input>");
    emit_cstr(arena, out, " (");
    emit_cstr(arena, out, kind);
    emit_cstr(arena, out, "). Do not edit. */\n");
}

static void emit_program(memops_arena *arena, Program *prog, string8 *out) {
    emit_line_directive_reset();
    emit_generated_file_banner(arena, out, "source");
    emit_cstr(arena, out, "#include <core.h>\n#include <reflect.h>\n#include <stddef.h>\n\n");
    for (i32 i = 0; i < prog->preprocessor_lines.length; i++) {
        emit_string8(arena, out, prog->preprocessor_lines.data[i]);
        emit_cstr(arena, out, "\n");
    }
    if (prog->preprocessor_lines.length > 0) {
        emit_cstr(arena, out, "\n");
    }
    /* ilang checks enum ranges itself, but only where the value is a literal
       or an implicit sequential; a constant expression needs an evaluator it
       does not have. clang has the whole check, and #line puts it on the real
       .i line, so it is borrowed here -- the same trade the external layout
       asserts already make. Placed after the includes so it governs the enums
       ilang emits rather than whatever a third-party header contains. */
    emit_cstr(arena, out, "#if defined(__clang__)\n");
    emit_cstr(arena, out, "#pragma clang diagnostic error \"-Wmicrosoft-enum-value\"\n");
    emit_cstr(arena, out, "#endif\n\n");
    for (i32 i = 0; i < prog->defines.length; i++) {
        string8 macro_lit = prog->defines.data[i];
        string8 macro = macro_lit;
        if (macro.length >= 2 && macro.data[0] == '"' && macro.data[macro.length - 1] == '"') {
            macro = string8_copy_from_slice(arena, macro.data + 1, macro.length - 2);
        }
        emit_cstr(arena, out, "#define ");
        emit_string8(arena, out, macro);
        emit_cstr(arena, out, "\n");
    }
    if (prog->defines.length > 0) {
        emit_cstr(arena, out, "\n");
    }
    for (i32 i = 0; i < prog->c_imports.length; i++) {
        emit_cstr(arena, out, "#include ");
        emit_string8(arena, out, prog->c_imports.data[i]);
        emit_cstr(arena, out, "\n");
    }
    if (prog->c_imports.length > 0) {
        emit_cstr(arena, out, "\n");
    }
    // Forward declarations for all structs (non-generic + monomorphized)
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic && !decl->is_external) {
            emit_struct_fwd_decl(arena, out, decl->name, decl->is_union);
        }
    }
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic || decl->is_external) continue;

        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        collect_generic_struct_instances(prog, decl, &instances, arena);
        for (i32 j = 0; j < instances.length; j++) {
            string8 mono = string8_reserve(arena, decl->name.length + 1 + instances.data[j].length);
            string8_append_bytes(arena, &mono, decl->name.data, decl->name.length);
            string8_append_cstr(arena, &mono, "_");
            string8_append_bytes(arena, &mono, instances.data[j].data, instances.data[j].length);
            emit_struct_fwd_decl(arena, out, mono, decl->is_union);
        }
    }
    emit_cstr(arena, out, "\n");

    for (i32 i = 0; i < prog->aliases.length; i++) {
        emit_alias_decl(arena, out, (AliasDecl *)prog->aliases.data[i]);
    }

    for (i32 i = 0; i < prog->enums.length; i++) {
        emit_enum_decl(arena, out, (EnumDecl *)prog->enums.data[i]);
    }

    emit_native_monomorph_umbrella_includes(arena, prog, out);
    emit_cstr(arena, out, "\n");

    emit_concrete_struct_defs_sorted(arena, out, prog);

    emit_reflect_table_fwd_decls(arena, prog, out);

    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic || decl->is_external) continue;

        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        collect_generic_struct_instances(prog, decl, &instances, arena);

        for (i32 j = 0; j < instances.length; j++) {
            string8 mangle = instances.data[j];
            string8 concrete_name = string8_reserve(arena, decl->name.length + 1 + mangle.length);
            string8_append_bytes(arena, &concrete_name, decl->name.data, decl->name.length);
            string8_append_cstr(arena, &concrete_name, "_");
            string8_append_bytes(arena, &concrete_name, mangle.data, mangle.length);

            TypeSub sub = {0};
            sub = struct_sub_for_instance(arena, decl, mangle);
            emit_struct_reflection(arena, out, prog, decl, concrete_name, sub);
        }
    }

    emit_external_layout_checks(arena, out, prog);

    for (i32 i = 0; i < prog->enums.length; i++) {
        emit_enum_reflection(arena, out, (EnumDecl *)prog->enums.data[i]);
    }

    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic && !decl->is_external) {
            emit_struct_reflection(arena, out, prog, decl, decl->name, (TypeSub){0});
        }
    }

    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *s = (Stmt *)prog->globals.data[i];
        emit_line_directive_path(arena, out, s->source_path, s->line);
        if (s->is_external) {
            /* C owns the definition and its own header declares it; anything
               emitted here would be a second declaration with a linkage of
               ilang's choosing rather than C's. See docs/attributes.md. */
            continue;
        }
        emit_stmt(arena, out, s, (TypeSub){0}, (string8){0});
    }
    emit_cstr(arena, out, "\n");

    // prototypes for non-generic procs
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        if (!decl->is_generic) {
            emit_proc_proto(arena, out, decl);
        }
    }

    // prototypes for monomorphized procs
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        if (!decl->is_generic) continue;

        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        Vec_string8 sub_instances = Vec_string8_reserve(arena, 4);
        Vec_voidptr instance_sites = ptr_array_reserve(arena, 4);
        collect_generic_proc_instances_with_sites(prog, decl, &instances, &sub_instances, &instance_sites, arena);
        for (i32 j = 0; j < instances.length; j++) {
            string8 mangle = instances.data[j];
            TypeExpr *arg = mangle_type_for(arena, sub_instances.data[j]);
            emit_proc_proto_mono(arena, out, decl, mangle, arg, generic_instance_site_find(&instance_sites, decl->name, mangle));
        }
    }

    emit_cstr(arena, out, "\n");

    // non-generic proc definitions
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        if (!decl->is_generic) {
            emit_proc_decl(arena, out, decl);
        }
    }

    // monomorphized proc definitions
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        if (!decl->is_generic) continue;

        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        Vec_string8 sub_instances = Vec_string8_reserve(arena, 4);
        Vec_voidptr instance_sites = ptr_array_reserve(arena, 4);
        collect_generic_proc_instances_with_sites(prog, decl, &instances, &sub_instances, &instance_sites, arena);
        for (i32 j = 0; j < instances.length; j++) {
            string8 mangle = instances.data[j];
            TypeExpr *arg = mangle_type_for(arena, sub_instances.data[j]);
            emit_proc_decl_mono(arena, out, decl, mangle, arg, generic_instance_site_find(&instance_sites, decl->name, mangle));
        }
    }
}

static void program_append_string_unique(memops_arena *arena, Vec_string8 *dst, string8 value);

/* ---------------------------------------------------------------------------
   Module output.

   Instead of one translation unit holding the whole program, emit a .h/.c pair
   per source module plus two shared files:

     i_types.h        every type declaration, in dependency order
     i_monomorphs.h   prototypes and reflect externs for monomorphised generics
     i_monomorphs.c   the one definition of each monomorphised body and table

   Types all live in one header because a monomorphised generic can embed a
   module's type by value (Box<Point> holds a Point), while that module needs
   Box<Point> in turn. Splitting types per module makes that a cycle; keeping
   them together does not, and the compiler already sorts them by dependency.

   Monomorphs have external linkage and are defined exactly once, in the shared
   TU. That is what makes the per-module split safe: no translation unit ever
   emits a second copy, so there is nothing for the linker to collide.
   --------------------------------------------------------------------------- */

static bool decl_in_module(const char *source_path, string8 module) {
    if (!source_path) {
        return false;
    }
    return string8_equals_cstr(&module, source_path);
}

/* "src/gops.i" -> "i_gops". The prefix matters: generated headers sit on the
   include path ahead of the vendored C headers, so an unprefixed module named
   cgltf.i would emit a cgltf.h that shadows the real one. */
static string8 module_stem(memops_arena *arena, string8 path) {
    i64 start = 0;
    for (i64 i = 0; i < (i64)path.length; i++) {
        if (path.data[i] == '/' || path.data[i] == '\\') {
            start = i + 1;
        }
    }
    i64 end = (i64)path.length;
    if (end - start > 2 && path.data[end - 2] == '.' && path.data[end - 1] == 'i') {
        end -= 2;
    }
    string8 stem = string8_reserve(arena, (u64)(end - start) + 2);
    string8_append_cstr(arena, &stem, "i_");
    string8_append_bytes(arena, &stem, path.data + start, (u64)(end - start));
    return stem;
}

/* Unique module paths in dependency order. Imports are appended before the
   file that imports them, so first appearance is already topological. */
static void collect_module_paths(memops_arena *arena, Program *prog, Vec_string8 *out) {
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *d = (StructDecl *)prog->structs.data[i];
        if (d->source_path) program_append_string_unique(arena, out, string8_from_cstr(arena, d->source_path));
    }
    for (i32 i = 0; i < prog->enums.length; i++) {
        EnumDecl *d = (EnumDecl *)prog->enums.data[i];
        if (d->source_path) program_append_string_unique(arena, out, string8_from_cstr(arena, d->source_path));
    }
    for (i32 i = 0; i < prog->aliases.length; i++) {
        AliasDecl *d = (AliasDecl *)prog->aliases.data[i];
        if (d->source_path) program_append_string_unique(arena, out, string8_from_cstr(arena, d->source_path));
    }
    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *d = (Stmt *)prog->globals.data[i];
        if (d->source_path) program_append_string_unique(arena, out, string8_from_cstr(arena, d->source_path));
    }
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *d = (ProcDecl *)prog->procs.data[i];
        if (d->source_path) program_append_string_unique(arena, out, string8_from_cstr(arena, d->source_path));
    }
}

static void emit_common_preamble(memops_arena *arena, Program *prog, string8 *out) {
    for (i32 i = 0; i < prog->preprocessor_lines.length; i++) {
        emit_string8(arena, out, prog->preprocessor_lines.data[i]);
        emit_cstr(arena, out, "\n");
    }
    if (prog->preprocessor_lines.length > 0) emit_cstr(arena, out, "\n");
    for (i32 i = 0; i < prog->defines.length; i++) {
        string8 macro = prog->defines.data[i];
        if (macro.length >= 2 && macro.data[0] == '"' && macro.data[macro.length - 1] == '"') {
            macro = string8_copy_from_slice(arena, macro.data + 1, macro.length - 2);
        }
        emit_cstr(arena, out, "#define ");
        emit_string8(arena, out, macro);
        emit_cstr(arena, out, "\n");
    }
    if (prog->defines.length > 0) emit_cstr(arena, out, "\n");
    for (i32 i = 0; i < prog->c_imports.length; i++) {
        emit_cstr(arena, out, "#include ");
        emit_string8(arena, out, prog->c_imports.data[i]);
        emit_cstr(arena, out, "\n");
    }
    if (prog->c_imports.length > 0) emit_cstr(arena, out, "\n");
}

static void emit_monomorph_struct_fwd_decls(memops_arena *arena, Program *prog, string8 *out) {
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic || decl->is_external) continue;
        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        collect_generic_struct_instances(prog, decl, &instances, arena);
        for (i32 j = 0; j < instances.length; j++) {
            string8 mono = string8_reserve(arena, decl->name.length + 1 + instances.data[j].length);
            string8_append_bytes(arena, &mono, decl->name.data, decl->name.length);
            string8_append_cstr(arena, &mono, "_");
            string8_append_bytes(arena, &mono, instances.data[j].data, instances.data[j].length);
            emit_struct_fwd_decl(arena, out, mono, decl->is_union);
        }
    }
}

/* i_types.h -- every type in the program, module and monomorphised alike. */
static void emit_types_header(memops_arena *arena, Program *prog, string8 *out) {
    emit_line_directive_reset();
    emit_generated_file_banner(arena, out, "types header");
    emit_cstr(arena, out, "#pragma once\n#include <core.h>\n#include <reflect.h>\n#include <stddef.h>\n\n");
    emit_common_preamble(arena, prog, out);

    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic && !decl->is_external) emit_struct_fwd_decl(arena, out, decl->name, decl->is_union);
    }
    emit_monomorph_struct_fwd_decls(arena, prog, out);
    emit_cstr(arena, out, "\n");

    for (i32 i = 0; i < prog->aliases.length; i++) {
        emit_alias_decl(arena, out, (AliasDecl *)prog->aliases.data[i]);
    }
    for (i32 i = 0; i < prog->enums.length; i++) {
        emit_enum_decl(arena, out, (EnumDecl *)prog->enums.data[i]);
    }
    emit_native_monomorph_umbrella_includes(arena, prog, out);
    emit_cstr(arena, out, "\n");

    /* This already covers monomorphised structs as well, ordered by
       dependency, which is why types can share one header without a cycle. */
    emit_concrete_struct_defs_sorted(arena, out, prog);
}

/* <module>.h -- this module's public surface only.

   The includes follow dependency order rather than only the module's declared
   imports. Modules are listed with every import ahead of the file that imports
   it, so including all earlier headers is acyclic and reproduces exactly what a
   single translation unit used to provide. Restricting this to declared imports
   is the stricter and better rule, but it surfaces every implicit cross-module
   dependency at once, and real code has plenty -- see docs/modules.md. */
static void emit_module_header(memops_arena *arena, Program *prog, Vec_string8 *modules, i32 index, string8 *out) {
    string8 module = modules->data[index];
    emit_line_directive_reset();
    emit_generated_file_banner(arena, out, "module header");
    emit_cstr(arena, out, "#pragma once\n");
    emit_cstr(arena, out, "#include \"i_types.h\"\n");
    emit_cstr(arena, out, "#include \"i_monomorphs.h\"\n");

    for (i32 i = 0; i < index; i++) {
        string8 stem = module_stem(arena, modules->data[i]);
        emit_cstr(arena, out, "#include \"");
        emit_string8(arena, out, stem);
        emit_cstr(arena, out, ".h\"\n");
    }
    emit_cstr(arena, out, "\n");

    for (i32 i = 0; i < prog->enums.length; i++) {
        EnumDecl *d = (EnumDecl *)prog->enums.data[i];
        if (decl_in_module(d->source_path, module)) emit_enum_reflection_extern(arena, out, d);
    }
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *d = (StructDecl *)prog->structs.data[i];
        if (d->is_generic || d->is_external) continue;
        if (decl_in_module(d->source_path, module)) emit_struct_reflection_extern(arena, out, d->name);
    }
    emit_cstr(arena, out, "\n");

    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *g = (Stmt *)prog->globals.data[i];
        if (g->is_static || !decl_in_module(g->source_path, module)) continue;
        emit_line_directive_path(arena, out, g->source_path, g->line);
        emit_cstr(arena, out, "extern ");
        emit_decl(arena, out, g->type, g->name, (TypeSub){0});
        emit_cstr(arena, out, ";\n");
    }
    emit_cstr(arena, out, "\n");

    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *d = (ProcDecl *)prog->procs.data[i];
        if (d->is_static || d->is_generic) continue;
        if (decl_in_module(d->source_path, module)) emit_proc_proto(arena, out, d);
    }
}

/* <module>.c -- this module's definitions only. */
static void emit_module_source(memops_arena *arena, Program *prog, string8 module, string8 *out) {
    emit_line_directive_reset();
    emit_generated_file_banner(arena, out, "module source");
    emit_cstr(arena, out, "#include \"i_all.h\"\n\n");

    for (i32 i = 0; i < prog->enums.length; i++) {
        EnumDecl *d = (EnumDecl *)prog->enums.data[i];
        if (decl_in_module(d->source_path, module)) emit_enum_reflection(arena, out, d);
    }
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *d = (StructDecl *)prog->structs.data[i];
        if (!decl_in_module(d->source_path, module)) continue;
        if (struct_layout_checkable(prog, d)) {
            emit_struct_layout_check(arena, out, prog, d);
            continue;
        }
        if (d->is_generic || d->is_external) continue;
        emit_struct_reflection(arena, out, prog, d, d->name, (TypeSub){0});
    }
    emit_cstr(arena, out, "\n");

    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *g = (Stmt *)prog->globals.data[i];
        if (!decl_in_module(g->source_path, module)) continue;
        emit_line_directive_path(arena, out, g->source_path, g->line);
        if (g->is_external) {
            /* C owns the definition and its own header declares it; anything
               emitted here would be a second declaration with a linkage of
               ilang's choosing rather than C's. See docs/attributes.md. */
            continue;
        }
        emit_stmt(arena, out, g, (TypeSub){0}, (string8){0});
    }
    emit_cstr(arena, out, "\n");

    /* Forward prototypes for this module's static procs. The public ones are
       already declared in the header, but a static proc called before its
       definition has nowhere else to be declared -- in the single-file build
       every prototype preceded every body, which hid the need for these. */
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *d = (ProcDecl *)prog->procs.data[i];
        if (d->is_generic || !d->is_static) continue;
        if (decl_in_module(d->source_path, module)) emit_proc_proto(arena, out, d);
    }
    emit_cstr(arena, out, "\n");

    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *d = (ProcDecl *)prog->procs.data[i];
        if (d->is_generic) continue;
        if (decl_in_module(d->source_path, module)) emit_proc_decl(arena, out, d);
    }
}

static void emit_monomorph_struct_reflection(memops_arena *arena, Program *prog, string8 *out) {
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic || decl->is_external) continue;
        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        collect_generic_struct_instances(prog, decl, &instances, arena);
        for (i32 j = 0; j < instances.length; j++) {
            string8 mangle = instances.data[j];
            string8 concrete_name = string8_reserve(arena, decl->name.length + 1 + mangle.length);
            string8_append_bytes(arena, &concrete_name, decl->name.data, decl->name.length);
            string8_append_cstr(arena, &concrete_name, "_");
            string8_append_bytes(arena, &concrete_name, mangle.data, mangle.length);

            TypeSub sub = {0};
            sub = struct_sub_for_instance(arena, decl, mangle);
            emit_struct_reflection(arena, out, prog, decl, concrete_name, sub);
        }
    }
}

/* i_all.h -- every module header in dependency order.

   Included by module sources, never by module headers, so it cannot form a
   cycle. It exists because real code has backward references between modules:
   a low-level module calling a diagnostic helper declared in a high-level one.
   njinn's memops_pool reaches for gin_fatal, declared in gin.i, which no
   acyclic per-module include can express. A stricter system would reject that
   and make the author restructure; this reproduces what the single translation
   unit provided, so the split stays a build change rather than a source one. */
static void emit_all_header(memops_arena *arena, Vec_string8 *modules, string8 *out) {
    emit_line_directive_reset();
    emit_generated_file_banner(arena, out, "all-modules header");
    emit_cstr(arena, out, "#pragma once\n");
    for (i32 i = 0; i < modules->length; i++) {
        string8 stem = module_stem(arena, modules->data[i]);
        emit_cstr(arena, out, "#include \"");
        emit_string8(arena, out, stem);
        emit_cstr(arena, out, ".h\"\n");
    }
}

/* i_monomorphs.h -- prototypes and reflect externs for every instantiation. */
static void emit_monomorph_header(memops_arena *arena, Program *prog, string8 *out) {
    emit_line_directive_reset();
    emit_generated_file_banner(arena, out, "monomorph header");
    emit_cstr(arena, out, "#pragma once\n#include \"i_types.h\"\n\n");

    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic || decl->is_external) continue;
        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        collect_generic_struct_instances(prog, decl, &instances, arena);
        for (i32 j = 0; j < instances.length; j++) {
            string8 concrete = string8_reserve(arena, decl->name.length + 1 + instances.data[j].length);
            string8_append_bytes(arena, &concrete, decl->name.data, decl->name.length);
            string8_append_cstr(arena, &concrete, "_");
            string8_append_bytes(arena, &concrete, instances.data[j].data, instances.data[j].length);
            emit_struct_reflection_extern(arena, out, concrete);
        }
    }
    emit_cstr(arena, out, "\n");

    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        if (!decl->is_generic) continue;
        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        Vec_string8 sub_instances = Vec_string8_reserve(arena, 4);
        Vec_voidptr instance_sites = ptr_array_reserve(arena, 4);
        collect_generic_proc_instances_with_sites(prog, decl, &instances, &sub_instances, &instance_sites, arena);
        for (i32 j = 0; j < instances.length; j++) {
            TypeExpr *arg = mangle_type_for(arena, sub_instances.data[j]);
            emit_proc_proto_mono(arena, out, decl, instances.data[j], arg,
                generic_instance_site_find(&instance_sites, decl->name, instances.data[j]));
        }
    }
}

/* i_monomorphs.c -- the single definition of each instantiation. */
static void emit_monomorph_source(memops_arena *arena, Program *prog, Vec_string8 *modules, string8 *out) {
    (void)modules;
    emit_line_directive_reset();
    emit_generated_file_banner(arena, out, "monomorph source");
    /* Instantiated bodies call ordinary module procs -- Array<T>reserve reaches
       for memops_arena_push_zero -- so this unit needs every declaration. */
    emit_cstr(arena, out, "#include \"i_all.h\"\n\n");

    emit_monomorph_struct_reflection(arena, prog, out);

    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        if (!decl->is_generic) continue;
        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        Vec_string8 sub_instances = Vec_string8_reserve(arena, 4);
        Vec_voidptr instance_sites = ptr_array_reserve(arena, 4);
        collect_generic_proc_instances_with_sites(prog, decl, &instances, &sub_instances, &instance_sites, arena);
        for (i32 j = 0; j < instances.length; j++) {
            TypeExpr *arg = mangle_type_for(arena, sub_instances.data[j]);
            emit_proc_decl_mono(arena, out, decl, instances.data[j], arg,
                generic_instance_site_find(&instance_sites, decl->name, instances.data[j]));
        }
    }
}

static void emit_header_program(memops_arena *arena, Program *prog, string8 *out) {
    emit_line_directive_reset();
    emit_generated_file_banner(arena, out, "header");
    emit_cstr(arena, out, "#pragma once\n#include <core.h>\n#include <reflect.h>\n#include <stddef.h>\n\n");
    for (i32 i = 0; i < prog->preprocessor_lines.length; i++) {
        emit_string8(arena, out, prog->preprocessor_lines.data[i]);
        emit_cstr(arena, out, "\n");
    }
    if (prog->preprocessor_lines.length > 0) emit_cstr(arena, out, "\n");
    for (i32 i = 0; i < prog->defines.length; i++) {
        string8 macro_lit = prog->defines.data[i];
        string8 macro = macro_lit;
        if (macro.length >= 2 && macro.data[0] == '"' && macro.data[macro.length - 1] == '"') {
            macro = string8_copy_from_slice(arena, macro.data + 1, macro.length - 2);
        }
        emit_cstr(arena, out, "#define ");
        emit_string8(arena, out, macro);
        emit_cstr(arena, out, "\n");
    }
    if (prog->defines.length > 0) emit_cstr(arena, out, "\n");
    for (i32 i = 0; i < prog->c_imports.length; i++) {
        emit_cstr(arena, out, "#include ");
        emit_string8(arena, out, prog->c_imports.data[i]);
        emit_cstr(arena, out, "\n");
    }
    if (prog->c_imports.length > 0) emit_cstr(arena, out, "\n");
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic && !decl->is_external) emit_struct_fwd_decl(arena, out, decl->name, decl->is_union);
    }
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic || decl->is_external) continue;
        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        collect_generic_struct_instances(prog, decl, &instances, arena);
        for (i32 j = 0; j < instances.length; j++) {
            string8 mono = string8_reserve(arena, decl->name.length + 1 + instances.data[j].length);
            string8_append_bytes(arena, &mono, decl->name.data, decl->name.length);
            string8_append_cstr(arena, &mono, "_");
            string8_append_bytes(arena, &mono, instances.data[j].data, instances.data[j].length);
            emit_struct_fwd_decl(arena, out, mono, decl->is_union);
        }
    }
    emit_cstr(arena, out, "\n");

    for (i32 i = 0; i < prog->aliases.length; i++) {
        emit_alias_decl(arena, out, (AliasDecl *)prog->aliases.data[i]);
    }

    for (i32 i = 0; i < prog->enums.length; i++) {
        emit_enum_decl(arena, out, (EnumDecl *)prog->enums.data[i]);
    }

    emit_native_monomorph_umbrella_includes(arena, prog, out);
    emit_cstr(arena, out, "\n");

    emit_concrete_struct_defs_sorted(arena, out, prog);

    for (i32 i = 0; i < prog->enums.length; i++) {
        emit_enum_reflection_extern(arena, out, (EnumDecl *)prog->enums.data[i]);
    }
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic && !decl->is_external) emit_struct_reflection_extern(arena, out, decl->name);
    }
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic || decl->is_external) continue;
        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        collect_generic_struct_instances(prog, decl, &instances, arena);
        for (i32 j = 0; j < instances.length; j++) {
            string8 concrete_name = string8_reserve(arena, decl->name.length + 1 + instances.data[j].length);
            string8_append_bytes(arena, &concrete_name, decl->name.data, decl->name.length);
            string8_append_cstr(arena, &concrete_name, "_");
            string8_append_bytes(arena, &concrete_name, instances.data[j].data, instances.data[j].length);
            emit_struct_reflection_extern(arena, out, concrete_name);
        }
    }
    emit_cstr(arena, out, "\n");

    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *s = (Stmt *)prog->globals.data[i];
        if (s->is_static) continue; // internal linkage: not part of the public surface
        emit_line_directive_path(arena, out, s->source_path, s->line);
        emit_cstr(arena, out, "extern ");
        emit_decl(arena, out, s->type, s->name, (TypeSub){0});
        emit_cstr(arena, out, ";\n");
    }
    emit_cstr(arena, out, "\n");

    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        if (decl->is_static) continue; // internal linkage: not part of the public surface
        if (!decl->is_generic) emit_proc_proto(arena, out, decl);
    }
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        if (!decl->is_generic) continue;
        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        Vec_string8 sub_instances = Vec_string8_reserve(arena, 4);
        Vec_voidptr instance_sites = ptr_array_reserve(arena, 4);
        collect_generic_proc_instances_with_sites(prog, decl, &instances, &sub_instances, &instance_sites, arena);
        for (i32 j = 0; j < instances.length; j++) {
            TypeExpr *arg = mangle_type_for(arena, sub_instances.data[j]);
            emit_proc_proto_mono(arena, out, decl, instances.data[j], arg, generic_instance_site_find(&instance_sites, decl->name, instances.data[j]));
        }
    }
}

static bool preprocessor_line_is_c_directive(u8 *line, u64 length) {
    u64 i = 0;
    while (i < length && (line[i] == ' ' || line[i] == '\t')) i++;
    if (i >= length || line[i] != '#') return false;
    i++;
    while (i < length && (line[i] == ' ' || line[i] == '\t')) i++;

    static const char *directives[] = {
        "define", "include", "if", "ifdef", "ifndef", "elif", "else", "endif",
        "undef", "pragma", "error", "warning", "line"
    };
    for (i32 d = 0; d < (i32)(sizeof(directives) / sizeof(directives[0])); d++) {
        const char *directive = directives[d];
        u64 directive_len = (u64)strlen(directive);
        if (i + directive_len > length) continue;
        if (strncmp((const char *)(line + i), directive, directive_len) != 0) continue;
        u64 end = i + directive_len;
        if (end == length || line[end] == ' ' || line[end] == '\t' || line[end] == '\r') {
            return true;
        }
    }
    return false;
}



static void preprocessor_error(i32 line, const char *message) {
    if (g_diag_json) {
        diag_json_error(g_source_path, line, 1, "preprocessor", message);
    } else {
        printf("%s:%d:1: preprocessor error: %s\n", g_source_path, line, message);
        diag_print_file_context(g_source_path, line, 1);
    }
    diag_record_error();
}




static void program_init_lists(memops_arena *arena, Program *prog) {
    memset(prog, 0, sizeof(*prog));
    prog->preprocessor_lines = Vec_string8_reserve(arena, 8);
    prog->defines = Vec_string8_reserve(arena, 8);
    prog->imports = Vec_string8_reserve(arena, 8);
    prog->c_imports = Vec_string8_reserve(arena, 8);
    prog->i_imports = Vec_string8_reserve(arena, 8);
    prog->i_import_lines = Vec_i32_reserve(arena, 8);
    prog->i_import_cols = Vec_i32_reserve(arena, 8);
    prog->structs = ptr_array_reserve(arena, 8);
    prog->enums = ptr_array_reserve(arena, 8);
    prog->aliases = ptr_array_reserve(arena, 8);
    prog->procs = ptr_array_reserve(arena, 8);
    prog->globals = ptr_array_reserve(arena, 8);
    prog->pending_array_counts = ptr_array_reserve(arena, 8);
    prog->module_edge_from = Vec_string8_reserve(arena, 8);
    prog->module_edge_to = Vec_string8_reserve(arena, 8);
}

static void program_append_string_unique(memops_arena *arena, Vec_string8 *dst, string8 value) {
    if (!array_string8_contains(dst, value)) {
        Vec_string8_append(arena, dst, value);
    }
}

static void program_append_program(memops_arena *arena, Program *dst, Program *src) {
    for (i32 i = 0; i < src->preprocessor_lines.length; i++) {
        program_append_string_unique(arena, &dst->preprocessor_lines, src->preprocessor_lines.data[i]);
    }
    for (i32 i = 0; i < src->defines.length; i++) {
        program_append_string_unique(arena, &dst->defines, src->defines.data[i]);
    }
    for (i32 i = 0; i < src->c_imports.length; i++) {
        program_append_string_unique(arena, &dst->c_imports, src->c_imports.data[i]);
    }
    for (i32 i = 0; i < src->module_edge_from.length; i++) {
        Vec_string8_append(arena, &dst->module_edge_from, src->module_edge_from.data[i]);
        Vec_string8_append(arena, &dst->module_edge_to, src->module_edge_to.data[i]);
    }
    for (i32 i = 0; i < src->structs.length; i++) {
        ptr_array_append(arena, &dst->structs, src->structs.data[i]);
    }
    for (i32 i = 0; i < src->enums.length; i++) {
        ptr_array_append(arena, &dst->enums, src->enums.data[i]);
    }
    for (i32 i = 0; i < src->aliases.length; i++) {
        ptr_array_append(arena, &dst->aliases, src->aliases.data[i]);
    }
    for (i32 i = 0; i < src->procs.length; i++) {
        ptr_array_append(arena, &dst->procs, src->procs.data[i]);
    }
    for (i32 i = 0; i < src->globals.length; i++) {
        ptr_array_append(arena, &dst->globals, src->globals.data[i]);
    }
    for (i32 i = 0; i < src->pending_array_counts.length; i++) {
        ptr_array_append(arena, &dst->pending_array_counts, src->pending_array_counts.data[i]);
    }
}

static Program parse_i_file(memops_arena *arena, const char *path) {
    double profile_step = profile_now_ms();
    string8 input = read_i_source_for_path(arena, path);
    if (g_profile) g_profile_import_read_ms += profile_now_ms() - profile_step;
    if (!input.data) {
        if (g_diag_json) {
            char message[1024];
            snprintf(message, sizeof(message), "failed to read import %s", path);
            diag_json_error(g_source_path, 0, 0, "semantic", message);
            diag_exit_with_errors();
        }
        printf("%s:0:0: semantic error: failed to read import %s\n", g_source_path, path);
        diag_note_import_chain();
        diag_exit_with_errors();
    }

    const char *prev_source_path = g_source_path;
    g_source_path = path;

    Vec_Token tokens = {0};
    Vec_string8 directives = {0};
    profile_step = profile_now_ms();
    lex_tokens(arena, input, &tokens, &directives);
    if (g_profile) g_profile_import_lex_ms += profile_now_ms() - profile_step;

    Parser parser = {0};
    parser.arena = arena;
    parser.source = input;
    parser.tokens = tokens;
    parser.index = 0;

    profile_step = profile_now_ms();
    Program prog = parse_program(&parser);
    prog.preprocessor_lines = directives;
    if (g_profile) {
        g_profile_import_parse_ms += profile_now_ms() - profile_step;
        g_profile_import_parse_count += 1;
    }

    g_source_path = prev_source_path;
    return prog;
}

static const char *import_chain_from_stack(memops_arena *arena, Vec_string8 *stack) {
    string8 out = string8_reserve(arena, 256);
    for (i32 i = 0; i < stack->length; i++) {
        if (i > 0) {
            string8_append_cstr(arena, &out, " -> ");
        }
        string8_append_bytes(arena, &out, stack->data[i].data, stack->data[i].length);
    }
    string8_append_byte(arena, &out, 0);
    return (const char *)out.data;
}

static void stmt_set_import_chain_if_empty(Stmt *s, const char *chain) {
    if (!s) return;
    if (!s->import_chain) {
        s->import_chain = chain;
    }
    if (s->for_init) stmt_set_import_chain_if_empty(s->for_init, chain);
    if (s->for_step) stmt_set_import_chain_if_empty(s->for_step, chain);
    if (s->if_else_if) stmt_set_import_chain_if_empty(s->if_else_if, chain);
    for (i32 i = 0; i < s->for_body.length; i++) {
        stmt_set_import_chain_if_empty((Stmt *)s->for_body.data[i], chain);
    }
    for (i32 i = 0; i < s->while_body.length; i++) {
        stmt_set_import_chain_if_empty((Stmt *)s->while_body.data[i], chain);
    }
    for (i32 i = 0; i < s->if_then_body.length; i++) {
        stmt_set_import_chain_if_empty((Stmt *)s->if_then_body.data[i], chain);
    }
    for (i32 i = 0; i < s->if_else_body.length; i++) {
        stmt_set_import_chain_if_empty((Stmt *)s->if_else_body.data[i], chain);
    }
    for (i32 i = 0; i < s->switch_cases.length; i++) {
        SwitchCase *sc = (SwitchCase *)s->switch_cases.data[i];
        for (i32 j = 0; j < sc->body.length; j++) {
            stmt_set_import_chain_if_empty((Stmt *)sc->body.data[j], chain);
        }
    }
    for (i32 i = 0; i < s->switch_default_body.length; i++) {
        stmt_set_import_chain_if_empty((Stmt *)s->switch_default_body.data[i], chain);
    }
}

static void program_set_import_chain_if_empty(Program *prog, const char *chain) {
    for (i32 i = 0; i < prog->aliases.length; i++) {
        AliasDecl *decl = (AliasDecl *)prog->aliases.data[i];
        if (!decl->import_chain) decl->import_chain = chain;
    }
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->import_chain) decl->import_chain = chain;
    }
    for (i32 i = 0; i < prog->enums.length; i++) {
        EnumDecl *decl = (EnumDecl *)prog->enums.data[i];
        if (!decl->import_chain) decl->import_chain = chain;
    }
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        if (!decl->import_chain) decl->import_chain = chain;
        for (i32 j = 0; j < decl->body.length; j++) {
            stmt_set_import_chain_if_empty((Stmt *)decl->body.data[j], chain);
        }
    }
    for (i32 i = 0; i < prog->globals.length; i++) {
        stmt_set_import_chain_if_empty((Stmt *)prog->globals.data[i], chain);
    }
}

static void print_import_cycle(Vec_string8 *stack, i32 cycle_start, string8 path, const char *source_path, const char *import_chain, i32 line, i32 col) {
    const char *diag_path = source_path ? source_path : g_source_path;
    if (g_diag_json) {
        char message[2048];
        i32 written = snprintf(message, sizeof(message), "import cycle: ");
        for (i32 i = cycle_start; i < stack->length && written > 0 && written < (i32)sizeof(message); i++) {
            written += snprintf(
                message + written,
                sizeof(message) - (u64)written,
                "%.*s -> ",
                (int)stack->data[i].length,
                stack->data[i].data
            );
        }
        if (written > 0 && written < (i32)sizeof(message)) {
            snprintf(message + written, sizeof(message) - (u64)written, "%.*s", (int)path.length, path.data);
        }
        bool has_note = false;
        diag_json_open(diag_path, line, col, "semantic", message);
        if (import_chain && import_chain[0]) {
            char note[2048];
            snprintf(note, sizeof(note), "imported through: %s", import_chain);
            diag_json_note_cstr(&has_note, diag_path, 0, 0, note);
        }
        diag_json_close();
        return;
    }
    printf("%s:%d:%d: semantic error: import cycle: ", diag_path, line, col);
    for (i32 i = cycle_start; i < stack->length; i++) {
        printf("%.*s -> ", (int)stack->data[i].length, stack->data[i].data);
    }
    printf("%.*s\n", (int)path.length, path.data);
}

static Program expand_i_imports(memops_arena *arena, Program *prog, Vec_string8 *visited, Vec_string8 *stack) {
    Program expanded;
    program_init_lists(arena, &expanded);

    for (i32 i = 0; i < prog->i_imports.length; i++) {
        const char *path = resolve_import_path(arena, prog->i_imports.data[i]);
        i32 import_line = i < prog->i_import_lines.length ? prog->i_import_lines.data[i] : 0;
        i32 import_col = i < prog->i_import_cols.length ? prog->i_import_cols.data[i] : 0;
        string8 path_s = string8_from_cstr(arena, path);
        {
            const char *importer = stack->length > 0
                ? (const char *)stack->data[stack->length - 1].data
                : g_source_path;
            Vec_string8_append(arena, &expanded.module_edge_from,
                string8_from_cstr(arena, importer ? importer : ""));
            Vec_string8_append(arena, &expanded.module_edge_to, path_s);
        }
        const char *import_chain = import_chain_from_stack(arena, stack);
        i32 cycle_start = array_string8_index(stack, path_s);
        if (cycle_start >= 0) {
            const char *source_path = stack->length > 0 ? (const char *)stack->data[stack->length - 1].data : g_source_path;
            print_import_cycle(stack, cycle_start, path_s, source_path, import_chain, import_line, import_col);
            diag_exit_with_errors();
        }
        if (array_string8_contains(visited, path_s)) {
            continue;
        }
        Vec_string8_append(arena, visited, path_s);
        Vec_string8_append(arena, stack, path_s);
        import_chain = import_chain_from_stack(arena, stack);

        double profile_probe_start = profile_now_ms();
        string8 input_check = read_i_source_for_path(arena, path);
        if (g_profile) g_profile_import_probe_ms += profile_now_ms() - profile_probe_start;
        if (!input_check.data) {
            if (g_diag_json) {
                char message[1024];
                snprintf(message, sizeof(message), "failed to read import %s", path);
                bool has_note = false;
                diag_json_open(g_source_path, import_line, import_col, "semantic", message);
                diag_json_note_import_chain(&has_note);
                if (import_chain && import_chain[0]) {
                    char note[2048];
                    snprintf(note, sizeof(note), "imported through: %s", import_chain);
                    diag_json_note_cstr(&has_note, g_source_path, 0, 0, note);
                }
                diag_json_close();
                diag_exit_with_errors();
            }
            printf("%s:%d:%d: semantic error: failed to read import %s\n", g_source_path, import_line, import_col, path);
            diag_print_file_context(g_source_path, import_line, import_col);
            if (import_chain && import_chain[0]) {
                printf("%s:0:0: note: imported through: %s\n", g_source_path, import_chain);
            }
            diag_exit_with_errors();
        }

        const char *prev_source_path = g_source_path;
        const char *prev_diag_import_chain = g_diag_import_chain;
        g_source_path = path;
        g_diag_import_chain = import_chain;
        Program imported = parse_i_file(arena, path);
        Program imported_expanded = expand_i_imports(arena, &imported, visited, stack);
        program_set_import_chain_if_empty(&imported_expanded, import_chain);
        g_source_path = prev_source_path;
        g_diag_import_chain = prev_diag_import_chain;
        stack->length -= 1;

        program_append_program(arena, &expanded, &imported_expanded);
    }

    program_append_program(arena, &expanded, prog);
    expanded.imports = Vec_string8_reserve(arena, 1);
    expanded.i_imports = Vec_string8_reserve(arena, 1);
    return expanded;
}

static bool write_string8_to_file(const char *path, string8 data) {
    char dir[4096];
    size_t len = strlen(path);
    if (len >= sizeof(dir)) return false;
    memcpy(dir, path, len + 1);
    for (size_t i = 0; i < len; i++) {
        if (dir[i] != '/' && dir[i] != '\\') continue;
        if (i == 0) continue;
#if defined(_WIN32)
        if (i == 2 && dir[1] == ':') continue;
#endif
        char saved = dir[i];
        dir[i] = 0;
        if (dir[0]) {
#if defined(_WIN32)
            if (!CreateDirectoryA(dir, NULL) && GetLastError() != ERROR_ALREADY_EXISTS) {
                dir[i] = saved;
                return false;
            }
#else
            if (mkdir(dir, 0777) != 0 && errno != EEXIST) {
                dir[i] = saved;
                return false;
            }
#endif
        }
        dir[i] = saved;
    }

    FILE *existing = i_fopen(path, "rb");
    if (existing) {
        bool same = false;
        if (fseek(existing, 0, SEEK_END) == 0) {
            long size = ftell(existing);
            if (size >= 0 && (u64)size == data.length && fseek(existing, 0, SEEK_SET) == 0) {
                same = true;
                u8 buf[8192];
                u64 offset = 0;
                while (offset < data.length) {
                    u64 remaining = data.length - offset;
                    size_t want = remaining < sizeof(buf) ? (size_t)remaining : sizeof(buf);
                    size_t got = fread(buf, 1, want, existing);
                    if (got != want || memcmp(buf, data.data + offset, want) != 0) {
                        same = false;
                        break;
                    }
                    offset += got;
                }
            }
        }
        fclose(existing);
        if (same) return true;
    }

    FILE *f = i_fopen(path, "wb");
    if (!f) return false;
    fwrite(data.data, 1, data.length, f);
    fclose(f);
    return true;
}

static string8 read_stdin_string8(memops_arena *arena) {
    string8 out = string8_reserve(arena, 4096);
    u8 buf[4096];
    for (;;) {
        size_t n = fread(buf, 1, sizeof(buf), stdin);
        if (n > 0) {
            string8_append_bytes(arena, &out, buf, (u64)n);
        }
        if (n < sizeof(buf)) {
            if (feof(stdin)) break;
            if (ferror(stdin)) return (string8){0};
        }
    }
    return out;
}

static const char *derive_header_path(memops_arena *arena, const char *output_path) {
    u64 len = (u64)strlen(output_path);
    u64 dot = len;
    for (u64 i = len; i > 0; i--) {
        char c = output_path[i - 1];
        if (c == '/' || c == '\\') break;
        if (c == '.') {
            dot = i - 1;
            break;
        }
    }
    string8 out = string8_reserve(arena, len + 3);
    if (dot < len) {
        string8_append_bytes(arena, &out, (u8 *)output_path, dot);
    } else {
        string8_append_bytes(arena, &out, (u8 *)output_path, len);
    }
    string8_append_cstr(arena, &out, ".h");
    string8_append_byte(arena, &out, 0);
    return (const char *)out.data;
}

static const char *derive_output_dir(memops_arena *arena, const char *output_path) {
    u64 len = (u64)strlen(output_path);
    u64 dir_len = 0;
    for (u64 i = len; i > 0; i--) {
        char c = output_path[i - 1];
        if (c == '/' || c == '\\') {
            dir_len = i;
            break;
        }
    }
    string8 out = string8_reserve(arena, dir_len + 1);
    if (dir_len > 0) {
        string8_append_bytes(arena, &out, (u8 *)output_path, dir_len);
    }
    string8_append_byte(arena, &out, 0);
    return (const char *)out.data;
}

static const char *join_output_path(memops_arena *arena, const char *dir, string8 file_name) {
    u64 dir_len = (u64)strlen(dir);
    string8 out = string8_reserve(arena, dir_len + file_name.length + 1);
    string8_append_bytes(arena, &out, (u8 *)dir, dir_len);
    string8_append_bytes(arena, &out, file_name.data, file_name.length);
    string8_append_byte(arena, &out, 0);
    return (const char *)out.data;
}

static ProcDecl *find_generic_owner_proc(Program *prog, string8 owner, string8 member) {
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        if (!decl->is_generic) continue;
        string8 proc_owner = {0};
        string8 proc_member = {0};
        if (!split_qualified_name(decl->name, &proc_owner, &proc_member)) continue;
        if (string8_equals(&proc_owner, &owner) && string8_equals(&proc_member, &member)) {
            return decl;
        }
    }
    return null;
}

static void emit_monomorph_header_protos(memops_arena *arena, Program *prog, string8 owner, string8 mangle, TypeExpr *arg, string8 *out) {
    static const char *members[] = {"reserve", "resize", "append", "destroy", "at", "is_empty"};
    for (i32 i = 0; i < (i32)(sizeof(members) / sizeof(members[0])); i++) {
        string8 member = string8_from_cstr(arena, members[i]);
        ProcDecl *decl = find_generic_owner_proc(prog, owner, member);
        if (decl) {
            emit_proc_proto_mono(arena, out, decl, mangle, arg, null);
        }
    }
}

static bool program_has_alias(Program *prog, string8 name) {
    if (!prog) return false;
    for (i32 i = 0; i < prog->aliases.length; i++) {
        AliasDecl *decl = (AliasDecl *)prog->aliases.data[i];
        if (string8_equals(&decl->name, &name)) return true;
    }
    return false;
}

static bool c_type_name_needs_structdecl(Program *prog, string8 name) {
    static const char *skip[] = {
        "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64",
        "f32", "f64", "usize", "void",
        "c8", "b8", "b16", "b32", "b64",
        "intptr", "uintptr", "ptrdiff", "intmax", "uintmax",
        "float", "double", "int", "long", "short"
    };
    for (i32 i = 0; i < (i32)(sizeof(skip) / sizeof(skip[0])); i++) {
        if (string8_equals_cstr(&name, skip[i])) return false;
    }
    if (program_has_alias(prog, name)) return false;
    return true;
}

static void emit_monomorph_arg_forward_decl(memops_arena *arena, Program *prog, string8 *out, TypeExpr *arg) {
    if (!arg || arg->kind != Type_Name) return;
    if (!c_type_name_needs_structdecl(prog, arg->name)) return;
    emit_cstr(arena, out, "structdecl(");
    emit_c_ident(arena, out, arg->name);
    emit_cstr(arena, out, ");\n\n");
}

static bool emit_native_monomorph_headers(memops_arena *arena, Program *prog, const char *output_path) {
    const char *out_dir = derive_output_dir(arena, output_path);

    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic || !decl->is_external) continue;

        Vec_string8 instances = Vec_string8_reserve(arena, 8);
        collect_generic_struct_instances(prog, decl, &instances, arena);
        if (instances.length == 0) continue;

        string8 umbrella = string8_reserve(arena, 256);
        emit_cstr(arena, &umbrella, "#pragma once\n");

        for (i32 j = 0; j < instances.length; j++) {
            string8 mangle = instances.data[j];
            string8 concrete_name = string8_reserve(arena, decl->name.length + 1 + mangle.length);
            string8_append_bytes(arena, &concrete_name, decl->name.data, decl->name.length);
            string8_append_cstr(arena, &concrete_name, "_");
            string8_append_bytes(arena, &concrete_name, mangle.data, mangle.length);

            TypeExpr *arg = mangle_type_for(arena, mangle);

            string8 file_name = string8_reserve(arena, concrete_name.length + 3);
            string8_append_bytes(arena, &file_name, concrete_name.data, concrete_name.length);
            string8_append_cstr(arena, &file_name, ".h");

            emit_cstr(arena, &umbrella, "#include \"");
            emit_string8(arena, &umbrella, file_name);
            emit_cstr(arena, &umbrella, "\"\n");

            string8 header = string8_reserve(arena, 1024);
            emit_cstr(arena, &header, "#pragma once\n#include <core.h>\n\nstructdecl(memops_arena);\n\n");
            emit_monomorph_arg_forward_decl(arena, prog, &header, arg);
            emit_struct_decl_mono(arena, &header, decl, mangle, arg);
            emit_monomorph_header_protos(arena, prog, decl->name, mangle, arg, &header);

            const char *path = join_output_path(arena, out_dir, file_name);
            if (!write_string8_to_file(path, header)) {
                if (g_diag_json) {
                    char message[1024];
                    snprintf(message, sizeof(message), "failed to write %s", path);
                    diag_json_error(path, 0, 0, "io", message);
                    return false;
                }
                printf("i: error: failed to write %s\n", path);
                return false;
            }
        }

        string8 umbrella_file = string8_reserve(arena, decl->name.length + 3);
        string8_append_bytes(arena, &umbrella_file, decl->name.data, decl->name.length);
        string8_append_cstr(arena, &umbrella_file, ".h");
        const char *umbrella_path = join_output_path(arena, out_dir, umbrella_file);
        if (!write_string8_to_file(umbrella_path, umbrella)) {
            if (g_diag_json) {
                char message[1024];
                snprintf(message, sizeof(message), "failed to write %s", umbrella_path);
                diag_json_error(umbrella_path, 0, 0, "io", message);
                return false;
            }
            printf("i: error: failed to write %s\n", umbrella_path);
            return false;
        }
    }

    return true;
}

static void cli_print_usage(void) {
    printf(
        "I compiler\n"
        "\n"
        "usage:\n"
        "  I compile [input.i] [-o output.c] [--header output.h] [--no-header]\n"
        "  I check   [input.i] [--diagnostics=json]\n"
        "  I symbols [input.i] [--stdin|--stdin-path file]\n"
        "  I lsp     [input.i] [--stdin|--stdin-path file]\n"
        "\n"
        "legacy usage:\n"
        "  I [input.i] [output.c] [output.h]\n"
        "  I --check [input.i] [--diagnostics=json]\n"
        "  I [input.i] --symbols=json\n"
        "  I [input.i] --lsp=json\n"
        "\n"
        "commands:\n"
        "  compile   transpile I to C and optionally a generated header\n"
        "  check     parse, import, validate, and type-check only\n"
        "  symbols   emit compiler symbol metadata as JSON\n"
        "  lsp       emit diagnostics plus compiler symbol metadata as JSON\n"
        "  help      print this help\n"
        "\n"
        "options:\n"
        "  -o, --output <file>       output C file for compile\n"
        "  -H, --header <file>       output header file for compile\n"
        "  --no-header               skip generated header emission\n"
        "  -I, --importdir <dir>     add an I import search directory\n"
        "  --check                   legacy check mode\n"
        "  --diagnostics=json        emit diagnostics as JSON\n"
        "  --symbols=json            legacy symbols JSON mode\n"
        "  --lsp=json                legacy LSP JSON mode\n"
        "  --profile                 print compiler phase timings to stderr\n"
        "  --stdin                   read input source from stdin\n"
        "  --stdin-path <file>       override a source file with stdin text\n"
        "  -h, --help                print this help\n"
        "  --version                 print compiler version\n"
    );
}

static void cli_print_version(void) {
    printf("I compiler dev\n");
}

static bool cli_is_command(const char *arg) {
    return cstr_equals(arg, "compile") ||
           cstr_equals(arg, "check") ||
           cstr_equals(arg, "symbols") ||
           cstr_equals(arg, "lsp") ||
           cstr_equals(arg, "help");
}

static void cli_error_json_or_text(const char *message) {
    if (g_diag_json) {
        diag_json_error("<cli>", 0, 0, "cli", message);
        diag_json_finish(); // CLI failures are always terminal
    } else {
        printf("i: error: %s\n", message);
    }
}

static bool cli_expect_value(i32 argc, char *argv[], i32 *index, const char *option, const char **out_value) {
    if (*index + 1 >= argc) {
        char message[1024];
        snprintf(message, sizeof(message), "%s expects a value", option);
        cli_error_json_or_text(message);
        return false;
    }
    *out_value = argv[++(*index)];
    return true;
}

i32 main(i32 argc, char *argv[]) {
    /* Guarantees the JSON diagnostic array is closed on every exit path, including
       the I/O failures that return straight out of main. */
    atexit(diag_json_finish);
    double profile_start = profile_now_ms();
    double profile_last = profile_start;
    bool check_only = false;
    bool symbols_json = false;
    bool lsp_json = false;
    bool emit_header = true;
    const char *modules_dir = null;
    bool read_source_from_stdin = false;
    const char *input_path = null;
    const char *stdin_override_path_arg = null;
    const char *output_path = null;
    const char *header_path = null;
    i32 positional = 0;
    const char *command = null;

    if (argc > 1) {
        const char *first = argv[1];
        if (cstr_equals(first, "-h") || cstr_equals(first, "--help") || cstr_equals(first, "help")) {
            cli_print_usage();
            return 0;
        }
        if (cstr_equals(first, "--version") || cstr_equals(first, "version")) {
            cli_print_version();
            return 0;
        }
        if (cli_is_command(first)) {
            command = first;
            if (cstr_equals(command, "check")) {
                check_only = true;
            } else if (cstr_equals(command, "symbols")) {
                symbols_json = true;
                g_diag_json = true;
            } else if (cstr_equals(command, "lsp")) {
                lsp_json = true;
                g_diag_json = true;
            }
        }
    }

    for (i32 i = 1; i < argc; i++) {
        const char *arg = argv[i];
        if (cstr_equals(arg, "--diagnostics=json")) {
            g_diag_json = true;
        } else if (cstr_equals(arg, "--diagnostics") && i + 1 < argc && cstr_equals(argv[i + 1], "json")) {
            g_diag_json = true;
        } else if (cstr_equals(arg, "--symbols=json")) {
            g_diag_json = true;
        } else if (cstr_equals(arg, "--symbols") && i + 1 < argc && cstr_equals(argv[i + 1], "json")) {
            g_diag_json = true;
        } else if (cstr_equals(arg, "--lsp=json")) {
            g_diag_json = true;
        } else if (cstr_equals(arg, "--lsp") && i + 1 < argc && cstr_equals(argv[i + 1], "json")) {
            g_diag_json = true;
        } else if (cstr_equals(arg, "symbols") || cstr_equals(arg, "lsp")) {
            g_diag_json = true;
        } else if (cstr_equals(arg, "--profile")) {
            g_profile = true;
        }
    }

    for (i32 i = command ? 2 : 1; i < argc; i++) {
        const char *arg = argv[i];
        if (cstr_equals(arg, "-h") || cstr_equals(arg, "--help")) {
            cli_print_usage();
            return 0;
        }
        if (cstr_equals(arg, "--version")) {
            cli_print_version();
            return 0;
        }
        if (cstr_equals(arg, "--profile")) {
            g_profile = true;
            continue;
        }
        if (cstr_equals(arg, "--no-header")) {
            emit_header = false;
            continue;
        }
        if (cstr_equals(arg, "--modules")) {
            if (i + 1 >= argc) {
                if (g_diag_json) {
                    diag_json_error("<cli>", 0, 0, "cli", "--modules expects a directory");
                    diag_json_finish();
                    return 1;
                }
                printf("i: error: --modules expects a directory\n");
                return 1;
            }
            modules_dir = argv[++i];
            continue;
        }
        if (cstr_equals(arg, "--emit-all-line-directives")) {
            g_emit_all_line_directives = true;
            continue;
        }
        if (cstr_equals(arg, "--check")) {
            check_only = true;
            if (i + 1 < argc && !cstr_starts_with(argv[i + 1], "--") && !input_path) {
                input_path = argv[++i];
            }
            continue;
        }
        if (cstr_equals(arg, "-o") || cstr_equals(arg, "--output")) {
            if (!cli_expect_value(argc, argv, &i, arg, &output_path)) return 1;
            continue;
        }
        if (cstr_starts_with(arg, "--output=")) {
            output_path = arg + strlen("--output=");
            continue;
        }
        if (cstr_equals(arg, "-H") || cstr_equals(arg, "--header")) {
            if (!cli_expect_value(argc, argv, &i, arg, &header_path)) return 1;
            continue;
        }
        if (cstr_starts_with(arg, "--header=")) {
            header_path = arg + strlen("--header=");
            continue;
        }
        if (cstr_equals(arg, "-I") || cstr_equals(arg, "--importdir") || cstr_equals(arg, "--import-dir")) {
            const char *dir = null;
            if (!cli_expect_value(argc, argv, &i, arg, &dir)) return 1;
            if (g_import_dir_count >= 64) {
                cli_error_json_or_text("too many import directories");
                return 1;
            }
            add_import_dir(dir);
            continue;
        }
        if (cstr_starts_with(arg, "--importdir=")) {
            if (g_import_dir_count >= 64) {
                cli_error_json_or_text("too many import directories");
                return 1;
            }
            add_import_dir(arg + strlen("--importdir="));
            continue;
        }
        if (cstr_starts_with(arg, "--import-dir=")) {
            if (g_import_dir_count >= 64) {
                cli_error_json_or_text("too many import directories");
                return 1;
            }
            add_import_dir(arg + strlen("--import-dir="));
            continue;
        }
        if (cstr_equals(arg, "--stdin")) {
            read_source_from_stdin = true;
            continue;
        }
        if (cstr_starts_with(arg, "--stdin-path=")) {
            stdin_override_path_arg = arg + strlen("--stdin-path=");
            continue;
        }
        if (cstr_equals(arg, "--stdin-path")) {
            if (i + 1 >= argc) {
                if (g_diag_json) {
                    diag_json_error("<cli>", 0, 0, "cli", "--stdin-path expects a file path");
                    diag_json_finish();
                    return 1;
                }
                printf("i: error: --stdin-path expects a file path\n");
                return 1;
            }
            stdin_override_path_arg = argv[++i];
            continue;
        }
        if (cstr_starts_with(arg, "--lsp=")) {
            const char *value = arg + strlen("--lsp=");
            if (cstr_equals(value, "json")) {
                lsp_json = true;
                g_diag_json = true;
            } else {
                if (g_diag_json) {
                    char message[1024];
                    snprintf(message, sizeof(message), "unsupported lsp format %s", value);
                    diag_json_error("<cli>", 0, 0, "cli", message);
                    diag_json_finish();
                    return 1;
                }
                printf("i: error: unsupported lsp format %s\n", value);
                return 1;
            }
            continue;
        }
        if (cstr_equals(arg, "--lsp")) {
            if (i + 1 >= argc) {
                if (g_diag_json) {
                    diag_json_error("<cli>", 0, 0, "cli", "--lsp expects a format");
                    diag_json_finish();
                    return 1;
                }
                printf("i: error: --lsp expects a format\n");
                return 1;
            }
            const char *value = argv[++i];
            if (cstr_equals(value, "json")) {
                lsp_json = true;
                g_diag_json = true;
            } else {
                if (g_diag_json) {
                    char message[1024];
                    snprintf(message, sizeof(message), "unsupported lsp format %s", value);
                    diag_json_error("<cli>", 0, 0, "cli", message);
                    diag_json_finish();
                    return 1;
                }
                printf("i: error: unsupported lsp format %s\n", value);
                return 1;
            }
            continue;
        }
        if (cstr_starts_with(arg, "--format=")) {
            const char *value = arg + strlen("--format=");
            if (cstr_equals(value, "json")) {
                if (command && cstr_equals(command, "symbols")) {
                    symbols_json = true;
                    g_diag_json = true;
                } else if (command && cstr_equals(command, "lsp")) {
                    lsp_json = true;
                    g_diag_json = true;
                } else {
                    g_diag_json = true;
                }
            } else {
                char message[1024];
                snprintf(message, sizeof(message), "unsupported format %s", value);
                cli_error_json_or_text(message);
                return 1;
            }
            continue;
        }
        if (cstr_equals(arg, "--format")) {
            const char *value = null;
            if (!cli_expect_value(argc, argv, &i, arg, &value)) return 1;
            if (cstr_equals(value, "json")) {
                if (command && cstr_equals(command, "symbols")) {
                    symbols_json = true;
                    g_diag_json = true;
                } else if (command && cstr_equals(command, "lsp")) {
                    lsp_json = true;
                    g_diag_json = true;
                } else {
                    g_diag_json = true;
                }
            } else {
                char message[1024];
                snprintf(message, sizeof(message), "unsupported format %s", value);
                cli_error_json_or_text(message);
                return 1;
            }
            continue;
        }
        if (cstr_starts_with(arg, "--symbols=")) {
            const char *value = arg + strlen("--symbols=");
            if (cstr_equals(value, "json")) {
                symbols_json = true;
                g_diag_json = true;
            } else {
                if (g_diag_json) {
                    char message[1024];
                    snprintf(message, sizeof(message), "unsupported symbols format %s", value);
                    diag_json_error("<cli>", 0, 0, "cli", message);
                    diag_json_finish();
                    return 1;
                }
                printf("i: error: unsupported symbols format %s\n", value);
                return 1;
            }
            continue;
        }
        if (cstr_equals(arg, "--symbols")) {
            if (i + 1 >= argc) {
                if (g_diag_json) {
                    diag_json_error("<cli>", 0, 0, "cli", "--symbols expects a format");
                    diag_json_finish();
                    return 1;
                }
                printf("i: error: --symbols expects a format\n");
                return 1;
            }
            const char *value = argv[++i];
            if (cstr_equals(value, "json")) {
                symbols_json = true;
                g_diag_json = true;
            } else {
                if (g_diag_json) {
                    char message[1024];
                    snprintf(message, sizeof(message), "unsupported symbols format %s", value);
                    diag_json_error("<cli>", 0, 0, "cli", message);
                    diag_json_finish();
                    return 1;
                }
                printf("i: error: unsupported symbols format %s\n", value);
                return 1;
            }
            continue;
        }
        if (cstr_starts_with(arg, "--diagnostics=")) {
            const char *value = arg + strlen("--diagnostics=");
            if (cstr_equals(value, "json")) {
                g_diag_json = true;
            } else {
                if (g_diag_json) {
                    char message[1024];
                    snprintf(message, sizeof(message), "unsupported diagnostics format %s", value);
                    diag_json_error("<cli>", 0, 0, "cli", message);
                    diag_json_finish();
                    return 1;
                }
                printf("i: error: unsupported diagnostics format %s\n", value);
                return 1;
            }
            continue;
        }
        if (cstr_equals(arg, "--diagnostics")) {
            if (i + 1 >= argc) {
                if (g_diag_json) {
                    diag_json_error("<cli>", 0, 0, "cli", "--diagnostics expects a format");
                    diag_json_finish();
                    return 1;
                }
                printf("i: error: --diagnostics expects a format\n");
                return 1;
            }
            const char *value = argv[++i];
            if (cstr_equals(value, "json")) {
                g_diag_json = true;
            } else {
                if (g_diag_json) {
                    char message[1024];
                    snprintf(message, sizeof(message), "unsupported diagnostics format %s", value);
                    diag_json_error("<cli>", 0, 0, "cli", message);
                    diag_json_finish();
                    return 1;
                }
                printf("i: error: unsupported diagnostics format %s\n", value);
                return 1;
            }
            continue;
        }
        if (cstr_starts_with(arg, "--")) {
            if (g_diag_json) {
                char message[1024];
                snprintf(message, sizeof(message), "unknown option %s", arg);
                diag_json_error("<cli>", 0, 0, "cli", message);
                diag_json_finish();
                return 1;
            }
            printf("i: error: unknown option %s\n", arg);
            return 1;
        }
        if (positional == 0 && !input_path) {
            input_path = arg;
        } else if ((!command || cstr_equals(command, "compile")) && positional <= 1 && !output_path) {
            output_path = arg;
        } else if ((!command || cstr_equals(command, "compile")) && positional <= 2 && !header_path) {
            header_path = arg;
        } else {
            if (g_diag_json) {
                char message[1024];
                snprintf(message, sizeof(message), "unexpected argument %s", arg);
                diag_json_error("<cli>", 0, 0, "cli", message);
                diag_json_finish();
                return 1;
            }
            printf("i: error: unexpected argument %s\n", arg);
            return 1;
        }
        positional++;
    }
    profile_mark("cli", &profile_last, profile_start);
    if (!input_path) input_path = "src/main.i";
    if (!output_path) output_path = "build/i_gen/main.c";
    if (!emit_header && header_path) {
        cli_error_json_or_text("--no-header cannot be used with --header or a positional header path");
        return 1;
    }
    g_source_path = input_path;

    memops_arena arena = {0};
    memops_arena_initialize(&arena);
    g_index_arena = &arena;
    add_import_dir(exe_import_root(&arena, argv[0]));

    const char *canonical_input_path = canonicalize_path(&arena, string8_from_cstr(&arena, input_path));
    if (stdin_override_path_arg) {
        g_stdin_override_path = canonicalize_path(&arena, string8_from_cstr(&arena, stdin_override_path_arg));
        g_stdin_override_source = read_stdin_string8(&arena);
        if (!g_stdin_override_source.data) {
            if (g_diag_json) {
                diag_json_error(stdin_override_path_arg, 0, 0, "io", "failed to read stdin");
                diag_json_finish();
                return 1;
            }
            printf("i: error: failed to read stdin\n");
            return 1;
        }
    }

    bool input_from_stdin = read_source_from_stdin && !g_stdin_override_path;
    if (g_stdin_override_path && cstr_equals(canonical_input_path, g_stdin_override_path)) {
        input_from_stdin = true;
    }
    profile_mark("setup", &profile_last, profile_start);
    string8 input = {0};
    if (g_stdin_override_path && cstr_equals(canonical_input_path, g_stdin_override_path)) {
        input = g_stdin_override_source;
    } else if (read_source_from_stdin && !g_stdin_override_path) {
        input = read_stdin_string8(&arena);
    } else {
        input = string8_read_file(&arena, input_path);
    }
    if (!input.data) {
        if (g_diag_json) {
            char message[1024];
            snprintf(message, sizeof(message), "failed to read %s", input_from_stdin ? "stdin" : input_path);
            diag_json_error(input_path, 0, 0, "io", message);
            diag_json_finish();
            return 1;
        }
        printf("i: error: failed to read %s\n", input_from_stdin ? "stdin" : input_path);
        return 1;
    }
    /* Before anything is lexed: every conditional, in every file, sees the same
       set of unconditional defines. */
    preproc_build_seed(&arena, canonical_input_path);

    profile_mark("read entry", &profile_last, profile_start);

    Vec_Token tokens = {0};
    Vec_string8 directives = {0};
    lex_tokens(&arena, input, &tokens, &directives);
    profile_mark("lex entry", &profile_last, profile_start);

    Parser parser = {0};
    parser.arena = &arena;
    parser.source = input;
    parser.tokens = tokens;
    parser.index = 0;

    Program prog = parse_program(&parser);
    prog.preprocessor_lines = directives;
    profile_mark("parse entry", &profile_last, profile_start);
    Vec_string8 visited_imports = Vec_string8_reserve(&arena, 8);
    Vec_string8 import_stack = Vec_string8_reserve(&arena, 8);
    if (argc > 1) {
        Vec_string8_append(&arena, &import_stack, string8_from_cstr(&arena, input_path));
    }
    prog = expand_i_imports(&arena, &prog, &visited_imports, &import_stack);
    profile_mark("expand imports", &profile_last, profile_start);
    /* Lex and parse errors are all reported together, but analysis does not run on
       a tree that failed to parse, because the follow-on errors would be noise. */
    if (g_error_count > 0) {
        diag_json_finish();
        return 1;
    }
    /* Every import is merged, so a `[Enum.Member]` count can now name an enum
       from any of them. Runs before analysis so array types are final. */
    resolve_array_count_constants(&arena, &prog);
    profile_import_summary();
    Vec_string8 symbol_known_types = semantic_collect_known_type_names(&prog, &arena);
    semantic_collect_program_external_type_names(&prog, &symbol_known_types, &arena);
    semantic_resolve_proc_angle_types(&prog, &symbol_known_types, &arena);
    profile_mark("resolve symbols", &profile_last, profile_start);
    if (symbols_json) {
        emit_symbols_json(&arena, &prog);
        profile_mark("emit symbols json", &profile_last, profile_start);
        return 0;
    }
    semantic_check_program(&prog, &arena);
    profile_mark("semantic check", &profile_last, profile_start);
    validate_generic_constraints(&prog, &arena);
    profile_mark("generic constraints", &profile_last, profile_start);
    type_check_program(&prog, &arena);
    profile_mark("type check", &profile_last, profile_start);
    rewrite_printfmt_formats(&prog, &arena);
    profile_mark("printfmt rewrite", &profile_last, profile_start);
    /* Every semantic and type diagnostic has been reported by here, so nothing is
       generated if any of them failed. */
    if (g_error_count > 0) {
        diag_json_finish();
        return 1;
    }

    if (lsp_json) {
        emit_lsp_json(&arena, &prog);
        profile_mark("emit lsp json", &profile_last, profile_start);
        return 0;
    }

    if (check_only) {
        if (g_diag_json) {
            printf("[]\n");
        } else {
            printf("i: checked %s\n", input_path);
        }
        profile_mark("check done", &profile_last, profile_start);
        return 0;
    }

    /* Per-module output: a .h/.c pair for each source module, plus the shared
       type header and the single translation unit that owns every monomorph.
       The whole-program single-file path below is unchanged and remains the
       default, so a unity build costs exactly what it did before. */
    if (modules_dir) {
        Vec_string8 modules = Vec_string8_reserve(&arena, 16);
        collect_module_paths(&arena, &prog, &modules);

        char path_buf[4096];
        string8 buf;

        buf = string8_reserve(&arena, input.length + 4096);
        emit_types_header(&arena, &prog, &buf);
        snprintf(path_buf, sizeof(path_buf), "%s/i_types.h", modules_dir);
        if (!write_string8_to_file(path_buf, buf)) {
            printf("i: error: failed to write %s\n", path_buf);
            return 1;
        }

        buf = string8_reserve(&arena, input.length + 4096);
        emit_monomorph_header(&arena, &prog, &buf);
        snprintf(path_buf, sizeof(path_buf), "%s/i_monomorphs.h", modules_dir);
        if (!write_string8_to_file(path_buf, buf)) {
            printf("i: error: failed to write %s\n", path_buf);
            return 1;
        }

        buf = string8_reserve(&arena, input.length + 4096);
        emit_monomorph_source(&arena, &prog, &modules, &buf);
        snprintf(path_buf, sizeof(path_buf), "%s/i_monomorphs.c", modules_dir);
        if (!write_string8_to_file(path_buf, buf)) {
            printf("i: error: failed to write %s\n", path_buf);
            return 1;
        }

        for (i32 m = 0; m < modules.length; m++) {
            string8 module = modules.data[m];
            string8 stem = module_stem(&arena, module);
            if (string8_equals_cstr(&stem, "i_types") || string8_equals_cstr(&stem, "i_monomorphs")) {
                printf("i: error: module name collides with a generated file: %.*s\n",
                    (int)module.length, module.data);
                return 1;
            }

            buf = string8_reserve(&arena, input.length + 4096);
            emit_module_header(&arena, &prog, &modules, m, &buf);
            snprintf(path_buf, sizeof(path_buf), "%s/%.*s.h", modules_dir, (int)stem.length, stem.data);
            if (!write_string8_to_file(path_buf, buf)) {
                printf("i: error: failed to write %s\n", path_buf);
                return 1;
            }

            buf = string8_reserve(&arena, input.length + 4096);
            emit_module_source(&arena, &prog, module, &buf);
            snprintf(path_buf, sizeof(path_buf), "%s/%.*s.c", modules_dir, (int)stem.length, stem.data);
            if (!write_string8_to_file(path_buf, buf)) {
                printf("i: error: failed to write %s\n", path_buf);
                return 1;
            }
        }

        buf = string8_reserve(&arena, 4096 + (u64)modules.length * 64);
        emit_all_header(&arena, &modules, &buf);
        snprintf(path_buf, sizeof(path_buf), "%s/i_all.h", modules_dir);
        if (!write_string8_to_file(path_buf, buf)) {
            printf("i: error: failed to write %s\n", path_buf);
            return 1;
        }

        printf("i: generated %d module(s) in %s\n", (int)modules.length, modules_dir);
        profile_mark("emit modules", &profile_last, profile_start);
        return 0;
    }

    string8 output = string8_reserve(&arena, input.length * 2 + 1024);
    emit_program(&arena, &prog, &output);
    profile_mark("emit c", &profile_last, profile_start);

    string8 header_output = {0};
    if (emit_header) {
        header_output = string8_reserve(&arena, input.length + 1024);
        emit_header_program(&arena, &prog, &header_output);
        profile_mark("emit header", &profile_last, profile_start);
        if (!header_path) {
            header_path = derive_header_path(&arena, output_path);
        }
    }

    if (!write_string8_to_file(output_path, output)) {
        if (g_diag_json) {
            char message[1024];
            snprintf(message, sizeof(message), "failed to write %s", output_path);
            diag_json_error(output_path, 0, 0, "io", message);
            diag_json_finish();
            return 1;
        }
        printf("i: error: failed to write %s\n", output_path);
        return 1;
    }
    profile_mark("write c", &profile_last, profile_start);
    if (emit_header) {
        if (!write_string8_to_file(header_path, header_output)) {
            if (g_diag_json) {
                char message[1024];
                snprintf(message, sizeof(message), "failed to write %s", header_path);
                diag_json_error(header_path, 0, 0, "io", message);
                diag_json_finish();
                return 1;
            }
            printf("i: error: failed to write %s\n", header_path);
            return 1;
        }
        profile_mark("write header", &profile_last, profile_start);
    }
    if (!emit_native_monomorph_headers(&arena, &prog, output_path)) {
        return 1;
    }
    profile_mark("native mono headers", &profile_last, profile_start);

    if (emit_header) {
        printf("i: generated %s and %s\n", output_path, header_path);
    } else {
        printf("i: generated %s\n", output_path);
    }
    profile_mark("total", &profile_last, profile_start);
    return 0;
}

#include <Vec.c>
