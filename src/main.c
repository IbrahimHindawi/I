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
#include <string.h>

#include "string8.h"
#include "string8slice.h"

static const char *g_source_path = "<input>";

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
    Token_Colon,
    Token_Semicolon,
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
    Token_Question,
    Token_Bang,
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
} TokenKind;

typedef struct Token Token;
struct Token {
    TokenKind kind;
    string8slice text;
    i32 line;
    i32 col;
};

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
    TypeExpr *elem;
    TypeExpr *ret_type;
    bool is_variadic;
    Vec_voidptr args; // TypeExpr*
};

typedef enum ExprKind {
    Expr_Name = 0,
    Expr_Number,
    Expr_String,
    Expr_Call,
    Expr_Addr,
    Expr_Binary,
    Expr_Index,
    Expr_Field,
    Expr_SizeofType,
    Expr_AlignofType,
    Expr_ZeroInit,
    Expr_InitList,
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
    string8 string_lit;
    Vec_voidptr args;      // Expr*
    Vec_voidptr type_args; // TypeExpr*
    Vec_voidptr designators; // Expr* or null, parallel with args for init lists
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
} StmtKind;

struct Stmt {
    StmtKind kind;
    string8 name;
    TypeExpr *type;
    bool is_external;
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
    string8 name;
    TypeExpr *type;
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
    bool is_generic;
    bool is_union;
    string8 type_param;
    Vec_voidptr fields; // Field*
    i32 line;
    i32 col;
};

struct EnumItem {
    string8 name;
    string8 value;
    i32 line;
    i32 col;
};

struct EnumDecl {
    string8 name;
    Vec_voidptr items; // EnumItem*
    i32 line;
    i32 col;
};

struct ProcDecl {
    string8 name;
    bool is_generic;
    bool is_external;
    bool emit_external_proto;
    bool is_variadic;
    string8 type_param;
    string8 constraint;
    string8 callconv;
    Vec_voidptr params; // Param*
    TypeExpr *ret_type;
    Vec_voidptr body; // Stmt*
    i32 line;
    i32 col;
};

typedef struct AliasDecl {
    string8 name;
    TypeExpr *type;
    i32 line;
    i32 col;
} AliasDecl;

typedef struct Program {
    Vec_string8 preprocessor_lines;
    Vec_string8 defines; // macro name
    Vec_string8 imports; // string literal include path token text
    Vec_string8 i_imports; // string literal import paths ending in .i
    Vec_voidptr structs; // StructDecl*
    Vec_voidptr enums;   // EnumDecl*
    Vec_voidptr aliases; // AliasDecl*
    Vec_voidptr procs;   // ProcDecl*
    Vec_voidptr globals; // Stmt* (var decl)
} Program;

typedef struct Scope {
    Vec_string8 locals;
    Vec_string8 globals;
    Vec_string8 procs;
} Scope;

typedef struct Parser {
    memops_arena *arena;
    Vec_Token tokens;
    i32 index;
    bool pending_equal;
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

static Token token_make(TokenKind kind, string8slice text, i32 line, i32 col) {
    Token t;
    t.kind = kind;
    t.text = text;
    t.line = line;
    t.col = col;
    return t;
}

static void lex_tokens(memops_arena *arena, string8 src, Vec_Token *out_tokens) {
    i32 line = 1;
    i32 col = 1;
    u8 *p = src.data;
    u8 *end = src.data + src.length;

    *out_tokens = Vec_Token_reserve(arena, 256);

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
            while (p < end && *p != '\n') {
                p++;
            }
            continue;
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
                if (*p == '\n') {
                    printf("%s:%d:%d: lexer error: unterminated string\n", g_source_path, line, start_col);
                    exit(1);
                }
                p++;
                col++;
            }
            if (p > end || start == p || p[-1] != '"') {
                printf("%s:%d:%d: lexer error: unterminated string\n", g_source_path, line, start_col);
                exit(1);
            }
            string8slice text = string8slice_from_parts(start, (u64)(p - start));
            Vec_Token_append(arena, out_tokens, token_make(Token_String, text, line, start_col));
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
            case '{': kind = Token_LBrace; break;
            case '}': kind = Token_RBrace; break;
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
            case '?': kind = Token_Question; break;
            case '!': kind = Token_Bang; break;
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

        printf("%s:%d:%d: lexer error: unexpected char '%c'\n", g_source_path, line, col, c);
        exit(1);
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

static Token *parser_expect(Parser *p, TokenKind kind, const char *msg) {
    if (kind == Token_Equal && p->pending_equal) {
        p->pending_equal = false;
        return parser_prev(p);
    }
    if (parser_peek(p)->kind != kind) {
        Token *t = parser_peek(p);
        printf("%s:%d:%d: parse error: %s\n", g_source_path, t->line, t->col, msg);
        exit(1);
    }
    return parser_next(p);
}

static string8 token_to_string8(memops_arena *arena, Token *t) {
    return string8_copy_from_slice(arena, t->text.data, t->text.length);
}

static Vec_string8 collect_preprocessor_lines(memops_arena *arena, string8 src);

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

static string8 i_import_to_h_import(memops_arena *arena, string8 lit) {
    string8 path = string_lit_inner(arena, lit);
    if (!string8_ends_with_cstr(path, ".i")) {
        return lit;
    }
    string8 out = string8_reserve(arena, path.length + 3);
    string8_append_byte(arena, &out, '"');
    string8_append_bytes(arena, &out, path.data, path.length - 2);
    string8_append_cstr(arena, &out, ".h\"");
    return out;
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

static bool token_is_generic_param_name(Token *t) {
    if (!t || t->kind != Token_Identifier || t->text.length == 0) return false;
    u8 c = t->text.data[0];
    return (c >= 'A' && c <= 'Z');
}

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

static TypeExpr *parse_type(Parser *p) {
    if (parser_match(p, Token_Keyword_Const)) {
        TypeExpr *inner = parse_type(p);
        inner->is_const = true;
        return inner;
    }

    if (parser_match(p, Token_Keyword_Proc)) {
        TypeExpr *t = type_new(p->arena, Type_Proc);
        t->args = ptr_array_reserve(p->arena, 8);

        if (parser_match(p, Token_LBracket)) {
            Token *callconv_tok = parser_expect(p, Token_Identifier, "expected call convention name");
            t->name = token_to_string8(p->arena, callconv_tok);
            parser_expect(p, Token_RBracket, "expected ']' after call convention");
        }

        parser_expect(p, Token_LParen, "expected '(' after proc type");
        if (!parser_match(p, Token_RParen)) {
            do {
                if (parser_match(p, Token_Ellipsis)) {
                    t->is_variadic = true;
                    break;
                }

                if (parser_peek(p)->kind == Token_Identifier &&
                    parser_peek_n(p, 1)->kind == Token_Colon) {
                    parser_next(p); // optional param name
                    parser_next(p); // ':'
                }

                TypeExpr *arg = parse_type(p);
                ptr_array_append(p->arena, &t->args, arg);
            } while (parser_match(p, Token_Comma));
            parser_expect(p, Token_RParen, "expected ')' after proc type params");
        }

        parser_expect(p, Token_Arrow, "expected '->' after proc type params");
        t->ret_type = parse_type(p);
        return t;
    }

    if (parser_match(p, Token_LBracket)) {
        Token *count_tok = parser_expect(p, Token_Number, "expected array count");
        parser_expect(p, Token_RBracket, "expected ']' after array count");
        TypeExpr *inner = parse_type(p);
        TypeExpr *array = type_new(p->arena, Type_Array);
        array->array_count = token_to_string8(p->arena, count_tok);
        array->elem = inner;
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
        t->args = args;
        return t;
    }

    TypeExpr *t = type_new(p->arena, Type_Name);
    t->name = name;
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

static void emit_mono_proc_name(memops_arena *arena, string8 *out, string8 base_name, string8 type_mangled) {
    string8 owner = {0};
    string8 member = {0};
    if (split_qualified_name(base_name, &owner, &member)) {
        emit_string8(arena, out, owner);
        emit_cstr(arena, out, "_");
        emit_string8(arena, out, type_mangled);
        emit_cstr(arena, out, "_");
        emit_string8(arena, out, member);
    } else {
        emit_string8(arena, out, base_name);
        emit_cstr(arena, out, "_");
        emit_string8(arena, out, type_mangled);
    }
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
static Expr *parse_postfix(Parser *p, Expr *base);

static Expr *parse_primary(Parser *p) {
    if (parser_match(p, Token_LBrace)) {
        Token *lb = parser_prev(p);
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
        e->line = lb->line;
        e->col = lb->col;
        do {
            Expr *designator = null;
            InitDesignatorKind designator_kind = InitDesignator_None;
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
            Vec_i32_append(p->arena, &e->designator_kinds, designator_kind);
        } while (parser_match(p, Token_Comma) && parser_peek(p)->kind != Token_RBrace);
        parser_expect(p, Token_RBrace, "expected '}' after initializer list");
        return e;
    }

    if (parser_match(p, Token_Number)) {
        Token *t = parser_prev(p);
        Expr *e = expr_new(p->arena, Expr_Number);
        e->number = token_to_string8(p->arena, t);
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

            Vec_voidptr args = ptr_array_reserve(p->arena, 2);
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
    printf("%s:%d:%d: parse error: expected expression\n", g_source_path, t->line, t->col);
    exit(1);
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
    Expr *left = parse_shift(p);
    while (parser_peek(p)->kind == Token_LAngle ||
           parser_peek(p)->kind == Token_RAngle ||
           parser_peek(p)->kind == Token_LessEqual ||
           parser_peek(p)->kind == Token_GreaterEqual) {
        Token *op_tok = parser_peek(p);
        TokenKind op = op_tok->kind;
        parser_next(p);
        Expr *right = parse_shift(p);
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
    Expr *left = parse_equality(p);
    while (parser_peek(p)->kind == Token_Ampersand) {
        Token *op_tok = parser_peek(p);
        parser_next(p);
        Expr *right = parse_equality(p);
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
    Expr *left = parse_bitwise_or(p);
    while (parser_peek(p)->kind == Token_Keyword_And) {
        Token *op_tok = parser_peek(p);
        parser_next(p);
        Expr *right = parse_bitwise_or(p);
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
    if (parser_match(p, Token_AmpersandEqual)) return Token_AmpersandEqual;
    if (parser_match(p, Token_CaretEqual)) return Token_CaretEqual;
    if (parser_match(p, Token_PipeEqual)) return Token_PipeEqual;
    return Token_EOF;
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
        s->line = name_tok->line;
        s->col = name_tok->col;
        if (parser_match(p, Token_Equal)) {
            s->expr = parse_expr(p);
        }
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
    if (parser_match(p, Token_Keyword_Do)) {
        Token *do_tok = parser_prev(p);
        Stmt *s = stmt_new(p->arena, Stmt_DoWhile);
        s->line = do_tok->line;
        s->col = do_tok->col;
        s->while_body = ptr_array_reserve(p->arena, 8);
        parser_expect(p, Token_LBrace, "expected '{' after do");
        while (!parser_match(p, Token_RBrace)) {
            ptr_array_append(p->arena, &s->while_body, parse_stmt(p));
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
        while (!parser_match(p, Token_RBrace)) {
            ptr_array_append(p->arena, &s->while_body, parse_stmt(p));
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
        while (!parser_match(p, Token_RBrace)) {
            if (parser_match(p, Token_Keyword_Case)) {
                Token *case_tok = parser_prev(p);
                SwitchCase *sc = memops_arena_push_struct(p->arena, SwitchCase);
                memset(sc, 0, sizeof(SwitchCase));
                sc->line = case_tok->line;
                sc->col = case_tok->col;
                sc->expr = parse_expr(p);
                sc->body = ptr_array_reserve(p->arena, 8);
                parser_expect(p, Token_Colon, "expected ':' after case");
                while (parser_peek(p)->kind != Token_Keyword_Case &&
                       parser_peek(p)->kind != Token_Keyword_Default &&
                       parser_peek(p)->kind != Token_RBrace) {
                    ptr_array_append(p->arena, &sc->body, parse_stmt(p));
                }
                ptr_array_append(p->arena, &s->switch_cases, sc);
                continue;
            }
            if (parser_match(p, Token_Keyword_Default)) {
                parser_expect(p, Token_Colon, "expected ':' after default");
                while (parser_peek(p)->kind != Token_Keyword_Case &&
                       parser_peek(p)->kind != Token_RBrace) {
                    ptr_array_append(p->arena, &s->switch_default_body, parse_stmt(p));
                }
                continue;
            }
            Token *t = parser_peek(p);
            printf("%s:%d:%d: parse error: expected case/default in switch\n", g_source_path, t->line, t->col);
            exit(1);
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
        while (!parser_match(p, Token_RBrace)) {
            Stmt *body_stmt = parse_stmt(p);
            ptr_array_append(p->arena, &s->if_then_body, body_stmt);
        }

        if (parser_match(p, Token_Keyword_Else)) {
            if (parser_peek(p)->kind == Token_Keyword_If) {
                s->if_else_if = parse_stmt(p);
            } else {
                parser_expect(p, Token_LBrace, "expected '{' in else body");
                while (!parser_match(p, Token_RBrace)) {
                    Stmt *else_stmt = parse_stmt(p);
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
        while (!parser_match(p, Token_RBrace)) {
            Stmt *body_stmt = parse_stmt(p);
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
            Stmt *s = stmt_new(p->arena, Stmt_Var);
            s->name = token_to_string8(p->arena, name_tok);
            s->type = parse_type(p);
            s->line = name_tok->line;
            s->col = name_tok->col;
            if (parser_match(p, Token_Equal)) {
                s->expr = parse_expr(p);
            }
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
    printf("%s:%d:%d: parse error: unexpected token\n", g_source_path, t->line, t->col);
    exit(1);
    return null;
}

static StructDecl *parse_struct_decl(Parser *p, Token *name_tok, bool is_union) {
    StructDecl *decl = memops_arena_push_struct(p->arena, StructDecl);
    memset(decl, 0, sizeof(StructDecl));
    decl->name = token_to_string8(p->arena, name_tok);
    decl->is_union = is_union;
    decl->line = name_tok->line;
    decl->col = name_tok->col;
    decl->fields = ptr_array_reserve(p->arena, 8);

    if (parser_match(p, Token_LAngle)) {
        Token *param_tok = parser_expect(p, Token_Identifier, "expected type param");
        decl->type_param = token_to_string8(p->arena, param_tok);
        decl->is_generic = true;
        parser_expect_generic_close(p);
    }

    parser_expect(p, Token_Equal, is_union ? "expected '=' after union" : "expected '=' after struct");
    parser_expect(p, Token_LBrace, is_union ? "expected '{' in union" : "expected '{' in struct");
    while (!parser_match(p, Token_RBrace)) {
        Token *field_tok = parser_expect(p, Token_Identifier, "expected field name");
        parser_expect(p, Token_Colon, "expected ':' after field name");
        Field *f = memops_arena_push_struct(p->arena, Field);
        f->name = token_to_string8(p->arena, field_tok);
        f->type = parse_type(p);
        f->line = field_tok->line;
        f->col = field_tok->col;
        ptr_array_append(p->arena, &decl->fields, f);
        parser_expect(p, Token_Semicolon, "expected ';' after field");
    }
    // optional ';' after struct decl
    parser_match(p, Token_Semicolon);
    return decl;
}

static AliasDecl *parse_alias_decl(Parser *p, Token *name_tok) {
    AliasDecl *decl = memops_arena_push_struct(p->arena, AliasDecl);
    memset(decl, 0, sizeof(AliasDecl));
    decl->name = token_to_string8(p->arena, name_tok);
    decl->line = name_tok->line;
    decl->col = name_tok->col;
    parser_expect(p, Token_Equal, "expected '=' after alias");
    decl->type = parse_type(p);
    parser_expect(p, Token_Semicolon, "expected ';' after alias");
    return decl;
}

static EnumDecl *parse_enum_decl(Parser *p, Token *name_tok) {
    EnumDecl *decl = memops_arena_push_struct(p->arena, EnumDecl);
    memset(decl, 0, sizeof(EnumDecl));
    decl->name = token_to_string8(p->arena, name_tok);
    decl->line = name_tok->line;
    decl->col = name_tok->col;
    decl->items = ptr_array_reserve(p->arena, 8);

    parser_expect(p, Token_Equal, "expected '=' after enum");
    parser_expect(p, Token_LBrace, "expected '{' in enum");
    while (!parser_match(p, Token_RBrace)) {
        Token *item_tok = parser_expect(p, Token_Identifier, "expected enum item name");
        EnumItem *item = memops_arena_push_struct(p->arena, EnumItem);
        memset(item, 0, sizeof(EnumItem));
        item->name = token_to_string8(p->arena, item_tok);
        item->line = item_tok->line;
        item->col = item_tok->col;

        if (parser_match(p, Token_Equal)) {
            Token *value_tok = parser_peek(p);
            if (value_tok->kind != Token_Number && value_tok->kind != Token_Identifier) {
                printf("%s:%d:%d: parse error: expected enum value\n", g_source_path, value_tok->line, value_tok->col);
                exit(1);
            }
            parser_next(p);
            item->value = token_to_string8(p->arena, value_tok);
        }

        ptr_array_append(p->arena, &decl->items, item);
        if (parser_match(p, Token_Comma) || parser_match(p, Token_Semicolon)) {
            continue;
        }
        parser_expect(p, Token_RBrace, "expected ',' or '}' after enum item");
        break;
    }
    parser_match(p, Token_Semicolon);
    return decl;
}

static ProcDecl *parse_proc_decl(Parser *p, Token *name_tok) {
    ProcDecl *decl = memops_arena_push_struct(p->arena, ProcDecl);
    memset(decl, 0, sizeof(ProcDecl));
    decl->name = token_to_string8(p->arena, name_tok);
    decl->line = name_tok->line;
    decl->col = name_tok->col;
    decl->params = ptr_array_reserve(p->arena, 8);
    decl->body = ptr_array_reserve(p->arena, 8);

    if (parser_match(p, Token_LAngle)) {
        Token *first = parser_peek(p);
        Token *second = parser_peek_n(p, 1);
        bool generic_param_form =
            first->kind == Token_Identifier &&
            (second->kind == Token_Colon ||
             (second->kind == Token_RAngle && token_is_generic_param_name(first)));

        if (generic_param_form) {
            Token *param_tok = parser_expect(p, Token_Identifier, "expected type param");
            decl->type_param = token_to_string8(p->arena, param_tok);
            decl->is_generic = true;
            if (parser_match(p, Token_Colon)) {
                Token *constraint_tok = parser_expect(p, Token_Identifier, "expected constraint");
                decl->constraint = token_to_string8(p->arena, constraint_tok);
            }
            parser_expect_generic_close(p);
        } else {
            TypeExpr *spec = parse_type(p);
            parser_expect_generic_close(p);
            string8 suffix = type_mangle_concrete(p->arena, spec);
            string8 full_name = string8_reserve(p->arena, decl->name.length + 1 + suffix.length);
            string8_append_bytes(p->arena, &full_name, decl->name.data, decl->name.length);
            string8_append_cstr(p->arena, &full_name, "_");
            string8_append_bytes(p->arena, &full_name, suffix.data, suffix.length);
            decl->name = full_name;
            decl->is_generic = false;
        }
    }

    if (parser_match(p, Token_LBracket)) {
        Token *callconv_tok = parser_expect(p, Token_Identifier, "expected call convention name");
        decl->callconv = token_to_string8(p->arena, callconv_tok);
        parser_expect(p, Token_RBracket, "expected ']' after call convention");
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
        parser_peek_n(p, 1)->kind == Token_Semicolon &&
        parser_peek_n(p, 2)->kind == Token_RBrace) {
        Token *external_tok = parser_next(p); // external/external_emit
        parser_next(p); // ;
        parser_next(p); // }
        decl->is_external = true;
        decl->emit_external_proto = string8slice_equals_cstr(external_tok->text, "external_emit");
        parser_match(p, Token_Semicolon); // optional ';' after proc decl
        return decl;
    }
    while (!parser_match(p, Token_RBrace)) {
        Stmt *s = parse_stmt(p);
        ptr_array_append(p->arena, &decl->body, s);
    }
    // optional ';' after proc decl
    parser_match(p, Token_Semicolon);
    return decl;
}

static Program parse_program(Parser *p) {
    Program prog = {0};
    prog.preprocessor_lines = Vec_string8_reserve(p->arena, 8);
    prog.defines = Vec_string8_reserve(p->arena, 8);
    prog.imports = Vec_string8_reserve(p->arena, 8);
    prog.i_imports = Vec_string8_reserve(p->arena, 8);
    prog.structs = ptr_array_reserve(p->arena, 8);
    prog.enums = ptr_array_reserve(p->arena, 8);
    prog.aliases = ptr_array_reserve(p->arena, 8);
    prog.procs = ptr_array_reserve(p->arena, 8);
    prog.globals = ptr_array_reserve(p->arena, 8);

    while (parser_peek(p)->kind != Token_EOF) {
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
            if (string8_ends_with_cstr(inner, ".i")) {
                Vec_string8_append(p->arena, &prog.i_imports, path);
            }
            Vec_string8_append(p->arena, &prog.imports, path);
            parser_match(p, Token_Semicolon);
            continue;
        }

        Token *head_tok = parser_peek(p);
        string8 base_name = token_to_string8(p->arena, head_tok);
        string8 parsed_name = parse_decl_name(p);
        parser_expect(p, Token_Colon, "expected ':' after identifier");

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
            ptr_array_append(p->arena, &prog.procs, decl);
            continue;
        }

        Stmt *s = stmt_new(p->arena, Stmt_Var);
        s->name = parsed_name;
        s->type = parse_type(p);
        s->line = head_tok->line;
        s->col = head_tok->col;
        if (parser_match(p, Token_Equal)) {
            if (parser_peek(p)->kind == Token_Identifier &&
                string8slice_equals_cstr(parser_peek(p)->text, "external") &&
                parser_peek_n(p, 1)->kind == Token_Semicolon) {
                parser_next(p);
                s->is_external = true;
            } else {
                s->expr = parse_expr(p);
            }
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

static void semantic_error(const char *msg, i32 line, i32 col) {
    printf("%s:%d:%d: semantic error: %s\n", g_source_path, line, col, msg);
    exit(1);
}

static void semantic_error_name(const char *msg, string8 name, i32 line, i32 col) {
    printf("%s:%d:%d: semantic error: %s '%.*s'\n", g_source_path, line, col, msg, (int)name.length, name.data);
    exit(1);
}

static void semantic_error_name_dup(
    const char *msg, string8 name,
    i32 line, i32 col,
    i32 prev_line, i32 prev_col
) {
    printf(
        "%s:%d:%d: semantic error: %s '%.*s' (previous at %d:%d)\n",
        g_source_path, line, col, msg, (int)name.length, name.data, prev_line, prev_col
    );
    exit(1);
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

static const char *resolve_import_path(memops_arena *arena, string8 import_lit) {
    string8 import_path = string_lit_inner(arena, import_lit);
    if (import_path.length >= 2 && import_path.data[1] == ':') {
        string8 out = string8_reserve(arena, import_path.length + 1);
        string8_append_bytes(arena, &out, import_path.data, import_path.length);
        string8_append_byte(arena, &out, 0);
        return (const char *)out.data;
    }
    if (import_path.length > 0 && (import_path.data[0] == '/' || import_path.data[0] == '\\')) {
        string8 out = string8_reserve(arena, import_path.length + 1);
        string8_append_bytes(arena, &out, import_path.data, import_path.length);
        string8_append_byte(arena, &out, 0);
        return (const char *)out.data;
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
    string8_append_byte(arena, &out, 0);
    return (const char *)out.data;
}

static void semantic_add_program_symbols(Program *prog, Scope *base, Vec_string8 *structs, memops_arena *arena);

static void semantic_add_import_symbols(Program *prog, Scope *base, Vec_string8 *structs, memops_arena *arena) {
    for (i32 i = 0; i < prog->i_imports.length; i++) {
        const char *path = resolve_import_path(arena, prog->i_imports.data[i]);
        string8 input = string8_read_file(arena, path);
        if (!input.data) {
            printf("%s:0:0: semantic error: failed to read import %s\n", g_source_path, path);
            exit(1);
        }

        Vec_Token tokens = {0};
        lex_tokens(arena, input, &tokens);

        Parser parser = {0};
        parser.arena = arena;
        parser.tokens = tokens;
        parser.index = 0;

        Program imported = parse_program(&parser);
        imported.preprocessor_lines = collect_preprocessor_lines(arena, input);
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
        Vec_string8_append(arena, &base->globals, concat_name2(arena, decl->name, "_", string8_from_cstr(arena, "reflect")));
    }

    for (i32 i = 0; i < prog->enums.length; i++) {
        EnumDecl *decl = (EnumDecl *)prog->enums.data[i];
        if (!scope_has(structs, decl->name)) {
            Vec_string8_append(arena, structs, decl->name);
        }
        Vec_string8_append(arena, &base->globals, concat_name2(arena, decl->name, "_", string8_from_cstr(arena, "reflect")));
        for (i32 j = 0; j < decl->items.length; j++) {
            EnumItem *item = (EnumItem *)decl->items.data[j];
            Vec_string8_append(arena, &base->globals, concat_name2(arena, decl->name, "_", item->name));
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

static void semantic_check_expr(Expr *e, Scope *scope);

static void semantic_check_expr(Expr *e, Scope *scope) {
    if (!e) return;
    if (e->kind == Expr_Number) return;
    if (e->kind == Expr_String) return;
    if (e->kind == Expr_SizeofType) return;
    if (e->kind == Expr_AlignofType) return;
    if (e->kind == Expr_ZeroInit) return;
    if (e->kind == Expr_InitList) {
        for (i32 i = 0; i < e->args.length; i++) {
            if (e->designator_kinds.data[i] == InitDesignator_Index) {
                semantic_check_expr((Expr *)e->designators.data[i], scope);
            }
            semantic_check_expr((Expr *)e->args.data[i], scope);
        }
        return;
    }
    if (e->kind == Expr_Name) {
        if (string8_equals_cstr(&e->name, "null")) return;
        if (scope_has(&scope->locals, e->name)) return;
        if (scope_has(&scope->globals, e->name)) return;
        if (scope_has(&scope->procs, e->name)) return;
        semantic_error_name("use of undeclared identifier", e->name, e->line, e->col);
    }
    if (e->kind == Expr_Addr) {
        semantic_check_expr(e->inner, scope);
        return;
    }
    if (e->kind == Expr_Index) {
        semantic_check_expr(e->base, scope);
        semantic_check_expr(e->index_expr, scope);
        return;
    }
    if (e->kind == Expr_Field) {
        semantic_check_expr(e->base, scope);
        return;
    }
    if (e->kind == Expr_Cast) {
        semantic_check_expr(e->inner, scope);
        return;
    }
    if (e->kind == Expr_Binary) {
        semantic_check_expr(e->left, scope);
        semantic_check_expr(e->right, scope);
        return;
    }
    if (e->kind == Expr_Ternary) {
        semantic_check_expr(e->left, scope);
        semantic_check_expr(e->right, scope);
        semantic_check_expr(e->third, scope);
        return;
    }
    if (e->kind == Expr_Call) {
        if (string8_equals_cstr(&e->name, "printf")) {
            for (i32 i = 0; i < e->args.length; i++) {
                semantic_check_expr((Expr *)e->args.data[i], scope);
            }
            return;
        }
        if (string8_equals_cstr(&e->name, "sizeof")) {
            if (e->args.length != 1) {
                semantic_error("sizeof expects exactly 1 argument", e->line, e->col);
            }
            semantic_check_expr((Expr *)e->args.data[0], scope);
            return;
        }
        if (string8_equals_cstr(&e->name, "alignof")) {
            if (e->args.length != 1) {
                semantic_error("alignof expects exactly 1 argument", e->line, e->col);
            }
            semantic_check_expr((Expr *)e->args.data[0], scope);
            return;
        }
        // Imported C headers are not lowered into the I symbol table. Let
        // unknown calls pass through so macro-heavy C APIs like va_start(),
        // FAILED(), and COM helper wrappers can still be emitted unchanged.
        for (i32 i = 0; i < e->args.length; i++) {
            semantic_check_expr((Expr *)e->args.data[i], scope);
        }
        return;
    }
}

static void semantic_check_stmt(Stmt *stmt, Scope *scope, memops_arena *arena);

static void semantic_check_stmt(Stmt *stmt, Scope *scope, memops_arena *arena) {
    if (stmt->kind == Stmt_Var) {
        if (stmt->expr) semantic_check_expr(stmt->expr, scope);
        if (scope_has(&scope->locals, stmt->name)) {
            semantic_error_name("duplicate local declaration", stmt->name, stmt->line, stmt->col);
        }
        Vec_string8_append(arena, &scope->locals, stmt->name);
        return;
    }
    if (stmt->kind == Stmt_Assign) {
        if (stmt->lhs && stmt->lhs->kind == Expr_Name) {
            if (!scope_has(&scope->locals, stmt->name) && !scope_has(&scope->globals, stmt->name)) {
                semantic_error_name("assignment to undeclared identifier", stmt->name, stmt->line, stmt->col);
            }
        }
        semantic_check_expr(stmt->lhs, scope);
        semantic_check_expr(stmt->expr, scope);
        return;
    }
    if (stmt->kind == Stmt_Return || stmt->kind == Stmt_Expr) {
        semantic_check_expr(stmt->expr, scope);
        return;
    }
    if (stmt->kind == Stmt_For) {
        Scope loop_scope = *scope;
        loop_scope.locals = Vec_string8_reserve(arena, scope->locals.length + 16);
        for (i32 i = 0; i < scope->locals.length; i++) {
            Vec_string8_append(arena, &loop_scope.locals, scope->locals.data[i]);
        }
        if (stmt->for_init) semantic_check_stmt(stmt->for_init, &loop_scope, arena);
        if (stmt->for_cond) semantic_check_expr(stmt->for_cond, &loop_scope);
        if (stmt->for_step) semantic_check_stmt(stmt->for_step, &loop_scope, arena);
        for (i32 i = 0; i < stmt->for_body.length; i++) {
            semantic_check_stmt((Stmt *)stmt->for_body.data[i], &loop_scope, arena);
        }
        return;
    }
    if (stmt->kind == Stmt_While) {
        semantic_check_expr(stmt->while_cond, scope);
        Scope loop_scope = *scope;
        loop_scope.locals = Vec_string8_reserve(arena, scope->locals.length + 16);
        for (i32 i = 0; i < scope->locals.length; i++) {
            Vec_string8_append(arena, &loop_scope.locals, scope->locals.data[i]);
        }
        for (i32 i = 0; i < stmt->while_body.length; i++) {
            semantic_check_stmt((Stmt *)stmt->while_body.data[i], &loop_scope, arena);
        }
        return;
    }
    if (stmt->kind == Stmt_DoWhile) {
        Scope loop_scope = *scope;
        loop_scope.locals = Vec_string8_reserve(arena, scope->locals.length + 16);
        for (i32 i = 0; i < scope->locals.length; i++) {
            Vec_string8_append(arena, &loop_scope.locals, scope->locals.data[i]);
        }
        for (i32 i = 0; i < stmt->while_body.length; i++) {
            semantic_check_stmt((Stmt *)stmt->while_body.data[i], &loop_scope, arena);
        }
        semantic_check_expr(stmt->while_cond, &loop_scope);
        return;
    }
    if (stmt->kind == Stmt_Break || stmt->kind == Stmt_Continue) {
        return;
    }
    if (stmt->kind == Stmt_Switch) {
        semantic_check_expr(stmt->switch_expr, scope);
        for (i32 i = 0; i < stmt->switch_cases.length; i++) {
            SwitchCase *sc = (SwitchCase *)stmt->switch_cases.data[i];
            semantic_check_expr(sc->expr, scope);
            Scope case_scope = *scope;
            case_scope.locals = Vec_string8_reserve(arena, scope->locals.length + 16);
            for (i32 j = 0; j < scope->locals.length; j++) {
                Vec_string8_append(arena, &case_scope.locals, scope->locals.data[j]);
            }
            for (i32 j = 0; j < sc->body.length; j++) {
                semantic_check_stmt((Stmt *)sc->body.data[j], &case_scope, arena);
            }
        }
        Scope default_scope = *scope;
        default_scope.locals = Vec_string8_reserve(arena, scope->locals.length + 16);
        for (i32 i = 0; i < scope->locals.length; i++) {
            Vec_string8_append(arena, &default_scope.locals, scope->locals.data[i]);
        }
        for (i32 i = 0; i < stmt->switch_default_body.length; i++) {
            semantic_check_stmt((Stmt *)stmt->switch_default_body.data[i], &default_scope, arena);
        }
        return;
    }
    if (stmt->kind == Stmt_If) {
        semantic_check_expr(stmt->if_cond, scope);

        Scope then_scope = *scope;
        then_scope.locals = Vec_string8_reserve(arena, scope->locals.length + 16);
        for (i32 i = 0; i < scope->locals.length; i++) {
            Vec_string8_append(arena, &then_scope.locals, scope->locals.data[i]);
        }
        for (i32 i = 0; i < stmt->if_then_body.length; i++) {
            semantic_check_stmt((Stmt *)stmt->if_then_body.data[i], &then_scope, arena);
        }

        if (stmt->if_else_if) {
            Scope else_if_scope = *scope;
            else_if_scope.locals = Vec_string8_reserve(arena, scope->locals.length + 16);
            for (i32 i = 0; i < scope->locals.length; i++) {
                Vec_string8_append(arena, &else_if_scope.locals, scope->locals.data[i]);
            }
            semantic_check_stmt(stmt->if_else_if, &else_if_scope, arena);
        } else {
            Scope else_scope = *scope;
            else_scope.locals = Vec_string8_reserve(arena, scope->locals.length + 16);
            for (i32 i = 0; i < scope->locals.length; i++) {
                Vec_string8_append(arena, &else_scope.locals, scope->locals.data[i]);
            }
            for (i32 i = 0; i < stmt->if_else_body.length; i++) {
                semantic_check_stmt((Stmt *)stmt->if_else_body.data[i], &else_scope, arena);
            }
        }
        return;
    }
    semantic_error("unknown statement kind", stmt->line, stmt->col);
}

static void semantic_check_proc(ProcDecl *proc, Scope *base_scope, memops_arena *arena) {
    Scope scope = *base_scope;
    scope.locals = Vec_string8_reserve(arena, 32);

    for (i32 i = 0; i < proc->params.length; i++) {
        Param *param = (Param *)proc->params.data[i];
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
    }

    for (i32 i = 0; i < proc->body.length; i++) {
        Stmt *stmt = (Stmt *)proc->body.data[i];
        semantic_check_stmt(stmt, &scope, arena);
    }
}

static void semantic_check_program(Program *prog, memops_arena *arena) {
    Scope base = {0};
    base.globals = Vec_string8_reserve(arena, 64);
    base.procs = Vec_string8_reserve(arena, 64);
    Vec_string8 structs = Vec_string8_reserve(arena, 64);

    semantic_add_import_symbols(prog, &base, &structs, arena);

    for (i32 i = 0; i < prog->preprocessor_lines.length; i++) {
        string8 define_name = preprocessor_define_name(arena, prog->preprocessor_lines.data[i]);
        if (define_name.data) {
            Vec_string8_append(arena, &base.globals, define_name);
        }
    }

    for (i32 i = 0; i < prog->aliases.length; i++) {
        AliasDecl *decl = (AliasDecl *)prog->aliases.data[i];
        if (scope_has(&structs, decl->name)) {
            semantic_error_name("duplicate type alias", decl->name, decl->line, decl->col);
        }
        Vec_string8_append(arena, &structs, decl->name);
    }

    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (scope_has(&structs, decl->name)) {
            i32 prev_line = decl->line;
            i32 prev_col = decl->col;
            for (i32 j = 0; j < i; j++) {
                StructDecl *prev = (StructDecl *)prog->structs.data[j];
                if (string8_equals(&prev->name, &decl->name)) {
                    prev_line = prev->line;
                    prev_col = prev->col;
                    break;
                }
            }
            semantic_error_name_dup("duplicate struct declaration", decl->name, decl->line, decl->col, prev_line, prev_col);
        }
        Vec_string8_append(arena, &structs, decl->name);
        Vec_string8_append(arena, &base.globals, concat_name2(arena, decl->name, "_", string8_from_cstr(arena, "reflect")));
    }

    for (i32 i = 0; i < prog->enums.length; i++) {
        EnumDecl *decl = (EnumDecl *)prog->enums.data[i];
        if (scope_has(&structs, decl->name)) {
            i32 prev_line = decl->line;
            i32 prev_col = decl->col;
            for (i32 j = 0; j < i; j++) {
                EnumDecl *prev = (EnumDecl *)prog->enums.data[j];
                if (string8_equals(&prev->name, &decl->name)) {
                    prev_line = prev->line;
                    prev_col = prev->col;
                    break;
                }
            }
            semantic_error_name_dup("duplicate enum declaration", decl->name, decl->line, decl->col, prev_line, prev_col);
        }
        Vec_string8_append(arena, &structs, decl->name);
        Vec_string8_append(arena, &base.globals, concat_name2(arena, decl->name, "_", string8_from_cstr(arena, "reflect")));

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
                semantic_error_name_dup("duplicate enum item", item->name, item->line, item->col, prev_line, prev_col);
            }
            Vec_string8_append(arena, &enum_items, item->name);

            string8 c_name = concat_name2(arena, decl->name, "_", item->name);
            Vec_string8_append(arena, &base.globals, c_name);
        }
    }

    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        if (scope_has(&base.procs, decl->name)) {
            i32 prev_line = decl->line;
            i32 prev_col = decl->col;
            for (i32 j = 0; j < i; j++) {
                ProcDecl *prev = (ProcDecl *)prog->procs.data[j];
                if (string8_equals(&prev->name, &decl->name)) {
                    prev_line = prev->line;
                    prev_col = prev->col;
                    break;
                }
            }
            semantic_error_name_dup("duplicate proc declaration", decl->name, decl->line, decl->col, prev_line, prev_col);
        }
        Vec_string8_append(arena, &base.procs, decl->name);
    }

    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *decl = (Stmt *)prog->globals.data[i];
        if (scope_has(&base.globals, decl->name)) {
            i32 prev_line = decl->line;
            i32 prev_col = decl->col;
            for (i32 j = 0; j < i; j++) {
                Stmt *prev = (Stmt *)prog->globals.data[j];
                if (string8_equals(&prev->name, &decl->name)) {
                    prev_line = prev->line;
                    prev_col = prev->col;
                    break;
                }
            }
            semantic_error_name_dup("duplicate global declaration", decl->name, decl->line, decl->col, prev_line, prev_col);
        }
        Vec_string8_append(arena, &base.globals, decl->name);
    }

    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *decl = (Stmt *)prog->globals.data[i];
        if (decl->expr) semantic_check_expr(decl->expr, &base);
    }

    for (i32 i = 0; i < prog->procs.length; i++) {
        semantic_check_proc((ProcDecl *)prog->procs.data[i], &base, arena);
    }
}

typedef struct TypeSub {
    bool has;
    string8 param;
    TypeExpr *arg;
} TypeSub;

static bool string8_equals_name(string8 a, string8 b) {
    return string8_equals(&a, &b);
}

static bool string8_is_symbolic_type_name(string8 s) {
    if (!s.data || s.length == 0) return false;
    return (s.data[0] >= 'A' && s.data[0] <= 'Z');
}

static string8 type_mangle(memops_arena *arena, TypeExpr *type, TypeSub sub);

static void emit_cstr(memops_arena *arena, string8 *out, const char *cstr) {
    string8_append_cstr(arena, out, cstr);
}

static void emit_string8(memops_arena *arena, string8 *out, string8 s) {
    string8_append_bytes(arena, out, s.data, s.length);
}

static void emit_type(memops_arena *arena, string8 *out, TypeExpr *type, TypeSub sub) {
    if (type->kind == Type_Name) {
        if (sub.has && string8_equals_name(type->name, sub.param)) {
            if (type->is_const) emit_cstr(arena, out, "const ");
            emit_type(arena, out, sub.arg, (TypeSub){0});
            return;
        }
        if (type->is_const) emit_cstr(arena, out, "const ");
        emit_string8(arena, out, type->name);
        return;
    }
    if (type->kind == Type_Ptr) {
        emit_type(arena, out, type->elem, sub);
        emit_cstr(arena, out, type->is_const ? " * const" : " *");
        return;
    }
    if (type->kind == Type_Generic) {
        if (type->is_const) emit_cstr(arena, out, "const ");
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

static string8 type_mangle(memops_arena *arena, TypeExpr *type, TypeSub sub) {
    string8 out = string8_reserve(arena, 64);
    if (type->is_const) {
        emit_cstr(arena, &out, "const_");
    }
    if (type->kind == Type_Name) {
        if (sub.has && string8_equals_name(type->name, sub.param)) {
            return type_mangle(arena, sub.arg, (TypeSub){0});
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

static bool array_string8_contains(Vec_string8 *arr, string8 value) {
    for (i32 i = 0; i < arr->length; i++) {
        if (string8_equals(&arr->data[i], &value)) {
            return true;
        }
    }
    return false;
}

static void collect_type_instances(TypeExpr *type, string8 base, Vec_string8 *out, memops_arena *arena) {
    if (!type) return;
    if (type->kind == Type_Generic && string8_equals_name(type->name, base)) {
        if (type->args.length == 1) {
            TypeExpr *arg = (TypeExpr *)type->args.data[0];
            if (arg->kind == Type_Name && string8_is_symbolic_type_name(arg->name)) {
                return;
            }
            string8 mangle = type_mangle(arena, arg, (TypeSub){0});
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

static void collect_type_instances_from_stmt(Stmt *s, string8 base, Vec_string8 *out, memops_arena *arena);
static void collect_type_instances_from_expr(Expr *e, string8 base, Vec_string8 *out, memops_arena *arena);

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
    } else if (s->kind == Stmt_DoWhile) {
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
        for (i32 i = 0; i < e->type_args.length; i++) {
            collect_type_instances((TypeExpr *)e->type_args.data[i], base, out, arena);
        }
        for (i32 i = 0; i < e->args.length; i++) {
            collect_type_instances_from_expr((Expr *)e->args.data[i], base, out, arena);
        }
    } else if (e->kind == Expr_Field) {
        collect_type_instances_from_expr(e->base, base, out, arena);
    } else if (e->kind == Expr_SizeofType || e->kind == Expr_AlignofType) {
        collect_type_instances(e->cast_type, base, out, arena);
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

static void collect_generic_struct_instances(Program *prog, StructDecl *decl, Vec_string8 *out, memops_arena *arena) {
    for (i32 i = 0; i < prog->aliases.length; i++) {
        AliasDecl *a = (AliasDecl *)prog->aliases.data[i];
        collect_type_instances(a->type, decl->name, out, arena);
    }

    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *s = (Stmt *)prog->globals.data[i];
        collect_type_instances(s->type, decl->name, out, arena);
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
    }
}

typedef struct GenericProcEntry {
    ProcDecl *decl;
    Vec_string8 instances; // mangled concrete type args
} GenericProcEntry;

static GenericProcEntry *generic_entry_from_name(Vec_voidptr *entries, string8 name) {
    for (i32 i = 0; i < entries->length; i++) {
        GenericProcEntry *e = (GenericProcEntry *)entries->data[i];
        if (string8_equals(&e->decl->name, &name)) return e;
    }
    return null;
}

static bool type_is_concrete_under_sub(TypeExpr *type, TypeSub sub) {
    if (!type) return false;
    if (type->kind == Type_Name) {
        if (sub.has && string8_equals(&type->name, &sub.param)) {
            return type_is_concrete_under_sub(sub.arg, (TypeSub){0});
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

static bool generic_entry_add_instance(memops_arena *arena, GenericProcEntry *entry, string8 mangle) {
    if (array_string8_contains(&entry->instances, mangle)) return false;
    Vec_string8_append(arena, &entry->instances, mangle);
    return true;
}

static bool collect_generic_calls_from_expr(
    Expr *e,
    TypeSub sub,
    Vec_voidptr *entries,
    memops_arena *arena
) {
    bool changed = false;
    if (!e) return false;
    if (e->kind == Expr_Call) {
        if (e->type_args.length == 1) {
            GenericProcEntry *target = generic_entry_from_name(entries, e->name);
            if (target) {
                TypeExpr *arg = (TypeExpr *)e->type_args.data[0];
                if (type_is_concrete_under_sub(arg, sub)) {
                    string8 mangle = type_mangle(arena, arg, sub);
                    if (generic_entry_add_instance(arena, target, mangle)) {
                        changed = true;
                    }
                }
            }
        }
        for (i32 i = 0; i < e->args.length; i++) {
            if (collect_generic_calls_from_expr((Expr *)e->args.data[i], sub, entries, arena)) changed = true;
        }
        return changed;
    }
    if (e->kind == Expr_Addr || e->kind == Expr_Cast || e->kind == Expr_Unary) {
        return collect_generic_calls_from_expr(e->inner, sub, entries, arena);
    }
    if (e->kind == Expr_Field) {
        return collect_generic_calls_from_expr(e->base, sub, entries, arena);
    }
    if (e->kind == Expr_Index) {
        bool c0 = collect_generic_calls_from_expr(e->base, sub, entries, arena);
        bool c1 = collect_generic_calls_from_expr(e->index_expr, sub, entries, arena);
        return c0 || c1;
    }
    if (e->kind == Expr_Binary) {
        bool c0 = collect_generic_calls_from_expr(e->left, sub, entries, arena);
        bool c1 = collect_generic_calls_from_expr(e->right, sub, entries, arena);
        return c0 || c1;
    }
    if (e->kind == Expr_Ternary) {
        bool c0 = collect_generic_calls_from_expr(e->left, sub, entries, arena);
        bool c1 = collect_generic_calls_from_expr(e->right, sub, entries, arena);
        bool c2 = collect_generic_calls_from_expr(e->third, sub, entries, arena);
        return c0 || c1 || c2;
    }
    return false;
}

static bool collect_generic_calls_from_stmt(
    Stmt *s,
    TypeSub sub,
    Vec_voidptr *entries,
    memops_arena *arena
) {
    if (!s) return false;
    bool changed = false;
    if (s->kind == Stmt_Var) {
        if (collect_generic_calls_from_expr(s->expr, sub, entries, arena)) changed = true;
        return changed;
    }
    if (s->kind == Stmt_Assign) {
        if (collect_generic_calls_from_expr(s->lhs, sub, entries, arena)) changed = true;
        if (collect_generic_calls_from_expr(s->expr, sub, entries, arena)) changed = true;
        return changed;
    }
    if (s->kind == Stmt_Return || s->kind == Stmt_Expr) {
        return collect_generic_calls_from_expr(s->expr, sub, entries, arena);
    }
    if (s->kind == Stmt_For) {
        if (collect_generic_calls_from_stmt(s->for_init, sub, entries, arena)) changed = true;
        if (collect_generic_calls_from_expr(s->for_cond, sub, entries, arena)) changed = true;
        if (collect_generic_calls_from_stmt(s->for_step, sub, entries, arena)) changed = true;
        for (i32 i = 0; i < s->for_body.length; i++) {
            if (collect_generic_calls_from_stmt((Stmt *)s->for_body.data[i], sub, entries, arena)) changed = true;
        }
        return changed;
    }
    if (s->kind == Stmt_If) {
        if (collect_generic_calls_from_expr(s->if_cond, sub, entries, arena)) changed = true;
        for (i32 i = 0; i < s->if_then_body.length; i++) {
            if (collect_generic_calls_from_stmt((Stmt *)s->if_then_body.data[i], sub, entries, arena)) changed = true;
        }
        if (s->if_else_if) {
            if (collect_generic_calls_from_stmt(s->if_else_if, sub, entries, arena)) changed = true;
        } else {
            for (i32 i = 0; i < s->if_else_body.length; i++) {
                if (collect_generic_calls_from_stmt((Stmt *)s->if_else_body.data[i], sub, entries, arena)) changed = true;
            }
        }
        return changed;
    }
    if (s->kind == Stmt_While) {
        if (collect_generic_calls_from_expr(s->while_cond, sub, entries, arena)) changed = true;
        for (i32 i = 0; i < s->while_body.length; i++) {
            if (collect_generic_calls_from_stmt((Stmt *)s->while_body.data[i], sub, entries, arena)) changed = true;
        }
        return changed;
    }
    if (s->kind == Stmt_DoWhile) {
        for (i32 i = 0; i < s->while_body.length; i++) {
            if (collect_generic_calls_from_stmt((Stmt *)s->while_body.data[i], sub, entries, arena)) changed = true;
        }
        if (collect_generic_calls_from_expr(s->while_cond, sub, entries, arena)) changed = true;
        return changed;
    }
    if (s->kind == Stmt_Switch) {
        if (collect_generic_calls_from_expr(s->switch_expr, sub, entries, arena)) changed = true;
        for (i32 i = 0; i < s->switch_cases.length; i++) {
            SwitchCase *sc = (SwitchCase *)s->switch_cases.data[i];
            if (collect_generic_calls_from_expr(sc->expr, sub, entries, arena)) changed = true;
            for (i32 j = 0; j < sc->body.length; j++) {
                if (collect_generic_calls_from_stmt((Stmt *)sc->body.data[j], sub, entries, arena)) changed = true;
            }
        }
        for (i32 i = 0; i < s->switch_default_body.length; i++) {
            if (collect_generic_calls_from_stmt((Stmt *)s->switch_default_body.data[i], sub, entries, arena)) changed = true;
        }
        return changed;
    }
    return false;
}

static void collect_generic_proc_instances(Program *prog, ProcDecl *decl, Vec_string8 *out, memops_arena *arena) {
    Vec_voidptr entries = ptr_array_reserve(arena, 32);
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *p = (ProcDecl *)prog->procs.data[i];
        if (!p->is_generic) continue;
        GenericProcEntry *entry = memops_arena_push_struct(arena, GenericProcEntry);
        memset(entry, 0, sizeof(*entry));
        entry->decl = p;
        entry->instances = Vec_string8_reserve(arena, 4);
        ptr_array_append(arena, &entries, entry);
    }

    // Seed from non-generic contexts only.
    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *s = (Stmt *)prog->globals.data[i];
        collect_generic_calls_from_stmt(s, (TypeSub){0}, &entries, arena);
    }
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *p = (ProcDecl *)prog->procs.data[i];
        if (p->is_generic) continue;
        for (i32 j = 0; j < p->body.length; j++) {
            Stmt *s = (Stmt *)p->body.data[j];
            collect_generic_calls_from_stmt(s, (TypeSub){0}, &entries, arena);
        }
    }

    // Closure: discovered generic instances can induce further generic calls.
    bool changed = true;
    while (changed) {
        changed = false;
        for (i32 i = 0; i < entries.length; i++) {
            GenericProcEntry *entry = (GenericProcEntry *)entries.data[i];
            for (i32 j = 0; j < entry->instances.length; j++) {
                TypeExpr *arg = type_new(arena, Type_Name);
                arg->name = entry->instances.data[j];
                TypeSub sub = {0};
                sub.has = true;
                sub.param = entry->decl->type_param;
                sub.arg = arg;
                for (i32 k = 0; k < entry->decl->body.length; k++) {
                    Stmt *s = (Stmt *)entry->decl->body.data[k];
                    if (collect_generic_calls_from_stmt(s, sub, &entries, arena)) {
                        changed = true;
                    }
                }
            }
        }
    }

    GenericProcEntry *target = generic_entry_from_name(&entries, decl->name);
    if (!target) return;
    for (i32 i = 0; i < target->instances.length; i++) {
        string8 mangle = target->instances.data[i];
        if (!array_string8_contains(out, mangle)) {
            Vec_string8_append(arena, out, mangle);
        }
    }
}

static bool program_has_proc_named(Program *prog, string8 name) {
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *p = (ProcDecl *)prog->procs.data[i];
        if (string8_equals(&p->name, &name)) {
            return true;
        }
    }
    return false;
}

static bool has_requirement_for_instance(Program *prog, memops_arena *arena, string8 requirement, string8 type_mangle) {
    string8 base = requirement;
    if (string8_equals_cstr(&requirement, "hashable")) {
        base = string8_from_cstr(arena, "hash");
    }

    string8 required = string8_reserve(arena, base.length + 1 + type_mangle.length);
    string8_append_bytes(arena, &required, base.data, base.length);
    string8_append_cstr(arena, &required, "_");
    string8_append_bytes(arena, &required, type_mangle.data, type_mangle.length);

    if (program_has_proc_named(prog, required)) return true;
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *p = (ProcDecl *)prog->procs.data[i];
        if (p->is_generic && string8_equals(&p->name, &base)) {
            return true;
        }
    }

    return false;
}

static void validate_generic_constraints(Program *prog, memops_arena *arena) {
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        if (!decl->is_generic || !decl->constraint.data || decl->constraint.length == 0) {
            continue;
        }

        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        collect_generic_proc_instances(prog, decl, &instances, arena);
        for (i32 j = 0; j < instances.length; j++) {
            string8 mangle = instances.data[j];

            if (!has_requirement_for_instance(prog, arena, decl->constraint, mangle)) {
                string8 base = decl->constraint;
                if (string8_equals_cstr(&decl->constraint, "hashable")) {
                    base = string8_from_cstr(arena, "hash");
                }
                printf(
                    "requirement error at %d:%d: proc '%.*s' requires '%.*s' for type '%.*s' (missing function '%.*s_%.*s')\n",
                    decl->line, decl->col,
                    (int)decl->name.length, decl->name.data,
                    (int)decl->constraint.length, decl->constraint.data,
                    (int)mangle.length, mangle.data,
                    (int)base.length, base.data,
                    (int)mangle.length, mangle.data
                );
                exit(1);
            }
        }
    }
}

typedef struct TypeScope {
    Vec_string8 names;
    Vec_voidptr types; // TypeExpr*
} TypeScope;

static TypeScope type_scope_make(memops_arena *arena, i32 cap) {
    TypeScope s = {0};
    s.names = Vec_string8_reserve(arena, cap);
    s.types = ptr_array_reserve(arena, cap);
    return s;
}

static TypeScope type_scope_copy(memops_arena *arena, TypeScope *src) {
    TypeScope dst = type_scope_make(arena, src->names.length + 8);
    for (i32 i = 0; i < src->names.length; i++) {
        Vec_string8_append(arena, &dst.names, src->names.data[i]);
        ptr_array_append(arena, &dst.types, src->types.data[i]);
    }
    return dst;
}

static void type_scope_add(memops_arena *arena, TypeScope *s, string8 name, TypeExpr *type) {
    Vec_string8_append(arena, &s->names, name);
    ptr_array_append(arena, &s->types, type);
}

static TypeExpr *type_scope_lookup(TypeScope *s, string8 name) {
    for (i32 i = s->names.length - 1; i >= 0; i--) {
        if (string8_equals(&s->names.data[i], &name)) {
            return (TypeExpr *)s->types.data[i];
        }
    }
    return null;
}

static TypeExpr *type_name_expr(memops_arena *arena, const char *name) {
    TypeExpr *t = type_new(arena, Type_Name);
    t->name = string8_from_cstr(arena, name);
    return t;
}

static TypeExpr *clone_type_expr(memops_arena *arena, TypeExpr *src) {
    if (!src) return null;
    TypeExpr *dst = type_new(arena, src->kind);
    dst->name = src->name;
    dst->array_count = src->array_count;
    dst->is_const = src->is_const;
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
    return dst;
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
    return dst;
}

static TypeExpr *lookup_field_type(Program *prog, TypeExpr *base_type, string8 field_name, memops_arena *arena) {
    if (!base_type) return null;
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (base_type->kind == Type_Name && !decl->is_generic) {
            if (!string8_equals(&decl->name, &base_type->name)) continue;
            for (i32 f = 0; f < decl->fields.length; f++) {
                Field *field = (Field *)decl->fields.data[f];
                if (string8_equals(&field->name, &field_name)) {
                    return clone_type_expr(arena, field->type);
                }
            }
        }
        if (base_type->kind == Type_Generic && decl->is_generic) {
            if (!string8_equals(&decl->name, &base_type->name)) continue;
            if (base_type->args.length != 1) return null;
            TypeExpr *arg = (TypeExpr *)base_type->args.data[0];
            for (i32 f = 0; f < decl->fields.length; f++) {
                Field *field = (Field *)decl->fields.data[f];
                if (string8_equals(&field->name, &field_name)) {
                    return substitute_type_param(arena, field->type, decl->type_param, arg);
                }
            }
        }
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
        return type_scope_lookup(scope, e->name);
    }
    if (e->kind == Expr_Number) {
        string8 n = e->number;
        if (n.length > 3 &&
            n.data[n.length - 3] == 'u' &&
            n.data[n.length - 2] == '6' &&
            n.data[n.length - 1] == '4') {
            return type_name_expr(arena, "u64");
        }
        return type_name_expr(arena, "i32");
    }
    if (e->kind == Expr_String) {
        TypeExpr *char_t = type_name_expr(arena, "char");
        TypeExpr *ptr_t = type_new(arena, Type_Ptr);
        ptr_t->elem = char_t;
        return ptr_t;
    }
    if (e->kind == Expr_SizeofType || e->kind == Expr_AlignofType) {
        return type_name_expr(arena, "usize");
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
        return e->cast_type;
    }
    if (e->kind == Expr_Index) {
        TypeExpr *base = infer_expr_type(e->base, scope, prog, arena);
        if (base && base->kind == Type_Ptr) return base->elem;
        return null;
    }
    if (e->kind == Expr_Field) {
        TypeExpr *base = infer_expr_type(e->base, scope, prog, arena);
        return lookup_field_type(prog, base, e->name, arena);
    }
    if (e->kind == Expr_Ternary) {
        return infer_expr_type(e->right, scope, prog, arena);
    }
    if (e->kind == Expr_Call) {
        if (string8_equals_cstr(&e->name, "sizeof") || string8_equals_cstr(&e->name, "alignof")) {
            return type_name_expr(arena, "usize");
        }
    }
    if (e->kind == Expr_Binary) {
        return infer_expr_type(e->left, scope, prog, arena);
    }
    return null;
}

static const char *printf_spec_for_type(TypeExpr *type) {
    if (!type) return null;
    if (type->kind == Type_Ptr) return "%p";
    if (type->kind != Type_Name) return null;
    if (string8_equals_cstr(&type->name, "u8")) return "%d";
    if (string8_equals_cstr(&type->name, "u16")) return "%d";
    if (string8_equals_cstr(&type->name, "u32")) return "%u";
    if (string8_equals_cstr(&type->name, "u64")) return "%llu";
    if (string8_equals_cstr(&type->name, "i8")) return "%d";
    if (string8_equals_cstr(&type->name, "i16")) return "%d";
    if (string8_equals_cstr(&type->name, "i32")) return "%d";
    if (string8_equals_cstr(&type->name, "i64")) return "%lld";
    if (string8_equals_cstr(&type->name, "usize")) return "%zu";
    if (string8_equals_cstr(&type->name, "b32")) return "%d";
    if (string8_equals_cstr(&type->name, "f32")) return "%f";
    if (string8_equals_cstr(&type->name, "f64")) return "%f";
    if (string8_equals_cstr(&type->name, "char")) return "%c";
    return null;
}

static void rewrite_printf_call(Expr *call, TypeScope *scope, Program *prog, memops_arena *arena) {
    if (!call || call->kind != Expr_Call || call->args.length < 1) return;
    if (!string8_equals_cstr(&call->name, "printf")) return;

    Expr *fmt = (Expr *)call->args.data[0];
    if (!fmt || fmt->kind != Expr_String) return;

    string8 in = fmt->string_lit;
    if (in.length < 2 || in.data[0] != '"' || in.data[in.length - 1] != '"') {
        return;
    }

    i32 placeholder_count = 0;
    for (u64 i = 1; i + 1 < in.length; i++) {
        if (in.data[i] == '{' && (i + 1) < (in.length - 1) && in.data[i + 1] == '}') {
            placeholder_count++;
            i++;
        }
    }
    if (placeholder_count == 0) {
        return;
    }

    i32 value_count = call->args.length - 1;
    const char **specs = memops_arena_push(
        arena,
        sizeof(const char *) * (value_count > 0 ? value_count : 1),
        _Alignof(const char *)
    );
    for (i32 i = 0; i < value_count; i++) {
        Expr *arg = (Expr *)call->args.data[i + 1];
        TypeExpr *ty = infer_expr_type(arg, scope, prog, arena);
        const char *spec = printf_spec_for_type(ty);
        if (!spec) {
            printf("%s:%d:%d: format error: cannot infer '{}' format for printf arg %d\n", g_source_path, arg->line, arg->col, (int)(i + 1));
            exit(1);
        }
        specs[i] = spec;
    }

    string8 out = string8_reserve(arena, in.length + 32);
    string8_append_byte(arena, &out, '"');
    i32 used = 0;
    for (u64 i = 1; i + 1 < in.length; i++) {
        if (in.data[i] == '{' && (i + 1) < (in.length - 1) && in.data[i + 1] == '}') {
            if (used >= value_count) {
                printf("%s:%d:%d: format error: too many '{}' placeholders in printf format\n", g_source_path, fmt->line, fmt->col);
                exit(1);
            }
            string8_append_cstr(arena, &out, specs[used]);
            used++;
            i++;
            continue;
        }
        string8_append_byte(arena, &out, in.data[i]);
    }
    string8_append_byte(arena, &out, '"');

    if (used != value_count) {
        printf("%s:%d:%d: format error: printf placeholder count (%d) does not match arg count (%d)\n",
               g_source_path, fmt->line, fmt->col, used, value_count);
        exit(1);
    }

    fmt->string_lit = out;
}

static void rewrite_printf_in_expr(Expr *e, TypeScope *scope, Program *prog, memops_arena *arena);
static void rewrite_printf_in_stmt(Stmt *s, TypeScope *scope, Program *prog, memops_arena *arena);

static void rewrite_printf_in_expr(Expr *e, TypeScope *scope, Program *prog, memops_arena *arena) {
    if (!e) return;
    if (e->kind == Expr_Call) {
        rewrite_printf_call(e, scope, prog, arena);
        for (i32 i = 0; i < e->args.length; i++) {
            rewrite_printf_in_expr((Expr *)e->args.data[i], scope, prog, arena);
        }
        return;
    }
    if (e->kind == Expr_Binary) {
        rewrite_printf_in_expr(e->left, scope, prog, arena);
        rewrite_printf_in_expr(e->right, scope, prog, arena);
        return;
    }
    if (e->kind == Expr_Ternary) {
        rewrite_printf_in_expr(e->left, scope, prog, arena);
        rewrite_printf_in_expr(e->right, scope, prog, arena);
        rewrite_printf_in_expr(e->third, scope, prog, arena);
        return;
    }
    if (e->kind == Expr_Addr || e->kind == Expr_Cast || e->kind == Expr_Unary) {
        rewrite_printf_in_expr(e->inner, scope, prog, arena);
        return;
    }
    if (e->kind == Expr_Index) {
        rewrite_printf_in_expr(e->base, scope, prog, arena);
        rewrite_printf_in_expr(e->index_expr, scope, prog, arena);
        return;
    }
    if (e->kind == Expr_Field) {
        rewrite_printf_in_expr(e->base, scope, prog, arena);
        return;
    }
}

static void rewrite_printf_in_stmt(Stmt *s, TypeScope *scope, Program *prog, memops_arena *arena) {
    if (!s) return;
    if (s->kind == Stmt_Var) {
        rewrite_printf_in_expr(s->expr, scope, prog, arena);
        type_scope_add(arena, scope, s->name, s->type);
        return;
    }
    if (s->kind == Stmt_Assign || s->kind == Stmt_Expr || s->kind == Stmt_Return) {
        rewrite_printf_in_expr(s->expr, scope, prog, arena);
        return;
    }
    if (s->kind == Stmt_For) {
        TypeScope loop_scope = type_scope_copy(arena, scope);
        if (s->for_init) rewrite_printf_in_stmt(s->for_init, &loop_scope, prog, arena);
        if (s->for_cond) rewrite_printf_in_expr(s->for_cond, &loop_scope, prog, arena);
        if (s->for_step) rewrite_printf_in_stmt(s->for_step, &loop_scope, prog, arena);
        for (i32 i = 0; i < s->for_body.length; i++) {
            rewrite_printf_in_stmt((Stmt *)s->for_body.data[i], &loop_scope, prog, arena);
        }
        return;
    }
    if (s->kind == Stmt_If) {
        rewrite_printf_in_expr(s->if_cond, scope, prog, arena);
        TypeScope then_scope = type_scope_copy(arena, scope);
        for (i32 i = 0; i < s->if_then_body.length; i++) {
            rewrite_printf_in_stmt((Stmt *)s->if_then_body.data[i], &then_scope, prog, arena);
        }
        if (s->if_else_if) {
            TypeScope else_if_scope = type_scope_copy(arena, scope);
            rewrite_printf_in_stmt(s->if_else_if, &else_if_scope, prog, arena);
        } else {
            TypeScope else_scope = type_scope_copy(arena, scope);
            for (i32 i = 0; i < s->if_else_body.length; i++) {
                rewrite_printf_in_stmt((Stmt *)s->if_else_body.data[i], &else_scope, prog, arena);
            }
        }
        return;
    }
    if (s->kind == Stmt_While) {
        rewrite_printf_in_expr(s->while_cond, scope, prog, arena);
        TypeScope loop_scope = type_scope_copy(arena, scope);
        for (i32 i = 0; i < s->while_body.length; i++) {
            rewrite_printf_in_stmt((Stmt *)s->while_body.data[i], &loop_scope, prog, arena);
        }
        return;
    }
    if (s->kind == Stmt_DoWhile) {
        TypeScope loop_scope = type_scope_copy(arena, scope);
        for (i32 i = 0; i < s->while_body.length; i++) {
            rewrite_printf_in_stmt((Stmt *)s->while_body.data[i], &loop_scope, prog, arena);
        }
        rewrite_printf_in_expr(s->while_cond, &loop_scope, prog, arena);
        return;
    }
    if (s->kind == Stmt_Switch) {
        rewrite_printf_in_expr(s->switch_expr, scope, prog, arena);
        for (i32 i = 0; i < s->switch_cases.length; i++) {
            SwitchCase *sc = (SwitchCase *)s->switch_cases.data[i];
            rewrite_printf_in_expr(sc->expr, scope, prog, arena);
            TypeScope case_scope = type_scope_copy(arena, scope);
            for (i32 j = 0; j < sc->body.length; j++) {
                rewrite_printf_in_stmt((Stmt *)sc->body.data[j], &case_scope, prog, arena);
            }
        }
        TypeScope default_scope = type_scope_copy(arena, scope);
        for (i32 i = 0; i < s->switch_default_body.length; i++) {
            rewrite_printf_in_stmt((Stmt *)s->switch_default_body.data[i], &default_scope, prog, arena);
        }
        return;
    }
}

static void rewrite_printf_formats(Program *prog, memops_arena *arena) {
    TypeScope globals = type_scope_make(arena, 64);
    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *g = (Stmt *)prog->globals.data[i];
        rewrite_printf_in_expr(g->expr, &globals, prog, arena);
        type_scope_add(arena, &globals, g->name, g->type);
    }

    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *p = (ProcDecl *)prog->procs.data[i];
        TypeScope scope = type_scope_copy(arena, &globals);
        for (i32 j = 0; j < p->params.length; j++) {
            Param *param = (Param *)p->params.data[j];
            type_scope_add(arena, &scope, param->name, param->type);
        }
        for (i32 j = 0; j < p->body.length; j++) {
            rewrite_printf_in_stmt((Stmt *)p->body.data[j], &scope, prog, arena);
        }
    }
}

static void emit_expr(memops_arena *arena, string8 *out, Expr *e, TypeSub sub, string8 generic_name);
static void emit_stmt(memops_arena *arena, string8 *out, Stmt *s, TypeSub sub, string8 generic_name);

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
    if (type && type->kind == Type_Proc) {
        if (type->ret_type) emit_type(arena, out, type->ret_type, sub);
        else emit_cstr(arena, out, "void");
        emit_cstr(arena, out, " (");
        if (type->name.data) {
            emit_string8(arena, out, type->name);
            emit_cstr(arena, out, " ");
        }
        emit_cstr(arena, out, "*");
        emit_string8(arena, out, name);
        emit_cstr(arena, out, ")(");
        if (type->args.length == 0 && !type->is_variadic) {
            emit_cstr(arena, out, "void");
        } else {
            for (i32 i = 0; i < type->args.length; i++) {
                if (i > 0) emit_cstr(arena, out, ", ");
                emit_type(arena, out, (TypeExpr *)type->args.data[i], sub);
            }
            if (type->is_variadic) {
                if (type->args.length > 0) emit_cstr(arena, out, ", ");
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

static void emit_if_stmt(memops_arena *arena, string8 *out, Stmt *s, TypeSub sub, string8 generic_name) {
    emit_cstr(arena, out, "if (");
    emit_expr(arena, out, s->if_cond, sub, generic_name);
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
    if (e->kind == Expr_String) {
        emit_string8(arena, out, e->string_lit);
        return;
    }
    if (e->kind == Expr_SizeofType) {
        emit_cstr(arena, out, "sizeof(");
        emit_type(arena, out, e->cast_type, sub);
        emit_cstr(arena, out, ")");
        return;
    }
    if (e->kind == Expr_AlignofType) {
        emit_cstr(arena, out, "_Alignof(");
        emit_type(arena, out, e->cast_type, sub);
        emit_cstr(arena, out, ")");
        return;
    }
    if (e->kind == Expr_ZeroInit) {
        emit_cstr(arena, out, "{}");
        return;
    }
    if (e->kind == Expr_InitList) {
        emit_cstr(arena, out, "{");
        for (i32 i = 0; i < e->args.length; i++) {
            if (i > 0) emit_cstr(arena, out, ", ");
            Expr *designator = (Expr *)e->designators.data[i];
            if (designator) {
                if (e->designator_kinds.data[i] == InitDesignator_Field) {
                    emit_cstr(arena, out, ".");
                    emit_string8(arena, out, designator->name);
                    emit_cstr(arena, out, " = ");
                } else {
                    emit_cstr(arena, out, "[");
                    emit_expr(arena, out, designator, sub, generic_name);
                    emit_cstr(arena, out, "] = ");
                }
            }
            emit_expr(arena, out, (Expr *)e->args.data[i], sub, generic_name);
        }
        emit_cstr(arena, out, "}");
        return;
    }
    if (e->kind == Expr_Name) {
        if (string8_equals_cstr(&e->name, "null")) {
            emit_cstr(arena, out, "0");
            return;
        }
        emit_string8(arena, out, e->name);
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
        emit_string8(arena, out, e->name);
        return;
    }
    if (e->kind == Expr_Binary) {
        emit_cstr(arena, out, "(");
        emit_expr(arena, out, e->left, sub, generic_name);
        if (e->op == Token_Plus) {
            emit_cstr(arena, out, " + ");
        } else if (e->op == Token_Minus) {
            emit_cstr(arena, out, " - ");
        } else if (e->op == Token_Star) {
            emit_cstr(arena, out, " * ");
        } else if (e->op == Token_Slash) {
            emit_cstr(arena, out, " / ");
        } else if (e->op == Token_Percent) {
            emit_cstr(arena, out, " % ");
        } else if (e->op == Token_LAngle) {
            emit_cstr(arena, out, " < ");
        } else if (e->op == Token_RAngle) {
            emit_cstr(arena, out, " > ");
        } else if (e->op == Token_LessEqual) {
            emit_cstr(arena, out, " <= ");
        } else if (e->op == Token_GreaterEqual) {
            emit_cstr(arena, out, " >= ");
        } else if (e->op == Token_EqualEqual) {
            emit_cstr(arena, out, " == ");
        } else if (e->op == Token_BangEqual) {
            emit_cstr(arena, out, " != ");
        } else if (e->op == Token_Ampersand) {
            emit_cstr(arena, out, " & ");
        } else if (e->op == Token_Caret) {
            emit_cstr(arena, out, " ^ ");
        } else if (e->op == Token_Pipe) {
            emit_cstr(arena, out, " | ");
        } else if (e->op == Token_Keyword_And) {
            emit_cstr(arena, out, " && ");
        } else if (e->op == Token_Keyword_Or) {
            emit_cstr(arena, out, " || ");
        } else if (e->op == Token_Keyword_Shl) {
            emit_cstr(arena, out, " << ");
        } else if (e->op == Token_Keyword_Shr) {
            emit_cstr(arena, out, " >> ");
        } else {
            emit_cstr(arena, out, " /* unsupported op */ ");
        }
        emit_expr(arena, out, e->right, sub, generic_name);
        emit_cstr(arena, out, ")");
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
        if (e->type_args.length == 1) {
            TypeExpr *arg = (TypeExpr *)e->type_args.data[0];
            string8 mangle = type_mangle(arena, arg, sub);
            emit_mono_proc_name(arena, out, e->name, mangle);
        } else if (generic_name.data && string8_equals_name(e->name, generic_name) && sub.has) {
            emit_string8(arena, out, e->name);
            emit_cstr(arena, out, "_");
            string8 mangle = type_mangle(arena, sub.arg, (TypeSub){0});
            emit_string8(arena, out, mangle);
        } else {
            emit_string8(arena, out, e->name);
        }

        emit_cstr(arena, out, "(");
        for (i32 i = 0; i < e->args.length; i++) {
            if (i > 0) emit_cstr(arena, out, ", ");
            emit_expr(arena, out, (Expr *)e->args.data[i], sub, generic_name);
        }
        emit_cstr(arena, out, ")");
        return;
    }
}

static void emit_stmt(memops_arena *arena, string8 *out, Stmt *s, TypeSub sub, string8 generic_name) {
    if (!s) return;
    if (s->kind == Stmt_Var) {
        emit_decl(arena, out, s->type, s->name, sub);
        if (s->expr) {
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
            emit_expr(arena, out, s->expr, sub, generic_name);
        }
        emit_cstr(arena, out, ";\n");
        return;
    }
    if (s->kind == Stmt_Assign) {
        if (s->lhs) emit_expr(arena, out, s->lhs, sub, generic_name);
        else emit_string8(arena, out, s->name);
        if (s->assign_op == Token_Equal) emit_cstr(arena, out, " = ");
        else if (s->assign_op == Token_PlusEqual) emit_cstr(arena, out, " += ");
        else if (s->assign_op == Token_MinusEqual) emit_cstr(arena, out, " -= ");
        else if (s->assign_op == Token_StarEqual) emit_cstr(arena, out, " *= ");
        else if (s->assign_op == Token_SlashEqual) emit_cstr(arena, out, " /= ");
        else if (s->assign_op == Token_PercentEqual) emit_cstr(arena, out, " %= ");
        else if (s->assign_op == Token_AmpersandEqual) emit_cstr(arena, out, " &= ");
        else if (s->assign_op == Token_CaretEqual) emit_cstr(arena, out, " ^= ");
        else if (s->assign_op == Token_PipeEqual) emit_cstr(arena, out, " |= ");
        else emit_cstr(arena, out, " = ");
        emit_expr(arena, out, s->expr, sub, generic_name);
        emit_cstr(arena, out, ";\n");
        return;
    }
    if (s->kind == Stmt_Expr) {
        emit_expr(arena, out, s->expr, sub, generic_name);
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
                emit_expr(arena, out, s->for_init->expr, sub, generic_name);
            } else if (s->for_init->kind == Stmt_Expr) {
                emit_expr(arena, out, s->for_init->expr, sub, generic_name);
            }
        }
        emit_cstr(arena, out, "; ");
        if (s->for_cond) {
            emit_expr(arena, out, s->for_cond, sub, generic_name);
        }
        emit_cstr(arena, out, "; ");
        if (s->for_step) {
            if (s->for_step->kind == Stmt_Assign) {
                if (s->for_step->lhs) emit_expr(arena, out, s->for_step->lhs, sub, generic_name);
                else emit_string8(arena, out, s->for_step->name);
                if (s->for_step->assign_op == Token_Equal) emit_cstr(arena, out, " = ");
                else if (s->for_step->assign_op == Token_PlusEqual) emit_cstr(arena, out, " += ");
                else if (s->for_step->assign_op == Token_MinusEqual) emit_cstr(arena, out, " -= ");
                else if (s->for_step->assign_op == Token_StarEqual) emit_cstr(arena, out, " *= ");
                else if (s->for_step->assign_op == Token_SlashEqual) emit_cstr(arena, out, " /= ");
                else if (s->for_step->assign_op == Token_PercentEqual) emit_cstr(arena, out, " %= ");
                else if (s->for_step->assign_op == Token_AmpersandEqual) emit_cstr(arena, out, " &= ");
                else if (s->for_step->assign_op == Token_CaretEqual) emit_cstr(arena, out, " ^= ");
                else if (s->for_step->assign_op == Token_PipeEqual) emit_cstr(arena, out, " |= ");
                else emit_cstr(arena, out, " = ");
                emit_expr(arena, out, s->for_step->expr, sub, generic_name);
            } else if (s->for_step->kind == Stmt_Expr) {
                emit_expr(arena, out, s->for_step->expr, sub, generic_name);
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
        emit_expr(arena, out, s->while_cond, sub, generic_name);
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
        emit_expr(arena, out, s->while_cond, sub, generic_name);
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
            emit_cstr(arena, out, ":\n");
            for (i32 j = 0; j < sc->body.length; j++) {
                emit_cstr(arena, out, "        ");
                emit_stmt(arena, out, (Stmt *)sc->body.data[j], sub, generic_name);
            }
        }
        if (s->switch_default_body.length > 0) {
            emit_cstr(arena, out, "    default:\n");
            for (i32 i = 0; i < s->switch_default_body.length; i++) {
                emit_cstr(arena, out, "        ");
                emit_stmt(arena, out, (Stmt *)s->switch_default_body.data[i], sub, generic_name);
            }
        }
        emit_cstr(arena, out, "    }\n");
        return;
    }
    if (s->kind == Stmt_If) {
        emit_if_stmt(arena, out, s, sub, generic_name);
        return;
    }
}

static void emit_struct_decl(memops_arena *arena, string8 *out, StructDecl *decl) {
    emit_cstr(arena, out, decl->is_union ? "uniondef(" : "structdef(");
    emit_string8(arena, out, decl->name);
    emit_cstr(arena, out, ") {\n");
    for (i32 i = 0; i < decl->fields.length; i++) {
        Field *f = (Field *)decl->fields.data[i];
        emit_cstr(arena, out, "    ");
        emit_decl(arena, out, f->type, f->name, (TypeSub){0});
        emit_cstr(arena, out, ";\n");
    }
    emit_cstr(arena, out, "};\n\n");
}

static void emit_alias_decl(memops_arena *arena, string8 *out, AliasDecl *decl) {
    emit_cstr(arena, out, "typedef ");
    emit_decl(arena, out, decl->type, decl->name, (TypeSub){0});
    emit_cstr(arena, out, ";\n\n");
}

static void emit_enum_decl(memops_arena *arena, string8 *out, EnumDecl *decl) {
    emit_cstr(arena, out, "typedef enum ");
    emit_string8(arena, out, decl->name);
    emit_cstr(arena, out, " {\n");
    for (i32 i = 0; i < decl->items.length; i++) {
        EnumItem *item = (EnumItem *)decl->items.data[i];
        emit_cstr(arena, out, "    ");
        emit_string8(arena, out, decl->name);
        emit_cstr(arena, out, "_");
        emit_string8(arena, out, item->name);
        if (item->value.data) {
            emit_cstr(arena, out, " = ");
            emit_string8(arena, out, item->value);
        }
        emit_cstr(arena, out, ",\n");
    }
    emit_cstr(arena, out, "} ");
    emit_string8(arena, out, decl->name);
    emit_cstr(arena, out, ";\n\n");
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

static void emit_struct_decl_mono(memops_arena *arena, string8 *out, StructDecl *decl, string8 type_mangled, TypeExpr *arg) {
    TypeSub sub = {0};
    sub.has = true;
    sub.param = decl->type_param;
    sub.arg = arg;

    emit_cstr(arena, out, decl->is_union ? "uniondef(" : "structdef(");
    emit_string8(arena, out, decl->name);
    emit_cstr(arena, out, "_");
    emit_string8(arena, out, type_mangled);
    emit_cstr(arena, out, ") {\n");
    for (i32 i = 0; i < decl->fields.length; i++) {
        Field *f = (Field *)decl->fields.data[i];
        emit_cstr(arena, out, "    ");
        emit_decl(arena, out, f->type, f->name, sub);
        emit_cstr(arena, out, ";\n");
    }
    emit_cstr(arena, out, "};\n\n");
}

static void emit_proc_decl(memops_arena *arena, string8 *out, ProcDecl *decl) {
    if (decl->is_external) return;
    emit_type(arena, out, decl->ret_type, (TypeSub){0});
    emit_cstr(arena, out, " ");
    if (decl->callconv.data) {
        emit_string8(arena, out, decl->callconv);
        emit_cstr(arena, out, " ");
    }
    emit_string8(arena, out, decl->name);
    emit_cstr(arena, out, "(");
    if (decl->params.length == 0 && !decl->is_variadic) {
        emit_cstr(arena, out, "void");
    } else {
        for (i32 i = 0; i < decl->params.length; i++) {
            if (i > 0) emit_cstr(arena, out, ", ");
            Param *p = (Param *)decl->params.data[i];
            emit_decl(arena, out, p->type, p->name, (TypeSub){0});
        }
    }
    if (decl->is_variadic) {
        if (decl->params.length > 0) emit_cstr(arena, out, ", ");
        emit_cstr(arena, out, "...");
    }
    emit_cstr(arena, out, ") {\n");
    for (i32 i = 0; i < decl->body.length; i++) {
        emit_cstr(arena, out, "    ");
        emit_stmt(arena, out, (Stmt *)decl->body.data[i], (TypeSub){0}, (string8){0});
    }
    emit_cstr(arena, out, "}\n\n");
}

static void emit_proc_proto(memops_arena *arena, string8 *out, ProcDecl *decl) {
    if (decl->is_external && !decl->emit_external_proto) return;
    emit_type(arena, out, decl->ret_type, (TypeSub){0});
    emit_cstr(arena, out, " ");
    if (decl->callconv.data) {
        emit_string8(arena, out, decl->callconv);
        emit_cstr(arena, out, " ");
    }
    emit_string8(arena, out, decl->name);
    emit_cstr(arena, out, "(");
    if (decl->params.length == 0 && !decl->is_variadic) {
        emit_cstr(arena, out, "void");
    } else {
        for (i32 i = 0; i < decl->params.length; i++) {
            if (i > 0) emit_cstr(arena, out, ", ");
            Param *p = (Param *)decl->params.data[i];
            emit_decl(arena, out, p->type, p->name, (TypeSub){0});
        }
    }
    if (decl->is_variadic) {
        if (decl->params.length > 0) emit_cstr(arena, out, ", ");
        emit_cstr(arena, out, "...");
    }
    emit_cstr(arena, out, ");\n");
}

static void emit_proc_decl_mono(memops_arena *arena, string8 *out, ProcDecl *decl, string8 type_mangled, TypeExpr *arg) {
    if (decl->is_external) return;
    TypeSub sub = {0};
    sub.has = true;
    sub.param = decl->type_param;
    sub.arg = arg;

    emit_type(arena, out, decl->ret_type, sub);
    emit_cstr(arena, out, " ");
    if (decl->callconv.data) {
        emit_string8(arena, out, decl->callconv);
        emit_cstr(arena, out, " ");
    }
    emit_mono_proc_name(arena, out, decl->name, type_mangled);
    emit_cstr(arena, out, "(");
    if (decl->params.length == 0 && !decl->is_variadic) {
        emit_cstr(arena, out, "void");
    } else {
        for (i32 i = 0; i < decl->params.length; i++) {
            if (i > 0) emit_cstr(arena, out, ", ");
            Param *p = (Param *)decl->params.data[i];
            emit_decl(arena, out, p->type, p->name, sub);
        }
    }
    if (decl->is_variadic) {
        if (decl->params.length > 0) emit_cstr(arena, out, ", ");
        emit_cstr(arena, out, "...");
    }
    emit_cstr(arena, out, ") {\n");
    for (i32 i = 0; i < decl->body.length; i++) {
        emit_cstr(arena, out, "    ");
        emit_stmt(arena, out, (Stmt *)decl->body.data[i], sub, decl->name);
    }
    emit_cstr(arena, out, "}\n\n");
}

static void emit_proc_proto_mono(memops_arena *arena, string8 *out, ProcDecl *decl, string8 type_mangled, TypeExpr *arg) {
    if (decl->is_external && !decl->emit_external_proto) return;
    TypeSub sub = {0};
    sub.has = true;
    sub.param = decl->type_param;
    sub.arg = arg;

    emit_type(arena, out, decl->ret_type, sub);
    emit_cstr(arena, out, " ");
    if (decl->callconv.data) {
        emit_string8(arena, out, decl->callconv);
        emit_cstr(arena, out, " ");
    }
    emit_mono_proc_name(arena, out, decl->name, type_mangled);
    emit_cstr(arena, out, "(");
    if (decl->params.length == 0 && !decl->is_variadic) {
        emit_cstr(arena, out, "void");
    } else {
        for (i32 i = 0; i < decl->params.length; i++) {
            if (i > 0) emit_cstr(arena, out, ", ");
            Param *p = (Param *)decl->params.data[i];
            emit_decl(arena, out, p->type, p->name, sub);
        }
    }
    if (decl->is_variadic) {
        if (decl->params.length > 0) emit_cstr(arena, out, ", ");
        emit_cstr(arena, out, "...");
    }
    emit_cstr(arena, out, ");\n");
}

static void emit_reflection_runtime_types(memops_arena *arena, string8 *out) {
    emit_cstr(arena, out,
        "#ifndef I_REFLECT_TYPES_DEFINED\n"
        "#define I_REFLECT_TYPES_DEFINED\n"
        "typedef struct i_reflect_field {\n"
        "    const char *name;\n"
        "    const char *type;\n"
        "    u64 offset;\n"
        "    u64 size;\n"
        "} i_reflect_field;\n\n"
        "typedef struct i_reflect_type {\n"
        "    const char *name;\n"
        "    u64 size;\n"
        "    u64 field_count;\n"
        "    const i_reflect_field *fields;\n"
        "} i_reflect_type;\n\n"
        "typedef struct i_reflect_enum_value {\n"
        "    const char *name;\n"
        "    i32 value;\n"
        "} i_reflect_enum_value;\n\n"
        "typedef struct i_reflect_enum {\n"
        "    const char *name;\n"
        "    u64 value_count;\n"
        "    const i_reflect_enum_value *values;\n"
        "} i_reflect_enum;\n\n"
        "#endif\n\n"
    );
}

static void emit_string8_as_c_string(memops_arena *arena, string8 *out, string8 s) {
    emit_cstr(arena, out, "\"");
    emit_string8(arena, out, s);
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

static void emit_line_directive(memops_arena *arena, string8 *out, i32 line) {
    char line_buf[64];
    snprintf(line_buf, sizeof(line_buf), "#line %d ", line);
    emit_cstr(arena, out, line_buf);
    emit_c_string_literal_from_cstr(arena, out, g_source_path);
    emit_cstr(arena, out, "\n");
}

static void emit_struct_reflection(
    memops_arena *arena,
    string8 *out,
    StructDecl *decl,
    string8 concrete_name,
    TypeSub sub
) {
    emit_cstr(arena, out, "static const i_reflect_field i_reflect_fields_");
    emit_string8(arena, out, concrete_name);
    emit_cstr(arena, out, "[] = {\n");
    for (i32 i = 0; i < decl->fields.length; i++) {
        Field *f = (Field *)decl->fields.data[i];
        string8 type_name = type_mangle(arena, f->type, sub);
        emit_cstr(arena, out, "    {");
        emit_string8_as_c_string(arena, out, f->name);
        emit_cstr(arena, out, ", ");
        emit_string8_as_c_string(arena, out, type_name);
        emit_cstr(arena, out, ", (u64)offsetof(");
        emit_string8(arena, out, concrete_name);
        emit_cstr(arena, out, ", ");
        emit_string8(arena, out, f->name);
        emit_cstr(arena, out, "), (u64)sizeof(((");
        emit_string8(arena, out, concrete_name);
        emit_cstr(arena, out, " *)0)->");
        emit_string8(arena, out, f->name);
        emit_cstr(arena, out, ")},\n");
    }
    emit_cstr(arena, out, "};\n");
    emit_cstr(arena, out, "const i_reflect_type ");
    emit_string8(arena, out, concrete_name);
    emit_cstr(arena, out, "_reflect = {");
    emit_string8_as_c_string(arena, out, concrete_name);
    emit_cstr(arena, out, ", (u64)sizeof(");
    emit_string8(arena, out, concrete_name);
    emit_cstr(arena, out, "), ");
    char count_buf[32];
    snprintf(count_buf, sizeof(count_buf), "%llu", (unsigned long long)decl->fields.length);
    emit_cstr(arena, out, count_buf);
    emit_cstr(arena, out, ", i_reflect_fields_");
    emit_string8(arena, out, concrete_name);
    emit_cstr(arena, out, "};\n\n");
}

static void emit_struct_reflection_extern(memops_arena *arena, string8 *out, string8 concrete_name) {
    emit_cstr(arena, out, "extern const i_reflect_type ");
    emit_string8(arena, out, concrete_name);
    emit_cstr(arena, out, "_reflect;\n");
}

static void emit_enum_reflection(memops_arena *arena, string8 *out, EnumDecl *decl) {
    emit_cstr(arena, out, "static const i_reflect_enum_value i_reflect_enum_values_");
    emit_string8(arena, out, decl->name);
    emit_cstr(arena, out, "[] = {\n");
    for (i32 i = 0; i < decl->items.length; i++) {
        EnumItem *item = (EnumItem *)decl->items.data[i];
        emit_cstr(arena, out, "    {");
        emit_string8_as_c_string(arena, out, item->name);
        emit_cstr(arena, out, ", ");
        emit_string8(arena, out, decl->name);
        emit_cstr(arena, out, "_");
        emit_string8(arena, out, item->name);
        emit_cstr(arena, out, "},\n");
    }
    emit_cstr(arena, out, "};\n");
    emit_cstr(arena, out, "const i_reflect_enum ");
    emit_string8(arena, out, decl->name);
    emit_cstr(arena, out, "_reflect = {");
    emit_string8_as_c_string(arena, out, decl->name);
    emit_cstr(arena, out, ", ");
    char count_buf[32];
    snprintf(count_buf, sizeof(count_buf), "%llu", (unsigned long long)decl->items.length);
    emit_cstr(arena, out, count_buf);
    emit_cstr(arena, out, ", i_reflect_enum_values_");
    emit_string8(arena, out, decl->name);
    emit_cstr(arena, out, "};\n\n");
}

static void emit_enum_reflection_extern(memops_arena *arena, string8 *out, EnumDecl *decl) {
    emit_cstr(arena, out, "extern const i_reflect_enum ");
    emit_string8(arena, out, decl->name);
    emit_cstr(arena, out, "_reflect;\n");
}

static void emit_program(memops_arena *arena, Program *prog, string8 *out) {
    emit_cstr(arena, out, "#include <core.h>\n#include <stddef.h>\n\n");
    for (i32 i = 0; i < prog->preprocessor_lines.length; i++) {
        emit_string8(arena, out, prog->preprocessor_lines.data[i]);
        emit_cstr(arena, out, "\n");
    }
    if (prog->preprocessor_lines.length > 0) {
        emit_cstr(arena, out, "\n");
    }
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
    for (i32 i = 0; i < prog->imports.length; i++) {
        emit_cstr(arena, out, "#include ");
        emit_string8(arena, out, i_import_to_h_import(arena, prog->imports.data[i]));
        emit_cstr(arena, out, "\n");
    }
    if (prog->imports.length > 0) {
        emit_cstr(arena, out, "\n");
    }

    emit_line_directive(arena, out, 1);
    emit_reflection_runtime_types(arena, out);

    // Forward declarations for all structs (non-generic + monomorphized)
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic) {
            emit_struct_fwd_decl(arena, out, decl->name, decl->is_union);
        }
    }
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic) continue;

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

    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic) {
            emit_struct_decl(arena, out, decl);
        }
    }

    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic) continue;

        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        collect_generic_struct_instances(prog, decl, &instances, arena);

        for (i32 j = 0; j < instances.length; j++) {
            string8 mangle = instances.data[j];
            TypeExpr *arg = type_new(arena, Type_Name);
            arg->name = mangle;
            emit_struct_decl_mono(arena, out, decl, mangle, arg);
        }
    }

    for (i32 i = 0; i < prog->enums.length; i++) {
        emit_enum_reflection(arena, out, (EnumDecl *)prog->enums.data[i]);
    }

    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic) {
            emit_struct_reflection(arena, out, decl, decl->name, (TypeSub){0});
        }
    }

    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic) continue;

        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        collect_generic_struct_instances(prog, decl, &instances, arena);

        for (i32 j = 0; j < instances.length; j++) {
            string8 mangle = instances.data[j];
            string8 concrete_name = string8_reserve(arena, decl->name.length + 1 + mangle.length);
            string8_append_bytes(arena, &concrete_name, decl->name.data, decl->name.length);
            string8_append_cstr(arena, &concrete_name, "_");
            string8_append_bytes(arena, &concrete_name, mangle.data, mangle.length);
            TypeExpr *arg = type_new(arena, Type_Name);
            arg->name = mangle;
            TypeSub sub = {0};
            sub.has = true;
            sub.param = decl->type_param;
            sub.arg = arg;
            emit_struct_reflection(arena, out, decl, concrete_name, sub);
        }
    }

    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *s = (Stmt *)prog->globals.data[i];
        if (s->is_external) {
            emit_cstr(arena, out, "extern ");
            emit_decl(arena, out, s->type, s->name, (TypeSub){0});
            emit_cstr(arena, out, ";\n");
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
        collect_generic_proc_instances(prog, decl, &instances, arena);
        for (i32 j = 0; j < instances.length; j++) {
            string8 mangle = instances.data[j];
            TypeExpr *arg = type_new(arena, Type_Name);
            arg->name = mangle;
            emit_proc_proto_mono(arena, out, decl, mangle, arg);
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
        collect_generic_proc_instances(prog, decl, &instances, arena);
        for (i32 j = 0; j < instances.length; j++) {
            string8 mangle = instances.data[j];
            TypeExpr *arg = type_new(arena, Type_Name);
            arg->name = mangle;
            emit_proc_decl_mono(arena, out, decl, mangle, arg);
        }
    }
}

static void emit_header_program(memops_arena *arena, Program *prog, string8 *out) {
    emit_cstr(arena, out, "#pragma once\n#include <core.h>\n#include <stddef.h>\n\n");
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
    for (i32 i = 0; i < prog->imports.length; i++) {
        emit_cstr(arena, out, "#include ");
        emit_string8(arena, out, i_import_to_h_import(arena, prog->imports.data[i]));
        emit_cstr(arena, out, "\n");
    }
    if (prog->imports.length > 0) emit_cstr(arena, out, "\n");

    emit_reflection_runtime_types(arena, out);

    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic) emit_struct_fwd_decl(arena, out, decl->name, decl->is_union);
    }
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic) continue;
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
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic) emit_struct_decl(arena, out, decl);
    }
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic) continue;
        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        collect_generic_struct_instances(prog, decl, &instances, arena);
        for (i32 j = 0; j < instances.length; j++) {
            TypeExpr *arg = type_new(arena, Type_Name);
            arg->name = instances.data[j];
            emit_struct_decl_mono(arena, out, decl, instances.data[j], arg);
        }
    }

    for (i32 i = 0; i < prog->enums.length; i++) {
        emit_enum_reflection_extern(arena, out, (EnumDecl *)prog->enums.data[i]);
    }
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic) emit_struct_reflection_extern(arena, out, decl->name);
    }
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic) continue;
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
        emit_cstr(arena, out, "extern ");
        emit_decl(arena, out, s->type, s->name, (TypeSub){0});
        emit_cstr(arena, out, ";\n");
    }
    emit_cstr(arena, out, "\n");

    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        if (!decl->is_generic) emit_proc_proto(arena, out, decl);
    }
    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *decl = (ProcDecl *)prog->procs.data[i];
        if (!decl->is_generic) continue;
        Vec_string8 instances = Vec_string8_reserve(arena, 4);
        collect_generic_proc_instances(prog, decl, &instances, arena);
        for (i32 j = 0; j < instances.length; j++) {
            TypeExpr *arg = type_new(arena, Type_Name);
            arg->name = instances.data[j];
            emit_proc_proto_mono(arena, out, decl, instances.data[j], arg);
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

static Vec_string8 collect_preprocessor_lines(memops_arena *arena, string8 src) {
    Vec_string8 lines = Vec_string8_reserve(arena, 8);
    u8 *p = src.data;
    u8 *end = src.data + src.length;
    while (p < end) {
        u8 *line_start = p;
        while (p < end && *p != '\n') p++;
        u8 *line_end = p;
        if (line_end > line_start && line_end[-1] == '\r') {
            line_end--;
        }
        u64 line_length = (u64)(line_end - line_start);
        if (preprocessor_line_is_c_directive(line_start, line_length)) {
            Vec_string8_append(arena, &lines, string8_copy_from_slice(arena, line_start, line_length));
        }
        if (p < end && *p == '\n') p++;
    }
    return lines;
}

static bool write_string8_to_file(const char *path, string8 data) {
    FILE *f = fopen(path, "wb");
    if (!f) return false;
    fwrite(data.data, 1, data.length, f);
    fclose(f);
    return true;
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

i32 main(i32 argc, char *argv[]) {
    const char *input_path = argc > 1 ? argv[1] : "src/main.i";
    const char *output_path = argc > 2 ? argv[2] : "src/main.i.c";
    const char *header_path = argc > 3 ? argv[3] : null;
    g_source_path = input_path;

    memops_arena arena = {0};
    memops_arena_initialize(&arena);

    string8 input = string8_read_file(&arena, input_path);
    if (!input.data) {
        printf("i: error: failed to read %s\n", input_path);
        return 1;
    }

    Vec_Token tokens = {0};
    lex_tokens(&arena, input, &tokens);

    Parser parser = {0};
    parser.arena = &arena;
    parser.tokens = tokens;
    parser.index = 0;

    Program prog = parse_program(&parser);
    prog.preprocessor_lines = collect_preprocessor_lines(&arena, input);
    semantic_check_program(&prog, &arena);
    validate_generic_constraints(&prog, &arena);
    rewrite_printf_formats(&prog, &arena);

    string8 output = string8_reserve(&arena, input.length * 2 + 1024);
    emit_program(&arena, &prog, &output);

    string8 header_output = string8_reserve(&arena, input.length + 1024);
    emit_header_program(&arena, &prog, &header_output);
    if (!header_path) {
        header_path = derive_header_path(&arena, output_path);
    }

    if (!write_string8_to_file(output_path, output)) {
        printf("i: error: failed to write %s\n", output_path);
        return 1;
    }
    if (!write_string8_to_file(header_path, header_output)) {
        printf("i: error: failed to write %s\n", header_path);
        return 1;
    }

    printf("i: generated %s and %s\n", output_path, header_path);
    return 0;
}

#include <Vec.c>
