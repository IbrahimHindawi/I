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
// for types that include a container of themselves eg `struct T { Array_T arr; };`
// the type must be included after the generated header.
// this is because the type needs to know the container definition.
// Warning: can be recursive type with `T *` but not `T`
//---------------------------------------------------------------------------------------------------
// primitives
//---------------------------------------------------------------------------------------------------
// haikal@Array:voidptr:p
// haikal@Array:i8:p
// haikal@Array:i32:p
// haikal@Array:f32:p
// haikal@Array:char:p
// haikal@Array:u8:p
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
// haikal@Array:string8:s
// haikal@Array:string8slice:s
// haikal@Array:Token:s
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

typedef enum TokenKind {
    Token_EOF = 0,
    Token_Identifier,
    Token_Number,
    Token_String,
    Token_Colon,
    Token_Semicolon,
    Token_Equal,
    Token_PlusEqual,
    Token_MinusEqual,
    Token_StarEqual,
    Token_SlashEqual,
    Token_AmpersandEqual,
    Token_CaretEqual,
    Token_PipeEqual,
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
    Token_Plus,
    Token_Minus,
    Token_Star,
    Token_Slash,
    Token_Keyword_Proc,
    Token_Keyword_Struct,
    Token_Keyword_Ret,
    Token_Keyword_For,
    Token_Keyword_Import,
} TokenKind;

typedef struct Token Token;
struct Token {
    TokenKind kind;
    string8slice text;
    i32 line;
    i32 col;
};

// add codegen
#include <Array.h>

typedef struct TypeExpr TypeExpr;
typedef struct Expr Expr;
typedef struct Stmt Stmt;
typedef struct StructDecl StructDecl;
typedef struct ProcDecl ProcDecl;
typedef struct Field Field;
typedef struct Param Param;

typedef enum TypeKind {
    Type_Name = 0,
    Type_Ptr,
    Type_Generic,
} TypeKind;

struct TypeExpr {
    TypeKind kind;
    string8 name;
    TypeExpr *elem;
    Array_voidptr args; // TypeExpr*
};

typedef enum ExprKind {
    Expr_Name = 0,
    Expr_Number,
    Expr_String,
    Expr_Call,
    Expr_Addr,
    Expr_Binary,
    Expr_Index,
    Expr_ZeroInit,
    Expr_Cast,
} ExprKind;

struct Expr {
    ExprKind kind;
    string8 name;
    string8 number;
    string8 string_lit;
    Array_voidptr args;      // Expr*
    Array_voidptr type_args; // TypeExpr*
    Expr *inner;
    Expr *left;
    Expr *right;
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
} StmtKind;

struct Stmt {
    StmtKind kind;
    string8 name;
    TypeExpr *type;
    Expr *expr;
    TokenKind assign_op;
    Stmt *for_init;
    Expr *for_cond;
    Stmt *for_step;
    Array_voidptr for_body; // Stmt*
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
    string8 type_param;
    Array_voidptr fields; // Field*
    i32 line;
    i32 col;
};

struct ProcDecl {
    string8 name;
    bool is_generic;
    bool is_external;
    string8 type_param;
    string8 constraint;
    Array_voidptr params; // Param*
    TypeExpr *ret_type;
    Array_voidptr body; // Stmt*
    i32 line;
    i32 col;
};

typedef struct Program {
    Array_string8 imports; // string literal include path token text
    Array_voidptr structs; // StructDecl*
    Array_voidptr procs;   // ProcDecl*
    Array_voidptr globals; // Stmt* (var decl)
} Program;

typedef struct Scope {
    Array_string8 locals;
    Array_string8 globals;
    Array_string8 procs;
} Scope;

typedef struct Parser {
    memops_arena *arena;
    Array_Token tokens;
    i32 index;
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

static void lex_tokens(memops_arena *arena, string8 src, Array_Token *out_tokens) {
    i32 line = 1;
    i32 col = 1;
    u8 *p = src.data;
    u8 *end = src.data + src.length;

    *out_tokens = Array_Token_reserve(arena, 256);

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
            else if (string8slice_equals_cstr(text, "ret")) kind = Token_Keyword_Ret;
            else if (string8slice_equals_cstr(text, "for")) kind = Token_Keyword_For;
            else if (string8slice_equals_cstr(text, "import")) kind = Token_Keyword_Import;
            Array_Token_append(arena, out_tokens, token_make(kind, text, line, start_col));
            continue;
        }
        if (is_digit(c)) {
            u8 *start = p;
            i32 start_col = col;
            while (p < end && is_digit(*p)) {
                p++;
                col++;
            }
            while (p < end && is_alnum(*p)) {
                p++;
                col++;
            }
            string8slice text = string8slice_from_parts(start, (u64)(p - start));
            Array_Token_append(arena, out_tokens, token_make(Token_Number, text, line, start_col));
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
                    printf("lexer error: unterminated string at %d:%d\n", line, start_col);
                    exit(1);
                }
                p++;
                col++;
            }
            if (p > end || start == p || p[-1] != '"') {
                printf("lexer error: unterminated string at %d:%d\n", line, start_col);
                exit(1);
            }
            string8slice text = string8slice_from_parts(start, (u64)(p - start));
            Array_Token_append(arena, out_tokens, token_make(Token_String, text, line, start_col));
            continue;
        }

        if ((p + 1) < end) {
            if (c == '+' && p[1] == '=') {
                Array_Token_append(arena, out_tokens, token_make(Token_PlusEqual, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
            if (c == '-' && p[1] == '=') {
                Array_Token_append(arena, out_tokens, token_make(Token_MinusEqual, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
            if (c == '*' && p[1] == '=') {
                Array_Token_append(arena, out_tokens, token_make(Token_StarEqual, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
            if (c == '/' && p[1] == '=') {
                Array_Token_append(arena, out_tokens, token_make(Token_SlashEqual, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
            if (c == '&' && p[1] == '=') {
                Array_Token_append(arena, out_tokens, token_make(Token_AmpersandEqual, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
            if (c == '^' && p[1] == '=') {
                Array_Token_append(arena, out_tokens, token_make(Token_CaretEqual, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
            if (c == '|' && p[1] == '=') {
                Array_Token_append(arena, out_tokens, token_make(Token_PipeEqual, string8slice_from_parts(p, 2), line, col));
                p += 2;
                col += 2;
                continue;
            }
            if (c == '-' && p[1] == '>') {
                Array_Token_append(arena, out_tokens, token_make(Token_Arrow, string8slice_from_parts(p, 2), line, col));
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
            case '+': kind = Token_Plus; break;
            case '-': kind = Token_Minus; break;
            case '*': kind = Token_Star; break;
            case '/': kind = Token_Slash; break;
            default: break;
        }

        if (kind != Token_EOF) {
            Array_Token_append(arena, out_tokens, token_make(kind, string8slice_from_parts(p, 1), line, col));
            p++;
            col++;
            continue;
        }

        printf("lexer error: unexpected char '%c' at %d:%d\n", c, line, col);
        exit(1);
    }

    Array_Token_append(arena, out_tokens, token_make(Token_EOF, string8slice_from_parts(end, 0), line, col));
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

static bool parser_match(Parser *p, TokenKind kind) {
    if (parser_peek(p)->kind == kind) {
        parser_next(p);
        return true;
    }
    return false;
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
        }
        idx++;
    }
    return false;
}

static Token *parser_expect(Parser *p, TokenKind kind, const char *msg) {
    if (parser_peek(p)->kind != kind) {
        Token *t = parser_peek(p);
        printf("parse error: %s at %d:%d\n", msg, t->line, t->col);
        exit(1);
    }
    return parser_next(p);
}

static string8 token_to_string8(memops_arena *arena, Token *t) {
    return string8_copy_from_slice(arena, t->text.data, t->text.length);
}

static Array_voidptr ptr_array_reserve(memops_arena *arena, i32 capacity) {
    return Array_voidptr_reserve(arena, capacity);
}

static void ptr_array_append(memops_arena *arena, Array_voidptr *arr, void *ptr) {
    Array_voidptr_append(arena, arr, ptr);
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
    return out;
}

static TypeExpr *parse_type(Parser *p) {
    if (parser_match(p, Token_Star)) {
        TypeExpr *inner = parse_type(p);
        TypeExpr *ptr = type_new(p->arena, Type_Ptr);
        ptr->elem = inner;
        return ptr;
    }

    Token *name_tok = parser_expect(p, Token_Identifier, "expected type name");
    string8 name = token_to_string8(p->arena, name_tok);

    if (parser_match(p, Token_LAngle)) {
        Array_voidptr args = ptr_array_reserve(p->arena, 4);
        do {
            TypeExpr *arg = parse_type(p);
            ptr_array_append(p->arena, &args, arg);
        } while (parser_match(p, Token_Comma));
        parser_expect(p, Token_RAngle, "expected '>'");

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
    parser_expect(p, Token_RAngle, "expected '>'");

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

static Expr *parse_expr(Parser *p);
static Expr *parse_unary(Parser *p);
static Expr *parse_multiplicative(Parser *p);
static Expr *parse_additive(Parser *p);
static Expr *parse_relational(Parser *p);
static Expr *parse_postfix(Parser *p, Expr *base);

static Expr *parse_primary(Parser *p) {
    if (parser_match(p, Token_LBrace)) {
        Token *lb = parser_prev(p);
        if (!parser_match(p, Token_RBrace)) {
            parser_expect(p, Token_RBrace, "expected '}' for zero initializer");
        }
        Expr *e = expr_new(p->arena, Expr_ZeroInit);
        e->line = lb->line;
        e->col = lb->col;
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

        if ((parser_next_is_generic_call(p) || parser_next_is_generic_qualified_call(p)) &&
            parser_match(p, Token_LAngle)) {
            Array_voidptr type_args = ptr_array_reserve(p->arena, 2);
            do {
                TypeExpr *arg = parse_type(p);
                ptr_array_append(p->arena, &type_args, arg);
            } while (parser_match(p, Token_Comma));
            parser_expect(p, Token_RAngle, "expected '>'");

            // Qualified generic call sugar:
            // array<T>reserve(...) -> array_reserve<T>(...)
            if (parser_peek(p)->kind == Token_Identifier && parser_peek_n(p, 1)->kind == Token_LParen) {
                Token *tail_tok = parser_next(p);
                string8 tail = token_to_string8(p->arena, tail_tok);
                name = concat_name2(p->arena, base_name, "@", tail);
            }

            parser_expect(p, Token_LParen, "expected '(' after type args");
            Array_voidptr args = ptr_array_reserve(p->arena, 4);
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
            Array_voidptr args = ptr_array_reserve(p->arena, 4);
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
    printf("parse error: expected expression at %d:%d\n", t->line, t->col);
    exit(1);
    return null;
}

static Expr *parse_postfix(Parser *p, Expr *base) {
    Expr *result = base;
    while (parser_match(p, Token_LBracket)) {
        Token *lb = parser_prev(p);
        Expr *index = parse_expr(p);
        parser_expect(p, Token_RBracket, "expected ']'");
        Expr *idx = expr_new(p->arena, Expr_Index);
        idx->base = result;
        idx->index_expr = index;
        idx->line = lb->line;
        idx->col = lb->col;
        result = idx;
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
    return parse_primary(p);
}

static Expr *parse_multiplicative(Parser *p) {
    Expr *left = parse_unary(p);
    while (parser_peek(p)->kind == Token_Star || parser_peek(p)->kind == Token_Slash) {
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

static Expr *parse_relational(Parser *p) {
    Expr *left = parse_additive(p);
    while (parser_peek(p)->kind == Token_LAngle || parser_peek(p)->kind == Token_RAngle) {
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

static Expr *parse_expr(Parser *p) {
    return parse_relational(p);
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
        s->expr = parse_expr(p);
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
        TokenKind assign_op = parser_match_assign_op(p);
        if (assign_op != Token_EOF) {
            Stmt *s = stmt_new(p->arena, Stmt_Assign);
            s->name = token_to_string8(p->arena, name_tok);
            s->assign_op = assign_op;
            s->expr = parse_expr(p);
            s->line = name_tok->line;
            s->col = name_tok->col;
            parser_expect(p, Token_Semicolon, "expected ';' after assignment");
            return s;
        }

        p->index--;
        Stmt *s = stmt_new(p->arena, Stmt_Expr);
        s->expr = parse_expr(p);
        s->line = s->expr ? s->expr->line : name_tok->line;
        s->col = s->expr ? s->expr->col : name_tok->col;
        parser_expect(p, Token_Semicolon, "expected ';' after expression");
        return s;
    }

    Token *t = parser_peek(p);
    printf("parse error: unexpected token at %d:%d\n", t->line, t->col);
    exit(1);
    return null;
}

static StructDecl *parse_struct_decl(Parser *p, Token *name_tok) {
    StructDecl *decl = memops_arena_push_struct(p->arena, StructDecl);
    memset(decl, 0, sizeof(StructDecl));
    decl->name = token_to_string8(p->arena, name_tok);
    decl->line = name_tok->line;
    decl->col = name_tok->col;
    decl->fields = ptr_array_reserve(p->arena, 8);

    if (parser_match(p, Token_LAngle)) {
        Token *param_tok = parser_expect(p, Token_Identifier, "expected type param");
        decl->type_param = token_to_string8(p->arena, param_tok);
        decl->is_generic = true;
        parser_expect(p, Token_RAngle, "expected '>'");
    }

    parser_expect(p, Token_Equal, "expected '=' after struct");
    parser_expect(p, Token_LBrace, "expected '{' in struct");
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
            parser_expect(p, Token_RAngle, "expected '>'");
        } else {
            TypeExpr *spec = parse_type(p);
            parser_expect(p, Token_RAngle, "expected '>'");
            string8 suffix = type_mangle_concrete(p->arena, spec);
            string8 full_name = string8_reserve(p->arena, decl->name.length + 1 + suffix.length);
            string8_append_bytes(p->arena, &full_name, decl->name.data, decl->name.length);
            string8_append_cstr(p->arena, &full_name, "_");
            string8_append_bytes(p->arena, &full_name, suffix.data, suffix.length);
            decl->name = full_name;
            decl->is_generic = false;
        }
    }

    parser_expect(p, Token_LParen, "expected '(' after proc");
    if (!parser_match(p, Token_RParen)) {
        do {
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
        string8slice_equals_cstr(parser_peek(p)->text, "external") &&
        parser_peek_n(p, 1)->kind == Token_Semicolon &&
        parser_peek_n(p, 2)->kind == Token_RBrace) {
        parser_next(p); // external
        parser_next(p); // ;
        parser_next(p); // }
        decl->is_external = true;
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
    prog.imports = Array_string8_reserve(p->arena, 8);
    prog.structs = ptr_array_reserve(p->arena, 8);
    prog.procs = ptr_array_reserve(p->arena, 8);
    prog.globals = ptr_array_reserve(p->arena, 8);

    while (parser_peek(p)->kind != Token_EOF) {
        if (parser_match(p, Token_Keyword_Import)) {
            Token *path_tok = parser_expect(p, Token_String, "expected string literal after import");
            string8 path = token_to_string8(p->arena, path_tok);
            Array_string8_append(p->arena, &prog.imports, path);
            parser_match(p, Token_Semicolon);
            continue;
        }

        Token *head_tok = parser_peek(p);
        string8 base_name = token_to_string8(p->arena, head_tok);
        string8 parsed_name = parse_decl_name(p);
        parser_expect(p, Token_Colon, "expected ':' after identifier");

        if (parser_match(p, Token_Keyword_Struct)) {
            StructDecl *decl = parse_struct_decl(p, head_tok);
            if (!string8_equals(&parsed_name, &base_name)) {
                decl->name = parsed_name;
            }
            ptr_array_append(p->arena, &prog.structs, decl);
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
            s->expr = parse_expr(p);
        }
        parser_expect(p, Token_Semicolon, "expected ';' after global var");
        ptr_array_append(p->arena, &prog.globals, s);
    }

    return prog;
}

static bool scope_has(Array_string8 *names, string8 name) {
    for (i32 i = 0; i < names->length; i++) {
        if (string8_equals(&names->data[i], &name)) return true;
    }
    return false;
}

static void semantic_error(const char *msg, i32 line, i32 col) {
    printf("semantic error at %d:%d: %s\n", line, col, msg);
    exit(1);
}

static void semantic_error_name(const char *msg, string8 name, i32 line, i32 col) {
    printf("semantic error at %d:%d: %s '%.*s'\n", line, col, msg, (int)name.length, name.data);
    exit(1);
}

static void semantic_error_name_dup(
    const char *msg, string8 name,
    i32 line, i32 col,
    i32 prev_line, i32 prev_col
) {
    printf(
        "semantic error at %d:%d: %s '%.*s' (previous at %d:%d)\n",
        line, col, msg, (int)name.length, name.data, prev_line, prev_col
    );
    exit(1);
}

static void semantic_check_expr(Expr *e, Scope *scope);

static void semantic_check_expr(Expr *e, Scope *scope) {
    if (!e) return;
    if (e->kind == Expr_Number) return;
    if (e->kind == Expr_String) return;
    if (e->kind == Expr_ZeroInit) return;
    if (e->kind == Expr_Name) {
        if (scope_has(&scope->locals, e->name)) return;
        if (scope_has(&scope->globals, e->name)) return;
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
    if (e->kind == Expr_Cast) {
        semantic_check_expr(e->inner, scope);
        return;
    }
    if (e->kind == Expr_Binary) {
        semantic_check_expr(e->left, scope);
        semantic_check_expr(e->right, scope);
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
        if (e->type_args.length == 0 && !scope_has(&scope->procs, e->name)) {
            semantic_error_name("call to undeclared proc", e->name, e->line, e->col);
        }
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
        Array_string8_append(arena, &scope->locals, stmt->name);
        return;
    }
    if (stmt->kind == Stmt_Assign) {
        if (!scope_has(&scope->locals, stmt->name) && !scope_has(&scope->globals, stmt->name)) {
            semantic_error_name("assignment to undeclared identifier", stmt->name, stmt->line, stmt->col);
        }
        semantic_check_expr(stmt->expr, scope);
        return;
    }
    if (stmt->kind == Stmt_Return || stmt->kind == Stmt_Expr) {
        semantic_check_expr(stmt->expr, scope);
        return;
    }
    if (stmt->kind == Stmt_For) {
        Scope loop_scope = *scope;
        loop_scope.locals = Array_string8_reserve(arena, scope->locals.length + 16);
        for (i32 i = 0; i < scope->locals.length; i++) {
            Array_string8_append(arena, &loop_scope.locals, scope->locals.data[i]);
        }
        if (stmt->for_init) semantic_check_stmt(stmt->for_init, &loop_scope, arena);
        if (stmt->for_cond) semantic_check_expr(stmt->for_cond, &loop_scope);
        if (stmt->for_step) semantic_check_stmt(stmt->for_step, &loop_scope, arena);
        for (i32 i = 0; i < stmt->for_body.length; i++) {
            semantic_check_stmt((Stmt *)stmt->for_body.data[i], &loop_scope, arena);
        }
        return;
    }
    semantic_error("unknown statement kind", stmt->line, stmt->col);
}

static void semantic_check_proc(ProcDecl *proc, Scope *base_scope, memops_arena *arena) {
    Scope scope = *base_scope;
    scope.locals = Array_string8_reserve(arena, 32);

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
        Array_string8_append(arena, &scope.locals, param->name);
    }

    for (i32 i = 0; i < proc->body.length; i++) {
        Stmt *stmt = (Stmt *)proc->body.data[i];
        semantic_check_stmt(stmt, &scope, arena);
    }
}

static void semantic_check_program(Program *prog, memops_arena *arena) {
    Scope base = {0};
    base.globals = Array_string8_reserve(arena, 64);
    base.procs = Array_string8_reserve(arena, 64);
    Array_string8 structs = Array_string8_reserve(arena, 64);

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
        Array_string8_append(arena, &structs, decl->name);
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
        Array_string8_append(arena, &base.procs, decl->name);
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
        Array_string8_append(arena, &base.globals, decl->name);
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
            emit_type(arena, out, sub.arg, (TypeSub){0});
            return;
        }
        emit_string8(arena, out, type->name);
        return;
    }
    if (type->kind == Type_Ptr) {
        emit_type(arena, out, type->elem, sub);
        emit_cstr(arena, out, " *");
        return;
    }
    if (type->kind == Type_Generic) {
        string8 mangle = type_mangle(arena, type, sub);
        emit_string8(arena, out, mangle);
        return;
    }
}

static string8 type_mangle(memops_arena *arena, TypeExpr *type, TypeSub sub) {
    string8 out = string8_reserve(arena, 64);
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
    return out;
}

static bool array_string8_contains(Array_string8 *arr, string8 value) {
    for (i32 i = 0; i < arr->length; i++) {
        if (string8_equals(&arr->data[i], &value)) {
            return true;
        }
    }
    return false;
}

static void collect_type_instances(TypeExpr *type, string8 base, Array_string8 *out, memops_arena *arena) {
    if (!type) return;
    if (type->kind == Type_Generic && string8_equals_name(type->name, base)) {
        if (type->args.length == 1) {
            TypeExpr *arg = (TypeExpr *)type->args.data[0];
            if (arg->kind == Type_Name && string8_is_symbolic_type_name(arg->name)) {
                return;
            }
            string8 mangle = type_mangle(arena, arg, (TypeSub){0});
            if (!array_string8_contains(out, mangle)) {
                Array_string8_append(arena, out, mangle);
            }
        }
    }

    if (type->kind == Type_Ptr) {
        collect_type_instances(type->elem, base, out, arena);
    }
    if (type->kind == Type_Generic) {
        for (i32 i = 0; i < type->args.length; i++) {
            collect_type_instances((TypeExpr *)type->args.data[i], base, out, arena);
        }
    }
}

static void collect_type_instances_from_stmt(Stmt *s, string8 base, Array_string8 *out, memops_arena *arena);
static void collect_type_instances_from_expr(Expr *e, string8 base, Array_string8 *out, memops_arena *arena);

static void collect_type_instances_from_stmt(Stmt *s, string8 base, Array_string8 *out, memops_arena *arena) {
    if (!s) return;
    if (s->kind == Stmt_Var) {
        collect_type_instances(s->type, base, out, arena);
        collect_type_instances_from_expr(s->expr, base, out, arena);
    } else if (s->kind == Stmt_Return) {
        collect_type_instances_from_expr(s->expr, base, out, arena);
    } else if (s->kind == Stmt_Expr) {
        collect_type_instances_from_expr(s->expr, base, out, arena);
    }
}

static void collect_type_instances_from_expr(Expr *e, string8 base, Array_string8 *out, memops_arena *arena) {
    if (!e) return;
    if (e->kind == Expr_Call) {
        for (i32 i = 0; i < e->type_args.length; i++) {
            collect_type_instances((TypeExpr *)e->type_args.data[i], base, out, arena);
        }
        for (i32 i = 0; i < e->args.length; i++) {
            collect_type_instances_from_expr((Expr *)e->args.data[i], base, out, arena);
        }
    } else if (e->kind == Expr_Addr) {
        collect_type_instances_from_expr(e->inner, base, out, arena);
    }
}

static void collect_generic_struct_instances(Program *prog, StructDecl *decl, Array_string8 *out, memops_arena *arena) {
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

static string8 find_proc_generic_instantiation(Expr *e, string8 proc_name, memops_arena *arena) {
    if (!e) return (string8){0};
    if (e->kind == Expr_Call && string8_equals_name(e->name, proc_name) && e->type_args.length == 1) {
        TypeExpr *arg = (TypeExpr *)e->type_args.data[0];
        if (arg->kind == Type_Name && string8_is_symbolic_type_name(arg->name)) {
            return (string8){0};
        }
        return type_mangle(arena, arg, (TypeSub){0});
    }

    if (e->kind == Expr_Call) {
        for (i32 i = 0; i < e->args.length; i++) {
            string8 found = find_proc_generic_instantiation((Expr *)e->args.data[i], proc_name, arena);
            if (found.data) return found;
        }
    } else if (e->kind == Expr_Addr) {
        return find_proc_generic_instantiation(e->inner, proc_name, arena);
    }

    return (string8){0};
}

static void collect_generic_proc_instances(Program *prog, ProcDecl *decl, Array_string8 *out, memops_arena *arena) {
    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *s = (Stmt *)prog->globals.data[i];
        string8 found = find_proc_generic_instantiation(s->expr, decl->name, arena);
        if (found.data && !array_string8_contains(out, found)) {
            Array_string8_append(arena, out, found);
        }
    }

    for (i32 i = 0; i < prog->procs.length; i++) {
        ProcDecl *p = (ProcDecl *)prog->procs.data[i];
        for (i32 j = 0; j < p->body.length; j++) {
            Stmt *s = (Stmt *)p->body.data[j];
            string8 found = find_proc_generic_instantiation(s->expr, decl->name, arena);
            if (found.data && !array_string8_contains(out, found)) {
                Array_string8_append(arena, out, found);
            }
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

        Array_string8 instances = Array_string8_reserve(arena, 4);
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
    Array_string8 names;
    Array_voidptr types; // TypeExpr*
} TypeScope;

static TypeScope type_scope_make(memops_arena *arena, i32 cap) {
    TypeScope s = {0};
    s.names = Array_string8_reserve(arena, cap);
    s.types = ptr_array_reserve(arena, cap);
    return s;
}

static TypeScope type_scope_copy(memops_arena *arena, TypeScope *src) {
    TypeScope dst = type_scope_make(arena, src->names.length + 8);
    for (i32 i = 0; i < src->names.length; i++) {
        Array_string8_append(arena, &dst.names, src->names.data[i]);
        ptr_array_append(arena, &dst.types, src->types.data[i]);
    }
    return dst;
}

static void type_scope_add(memops_arena *arena, TypeScope *s, string8 name, TypeExpr *type) {
    Array_string8_append(arena, &s->names, name);
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

static TypeExpr *infer_expr_type(Expr *e, TypeScope *scope, memops_arena *arena) {
    if (!e) return null;
    if (e->kind == Expr_Name) {
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
    if (e->kind == Expr_Addr) {
        TypeExpr *inner = infer_expr_type(e->inner, scope, arena);
        if (!inner) return null;
        TypeExpr *ptr_t = type_new(arena, Type_Ptr);
        ptr_t->elem = inner;
        return ptr_t;
    }
    if (e->kind == Expr_Cast) {
        return e->cast_type;
    }
    if (e->kind == Expr_Index) {
        TypeExpr *base = infer_expr_type(e->base, scope, arena);
        if (base && base->kind == Type_Ptr) return base->elem;
        return null;
    }
    if (e->kind == Expr_Binary) {
        return infer_expr_type(e->left, scope, arena);
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

static void rewrite_printf_call(Expr *call, TypeScope *scope, memops_arena *arena) {
    if (!call || call->kind != Expr_Call || call->args.length < 1) return;
    if (!string8_equals_cstr(&call->name, "printf")) return;

    Expr *fmt = (Expr *)call->args.data[0];
    if (!fmt || fmt->kind != Expr_String) return;

    i32 value_count = call->args.length - 1;
    const char **specs = memops_arena_push(
        arena,
        sizeof(const char *) * (value_count > 0 ? value_count : 1),
        _Alignof(const char *)
    );
    for (i32 i = 0; i < value_count; i++) {
        Expr *arg = (Expr *)call->args.data[i + 1];
        TypeExpr *ty = infer_expr_type(arg, scope, arena);
        const char *spec = printf_spec_for_type(ty);
        if (!spec) {
            printf("format error at %d:%d: cannot infer '{}' format for printf arg %d\n", arg->line, arg->col, (int)(i + 1));
            exit(1);
        }
        specs[i] = spec;
    }

    string8 in = fmt->string_lit;
    if (in.length < 2 || in.data[0] != '"' || in.data[in.length - 1] != '"') {
        return;
    }

    string8 out = string8_reserve(arena, in.length + 32);
    string8_append_byte(arena, &out, '"');
    i32 used = 0;
    for (u64 i = 1; i + 1 < in.length; i++) {
        if (in.data[i] == '{' && (i + 1) < (in.length - 1) && in.data[i + 1] == '}') {
            if (used >= value_count) {
                printf("format error at %d:%d: too many '{}' placeholders in printf format\n", fmt->line, fmt->col);
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
        printf("format error at %d:%d: printf placeholder count (%d) does not match arg count (%d)\n",
               fmt->line, fmt->col, used, value_count);
        exit(1);
    }

    fmt->string_lit = out;
}

static void rewrite_printf_in_expr(Expr *e, TypeScope *scope, memops_arena *arena);
static void rewrite_printf_in_stmt(Stmt *s, TypeScope *scope, memops_arena *arena);

static void rewrite_printf_in_expr(Expr *e, TypeScope *scope, memops_arena *arena) {
    if (!e) return;
    if (e->kind == Expr_Call) {
        rewrite_printf_call(e, scope, arena);
        for (i32 i = 0; i < e->args.length; i++) {
            rewrite_printf_in_expr((Expr *)e->args.data[i], scope, arena);
        }
        return;
    }
    if (e->kind == Expr_Binary) {
        rewrite_printf_in_expr(e->left, scope, arena);
        rewrite_printf_in_expr(e->right, scope, arena);
        return;
    }
    if (e->kind == Expr_Addr || e->kind == Expr_Cast) {
        rewrite_printf_in_expr(e->inner, scope, arena);
        return;
    }
    if (e->kind == Expr_Index) {
        rewrite_printf_in_expr(e->base, scope, arena);
        rewrite_printf_in_expr(e->index_expr, scope, arena);
        return;
    }
}

static void rewrite_printf_in_stmt(Stmt *s, TypeScope *scope, memops_arena *arena) {
    if (!s) return;
    if (s->kind == Stmt_Var) {
        rewrite_printf_in_expr(s->expr, scope, arena);
        type_scope_add(arena, scope, s->name, s->type);
        return;
    }
    if (s->kind == Stmt_Assign || s->kind == Stmt_Expr || s->kind == Stmt_Return) {
        rewrite_printf_in_expr(s->expr, scope, arena);
        return;
    }
    if (s->kind == Stmt_For) {
        TypeScope loop_scope = type_scope_copy(arena, scope);
        if (s->for_init) rewrite_printf_in_stmt(s->for_init, &loop_scope, arena);
        if (s->for_cond) rewrite_printf_in_expr(s->for_cond, &loop_scope, arena);
        if (s->for_step) rewrite_printf_in_stmt(s->for_step, &loop_scope, arena);
        for (i32 i = 0; i < s->for_body.length; i++) {
            rewrite_printf_in_stmt((Stmt *)s->for_body.data[i], &loop_scope, arena);
        }
        return;
    }
}

static void rewrite_printf_formats(Program *prog, memops_arena *arena) {
    TypeScope globals = type_scope_make(arena, 64);
    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *g = (Stmt *)prog->globals.data[i];
        rewrite_printf_in_expr(g->expr, &globals, arena);
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
            rewrite_printf_in_stmt((Stmt *)p->body.data[j], &scope, arena);
        }
    }
}

static void emit_expr(memops_arena *arena, string8 *out, Expr *e, TypeSub sub, string8 generic_name);

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
    if (e->kind == Expr_ZeroInit) {
        emit_cstr(arena, out, "{}");
        return;
    }
    if (e->kind == Expr_Name) {
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
        emit_cstr(arena, out, "&");
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
    if (e->kind == Expr_Binary) {
        emit_expr(arena, out, e->left, sub, generic_name);
        if (e->op == Token_Plus) {
            emit_cstr(arena, out, " + ");
        } else if (e->op == Token_Minus) {
            emit_cstr(arena, out, " - ");
        } else if (e->op == Token_Star) {
            emit_cstr(arena, out, " * ");
        } else if (e->op == Token_Slash) {
            emit_cstr(arena, out, " / ");
        } else if (e->op == Token_LAngle) {
            emit_cstr(arena, out, " < ");
        } else if (e->op == Token_RAngle) {
            emit_cstr(arena, out, " > ");
        } else {
            emit_cstr(arena, out, " /* unsupported op */ ");
        }
        emit_expr(arena, out, e->right, sub, generic_name);
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
        emit_type(arena, out, s->type, sub);
        emit_cstr(arena, out, " ");
        emit_string8(arena, out, s->name);
        if (s->expr) {
            emit_cstr(arena, out, " = ");
            emit_expr(arena, out, s->expr, sub, generic_name);
        }
        emit_cstr(arena, out, ";\n");
        return;
    }
    if (s->kind == Stmt_Return) {
        emit_cstr(arena, out, "return ");
        emit_expr(arena, out, s->expr, sub, generic_name);
        emit_cstr(arena, out, ";\n");
        return;
    }
    if (s->kind == Stmt_Assign) {
        emit_string8(arena, out, s->name);
        if (s->assign_op == Token_Equal) emit_cstr(arena, out, " = ");
        else if (s->assign_op == Token_PlusEqual) emit_cstr(arena, out, " += ");
        else if (s->assign_op == Token_MinusEqual) emit_cstr(arena, out, " -= ");
        else if (s->assign_op == Token_StarEqual) emit_cstr(arena, out, " *= ");
        else if (s->assign_op == Token_SlashEqual) emit_cstr(arena, out, " /= ");
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
                emit_type(arena, out, s->for_init->type, sub);
                emit_cstr(arena, out, " ");
                emit_string8(arena, out, s->for_init->name);
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
                emit_string8(arena, out, s->for_step->name);
                if (s->for_step->assign_op == Token_Equal) emit_cstr(arena, out, " = ");
                else if (s->for_step->assign_op == Token_PlusEqual) emit_cstr(arena, out, " += ");
                else if (s->for_step->assign_op == Token_MinusEqual) emit_cstr(arena, out, " -= ");
                else if (s->for_step->assign_op == Token_StarEqual) emit_cstr(arena, out, " *= ");
                else if (s->for_step->assign_op == Token_SlashEqual) emit_cstr(arena, out, " /= ");
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
}

static void emit_struct_decl(memops_arena *arena, string8 *out, StructDecl *decl) {
    emit_cstr(arena, out, "structdef(");
    emit_string8(arena, out, decl->name);
    emit_cstr(arena, out, ") {\n");
    for (i32 i = 0; i < decl->fields.length; i++) {
        Field *f = (Field *)decl->fields.data[i];
        emit_cstr(arena, out, "    ");
        emit_type(arena, out, f->type, (TypeSub){0});
        emit_cstr(arena, out, " ");
        emit_string8(arena, out, f->name);
        emit_cstr(arena, out, ";\n");
    }
    emit_cstr(arena, out, "};\n\n");
}

static void emit_struct_fwd_decl(memops_arena *arena, string8 *out, string8 name) {
    emit_cstr(arena, out, "structdecl(");
    emit_string8(arena, out, name);
    emit_cstr(arena, out, ");\n");
}

static void emit_struct_decl_mono(memops_arena *arena, string8 *out, StructDecl *decl, string8 type_mangled, TypeExpr *arg) {
    TypeSub sub = {0};
    sub.has = true;
    sub.param = decl->type_param;
    sub.arg = arg;

    emit_cstr(arena, out, "structdef(");
    emit_string8(arena, out, decl->name);
    emit_cstr(arena, out, "_");
    emit_string8(arena, out, type_mangled);
    emit_cstr(arena, out, ") {\n");
    for (i32 i = 0; i < decl->fields.length; i++) {
        Field *f = (Field *)decl->fields.data[i];
        emit_cstr(arena, out, "    ");
        emit_type(arena, out, f->type, sub);
        emit_cstr(arena, out, " ");
        emit_string8(arena, out, f->name);
        emit_cstr(arena, out, ";\n");
    }
    emit_cstr(arena, out, "};\n\n");
}

static void emit_proc_decl(memops_arena *arena, string8 *out, ProcDecl *decl) {
    if (decl->is_external) return;
    emit_type(arena, out, decl->ret_type, (TypeSub){0});
    emit_cstr(arena, out, " ");
    emit_string8(arena, out, decl->name);
    emit_cstr(arena, out, "(");
    for (i32 i = 0; i < decl->params.length; i++) {
        if (i > 0) emit_cstr(arena, out, ", ");
        Param *p = (Param *)decl->params.data[i];
        emit_type(arena, out, p->type, (TypeSub){0});
        emit_cstr(arena, out, " ");
        emit_string8(arena, out, p->name);
    }
    emit_cstr(arena, out, ") {\n");
    for (i32 i = 0; i < decl->body.length; i++) {
        emit_cstr(arena, out, "    ");
        emit_stmt(arena, out, (Stmt *)decl->body.data[i], (TypeSub){0}, (string8){0});
    }
    emit_cstr(arena, out, "}\n\n");
}

static void emit_proc_proto(memops_arena *arena, string8 *out, ProcDecl *decl) {
    emit_type(arena, out, decl->ret_type, (TypeSub){0});
    emit_cstr(arena, out, " ");
    emit_string8(arena, out, decl->name);
    emit_cstr(arena, out, "(");
    for (i32 i = 0; i < decl->params.length; i++) {
        if (i > 0) emit_cstr(arena, out, ", ");
        Param *p = (Param *)decl->params.data[i];
        emit_type(arena, out, p->type, (TypeSub){0});
        emit_cstr(arena, out, " ");
        emit_string8(arena, out, p->name);
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
    emit_mono_proc_name(arena, out, decl->name, type_mangled);
    emit_cstr(arena, out, "(");
    for (i32 i = 0; i < decl->params.length; i++) {
        if (i > 0) emit_cstr(arena, out, ", ");
        Param *p = (Param *)decl->params.data[i];
        emit_type(arena, out, p->type, sub);
        emit_cstr(arena, out, " ");
        emit_string8(arena, out, p->name);
    }
    emit_cstr(arena, out, ") {\n");
    for (i32 i = 0; i < decl->body.length; i++) {
        emit_cstr(arena, out, "    ");
        emit_stmt(arena, out, (Stmt *)decl->body.data[i], sub, decl->name);
    }
    emit_cstr(arena, out, "}\n\n");
}

static void emit_proc_proto_mono(memops_arena *arena, string8 *out, ProcDecl *decl, string8 type_mangled, TypeExpr *arg) {
    TypeSub sub = {0};
    sub.has = true;
    sub.param = decl->type_param;
    sub.arg = arg;

    emit_type(arena, out, decl->ret_type, sub);
    emit_cstr(arena, out, " ");
    emit_mono_proc_name(arena, out, decl->name, type_mangled);
    emit_cstr(arena, out, "(");
    for (i32 i = 0; i < decl->params.length; i++) {
        if (i > 0) emit_cstr(arena, out, ", ");
        Param *p = (Param *)decl->params.data[i];
        emit_type(arena, out, p->type, sub);
        emit_cstr(arena, out, " ");
        emit_string8(arena, out, p->name);
    }
    emit_cstr(arena, out, ");\n");
}

static void emit_program(memops_arena *arena, Program *prog, string8 *out) {
    emit_cstr(arena, out, "#include <core.h>\n\n");
    for (i32 i = 0; i < prog->imports.length; i++) {
        emit_cstr(arena, out, "#include ");
        emit_string8(arena, out, prog->imports.data[i]);
        emit_cstr(arena, out, "\n");
    }
    if (prog->imports.length > 0) {
        emit_cstr(arena, out, "\n");
    }

    // Forward declarations for all structs (non-generic + monomorphized)
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic) {
            emit_struct_fwd_decl(arena, out, decl->name);
        }
    }
    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic) continue;

        Array_string8 instances = Array_string8_reserve(arena, 4);
        collect_generic_struct_instances(prog, decl, &instances, arena);
        for (i32 j = 0; j < instances.length; j++) {
            string8 mono = string8_reserve(arena, decl->name.length + 1 + instances.data[j].length);
            string8_append_bytes(arena, &mono, decl->name.data, decl->name.length);
            string8_append_cstr(arena, &mono, "_");
            string8_append_bytes(arena, &mono, instances.data[j].data, instances.data[j].length);
            emit_struct_fwd_decl(arena, out, mono);
        }
    }
    emit_cstr(arena, out, "\n");

    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic) {
            emit_struct_decl(arena, out, decl);
        }
    }

    for (i32 i = 0; i < prog->structs.length; i++) {
        StructDecl *decl = (StructDecl *)prog->structs.data[i];
        if (!decl->is_generic) continue;

        Array_string8 instances = Array_string8_reserve(arena, 4);
        collect_generic_struct_instances(prog, decl, &instances, arena);

        for (i32 j = 0; j < instances.length; j++) {
            string8 mangle = instances.data[j];
            TypeExpr *arg = type_new(arena, Type_Name);
            arg->name = mangle;
            emit_struct_decl_mono(arena, out, decl, mangle, arg);
        }
    }

    for (i32 i = 0; i < prog->globals.length; i++) {
        Stmt *s = (Stmt *)prog->globals.data[i];
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

        Array_string8 instances = Array_string8_reserve(arena, 4);
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

        Array_string8 instances = Array_string8_reserve(arena, 4);
        collect_generic_proc_instances(prog, decl, &instances, arena);
        for (i32 j = 0; j < instances.length; j++) {
            string8 mangle = instances.data[j];
            TypeExpr *arg = type_new(arena, Type_Name);
            arg->name = mangle;
            emit_proc_decl_mono(arena, out, decl, mangle, arg);
        }
    }
}

static bool write_string8_to_file(const char *path, string8 data) {
    FILE *f = fopen(path, "wb");
    if (!f) return false;
    fwrite(data.data, 1, data.length, f);
    fclose(f);
    return true;
}

i32 main(i32 argc, char *argv[]) {
    (void)argc;
    (void)argv;
    memops_arena arena = {0};
    memops_arena_initialize(&arena);

    string8 input = string8_read_file(&arena, "src/main.i");
    if (!input.data) {
        printf("failed to read src/main.i\n");
        return 1;
    }

    Array_Token tokens = {0};
    lex_tokens(&arena, input, &tokens);

    Parser parser = {0};
    parser.arena = &arena;
    parser.tokens = tokens;
    parser.index = 0;

    Program prog = parse_program(&parser);
    semantic_check_program(&prog, &arena);
    validate_generic_constraints(&prog, &arena);
    rewrite_printf_formats(&prog, &arena);

    string8 output = string8_reserve(&arena, input.length * 2 + 1024);
    emit_program(&arena, &prog, &output);

    if (!write_string8_to_file("src/main.i.c", output)) {
        printf("failed to write src/main.i.c\n");
        return 1;
    }

    printf("generated src/main.i.c\n");
    return 0;
}

#include <Array.c>

