#include "token.hpp"

#include <iostream>

#define CONSTANT_TOKEN_CASE(kind, literal)                                     \
    case TokenKind::kind:                                                      \
        std::cout << #kind ": " literal << " | line: " << tok.line             \
                  << std::endl;                                                \
        break

#define TOKEN_TEXT_CASE(kind)                                                  \
    case TokenKind::kind:                                                      \
        std::cout << #kind ": " << tok.text << " | line: " << tok.line         \
                  << std::endl;                                                \
        break

void printToken(Token tok) {
    switch (tok.type) {
        CONSTANT_TOKEN_CASE(TOKEN_LEFT_PAREN, "(");
        CONSTANT_TOKEN_CASE(TOKEN_RIGHT_PAREN, ")");
        CONSTANT_TOKEN_CASE(TOKEN_LEFT_BRACE, "{");
        CONSTANT_TOKEN_CASE(TOKEN_RIGHT_BRACE, "}");
        CONSTANT_TOKEN_CASE(TOKEN_COMMA, ",");
        CONSTANT_TOKEN_CASE(TOKEN_DOT, ".");
        CONSTANT_TOKEN_CASE(TOKEN_MINUS, "-");
        CONSTANT_TOKEN_CASE(TOKEN_PLUS, "+");
        CONSTANT_TOKEN_CASE(TOKEN_SEMICOLON, ";");
        CONSTANT_TOKEN_CASE(TOKEN_SLASH, "/");
        CONSTANT_TOKEN_CASE(TOKEN_STAR, "*");
        CONSTANT_TOKEN_CASE(TOKEN_COLON, ":");
        CONSTANT_TOKEN_CASE(TOKEN_BANG, "!");
        CONSTANT_TOKEN_CASE(TOKEN_BANG_EQUAL, "!=");
        CONSTANT_TOKEN_CASE(TOKEN_EQUAL, "=");
        CONSTANT_TOKEN_CASE(TOKEN_EQUAL_EQUAL, "==");
        CONSTANT_TOKEN_CASE(TOKEN_GREATER, ">");
        CONSTANT_TOKEN_CASE(TOKEN_GREATER_EQUAL, ">=");
        CONSTANT_TOKEN_CASE(TOKEN_LESS, "<");
        CONSTANT_TOKEN_CASE(TOKEN_LESS_EQUAL, "<=");
        CONSTANT_TOKEN_CASE(TOKEN_AND, "and");
        CONSTANT_TOKEN_CASE(TOKEN_BOOL, "bool");
        CONSTANT_TOKEN_CASE(TOKEN_BREAK, "break");
        CONSTANT_TOKEN_CASE(TOKEN_CONTINUE, "continue");
        CONSTANT_TOKEN_CASE(TOKEN_ELSE, "else");
        CONSTANT_TOKEN_CASE(TOKEN_FALSE, "false");
        CONSTANT_TOKEN_CASE(TOKEN_FOR, "for");
        CONSTANT_TOKEN_CASE(TOKEN_FN, "fn");
        CONSTANT_TOKEN_CASE(TOKEN_IF, "if");
        CONSTANT_TOKEN_CASE(TOKEN_NUM, "num");
        CONSTANT_TOKEN_CASE(TOKEN_OR, "or");
        CONSTANT_TOKEN_CASE(TOKEN_PRINT, "print");
        CONSTANT_TOKEN_CASE(TOKEN_RETURN, "return");
        CONSTANT_TOKEN_CASE(TOKEN_SUPER, "super");
        CONSTANT_TOKEN_CASE(TOKEN_THIS, "this");
        CONSTANT_TOKEN_CASE(TOKEN_TRUE, "true");
        CONSTANT_TOKEN_CASE(TOKEN_VAR, "var");
        CONSTANT_TOKEN_CASE(TOKEN_VOID, "void");
        CONSTANT_TOKEN_CASE(TOKEN_WHILE, "while");
        CONSTANT_TOKEN_CASE(TOKEN_EOF, "<eof>");
        TOKEN_TEXT_CASE(TOKEN_IDENTIFIER);
        TOKEN_TEXT_CASE(TOKEN_STRING);
        TOKEN_TEXT_CASE(TOKEN_NUMBER);
    }
}

#undef BASIC_TOKEN_CASE