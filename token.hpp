#ifndef TOKEN_HPP
#define TOKEN_HPP

#include <string>

/// @brief Represents the types of valid tokens
enum TokenKind {
    // Single-character tokens.
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_MINUS,
    TOKEN_PLUS,
    TOKEN_SEMICOLON,
    TOKEN_SLASH,
    TOKEN_STAR,
    TOKEN_COLON,
    // One or two character tokens.
    TOKEN_BANG,
    TOKEN_BANG_EQUAL,
    TOKEN_EQUAL,
    TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,
    // Literals.
    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_NUMBER,
    // Keywords.
    TOKEN_AND,
    TOKEN_BOOL,
    TOKEN_BREAK,
    TOKEN_CONTINUE,
    TOKEN_ELSE,
    TOKEN_FALSE,
    TOKEN_FOR,
    TOKEN_FN,
    TOKEN_IF,
    TOKEN_NUM,
    TOKEN_OR,
    TOKEN_PRINT,
    TOKEN_RETURN,
    TOKEN_SUPER,
    TOKEN_THIS,
    TOKEN_TRUE,
    TOKEN_VAR,
    TOKEN_VOID,
    TOKEN_WHILE,

    TOKEN_EOF
};

/// @brief A struct representing a piece of text present in the source file
struct Token {
    /// @brief The type of token this struct represents
    TokenKind type;
    /// @brief The text in the source file that this token represents
    std::string text;
    /// @brief The line this token occurred on
    size_t line;
};

/// @brief Prints a Token, used for debugging
/// @param tok The Token to print
void printToken(Token tok);

#endif