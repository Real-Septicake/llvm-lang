#include "scanner.hpp"

#include "error.hpp"

#include <iostream>
#include <unordered_map>

static std::unordered_map<std::string, TokenKind> keywords = {
    {"and",      TokenKind::TOKEN_AND     },
    {"bool",     TokenKind::TOKEN_BOOL    },
    {"break",    TokenKind::TOKEN_BREAK   },
    {"continue", TokenKind::TOKEN_CONTINUE},
    {"else",     TokenKind::TOKEN_ELSE    },
    {"false",    TokenKind::TOKEN_FALSE   },
    {"for",      TokenKind::TOKEN_FOR     },
    {"fn",       TokenKind::TOKEN_FN      },
    {"if",       TokenKind::TOKEN_IF      },
    {"num",      TokenKind::TOKEN_NUM     },
    {"or",       TokenKind::TOKEN_OR      },
    {"print",    TokenKind::TOKEN_PRINT   },
    {"return",   TokenKind::TOKEN_RETURN  },
    {"super",    TokenKind::TOKEN_SUPER   },
    {"this",     TokenKind::TOKEN_THIS    },
    {"true",     TokenKind::TOKEN_TRUE    },
    {"var",      TokenKind::TOKEN_VAR     },
    {"while",    TokenKind::TOKEN_WHILE   }
};

static bool isAlpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static bool isDigit(char c) {
    return c >= '0' && c <= '9';
}

static bool isAlphaNumeric(char c) {
    return isAlpha(c) || isDigit(c);
}

char Scanner::advance() {
    return source[current++];
}

bool Scanner::isAtEnd() {
    return current >= source.length();
}

bool Scanner::match(char expected) {
    if (isAtEnd())
        return false;
    if (source[current] != expected)
        return false;

    current++;
    return true;
}

char Scanner::peek() {
    if (isAtEnd())
        return '\0';
    return source[current];
}

char Scanner::peekNext() {
    if (current + 1 >= source.length())
        return '\0';
    return source[current + 1];
}

void Scanner::scanToken() {
    char c = advance();
    switch (c) {
    case '(':
        addToken(TokenKind::TOKEN_LEFT_PAREN);
        break;
    case ')':
        addToken(TokenKind::TOKEN_RIGHT_PAREN);
        break;
    case '{':
        addToken(TokenKind::TOKEN_LEFT_BRACE);
        break;
    case '}':
        addToken(TokenKind::TOKEN_RIGHT_BRACE);
        break;
    case ',':
        addToken(TokenKind::TOKEN_COMMA);
        break;
    case '.':
        addToken(TokenKind::TOKEN_DOT);
        break;
    case '-':
        addToken(TokenKind::TOKEN_MINUS);
        break;
    case '+':
        addToken(TokenKind::TOKEN_PLUS);
        break;
    case ';':
        addToken(TokenKind::TOKEN_SEMICOLON);
        break;
    case '*':
        addToken(TokenKind::TOKEN_STAR);
        break;
    case ':':
        addToken(TokenKind::TOKEN_COLON);
        break;
    case '!':
        addToken(match('=') ? TokenKind::TOKEN_BANG_EQUAL
                            : TokenKind::TOKEN_BANG);
        break;
    case '=':
        addToken(match('=') ? TokenKind::TOKEN_EQUAL_EQUAL
                            : TokenKind::TOKEN_EQUAL);
        break;
    case '<':
        addToken(match('=') ? TokenKind::TOKEN_LESS_EQUAL
                            : TokenKind::TOKEN_LESS);
        break;
    case '>':
        addToken(match('=') ? TokenKind::TOKEN_GREATER_EQUAL
                            : TokenKind::TOKEN_GREATER);
        break;
    case '/':
        if (match('/')) {
            while (peek() != '\n' && !isAtEnd())
                advance();
        } else {
            addToken(TokenKind::TOKEN_SLASH);
        }
        break;
    case ' ':
    case '\r':
    case '\t':
        break;
    case '\n':
        line++;
        break;
    default:
        if (isDigit(c)) {
            number();
        } else if (isAlpha(c)) {
            identifier();
        } else {
            error::error(line, "Unexpected character");
        }
        break;
    }
}

void Scanner::identifier() {
    while (isAlphaNumeric(peek())) {
        advance();
    }

    std::string text = source.substr(start, current - start);
    auto type        = keywords.find(text);
    if (type == keywords.end())
        addToken(TokenKind::TOKEN_IDENTIFIER);
    else
        addToken(type->second);
}

void Scanner::number() {
    while (isDigit(peek()))
        advance();
    if (peek() == '.' && isDigit(peekNext())) {
        advance();

        while (isDigit(peek()))
            advance();
    }

    addToken(TokenKind::TOKEN_NUMBER);
}

std::vector<Token *> Scanner::scanTokens() {
    while (!isAtEnd()) {
        start = current;
        scanToken();
    }
    tokens.push_back(new Token{TokenKind::TOKEN_EOF, "", line});
    return tokens;
}

void Scanner::addToken(TokenKind type) {
    // char text[current - start];
    // source.substr(start, current - start).copy(text, current-start);
    std::string text = source.substr(start, current - start);
    tokens.push_back(new Token{type, std::string(text), line});
}

Scanner::Scanner(std::string src) {
    source = src;
}