#include "parser.hpp"

#include "error.hpp"
#include "value.hpp"

#include <string>
#include <vector>
#include <unordered_map>
#include <iostream>

static std::unordered_map<TokenKind, value::ValueType> type_map = {
    {TokenKind::TOKEN_BOOL, value::ValueType::VAL_BOOL},
    {TokenKind::TOKEN_NUM, value::ValueType::VAL_NUM}
};

std::vector<Stmt *> parser::Parser::parse() {
    std::vector<Stmt *> statements = std::vector<Stmt *>();
    while (!isAtEnd()) {
        statements.push_back(declaration());
    }

    return statements;
}

void parser::Parser::synchronize() {
    advance();

    while (!isAtEnd()) {
        if (previous()->type == TokenKind::TOKEN_SEMICOLON)
            return;

        switch (peek()->type) {
        case TokenKind::TOKEN_FN:
        case TokenKind::TOKEN_VAR:
        case TokenKind::TOKEN_FOR:
        case TokenKind::TOKEN_IF:
        case TokenKind::TOKEN_WHILE:
        case TokenKind::TOKEN_PRINT:
        case TokenKind::TOKEN_RETURN:
            return;
        }

        advance();
    }
}

Token *parser::Parser::advance() {
    if (!isAtEnd())
        current++;
    return previous();
}

Token *parser::Parser::previous() {
    return tokens[current - 1];
}

Token *parser::Parser::peek() {
    return tokens[current];
}

bool parser::Parser::check(TokenKind type) {
    if (isAtEnd())
        return false;
    return peek()->type == type;
}

bool parser::Parser::match(std::initializer_list<TokenKind> tokens) {
    for (auto type : tokens) {
        if (check(type)) {
            advance();
            return true;
        }
    }

    return false;
}

Token *parser::Parser::consume(TokenKind type, std::string message) {
    if (check(type))
        return advance();

    throw error(peek(), message);
}

std::pair<value::ValueType, Token *> parser::Parser::consume_type() {
    if(match({TOKEN_BOOL, TOKEN_NUM})) {
        Token *tok = previous();
        auto val_type = type_map.find(tok->type);
        if(val_type != type_map.end()) {
            return std::make_pair(val_type->second, tok);
        }
    }

    throw error(peek(), "Expect type name.");
}

bool parser::Parser::isAtEnd() {
    return peek()->type == TokenKind::TOKEN_EOF;
}

parser::ParseError parser::Parser::error(Token *token, std::string message) {
    error::error(token, message);
    return ParseError();
}

Stmt *parser::Parser::declaration() {
    try {
        if (match({TOKEN_FN}))
            return function("function");
        if (match({TOKEN_VAR}))
            return varDeclaration();

        return statement();
    } catch (ParseError) {
        synchronize();
        return nullptr;
    }
}

AST::Function *parser::Parser::function(std::string kind) {
    Token *name = consume(TOKEN_IDENTIFIER, "Expect " + kind + " name.");
    consume(TOKEN_LEFT_PAREN, "Expect '(' after " + kind + " name.");

    std::vector<Token *> params = std::vector<Token *>();
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            if (params.size() >= 255) {
                error(peek(), "Can't have more than 255 parameters");
            }

            Token *param = consume(TOKEN_IDENTIFIER, "Expect parameter name.");
            params.push_back(param);
        } while (match({TOKEN_COMMA}));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");

    consume(TOKEN_LEFT_BRACE, "Expect '{' before " + kind + " body.");
    std::vector<Stmt *> body = block();
    return new AST::Function(name, params, body);
}

AST::Var *parser::Parser::varDeclaration() {
    Token *name = consume(TOKEN_IDENTIFIER, "Expect variable name.");
    consume(TOKEN_COLON, "Expect ':' after variable name.");
    auto type = consume_type();

    Expr *initializer = nullptr;
    if (match({TOKEN_EQUAL})) {
        initializer = expression();
    }

    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");
    return new AST::Var(name, type, initializer);
}

Stmt *parser::Parser::statement() {
    if (match({TOKEN_FOR}))
        return forStatement();
    if (match({TOKEN_IF}))
        return ifStatement();
    if (match({TOKEN_PRINT}))
        return printStatement();
    if (match({TOKEN_RETURN}))
        return returnStatement();
    if (match({TOKEN_WHILE}))
        return whileStatement();
    if (match({TOKEN_BREAK}))
        return breakStatement();
    if (match({TOKEN_CONTINUE}))
        return continueStatement();
    if (match({TOKEN_LEFT_BRACE}))
        return new AST::Block(block());

    return expressionStatement();
}

std::vector<Stmt *> parser::Parser::block() {
    std::vector<Stmt *> stmts = std::vector<Stmt *>();

    while (!check(TOKEN_RIGHT_BRACE) && !isAtEnd()) {
        stmts.push_back(declaration());
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
    return stmts;
}

AST::For *parser::Parser::forStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

    Stmt *initializer;
    if (match({TOKEN_SEMICOLON})) {
        initializer = nullptr;
    } else if (match({TOKEN_VAR})) {
        initializer = varDeclaration();
    } else {
        initializer = expressionStatement();
    }

    Expr *condition = new AST::Literal(value::boolVal(true));
    if (!check({TOKEN_SEMICOLON})) {
        condition = expression();
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after loop condition");

    Expr *increment = nullptr;
    if (!check({TOKEN_RIGHT_PAREN})) {
        increment = expression();
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

    Stmt *body = statement();

    return new AST::For(initializer, condition, increment, body);
}

AST::If *parser::Parser::ifStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    Expr *condition = expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after if condition.");

    Stmt *thenBranch = statement();
    Stmt *elseBranch = nullptr;
    if (match({TOKEN_ELSE})) {
        elseBranch = statement();
    }

    return new AST::If(condition, thenBranch, elseBranch);
}

AST::Print *parser::Parser::printStatement() {
    Expr *value = expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    return new AST::Print(value);
}

AST::Return *parser::Parser::returnStatement() {
    Token *keyword = previous();
    Expr *value    = nullptr;
    if (!check(TOKEN_SEMICOLON)) {
        value = expression();
    }

    consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
    return new AST::Return(keyword, value);
}

AST::While *parser::Parser::whileStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    Expr *condition = expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");
    Stmt *body = statement();

    return new AST::While(condition, body);
}

AST::Break *parser::Parser::breakStatement() {
    Token *keyword = previous();
    consume(TOKEN_SEMICOLON, "Expect ';' after 'break'");
    return new AST::Break(keyword);
}

AST::Continue *parser::Parser::continueStatement() {
    Token *keyword = previous();
    consume(TOKEN_SEMICOLON, "Expect ';' after 'break'");
    return new AST::Continue(keyword);
}

Stmt *parser::Parser::expressionStatement() {
    Expr *expr = expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    return new AST::Expression(expr);
}

Expr *parser::Parser::expression() {
    return assignment();
}

Expr *parser::Parser::assignment() {
    Expr *expr = or_();

    if (match({TOKEN_EQUAL})) {
        Token *equals = previous();
        Expr *value   = assignment();

        if (expr->type == AST::ExprType::VariableType) {
            Token *name = static_cast<AST::Variable *>(expr)->name;
            return new AST::Assign(name, value);
        } else if (expr->type == AST::ExprType::GetType) {
            AST::Get *get = static_cast<AST::Get *>(expr);
            return new AST::Set(get->object, get->name, value);
        }

        error(equals, "Invalid assignment target.");
    }

    return expr;
}

Expr *parser::Parser::or_() {
    Expr *expr = and_();

    while (match({TOKEN_OR})) {
        Token *op   = previous();
        Expr *right = and_();
        expr        = new AST::Logical(expr, op, right);
    }

    return expr;
}

Expr *parser::Parser::and_() {
    Expr *expr = equality();

    while (match({TOKEN_AND})) {
        Token *op   = previous();
        Expr *right = equality();
        expr        = new AST::Logical(expr, op, right);
    }

    return expr;
}

Expr *parser::Parser::equality() {
    Expr *expr = comparison();

    while (match({TOKEN_BANG_EQUAL, TOKEN_EQUAL_EQUAL})) {
        Token *op   = previous();
        Expr *right = comparison();
        expr        = new AST::Binary(expr, op, right);
    }

    return expr;
}

Expr *parser::Parser::comparison() {
    Expr *expr = term();

    while (match(
        {TOKEN_GREATER, TOKEN_GREATER_EQUAL, TOKEN_LESS, TOKEN_LESS_EQUAL})) {
        Token *op   = previous();
        Expr *right = term();
        expr        = new AST::Binary(expr, op, right);
    }

    return expr;
}

Expr *parser::Parser::term() {
    Expr *expr = factor();

    while (match({TOKEN_MINUS, TOKEN_PLUS})) {
        Token *op   = previous();
        Expr *right = factor();
        expr        = new AST::Binary(expr, op, right);
    }

    return expr;
}

Expr *parser::Parser::factor() {
    Expr *expr = unary();

    while (match({TOKEN_SLASH, TOKEN_STAR})) {
        Token *op   = previous();
        Expr *right = unary();
        expr        = new AST::Binary(expr, op, right);
    }

    return expr;
}

Expr *parser::Parser::unary() {
    if (match({TOKEN_BANG, TOKEN_MINUS})) {
        Token *op   = previous();
        Expr *right = unary();
        return new AST::Unary(op, right);
    }

    return call();
}

Expr *parser::Parser::call() {
    Expr *expr = primary();

    while (1) {
        if (match({TOKEN_LEFT_PAREN})) {
            expr = finishCall(expr);
        } else if (match({TOKEN_DOT})) {
            Token *name =
                consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
            expr = new AST::Get(expr, name);
        } else {
            break;
        }
    }

    return expr;
}

Expr *parser::Parser::finishCall(Expr *callee) {
    std::vector<Expr *> args = std::vector<Expr *>();
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            if (args.size() >= 255) {
                error(peek(), "Can't have more than 255 arguments.");
            }

            args.push_back(expression());
        } while (match({TOKEN_COMMA}));
    }

    Token *paren = consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");

    return new AST::Call(callee, paren, args);
}

Expr *parser::Parser::primary() {
    if (match({TOKEN_TRUE}))
        return new AST::Literal(value::boolVal(true));
    if (match({TOKEN_FALSE}))
        return new AST::Literal(value::boolVal(false));
    if (match({TOKEN_NUMBER}))
        return new AST::Literal(value::numVal(stod(previous()->text)));

    if (match({TOKEN_SUPER})) {
        Token *keyword = previous();
        consume(TOKEN_DOT, "Expect '.' after 'super'.");
        Token *method =
            consume(TOKEN_IDENTIFIER, "Expect superclass method name.");
        return new AST::Super(keyword, method);
    }

    if (match({TOKEN_THIS})) {
        Token *keyword = previous();
        return new AST::This(keyword);
    }

    if (match({TOKEN_IDENTIFIER})) {
        Token *name = previous();
        consume(TOKEN_COLON, "Expect ':' after variable name.");
        return new AST::Variable(name);
    }

    if (match({TOKEN_LEFT_PAREN})) {
        Expr *expr = expression();
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
        return new AST::Grouping(expr);
    }

    throw error(peek(), "Expect expression.");
}

parser::Parser::Parser(std::vector<Token *> tokens) {
    this->tokens = tokens;
}

parser::ParseError::ParseError() throw() : runtime_error("Parser error.") {}
