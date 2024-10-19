#ifndef PARSER_HPP
#define PARSER_HPP

#include "expr.hpp"
#include "stmt.hpp"
#include "token.hpp"

#include <stdexcept>
#include <vector>

using AST::Expr;
using AST::Stmt;

namespace parser {
class ParseError : public std::runtime_error {
  public:
    ParseError() throw();
};

class Parser {
  private:
    enum TypeExpect { VAR, FN };
    std::vector<Token *> tokens;
    size_t current = 0;
    void synchronize();
    Token *advance();
    Token *previous();
    Token *peek();
    bool check(TokenKind type);
    bool match(std::initializer_list<TokenKind> types);
    std::pair<value::ValueType, Token *> consume_type(TypeExpect expected);
    Token *consume(TokenKind type, std::string message);
    bool isAtEnd();
    ParseError error(Token *token, std::string message);
    Stmt *declaration();
    AST::Proto *proto();
    AST::Function *function(std::string kind);
    AST::Var *varDeclaration();
    Stmt *statement();
    std::vector<Stmt *> block();
    AST::For *forStatement();
    AST::If *ifStatement();
    AST::Print *printStatement();
    AST::Return *returnStatement();
    AST::While *whileStatement();
    AST::Break *breakStatement();
    AST::Continue *continueStatement();
    Stmt *expressionStatement();
    Expr *expression();
    Expr *assignment();
    Expr *or_();
    Expr *and_();
    Expr *equality();
    Expr *comparison();
    Expr *term();
    Expr *factor();
    Expr *unary();
    Expr *call();
    Expr *finishCall(Expr *callee);
    Expr *primary();

  public:
    Parser(std::vector<Token *> tokens);
    ~Parser() = default;
    std::vector<Stmt *> parse();
};
} // namespace parser

#endif