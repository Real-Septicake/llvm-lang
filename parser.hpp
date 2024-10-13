#ifndef PARSER_HPP
#define PARSER_HPP

#include "expr.hpp"
#include "stmt.hpp"
#include "token.hpp"

#include <stdexcept>
#include <vector>

using AST::Expr;
using AST::Stmt;

/// @brief A namespace containing members used for parsing
namespace parser {
/// @brief An error thrown if there is an error during parsing
class ParseError : public std::runtime_error {
  public:
    /// @brief Creates a parse error
    /// @return A new parse error
    ParseError() throw();
};

/// @brief The class used for creating the abstract syntax tree for use in later
/// parts of compilation
class Parser {
  private:
    /// @brief The tokens being parsed
    std::vector<Token *> tokens;
    /// @brief The index of the current token
    size_t current = 0;
    /// @brief Skips over all tokens until a statement ends or a new statement
    /// begins
    /// @note It is kinda fucky if it errors just before the start of a new
    /// statement, so that's something to note
    void synchronize();
    /// @brief Pops the next Token
    /// @return The popped Token
    Token *advance();
    /// @brief Returns the most recently popped character
    /// @return The most recently popped character
    Token *previous();
    /// @brief Peeks the next Token
    /// @return The peeked Token
    Token *peek();
    /// @brief Checks if the next Token is of the specificed kind
    /// @param type The type to check for
    /// @return True if the Token is of the specified type, false otherwise or
    /// if the scanner has reached the end
    bool check(TokenKind type);
    /// @brief Checks if the next Token is of any of the specified kinds, and
    /// pops it if it is
    /// @param types The types to check for
    /// @return True if the next Token is of any of the specified types, false
    /// otherwise or if the scanner has reached the end
    bool match(std::initializer_list<TokenKind> types);
    std::pair<value::ValueType, Token *> consume_type();
    /// @brief Attempt to pop a Token of the specified type, and produce an
    /// error otherwise
    /// @param type The type of Token to attempt to pop
    /// @param message The message to produce if the type of the next token does
    /// not match
    /// @return The popped token
    Token *consume(TokenKind type, std::string message);
    /// @brief Checks if the Parser has reached the end of the input
    /// @return True if the Parser has reached the end, false otherwise
    bool isAtEnd();
    /// @brief Produces an error from the provided Token and message
    /// @param token The Token the error occured at
    /// @param message The message of the error
    /// @return A \ref parser::ParseError "parse error" that can be thrown by
    /// the Parser
    ParseError error(Token *token, std::string message);
    /// @brief Parses a declaration from the current position in the supplied
    /// tokens
    /// @return The parsed statement, or `nullptr` if an error occurred
    Stmt *declaration();
    /// @brief Parses a function declaration of the specified type from the
    /// current position in the supplied tokens
    /// @param kind The kind of function to parse
    /// @return The parsed function declaration
    AST::Function *function(std::string kind);
    /// @brief Parses a variable declaration from the current position in the
    /// supplied tokens
    /// @return The parsed variable declaration
    AST::Var *varDeclaration();
    /// @brief Parses a statement from the current position in the supplied
    /// tokens
    /// @return The parsed statement
    Stmt *statement();
    /// @brief Parses a block statement from the current position in the
    /// supplied tokens
    /// @return The parsed block statement
    std::vector<Stmt *> block();
    /// @brief Parses a for statement from the current position in the supplied
    /// tokens
    /// @return The parsed for statement
    AST::For *forStatement();
    /// @brief Parses an if statement from the current position in the supplied
    /// tokens
    /// @return The parsed if statement
    AST::If *ifStatement();
    /// @brief Parses a print statement from the current position in the
    /// supplied tokens
    /// @return The parsed print statement
    AST::Print *printStatement();
    /// @brief Parse a return statement from the current position in the
    /// supplied tokens
    /// @return The parsed return statement
    AST::Return *returnStatement();
    /// @brief Parse a while statement from the current position in the supplied
    /// tokens
    /// @return The parsed while statement
    AST::While *whileStatement();
    /// @brief Parse a break statement from the current position in the supplied
    /// tokens
    /// @return The parsed break statement
    AST::Break *breakStatement();
    /// @brief Parse a continue statement from the current position in the
    /// supplied tokens
    /// @return The parsed continue statement
    AST::Continue *continueStatement();
    /// @brief Parses an expression statement from the current position in the
    /// supplied tokens
    /// @return The parsed expression statement
    Stmt *expressionStatement();
    /// @brief Parses an exprssion from the current position in the supplied
    /// tokens
    /// @return The parsed expression
    Expr *expression();
    /// @brief Parses an expression of `assignment` precendence from the current
    /// position in the supplied tokens
    /// @return The parsed expression
    Expr *assignment();
    /// @brief Parses an expression of `or_` precendence from the current
    /// position in the supplied tokens
    /// @return The parsed expression
    Expr *or_();
    /// @brief Parses an expression of `and_` precendence from the current
    /// position in the supplied tokens
    /// @return The parsed expression
    Expr *and_();
    /// @brief Parses an expresion of `equality` precedence from the current
    /// position in the supplied tokens
    /// @return The parsed expression
    Expr *equality();
    /// @brief Parses an expression of `comparison` precedence from the current
    /// position in the supplied tokens
    /// @return The parsed expression
    Expr *comparison();
    /// @brief Parses an expression of `term` precedence from the current
    /// position in the supplied tokens
    /// @return The parsed expression
    Expr *term();
    /// @brief Parses an expression of `factor` precedence from the current
    /// position in the supplied tokens
    /// @return The parsed expression
    Expr *factor();
    /// @brief Parses an expression of `unary` precedence from the current
    /// position in the supplied tokens
    /// @return The parsed expression
    Expr *unary();
    /// @brief Parses an expression of `call` precedence from the current
    /// position in the supplied tokens
    /// @return The parsed expression
    Expr *call();
    /// @brief Finishes a call expression started by a call to Parser::call
    /// @param callee The expression being called
    /// @return The finished expression
    Expr *finishCall(Expr *callee);
    /// @brief Parses an expression of `primary` precedence from the current
    /// position in the supplied tokens
    /// @return The parsed expression
    Expr *primary();

  public:
    /// @brief Creates a parser with the supplied list of tokens
    /// @param tokens The tokens to parse
    Parser(std::vector<Token *> tokens);
    /// @brief Destructor
    ~Parser() = default;
    /// @brief Parse the tokens into statements
    /// @return The parsed list of statements
    std::vector<Stmt *> parse();
};
} // namespace parser

#endif