#ifndef STMT_HH
#define STMT_HH

#include "expr.hpp"
#include "token.hpp"

#include <vector>
/// @brief A namespace containing members used for the creation and usage of the
/// abstract syntax tree
namespace AST {

/// @brief An enum representing the possible AST::Stmt types
enum StmtType {
    BlockType,
    ExpressionType,
    FunctionType,
    ClassType,
    IfType,
    PrintType,
    ReturnType,
    VarType,
    WhileType,
    ForType,
    BreakType,
    ContinueType,
};

// Forward declaration of visitor
class StmtVisitor;

/// @brief The base class representing statement nodes
class Stmt {
  public:
    /// @brief The type of this node, used for quick type checks
    StmtType type;
    /// @brief Accept an AST::StmtVisitor
    /// @param visitor The visitor to accept
    virtual void accept(StmtVisitor *visitor) = 0;
    /// @brief Create an AST::Stmt with the specified type
    /// @param type The type of this AST::Stmt node
    Stmt(StmtType type) : type(type) {}
}; // Class Stmt

/// @brief The node for a block statement
class Block : public Stmt {
  public:
    std::vector<Stmt *> statements;
    Block(std::vector<Stmt *> statements);
    void accept(StmtVisitor *visitor) override;
};

/// @brief The node for an expression statement
class Expression : public Stmt {
  public:
    Expr *expression;
    Expression(Expr *expression);
    void accept(StmtVisitor *visitor) override;
};

/// @brief The node for a function statement
class Function : public Stmt {
  public:
    Token *name;
    std::vector<Token *> params;
    std::vector<Stmt *> body;
    Function(Token *name, std::vector<Token *> params,
             std::vector<Stmt *> body);
    void accept(StmtVisitor *visitor) override;
};

/// @brief The node for a class statement
class Class : public Stmt {
  public:
    Token *name;
    AST::Variable *superclass;
    std::vector<AST::Function *> methods;
    Class(Token *name, AST::Variable *superclass,
          std::vector<AST::Function *> methods);
    void accept(StmtVisitor *visitor) override;
};

/// @brief The node for an if statement
class If : public Stmt {
  public:
    Expr *condition;
    Stmt *thenBranch;
    Stmt *elseBranch;
    If(Expr *condition, Stmt *thenBranch, Stmt *elseBranch);
    void accept(StmtVisitor *visitor) override;
};

/// @brief The node for a print statement
class Print : public Stmt {
  public:
    Expr *expression;
    Print(Expr *expression);
    void accept(StmtVisitor *visitor) override;
};

/// @brief The node for a return statement
class Return : public Stmt {
  public:
    Token *keyword;
    Expr *value;
    Return(Token *keyword, Expr *value);
    void accept(StmtVisitor *visitor) override;
};

/// @brief The node for a var statement
class Var : public Stmt {
  public:
    Token *name;
    std::pair<value::ValueType, Token *> type;
    Expr *initializer;
    Var(Token *name, std::pair<value::ValueType, Token *> type,
        Expr *initializer);
    void accept(StmtVisitor *visitor) override;
};

/// @brief The node for a while statement
class While : public Stmt {
  public:
    Expr *condition;
    Stmt *body;
    While(Expr *condition, Stmt *body);
    void accept(StmtVisitor *visitor) override;
};

/// @brief The node for a for statement
class For : public Stmt {
  public:
    Stmt *initializer;
    Expr *condition;
    Expr *increment;
    Stmt *body;
    For(Stmt *initializer, Expr *condition, Expr *increment, Stmt *body);
    void accept(StmtVisitor *visitor) override;
};

/// @brief The node for a break statement
class Break : public Stmt {
  public:
    Token *keyword;
    Break(Token *keyword);
    void accept(StmtVisitor *visitor) override;
};

/// @brief The node for a continue statement
class Continue : public Stmt {
  public:
    Token *keyword;
    Continue(Token *keyword);
    void accept(StmtVisitor *visitor) override;
};

/// @brief The visitor for the AST::Stmt class
class StmtVisitor {
  public:
    /// @brief Visit the AST::Block node
    /// @param stmt The node to visit
    virtual void visitBlockStmt(AST::Block *stmt) = 0;
    /// @brief Visit the AST::Expression node
    /// @param stmt The node to visit
    virtual void visitExpressionStmt(AST::Expression *stmt) = 0;
    /// @brief Visit the AST::Function node
    /// @param stmt The node to visit
    virtual void visitFunctionStmt(AST::Function *stmt) = 0;
    /// @brief Visit the AST::Class node
    /// @param stmt The node to visit
    virtual void visitClassStmt(AST::Class *stmt) = 0;
    /// @brief Visit the AST::If node
    /// @param stmt The node to visit
    virtual void visitIfStmt(AST::If *stmt) = 0;
    /// @brief Visit the AST::Print node
    /// @param stmt The node to visit
    virtual void visitPrintStmt(AST::Print *stmt) = 0;
    /// @brief Visit the AST::Return node
    /// @param stmt The node to visit
    virtual void visitReturnStmt(AST::Return *stmt) = 0;
    /// @brief Visit the AST::Var node
    /// @param stmt The node to visit
    virtual void visitVarStmt(AST::Var *stmt) = 0;
    /// @brief Visit the AST::While node
    /// @param stmt The node to visit
    virtual void visitWhileStmt(AST::While *stmt) = 0;
    /// @brief Visit the AST::For node
    /// @param stmt The node to visit
    virtual void visitForStmt(AST::For *stmt) = 0;
    /// @brief Visit the AST::Break node
    /// @param stmt The node to visit
    virtual void visitBreakStmt(AST::Break *stmt) = 0;
    /// @brief Visit the AST::Continue node
    /// @param stmt The node to visit
    virtual void visitContinueStmt(AST::Continue *stmt) = 0;
}; // Visitor

} // namespace AST

#endif