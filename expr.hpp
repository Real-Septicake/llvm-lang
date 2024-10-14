#ifndef EXPR_HH
#define EXPR_HH

#include "token.hpp"
#include "value.hpp"

#include <llvm/IR/Value.h>
#include <utility>
#include <vector>
/// @brief A namespace containing members used for the creation and usage of the
/// abstract syntax tree
namespace AST {

/// @brief An enum representing the possible AST::Expr types
enum ExprType {
    AssignType,
    BinaryType,
    CallType,
    GetType,
    GroupingType,
    LogicalType,
    SetType,
    SuperType,
    ThisType,
    UnaryType,
    VariableType,
    LiteralType,
};

// Forward declaration of visitor
class ExprVisitor;

/// @brief The base class representing expression nodes
class Expr {
  public:
    /// @brief The type of this node, used for quick type checks
    ExprType type;
    /// @brief Accept an AST::ExprVisitor
    /// @param visitor The visitor to accept
    virtual void accept(ExprVisitor *visitor)          = 0;
    virtual llvm::Value *codegen(ExprVisitor *visitor) = 0;
    /// @brief Create an AST::Expr with the specified type
    /// @param type The type of this AST::Expr node
    Expr(ExprType type) : type(type) {}
}; // Class Expr

/// @brief The node for an assign expression
class Assign : public Expr {
  public:
    Token *name;
    Token *equals;
    Expr *value;
    Assign(Token *name, Token *equals, Expr *value);
    void accept(ExprVisitor *visitor) override;
    llvm::Value *codegen(ExprVisitor *visitor) override;
};

/// @brief The node for a binary expression
class Binary : public Expr {
  public:
    Expr *left;
    Token *op;
    Expr *right;
    Binary(Expr *left, Token *op, Expr *right);
    void accept(ExprVisitor *visitor) override;
    llvm::Value *codegen(ExprVisitor *visitor) override;
};

/// @brief The node for a call expression
class Call : public Expr {
  public:
    Expr *callee;
    Token *paren;
    std::vector<Expr *> args;
    Call(Expr *callee, Token *paren, std::vector<Expr *> args);
    void accept(ExprVisitor *visitor) override;
    llvm::Value *codegen(ExprVisitor *visitor) override;
};

/// @brief The node for a get expression
class Get : public Expr {
  public:
    Expr *object;
    Token *name;
    Get(Expr *object, Token *name);
    void accept(ExprVisitor *visitor) override;
    llvm::Value *codegen(ExprVisitor *visitor) override;
};

/// @brief The node for a grouping expression
class Grouping : public Expr {
  public:
    Expr *expression;
    Grouping(Expr *expression);
    void accept(ExprVisitor *visitor) override;
    llvm::Value *codegen(ExprVisitor *visitor) override;
};

/// @brief The node for a logical expression
class Logical : public Expr {
  public:
    Expr *left;
    Token *op;
    Expr *right;
    Logical(Expr *left, Token *op, Expr *right);
    void accept(ExprVisitor *visitor) override;
    llvm::Value *codegen(ExprVisitor *visitor) override;
};

/// @brief The node for a set expression
class Set : public Expr {
  public:
    Expr *object;
    Token *name;
    Expr *value;
    Set(Expr *object, Token *name, Expr *value);
    void accept(ExprVisitor *visitor) override;
    llvm::Value *codegen(ExprVisitor *visitor) override;
};

/// @brief The node for a super expression
class Super : public Expr {
  public:
    Token *keyword;
    Token *method;
    Super(Token *keyword, Token *method);
    void accept(ExprVisitor *visitor) override;
    llvm::Value *codegen(ExprVisitor *visitor) override;
};

/// @brief The node for a this expression
class This : public Expr {
  public:
    Token *keyword;
    This(Token *keyword);
    void accept(ExprVisitor *visitor) override;
    llvm::Value *codegen(ExprVisitor *visitor) override;
};

/// @brief The node for an unary expression
class Unary : public Expr {
  public:
    Token *op;
    Expr *right;
    Unary(Token *op, Expr *right);
    void accept(ExprVisitor *visitor) override;
    llvm::Value *codegen(ExprVisitor *visitor) override;
};

/// @brief The node for a variable expression
class Variable : public Expr {
  public:
    Token *name;
    Variable(Token *name);
    void accept(ExprVisitor *visitor) override;
    llvm::Value *codegen(ExprVisitor *visitor) override;
};

/// @brief The node for a literal expression
class Literal : public Expr {
  public:
    value::Value value;
    Literal(value::Value value);
    void accept(ExprVisitor *visitor) override;
    llvm::Value *codegen(ExprVisitor *visitor) override;
};

/// @brief The visitor for the AST::Expr class
class ExprVisitor {
  public:
    /// @brief Visit the AST::Assign node
    /// @param expr The node to visit
    virtual void visitAssignExpr(AST::Assign *expr) {
        return;
    }
    virtual llvm::Value *genAssignExpr(AST::Assign *expr) {
        return nullptr;
    }
    /// @brief Visit the AST::Binary node
    /// @param expr The node to visit
    virtual void visitBinaryExpr(AST::Binary *expr) {
        return;
    }
    virtual llvm::Value *genBinaryExpr(AST::Binary *expr) {
        return nullptr;
    }
    /// @brief Visit the AST::Call node
    /// @param expr The node to visit
    virtual void visitCallExpr(AST::Call *expr) {
        return;
    }
    virtual llvm::Value *genCallExpr(AST::Call *expr) {
        return nullptr;
    }
    /// @brief Visit the AST::Get node
    /// @param expr The node to visit
    virtual void visitGetExpr(AST::Get *expr) {
        return;
    }
    virtual llvm::Value *genGetExpr(AST::Get *expr) {
        return nullptr;
    }
    /// @brief Visit the AST::Grouping node
    /// @param expr The node to visit
    virtual void visitGroupingExpr(AST::Grouping *expr) {
        return;
    }
    virtual llvm::Value *genGroupingExpr(AST::Grouping *expr) {
        return nullptr;
    }
    /// @brief Visit the AST::Logical node
    /// @param expr The node to visit
    virtual void visitLogicalExpr(AST::Logical *expr) {
        return;
    }
    virtual llvm::Value *genLogicalExpr(AST::Logical *expr) {
        return nullptr;
    }
    /// @brief Visit the AST::Set node
    /// @param expr The node to visit
    virtual void visitSetExpr(AST::Set *expr) {
        return;
    }
    virtual llvm::Value *genSetExpr(AST::Set *expr) {
        return nullptr;
    }
    /// @brief Visit the AST::Super node
    /// @param expr The node to visit
    virtual void visitSuperExpr(AST::Super *expr) {
        return;
    }
    virtual llvm::Value *genSuperExpr(AST::Super *expr) {
        return nullptr;
    }
    /// @brief Visit the AST::This node
    /// @param expr The node to visit
    virtual void visitThisExpr(AST::This *expr) {
        return;
    }
    virtual llvm::Value *genThisExpr(AST::This *expr) {
        return nullptr;
    }
    /// @brief Visit the AST::Unary node
    /// @param expr The node to visit
    virtual void visitUnaryExpr(AST::Unary *expr) {
        return;
    }
    virtual llvm::Value *genUnaryExpr(AST::Unary *expr) {
        return nullptr;
    }
    /// @brief Visit the AST::Variable node
    /// @param expr The node to visit
    virtual void visitVariableExpr(AST::Variable *expr) {
        return;
    }
    virtual llvm::Value *genVariableExpr(AST::Variable *expr) {
        return nullptr;
    }
    /// @brief Visit the AST::Literal node
    /// @param expr The node to visit
    virtual void visitLiteralExpr(AST::Literal *expr) {
        return;
    }
    virtual llvm::Value *genLiteralExpr(AST::Literal *expr) {
        return nullptr;
    }
}; // Visitor

} // namespace AST

#endif