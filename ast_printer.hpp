#ifndef AST_PRINTER_HH
#define AST_PRINTER_HH

#include "expr.hpp"
#include "stmt.hpp"

class ASTPrinter : public AST::ExprVisitor, public AST::StmtVisitor {
  private:
    /// @brief Print the provided expression
    /// @param expr The expression to print
    void print(AST::Expr *expr);
    /// @brief Print the provided statement
    /// @param stmt The statement to print
    void print(AST::Stmt *stmt);

  public:
    void visitAssignExpr(AST::Assign *expr) override;
    void visitBinaryExpr(AST::Binary *expr) override;
    void visitCallExpr(AST::Call *expr) override;
    void visitGetExpr(AST::Get *expr) override;
    void visitGroupingExpr(AST::Grouping *expr) override;
    void visitLogicalExpr(AST::Logical *expr) override;
    void visitSetExpr(AST::Set *expr) override;
    void visitSuperExpr(AST::Super *expr) override;
    void visitThisExpr(AST::This *expr) override;
    void visitUnaryExpr(AST::Unary *expr) override;
    void visitTernaryIfExpr(AST::TernaryIf *expr) override;
    void visitVariableExpr(AST::Variable *expr) override;
    void visitLiteralExpr(AST::Literal *expr) override;
    void visitBlockStmt(AST::Block *stmt) override;
    void visitExpressionStmt(AST::Expression *stmt) override;
    void visitFunctionStmt(AST::Function *stmt) override;
    void visitClassStmt(AST::Class *stmt) override;
    void visitIfStmt(AST::If *stmt) override;
    void visitPrintStmt(AST::Print *stmt) override;
    void visitReturnStmt(AST::Return *stmt) override;
    void visitVarStmt(AST::Var *stmt) override;
    void visitWhileStmt(AST::While *stmt) override;
    void visitForStmt(AST::For *stmt) override;
    void visitBreakStmt(AST::Break *stmt) override;
    void visitContinueStmt(AST::Continue *stmt) override;
};

#endif