#ifndef COMPILER_HPP
#define COMPILER_HPP

#include "expr.hpp"
#include "stmt.hpp"

#include <llvm/IR/Value.h>

using namespace llvm;

class Compiler : public AST::ExprVisitor, AST::StmtVisitor {
  public:
    virtual Value *genAssignExpr(AST::Assign *expr) override;
    virtual Value *genBinaryExpr(AST::Binary *expr) override;
    virtual Value *genCallExpr(AST::Call *expr) override;
    virtual Value *genGetExpr(AST::Get *expr) override;
    virtual Value *genGroupingExpr(AST::Grouping *expr) override;
    virtual Value *genLogicalExpr(AST::Logical *expr) override;
    virtual Value *genSetExpr(AST::Set *expr) override;
    virtual Value *genSuperExpr(AST::Super *expr) override;
    virtual Value *genThisExpr(AST::This *expr) override;
    virtual Value *genUnaryExpr(AST::Unary *expr) override;
    virtual Value *genVariableExpr(AST::Variable *expr) override;
    virtual Value *genLiteralExpr(AST::Literal *expr) override;
    virtual Value *genBlockStmt(AST::Block *stmt) override;
    virtual Value *genExpressionStmt(AST::Expression *stmt) override;
    virtual Value *genFunctionStmt(AST::Function *stmt) override;
    virtual Value *genClassStmt(AST::Class *stmt) override;
    virtual Value *genIfStmt(AST::If *stmt) override;
    virtual Value *genPrintStmt(AST::Print *stmt) override;
    virtual Value *genReturnStmt(AST::Return *stmt) override;
    virtual Value *genVarStmt(AST::Var *stmt) override;
    virtual Value *genWhileStmt(AST::While *stmt) override;
    virtual Value *genForStmt(AST::For *stmt) override;
    virtual Value *genBreakStmt(AST::Break *stmt) override;
    virtual Value *genContinueStmt(AST::Continue *stmt) override;
};

#endif