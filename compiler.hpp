#ifndef COMPILER_HPP
#define COMPILER_HPP

#include "expr.hpp"
#include "stmt.hpp"

#include <llvm/ADT/APFloat.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <map>

typedef std::pair<value::ValueType, Token *> TypePair;

namespace compiler {
typedef llvm::Type *(*ToType)(llvm::LLVMContext &context);

static std::map<value::ValueType, ToType> value_to_type = {
    {value::ValueType::VAL_BOOL, llvm::Type::getFloatTy           },
    {value::ValueType::VAL_NUM,
     (llvm::Type * (*)(llvm::LLVMContext &)) llvm::Type::getInt1Ty}
};

class Compiler : public AST::ExprVisitor, AST::StmtVisitor {
  public:
    virtual llvm::Value *genAssignExpr(AST::Assign *expr) override;
    virtual llvm::Value *genBinaryExpr(AST::Binary *expr) override;
    virtual llvm::Value *genCallExpr(AST::Call *expr) override;
    virtual llvm::Value *genGetExpr(AST::Get *expr) override;
    virtual llvm::Value *genGroupingExpr(AST::Grouping *expr) override;
    virtual llvm::Value *genLogicalExpr(AST::Logical *expr) override;
    virtual llvm::Value *genSetExpr(AST::Set *expr) override;
    virtual llvm::Value *genSuperExpr(AST::Super *expr) override;
    virtual llvm::Value *genThisExpr(AST::This *expr) override;
    virtual llvm::Value *genUnaryExpr(AST::Unary *expr) override;
    virtual llvm::Value *genVariableExpr(AST::Variable *expr) override;
    virtual llvm::Value *genLiteralExpr(AST::Literal *expr) override;
    virtual llvm::Value *genBlockStmt(AST::Block *stmt) override;
    virtual llvm::Value *genExpressionStmt(AST::Expression *stmt) override;
    virtual llvm::Value *genFunctionStmt(AST::Function *stmt) override;
    virtual llvm::Value *genClassStmt(AST::Class *stmt) override;
    virtual llvm::Value *genIfStmt(AST::If *stmt) override;
    virtual llvm::Value *genPrintStmt(AST::Print *stmt) override;
    virtual llvm::Value *genReturnStmt(AST::Return *stmt) override;
    virtual llvm::Value *genVarStmt(AST::Var *stmt) override;
    virtual llvm::Value *genWhileStmt(AST::While *stmt) override;
    virtual llvm::Value *genForStmt(AST::For *stmt) override;
    virtual llvm::Value *genBreakStmt(AST::Break *stmt) override;
    virtual llvm::Value *genContinueStmt(AST::Continue *stmt) override;
    Compiler();

  private:
    llvm::LLVMContext *context;
    llvm::Module *module;
    llvm::IRBuilder<> *builder;
    std::map<std::string, llvm::Value *> named_values;
    llvm::Value *toBool(llvm::Value *val);
};
} // namespace compiler

#endif