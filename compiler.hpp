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
#include <set>

typedef std::pair<value::ValueType, Token *> TypePair;

namespace compiler {
typedef llvm::Type *(*ToType)(llvm::LLVMContext &context);

static ToType value_to_type(value::ValueType kind) {
    switch (kind) {
    case value::ValueType::VAL_BOOL:
        return (llvm::Type * (*)(llvm::LLVMContext &)) llvm::Type::getInt1Ty;
    case value::ValueType::VAL_NUM:
        return llvm::Type::getDoubleTy;
    case value::ValueType::VAL_VOID:
        return llvm::Type::getVoidTy;
    default:
        return nullptr;
    }
}

class Compiler : public AST::ExprVisitor, public AST::StmtVisitor {
  public:
    bool errored = false;
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
    virtual llvm::Value *genTernaryIfExpr(AST::TernaryIf *expr) override;
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
    Compiler(std::string file_name);
    void print_code();
    void verify();

  private:
    llvm::LLVMContext *context;
    llvm::Module *module;
    llvm::IRBuilder<> *builder;

    llvm::Value *print_fmt;

    llvm::BasicBlock *break_block = nullptr;
    llvm::BasicBlock *cont_block  = nullptr;
    llvm::BasicBlock *ret_block   = nullptr;
    llvm::AllocaInst *ret_alloca  = nullptr;
    bool br_created               = false;

    llvm::Value *toBool(llvm::Value *val);
    /// @brief Convenience method for turning bools to doubles
    /// @param val Value to convert
    /// @return The value if it isn't a bool, or the bool converted to a double
    /// if it is
    llvm::Value *awayFromBool(llvm::Value *val);
    llvm::Value *toFloat(llvm::Value *val);
    llvm::Value *castToType(llvm::Type *ty, llvm::Value *val);
    std::string type_to_string(llvm::Type *ty);
    std::string gen_func_name(std::vector<llvm::Type *> tys, std::string name);

    void begin_scope();
    void end_scope();

    llvm::AllocaInst *resolve(Token *name);
    void reportMissing(Token *name);
    std::pair<std::string, double> getClosest(Token *name);

    std::vector<std::map<std::string, llvm::AllocaInst *>> named_values = {
        std::map<std::string, llvm::AllocaInst *>()};
    std::set<std::string> reported_missing;

    void init_lib();

    void createFToBFunc();
};
} // namespace compiler

#endif