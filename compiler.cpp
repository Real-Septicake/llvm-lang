#include "compiler.hpp"

#include "error.hpp"
#include "value.hpp"

#include <llvm/IR/Verifier.h>
#include <llvm/IR/BasicBlock.h>

llvm::Value *Compiler::Compiler::genAssignExpr(AST::Assign *expr) {
    return nullptr;
}

llvm::Value *Compiler::Compiler::genBinaryExpr(AST::Binary *expr) {
    llvm::Value *left  = expr->left->codegen(this);
    llvm::Value *right = expr->right->codegen(this);
    return nullptr;
}

llvm::Value *Compiler::Compiler::genCallExpr(AST::Call *expr) {
    return nullptr;
}

llvm::Value *Compiler::genGetExpr(AST::Get *expr) {
    return nullptr;
}

llvm::Value *Compiler::genGroupingExpr(AST::Grouping *expr) {
    return expr->expression->codegen(this);
}

llvm::Value *Compiler::genLogicalExpr(AST::Logical *expr) {
    llvm::Value *left  = expr->left->codegen(this);
    llvm::Value *right = expr->right->codegen(this);

    llvm::Value *bool_left  = toBool(left);
    llvm::Value *bool_right = toBool(right);

    switch (expr->op->type) {
    case TokenKind::TOKEN_AND:
        return builder->CreateAnd(bool_left, bool_right, "and_tmp");
    case TokenKind::TOKEN_OR:
        return builder->CreateOr(bool_left, bool_right, "or_tmp");
    default:
        return nullptr;
    }
}

llvm::Value *Compiler::genSetExpr(AST::Set *expr) {
    return nullptr;
}

llvm::Value *Compiler::genSuperExpr(AST::Super *expr) {
    return nullptr;
}

llvm::Value *Compiler::genThisExpr(AST::This *expr) {
    return nullptr;
}

llvm::Value *Compiler::genUnaryExpr(AST::Unary *expr) {
    llvm::Value *right   = expr->right->codegen(this);
    llvm::Value *ret_val = nullptr;

    switch (expr->op->type) {
    case TokenKind::TOKEN_BANG:
        ret_val = builder->CreateFCmpOEQ(
            right, llvm::ConstantFP::get(*context, llvm::APFloat(0.0)),
            "not_tmp");
        break;
    case TokenKind::TOKEN_MINUS:
        ret_val = builder->CreateFNeg(right, "neg_tmp");
        break;
    default:
        break;
    }

    return ret_val;
}

llvm::Value *Compiler::genVariableExpr(AST::Variable *expr) {
    llvm::Value *val = named_values[expr->name->text];
    if (val)
        return val;
    error::error(expr->name, "No variable with that name in this scope.");
    return nullptr;
}

llvm::Value *Compiler::genLiteralExpr(AST::Literal *expr) {
    switch (expr->value.type) {
    case value::ValueType::VAL_BOOL:
        return llvm::ConstantFP::get(
            *context, llvm::APFloat(value::asBool(expr->value) ? 1.0 : 0.0));
    case value::ValueType::VAL_NUM:
        return llvm::ConstantFP::get(*context,
                                     llvm::APFloat(value::asNum(expr->value)));
    }
    return nullptr;
}

llvm::Value *Compiler::genBlockStmt(AST::Block *stmt) {
    return nullptr;
}

llvm::Value *Compiler::genExpressionStmt(AST::Expression *stmt) {
    return stmt->expression->codegen(this);
}

llvm::Value *Compiler::genFunctionStmt(AST::Function *stmt) {
    return nullptr;
}

llvm::Value *Compiler::genClassStmt(AST::Class *stmt) {
    return nullptr;
}

llvm::Value *Compiler::genIfStmt(AST::If *stmt) {
    return nullptr;
}

llvm::Value *Compiler::genPrintStmt(AST::Print *stmt) {
    return nullptr;
}

llvm::Value *Compiler::genReturnStmt(AST::Return *stmt) {
    return nullptr;
}

llvm::Value *Compiler::genVarStmt(AST::Var *stmt) {
    return nullptr;
}

llvm::Value *Compiler::genWhileStmt(AST::While *stmt) {
    return nullptr;
}

llvm::Value *Compiler::genForStmt(AST::For *stmt) {
    return nullptr;
}

llvm::Value *Compiler::genBreakStmt(AST::Break *stmt) {
    return nullptr;
}

llvm::Value *Compiler::genContinueStmt(AST::Continue *stmt) {
    return nullptr;
}

Compiler::Compiler() {
    context = new llvm::LLVMContext();
    module  = new llvm::Module("top level", *context);
    builder = new llvm::IRBuilder(*context);
}

llvm::Value *Compiler::toBool(llvm::Value *val) {
    return builder->CreateFCmpONE(
        val, llvm::ConstantFP::get(*context, llvm::APFloat(0.0)), "to_bool");
}
