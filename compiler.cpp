#include "compiler.hpp"

#include "error.hpp"
#include "value.hpp"

#include <iostream>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Verifier.h>

llvm::Value *compiler::Compiler::genAssignExpr(AST::Assign *expr) {
    return nullptr;
}

llvm::Value *compiler::Compiler::genBinaryExpr(AST::Binary *expr) {
    llvm::Value *left  = toFloat(expr->left->codegen(this));
    llvm::Value *right = toFloat(expr->right->codegen(this));
    if (!left || !right)
        return nullptr;

    llvm::Value *cmp_eval;

    switch (expr->op->type) {
    case TokenKind::TOKEN_PLUS:
        return builder->CreateFAdd(left, right, "add_tmp");
    case TokenKind::TOKEN_MINUS:
        return builder->CreateFSub(left, right, "sub_tmp");
    case TokenKind::TOKEN_STAR:
        return builder->CreateFMul(left, right, "mul_tmp");
    case TokenKind::TOKEN_SLASH:
        return builder->CreateFDiv(left, right, "div_tmp");
    case TokenKind::TOKEN_GREATER:
        cmp_eval = builder->CreateFCmpOGT(left, right, "gt_tmp");
        return builder->CreateUIToFP(cmp_eval, llvm::Type::getFloatTy(*context),
                                     "gt_eval_tmp");
    case TokenKind::TOKEN_GREATER_EQUAL:
        cmp_eval = builder->CreateFCmpOGE(left, right, "ge_tmp");
        return builder->CreateUIToFP(cmp_eval, llvm::Type::getFloatTy(*context),
                                     "ge_eval_tmp");
    case TokenKind::TOKEN_LESS:
        cmp_eval = builder->CreateFCmpOLT(left, right, "lt_tmp");
        return builder->CreateUIToFP(cmp_eval, llvm::Type::getFloatTy(*context),
                                     "lt_eval_tmp");
    case TokenKind::TOKEN_LESS_EQUAL:
        cmp_eval = builder->CreateFCmpOLE(left, right, "le_tmp");
        return builder->CreateUIToFP(cmp_eval, llvm::Type::getFloatTy(*context),
                                     "le_eval_tmp");
    case TokenKind::TOKEN_EQUAL_EQUAL:
        cmp_eval = builder->CreateFCmpOEQ(left, right, "eq_tmp");
        return builder->CreateUIToFP(cmp_eval, llvm::Type::getFloatTy(*context),
                                     "eq_eval_tmp");
    case TokenKind::TOKEN_BANG_EQUAL:
        cmp_eval = builder->CreateFCmpONE(left, right, "ne_tmp");
        return builder->CreateUIToFP(cmp_eval, llvm::Type::getFloatTy(*context),
                                     "ne_eval_tmp");
    default:
        break;
    }

    return nullptr;
}

llvm::Value *compiler::Compiler::genCallExpr(AST::Call *expr) {
    std::string name;

    switch (expr->callee->type) {
    case AST::ExprType::VariableType:
        name = static_cast<AST::Variable *>(expr->callee)->name->text;
        break;
    default:
        std::cerr << "Unsupported, don't do that." << std::endl;
        return nullptr;
    }

    llvm::Function *callee = module->getFunction(name);

    if (callee->arg_size() != expr->args.size()) {
        std::cerr << "Incorrect number of args passed" << std::endl;
        return nullptr;
    }

    std::vector<llvm::Value *> args;
    for (auto ex : expr->args) {
        args.push_back(ex->codegen(this));
        if (!args.back())
            return nullptr;
    }

    return builder->CreateCall(callee, args, "call_tmp");
}

llvm::Value *compiler::Compiler::genGetExpr(AST::Get *expr) {
    return nullptr;
}

llvm::Value *compiler::Compiler::genGroupingExpr(AST::Grouping *expr) {
    return expr->expression->codegen(this);
}

llvm::Value *compiler::Compiler::genLogicalExpr(AST::Logical *expr) {
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

llvm::Value *compiler::Compiler::genSetExpr(AST::Set *expr) {
    return nullptr;
}

llvm::Value *compiler::Compiler::genSuperExpr(AST::Super *expr) {
    return nullptr;
}

llvm::Value *compiler::Compiler::genThisExpr(AST::This *expr) {
    return nullptr;
}

llvm::Value *compiler::Compiler::genUnaryExpr(AST::Unary *expr) {
    llvm::Value *right = toFloat(expr->right->codegen(this));

    switch (expr->op->type) {
    case TokenKind::TOKEN_BANG:
        return builder->CreateFCmpOEQ(
            right, llvm::ConstantFP::get(*context, llvm::APFloat(0.0)),
            "not_tmp");
    case TokenKind::TOKEN_MINUS:
        return builder->CreateFNeg(right, "neg_tmp");
    default:
        break;
    }

    return nullptr;
}

llvm::Value *compiler::Compiler::genVariableExpr(AST::Variable *expr) {
    llvm::Value *val = named_values[expr->name->text];
    if (val)
        return val;
    error::error(expr->name, "No variable with that name in this scope.");
    return nullptr;
}

llvm::Value *compiler::Compiler::genLiteralExpr(AST::Literal *expr) {
    switch (expr->value.type) {
    case value::ValueType::VAL_BOOL:
        return llvm::ConstantInt::getBool(*context, value::asBool(expr->value));
    case value::ValueType::VAL_NUM:
        return llvm::ConstantFP::get(*context,
                                     llvm::APFloat(value::asNum(expr->value)));
    }
    return nullptr;
}

llvm::Value *compiler::Compiler::genBlockStmt(AST::Block *stmt) {
    for (auto statement : stmt->statements) {
        statement->codegen(this);
    }

    return nullptr;
}

llvm::Value *compiler::Compiler::genExpressionStmt(AST::Expression *stmt) {
    stmt->expression->codegen(this);
    return nullptr;
}

llvm::Value *compiler::Compiler::genFunctionStmt(AST::Function *stmt) {
    llvm::BasicBlock *parent = builder->GetInsertBlock();
    llvm::Function *func     = module->getFunction(stmt->name->text);

    if (func) {
        error::error(stmt->name, "Functions cannot be redefined");
        return nullptr;
    }

    std::vector<llvm::Type *> param_types;
    for (auto ty : stmt->types) {
        param_types.push_back(value_to_type(ty.first)(*context));
    }

    llvm::Type *ret_type = value_to_type(stmt->ret_type.first)(*context);

    llvm::FunctionType *func_type =
        llvm::FunctionType::get(ret_type, param_types, false);

    func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage,
                                  stmt->name->text, *module);
    unsigned idx = 0;
    for (auto &arg : func->args()) {
        arg.setName(stmt->params[idx++]->text);
    }

    llvm::BasicBlock *body = llvm::BasicBlock::Create(*context, "body", func);
    builder->SetInsertPoint(body);

    named_values.clear();
    for (auto &a : func->args()) {
        named_values[std::string(a.getName())] = &a;
    }

    for (auto statement : stmt->body) {
        statement->codegen(this);
    }

    if (error::errored) {
        func->removeFromParent();
        error::errored = 0;
    } else {
        llvm::verifyFunction(*func);
    }
    builder->SetInsertPoint(parent);

    return nullptr;
}

llvm::Value *compiler::Compiler::genClassStmt(AST::Class *stmt) {
    return nullptr;
}

llvm::Value *compiler::Compiler::genIfStmt(AST::If *stmt) {
    return nullptr;
}

llvm::Value *compiler::Compiler::genPrintStmt(AST::Print *stmt) {
    llvm::FunctionCallee print_func = module->getOrInsertFunction(
        "printf",
        llvm::FunctionType::get(
            llvm::IntegerType::getInt32Ty(*context),
            llvm::PointerType::get(llvm::Type::getInt8Ty(*context), 0), true));

    llvm::Value *fmt  = builder->CreateGlobalStringPtr("%f");
    llvm::Value *expr = stmt->expression->codegen(this);

    builder->CreateCall(print_func, {fmt, expr});

    return nullptr;
}

llvm::Value *compiler::Compiler::genReturnStmt(AST::Return *stmt) {
    if (stmt->value)
        builder->CreateRet(stmt->value->codegen(this));
    else
        builder->CreateRetVoid();
    return nullptr;
}

llvm::Value *compiler::Compiler::genVarStmt(AST::Var *stmt) {
    return nullptr;
}

llvm::Value *compiler::Compiler::genWhileStmt(AST::While *stmt) {
    return nullptr;
}

llvm::Value *compiler::Compiler::genForStmt(AST::For *stmt) {
    return nullptr;
}

llvm::Value *compiler::Compiler::genBreakStmt(AST::Break *stmt) {
    if (break_block)
        builder->CreateBr(break_block);
    else
        error::error(stmt->keyword, "No break block present.");
    return nullptr;
}

llvm::Value *compiler::Compiler::genContinueStmt(AST::Continue *stmt) {
    if (cont_block)
        builder->CreateBr(cont_block);
    else
        error::error(stmt->keyword, "No continue block present.");
    return nullptr;
}

compiler::Compiler::Compiler() {
    context = new llvm::LLVMContext();
    module  = new llvm::Module("top level", *context);
    builder = new llvm::IRBuilder(*context);

    llvm::Function *top_level = llvm::Function::Create(
        llvm::FunctionType::get(llvm::Type::getVoidTy(*context),
                                llvm::Type::getVoidTy(*context)),
        llvm::Function::ExternalLinkage, "__main", *module);

    llvm::BasicBlock *main =
        llvm::BasicBlock::Create(*context, "main_body", top_level);
    builder->SetInsertPoint(main);
}

void compiler::Compiler::print_code() {
    module->print(llvm::errs(), nullptr);
}

llvm::Value *compiler::Compiler::toBool(llvm::Value *val) {
    if (val->getType()->isIntegerTy())
        return builder->CreateICmpNE(val, llvm::ConstantInt::getFalse(*context),
                                     "i_to_bool");
    return builder->CreateFCmpONE(
        val, llvm::ConstantFP::get(*context, llvm::APFloat(0.0)), "f_to_bool");
}

llvm::Value *compiler::Compiler::toFloat(llvm::Value *val) {
    if (val->getType()->isIntegerTy())
        return builder->CreateUIToFP(val, llvm::Type::getFloatTy(*context),
                                     "i_to_fp");
    return val;
}
