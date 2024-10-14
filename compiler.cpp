#include "compiler.hpp"

#include "error.hpp"
#include "value.hpp"

#include <iostream>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Verifier.h>
#include <termcolor/termcolor.hpp>

llvm::Value *compiler::Compiler::genAssignExpr(AST::Assign *expr) {
    llvm::Value *val = expr->value->codegen(this);
    if (!val) {
        error::error(expr->equals, "Error evaluating expression.");
        errored = true;
        return nullptr;
    }

    llvm::AllocaInst *var = resolve(expr->name);
    if (!var) {
        error::error(expr->name, "Unknown variable name.");
        errored = true;
        return nullptr;
    }
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
        return builder->CreateUIToFP(
            cmp_eval, llvm::Type::getDoubleTy(*context), "gt_eval_tmp");
    case TokenKind::TOKEN_GREATER_EQUAL:
        cmp_eval = builder->CreateFCmpOGE(left, right, "ge_tmp");
        return builder->CreateUIToFP(
            cmp_eval, llvm::Type::getDoubleTy(*context), "ge_eval_tmp");
    case TokenKind::TOKEN_LESS:
        cmp_eval = builder->CreateFCmpOLT(left, right, "lt_tmp");
        return builder->CreateUIToFP(
            cmp_eval, llvm::Type::getDoubleTy(*context), "lt_eval_tmp");
    case TokenKind::TOKEN_LESS_EQUAL:
        cmp_eval = builder->CreateFCmpOLE(left, right, "le_tmp");
        return builder->CreateUIToFP(
            cmp_eval, llvm::Type::getDoubleTy(*context), "le_eval_tmp");
    case TokenKind::TOKEN_EQUAL_EQUAL:
        cmp_eval = builder->CreateFCmpOEQ(left, right, "eq_tmp");
        return builder->CreateUIToFP(
            cmp_eval, llvm::Type::getDoubleTy(*context), "eq_eval_tmp");
    case TokenKind::TOKEN_BANG_EQUAL:
        cmp_eval = builder->CreateFCmpONE(left, right, "ne_tmp");
        return builder->CreateUIToFP(
            cmp_eval, llvm::Type::getDoubleTy(*context), "ne_eval_tmp");
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
        errored = true;
        return nullptr;
    }

    llvm::Function *callee = module->getFunction(name);

    if (!callee) {
        error::error(expr->paren,
                     std::string("No function of name ")
                         .append(name)
                         .append(" exists in the current context"));
        errored = true;
        return nullptr;
    }

    if (callee->arg_size() != expr->args.size()) {
        // std::cerr << "Incorrect number of args passed, expected " <<
        // callee->arg_size() << " but got " << expr->args.size() << std::endl;
        error::error(expr->paren,
                     std::string("Incorrect number of args passed, expected ")
                         .append(std::to_string(callee->arg_size()))
                         .append(" but got ")
                         .append(std::to_string(expr->args.size())));
        errored = true;
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
    llvm::Value *right = expr->right->codegen(this);

    switch (expr->op->type) {
    case TokenKind::TOKEN_BANG:
        return builder->CreateICmpEQ(toBool(right), builder->getFalse(),
                                     "not_tmp");
    case TokenKind::TOKEN_MINUS:
        return builder->CreateFNeg(toFloat(right), "neg_tmp");
    default:
        break;
    }

    return nullptr;
}

llvm::Value *compiler::Compiler::genVariableExpr(AST::Variable *expr) {
    llvm::AllocaInst *val = resolve(expr->name);
    if (val)
        return builder->CreateLoad(val->getAllocatedType(), val,
                                   expr->name->text);
    errored = true;
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
    begin_scope();
    for (auto statement : stmt->statements) {
        statement->codegen(this);
    }
    end_scope();

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
        errored = true;
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

    begin_scope();
    for (auto &a : func->args()) {
        llvm::AllocaInst *alloca =
            builder->CreateAlloca(a.getType(), nullptr, a.getName());

        builder->CreateStore(&a, alloca);

        named_values.back()[std::string(a.getName())] = alloca;
    }

    for (auto statement : stmt->body) {
        statement->codegen(this);
    }
    end_scope();

    if (error::errored || llvm::verifyFunction(*func, &(llvm::errs()))) {
        func->removeFromParent();
        error::errored = 0;
        errored        = true;
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

    if (!expr) {
        error::error(stmt->keyword, "Error evaluating expression");
        return nullptr;
    }

    builder->CreateCall(print_func, {fmt, expr});

    return nullptr;
}

llvm::Value *compiler::Compiler::genReturnStmt(AST::Return *stmt) {
    if (stmt->value) {
        if (builder->GetInsertBlock()->getParent()->getReturnType() !=
            builder->getVoidTy()) {
            llvm::Value *val = castToType(
                builder->GetInsertBlock()->getParent()->getReturnType(),
                stmt->value->codegen(this));
            builder->CreateRet(val);
        } else {
            error::error(stmt->keyword,
                         "Returning value in function of return type 'void'.");
            errored = 1;
        }
    } else {
        if (builder->GetInsertBlock()->getParent()->getReturnType() ==
            builder->getVoidTy())
            builder->CreateRetVoid();
        else {
            error::error(stmt->keyword, "Return statement with no value in "
                                        "function with non-void return type");
        }
    }
    return nullptr;
}

llvm::Value *compiler::Compiler::genVarStmt(AST::Var *stmt) {
    llvm::Function *func = builder->GetInsertBlock()->getParent();
    llvm::AllocaInst *alloca;

    if (!stmt->initializer) {
        llvm::Value *init_val;
        switch (stmt->type.first) {
        case value::VAL_NUM:
            init_val = llvm::ConstantFP::get(*context, llvm::APFloat(0.0));
            break;
        case value::VAL_BOOL:
            init_val = builder->getFalse();
            break;
        case value::VAL_VOID:
            error::error(stmt->type.second,
                         "Variable cannot have a type of 'void'.");
            errored = true;
            return nullptr;
        }

        alloca =
            builder->CreateAlloca(value_to_type(stmt->type.first)(*context),
                                  nullptr, stmt->name->text);
        builder->CreateStore(init_val, alloca);
        named_values.back()[stmt->name->text] = alloca;
    } else {
        llvm::Value *init_val;
        switch (stmt->type.first) {
        case value::VAL_NUM:
            init_val = toFloat(stmt->initializer->codegen(this));
            break;
        case value::VAL_BOOL:
            init_val = toBool(stmt->initializer->codegen(this));
            break;
        case value::VAL_VOID:
            error::error(stmt->type.second,
                         "Variable cannot have a type of 'void'.");
            errored = true;
            return nullptr;
        }

        alloca =
            builder->CreateAlloca(value_to_type(stmt->type.first)(*context),
                                  nullptr, stmt->name->text);

        if (error::errored) {
            switch (stmt->type.first) {
            case value::VAL_NUM:
                init_val = llvm::ConstantFP::get(*context, llvm::APFloat(0.0));
                break;
            case value::VAL_BOOL:
                init_val = builder->getFalse();
                break;
            }
            errored = true;
        }
        builder->CreateStore(init_val, alloca);
        named_values.back()[stmt->name->text] = alloca;
    }
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
    else {
        error::error(stmt->keyword, "No break block present.");
        errored = true;
    }
    return nullptr;
}

llvm::Value *compiler::Compiler::genContinueStmt(AST::Continue *stmt) {
    if (cont_block)
        builder->CreateBr(cont_block);
    else {
        error::error(stmt->keyword, "No continue block present.");
        errored = true;
    }
    return nullptr;
}

compiler::Compiler::Compiler() {
    context = new llvm::LLVMContext();
    module  = new llvm::Module("top level", *context);
    builder = new llvm::IRBuilder(*context);

    llvm::Function *top_level = llvm::Function::Create(
        llvm::FunctionType::get(llvm::Type::getVoidTy(*context), false),
        llvm::Function::ExternalLinkage, "__main", *module);

    llvm::BasicBlock *main =
        llvm::BasicBlock::Create(*context, "main_body", top_level);
    builder->SetInsertPoint(main);
}

void compiler::Compiler::print_code() {
    module->print(llvm::errs(), nullptr);
}

void compiler::Compiler::verify() {
    builder->CreateRetVoid();
    if (llvm::verifyFunction(*(builder->GetInsertBlock()->getParent()),
                             &(llvm::errs()))) {
        errored = true;
    }
}

llvm::Value *compiler::Compiler::toBool(llvm::Value *val) {
    if (val->getType()->isIntegerTy())
        return val;
    return builder->CreateFCmpONE(
        val, llvm::ConstantFP::get(*context, llvm::APFloat(0.0)), "d_to_i");
}

llvm::Value *compiler::Compiler::toFloat(llvm::Value *val) {
    if (!val) {
        error::errored = 1;
        return nullptr;
    }
    if (val->getType()->isIntegerTy())
        return builder->CreateUIToFP(val, llvm::Type::getDoubleTy(*context),
                                     "i_to_d");
    return val;
}

void compiler::Compiler::begin_scope() {
    named_values.push_back(std::map<std::string, llvm::AllocaInst *>());
}

void compiler::Compiler::end_scope() {
    named_values.pop_back();
}

llvm::Value *compiler::Compiler::castToType(llvm::Type *ty, llvm::Value *val) {
    if (!val) {
        error::errored = 1;
        return nullptr;
    }
    switch (ty->getTypeID()) {
    case llvm::Type::TypeID::VoidTyID:
        return val;
    case llvm::Type::TypeID::DoubleTyID:
        return toFloat(val);
    case llvm::Type::TypeID::IntegerTyID:
        return toBool(val);
    }
    return nullptr;
}

llvm::AllocaInst *compiler::Compiler::resolve(Token *name) {
    for (int i = named_values.size() - 1; i >= 0; i--) {
        if (auto *alloca = named_values[i][name->text])
            return alloca;
    }

    reportMissing(name);
    return nullptr;
}

void compiler::Compiler::reportMissing(Token *name) {
    if (reported_missing.find(name->text) == reported_missing.end()) {
        error::error(name, "Variable not defined in scope.");
        auto const [val, dist] = getClosest(name);
        std::cerr << "Did you mean " << termcolor::bright_green
                  << termcolor::bold << "'" << val << "'" << termcolor::reset
                  << "? Distance to suggestion: " << dist << "." << std::endl;
        reported_missing.insert(name->text);
    }
}

static double damerauLevenstein(std::string_view const &a,
                                std::string_view const &b) {
    int l_string_length1 = a.length();
    int l_string_length2 = b.length();
    int d[l_string_length1 + 1][l_string_length2 + 1];

    int i;
    int j;
    int l_cost;

    for (i = 0; i <= l_string_length1; i++) {
        d[i][0] = i;
    }
    for (j = 0; j <= l_string_length2; j++) {
        d[0][j] = j;
    }
    for (i = 1; i <= l_string_length1; i++) {
        for (j = 1; j <= l_string_length2; j++) {
            if (a[i - 1] == b[j - 1]) {
                l_cost = 0;
            } else {
                l_cost = 1;
            }
            d[i][j] =
                std::min(d[i - 1][j] + 1,                   // delete
                         std::min(d[i][j - 1] + 1,          // insert
                                  d[i - 1][j - 1] + l_cost) // substitution
                );
            if ((i > 1) && (j > 1) && (a[i - 1] == b[j - 2]) &&
                (a[i - 2] == b[j - 1])) {
                d[i][j] = std::min(d[i][j],
                                   d[i - 2][j - 2] + l_cost // transposition
                );
            }
        }
    }
    return d[l_string_length1][l_string_length2];
}

std::pair<std::string, double> compiler::Compiler::getClosest(Token *name) {
    std::string closest;
    double minv = 2.0;
    for (auto scope : named_values) {
        for (auto key : scope) {
            double dist   = damerauLevenstein(key.first, name->text);
            double normal = (2 * dist) / (double)(name->text.length() +
                                                  key.first.length() + dist);
            if (normal < minv) {
                closest = key.first;
                minv    = normal;
            }
        }
    }

    return std::make_pair(closest, minv);
}
