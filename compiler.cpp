#include "compiler.hpp"

#include "error.hpp"
#include "value.hpp"

#include <iostream>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>
#include <system_error>
#include <termcolor/termcolor.hpp>

#ifdef DEBUG
#define DEBUG_NAME(name)  (name)
#define VERIFY_FUNC(func) llvm::verifyFunction((func), &(llvm::errs()))
#else
#define DEBUG_NAME(name)  ""
#define VERIFY_FUNC(func) llvm::verifyFunction((func))
#endif

llvm::Value *compiler::Compiler::genAssignExpr(AST::Assign *expr) {
    llvm::Value *val = expr->value->codegen(this);
    if (!val) {
        error::error(expr->equals, "Error evaluating expression.");
        errored = true;
        return nullptr;
    }

    llvm::AllocaInst *var = resolve(expr->name);
    if (!var) {
        errored = true;
        return nullptr;
    }
    builder->CreateStore(val, var);
    return val;
}

llvm::Value *compiler::Compiler::genBinaryExpr(AST::Binary *expr) {
    llvm::Value *left  = toFloat(expr->left->codegen(this));
    llvm::Value *right = toFloat(expr->right->codegen(this));
    if (!left || !right)
        return nullptr;

    switch (expr->op->type) {
    case TokenKind::TOKEN_PLUS:
        return builder->CreateFAdd(left, right, DEBUG_NAME("add_tmp"));
    case TokenKind::TOKEN_MINUS:
        return builder->CreateFSub(left, right, DEBUG_NAME("sub_tmp"));
    case TokenKind::TOKEN_STAR:
        return builder->CreateFMul(left, right, DEBUG_NAME("mul_tmp"));
    case TokenKind::TOKEN_MODULO:
        return builder->CreateFRem(left, right, DEBUG_NAME("rem_tmp"));
    case TokenKind::TOKEN_SLASH:
        return builder->CreateFDiv(left, right, DEBUG_NAME("div_tmp"));
    case TokenKind::TOKEN_GREATER:
        return builder->CreateFCmpOGT(left, right, DEBUG_NAME("gt_tmp"));
    case TokenKind::TOKEN_GREATER_EQUAL:
        return builder->CreateFCmpOGE(left, right, DEBUG_NAME("ge_tmp"));
    case TokenKind::TOKEN_LESS:
        return builder->CreateFCmpOLT(left, right, DEBUG_NAME("lt_tmp"));
    case TokenKind::TOKEN_LESS_EQUAL:
        return builder->CreateFCmpOLE(left, right, DEBUG_NAME("le_tmp"));
    case TokenKind::TOKEN_EQUAL_EQUAL:
        return builder->CreateFCmpOEQ(left, right, DEBUG_NAME("eq_tmp"));
    case TokenKind::TOKEN_BANG_EQUAL:
        return builder->CreateFCmpONE(left, right, DEBUG_NAME("ne_tmp"));
    default:
        break;
    }

    return nullptr;
}

llvm::Value *compiler::Compiler::genCallExpr(AST::Call *expr) {
    std::vector<llvm::Value *> args;
    std::vector<llvm::Type *> tys;
    for (auto arg : expr->args) {
        args.push_back(arg->codegen(this));
        tys.push_back(args.back()->getType());
        if (!args.back())
            return nullptr;
    }

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

    llvm::Function *callee = module->getFunction(gen_func_name(tys, name));
    if (!callee)
        callee = module->getFunction(name);

    if (!callee) {
        error::error(expr->paren,
                     std::string("No function of name ")
                         .append(name)
                         .append(" exists in the current context"));
        errored = true;
        return nullptr;
    }

    if (callee->arg_size() != expr->args.size()) {
        error::error(expr->paren,
                     std::string("Incorrect number of args passed, expected ")
                         .append(std::to_string(callee->arg_size()))
                         .append(" but got ")
                         .append(std::to_string(expr->args.size())));
        errored = true;
        return nullptr;
    }

    std::string val_name =
        (callee->getReturnType()->isVoidTy() ? "" : DEBUG_NAME("call_tmp"));

    return builder->CreateCall(callee, args, val_name);
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
        return builder->CreateAnd(bool_left, bool_right, DEBUG_NAME("and_tmp"));
    case TokenKind::TOKEN_OR:
        return builder->CreateOr(bool_left, bool_right, DEBUG_NAME("or_tmp"));
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
                                     DEBUG_NAME("not_tmp"));
    case TokenKind::TOKEN_MINUS:
        return builder->CreateFNeg(toFloat(right), DEBUG_NAME("neg_tmp"));
    default:
        break;
    }

    return nullptr;
}

llvm::Value *compiler::Compiler::genTernaryIfExpr(AST::TernaryIf *expr) {
    llvm::Value *cond  = toBool(expr->condition->codegen(this));
    llvm::Value *then  = expr->then->codegen(this);
    llvm::Value *_else = expr->_else->codegen(this);

    if (then->getType() != _else->getType()) {
        error::error(expr->colon,
                     "Ternary 'if' results are not of the same type.");
        errored = true;
        return nullptr;
    }

    return builder->CreateSelect(cond, then, _else, "tern.if");
}

llvm::Value *compiler::Compiler::genVariableExpr(AST::Variable *expr) {
    llvm::AllocaInst *val = resolve(expr->name);
    if (val)
        return builder->CreateLoad(val->getAllocatedType(), val,
                                   DEBUG_NAME(expr->name->text));
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

llvm::Value *compiler::Compiler::genProtoStmt(AST::Proto *stmt) {
    std::vector<llvm::Type *> tys;
    for (auto t : stmt->types) {
        tys.push_back(value_to_type(t.first)(*context));
    }

    llvm::FunctionType *func_ty = llvm::FunctionType::get(
        value_to_type(stmt->ret_type.first)(*context), tys, false);

    llvm::Function *f = llvm::Function::Create(
        func_ty, llvm::Function::ExternalLinkage,
        (stmt->is_intern ? gen_func_name(tys, stmt->name->text)
                         : stmt->name->text),
        *module);
    return nullptr;
}

llvm::Value *compiler::Compiler::genFunctionStmt(AST::Function *stmt) {
    llvm::BasicBlock *parent = builder->GetInsertBlock();

    std::vector<llvm::Type *> param_types;
    for (auto ty : stmt->types) {
        param_types.push_back(value_to_type(ty.first)(*context));
    }

    std::string name = gen_func_name(param_types, stmt->name->text);

    llvm::Function *func = module->getFunction(name);

    if (func) {
        error::error(stmt->name, "Functions cannot be redefined");
        errored = true;
        return nullptr;
    }

    llvm::Type *ret_type = value_to_type(stmt->ret_type.first)(*context);

    llvm::BasicBlock *ret_bb =
        llvm::BasicBlock::Create(*context, DEBUG_NAME("ret_block"));
    ret_block = ret_bb;

    llvm::FunctionType *func_type =
        llvm::FunctionType::get(ret_type, param_types, false);

    func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage,
                                  name, *module);
    unsigned idx = 0;
    for (auto &arg : func->args()) {
        arg.setName(DEBUG_NAME(stmt->params[idx++]->text));
    }

    llvm::BasicBlock *body =
        llvm::BasicBlock::Create(*context, DEBUG_NAME("entry"), func);
    builder->SetInsertPoint(body);

    if (!ret_type->isVoidTy()) {
        llvm::AllocaInst *alloca =
            builder->CreateAlloca(ret_type, nullptr, DEBUG_NAME("ret_alloca"));
        ret_alloca = alloca;
    }

    begin_scope();
    idx = 0;
    for (auto &a : func->args()) {
        llvm::AllocaInst *alloca = builder->CreateAlloca(
            a.getType(), nullptr, DEBUG_NAME(a.getName() + ".addr"));

        builder->CreateStore(&a, alloca);

        named_values.back()[std::string(stmt->params[idx++]->text)] = alloca;
    }

    for (auto statement : stmt->body) {
        statement->codegen(this);
    }

    // Make sure it gets reset
    br_created = false;

    func->insert(func->end(), ret_bb);
    builder->SetInsertPoint(ret_bb);

    if (ret_alloca) {
        llvm::Value *ret_val = builder->CreateLoad(
            ret_alloca->getAllocatedType(), ret_alloca, DEBUG_NAME("ret_val"));
        builder->CreateRet(ret_val);
        ret_alloca = nullptr;
    } else {
        builder->CreateRetVoid();
    }

    end_scope();
    if (VERIFY_FUNC(*func)) {
        for (auto block = func->begin(); block != func->end(); block++) {
            for (auto inst = block->begin(); inst != block->end(); inst++) {
                if (inst->isTerminator() && inst != --(block->end())) {
                    error::error(stmt->name,
                                 "Function contains unreachable code.");
                    break;
                }
            }
            if (error::errored)
                break;
        }
    }

    if (error::errored) {
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
    llvm::Value *cond = toBool(stmt->condition->codegen(this));
    if (!cond) {
        error::error(stmt->paren, "Error evaluating condition.");
        errored = true;
        return nullptr;
    }

    llvm::BasicBlock *parent = builder->GetInsertBlock();
    llvm::Function *func     = builder->GetInsertBlock()->getParent();

    llvm::BasicBlock *then_block =
        llvm::BasicBlock::Create(*context, DEBUG_NAME("if.then"), func);
    llvm::BasicBlock *merge_block =
        llvm::BasicBlock::Create(*context, DEBUG_NAME("if.end"));

    builder->SetInsertPoint(then_block);
    stmt->thenBranch->codegen(this);

    if (stmt->elseBranch) {
        if (br_created)
            br_created = false;
        else
            builder->CreateBr(merge_block);

        llvm::BasicBlock *else_block =
            llvm::BasicBlock::Create(*context, DEBUG_NAME("if.else"));
        func->insert(func->end(), else_block);
        builder->SetInsertPoint(else_block);
        stmt->elseBranch->codegen(this);
        if (br_created)
            br_created = false;
        else
            builder->CreateBr(merge_block);

        builder->SetInsertPoint(parent);
        builder->CreateCondBr(cond, then_block, else_block);
    } else {
        if (br_created)
            br_created = false;
        else
            builder->CreateBr(merge_block);
        builder->SetInsertPoint(parent);
        builder->CreateCondBr(cond, then_block, merge_block);
    }
    func->insert(func->end(), merge_block);
    builder->SetInsertPoint(merge_block);

    return nullptr;
}

llvm::Value *compiler::Compiler::genPrintStmt(AST::Print *stmt) {
    llvm::Function *print_func = module->getFunction("printf");

    llvm::Value *fmt  = print_fmt;
    llvm::Value *expr = toFloat(stmt->expression->codegen(this));

    if (!expr) {
        error::error(stmt->keyword, "Error evaluating expression");
        return nullptr;
    }

    builder->CreateCall(print_func, {fmt, expr});

    return nullptr;
}

llvm::Value *compiler::Compiler::genReturnStmt(AST::Return *stmt) {
    if (stmt->value) {
        if (!builder->GetInsertBlock()
                 ->getParent()
                 ->getReturnType()
                 ->isVoidTy()) {
            llvm::Value *val = stmt->value->codegen(this);
            if (!val) {
                error::error(stmt->keyword,
                             "Error evaluating return expression.");
                errored = true;
                return nullptr;
            }
            val = castToType(
                builder->GetInsertBlock()->getParent()->getReturnType(), val);
            if (!ret_alloca) {
                error::error(stmt->keyword, "No ret_alloca present");
                errored = true;
                return nullptr;
            }
            builder->CreateStore(val, ret_alloca);
            if (!ret_block) {
                error::error(stmt->keyword, "No ret_block present");
                errored = true;
                return nullptr;
            }
            builder->CreateBr(ret_block);
            br_created = true;
        } else {
            error::error(stmt->keyword,
                         "Returning value in function of return type 'void'.");
            errored = 1;
        }
    } else {
        if (builder->GetInsertBlock()
                ->getParent()
                ->getReturnType()
                ->isVoidTy()) {
            if (!ret_block) {
                error::error(stmt->keyword, "No ret_block present");
                errored = true;
                return nullptr;
            }
            builder->CreateBr(ret_block);
            br_created = true;
        } else {
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
                                  nullptr, DEBUG_NAME(stmt->name->text));

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
    llvm::Function *parent = builder->GetInsertBlock()->getParent();

    llvm::BasicBlock *prev_break = break_block;
    llvm::BasicBlock *prev_cont  = cont_block;

    llvm::BasicBlock *condition =
        llvm::BasicBlock::Create(*context, DEBUG_NAME("while.cond"), parent);
    cont_block = condition;
    llvm::BasicBlock *body =
        llvm::BasicBlock::Create(*context, DEBUG_NAME("while.body"));
    llvm::BasicBlock *merge_block =
        llvm::BasicBlock::Create(*context, DEBUG_NAME("while.end"));
    break_block = merge_block;

    begin_scope();

    builder->CreateBr(condition);
    builder->SetInsertPoint(condition);
    llvm::Value *cond = toBool(stmt->condition->codegen(this));
    builder->CreateCondBr(cond, body, merge_block);

    parent->insert(parent->end(), body);
    builder->SetInsertPoint(body);
    stmt->body->codegen(this);

    if (br_created)
        br_created = false;
    else
        builder->CreateBr(condition);

    parent->insert(parent->end(), merge_block);
    builder->SetInsertPoint(merge_block);

    end_scope();

    break_block = prev_break;
    cont_block  = prev_cont;

    return nullptr;
}

llvm::Value *compiler::Compiler::genForStmt(AST::For *stmt) {
    llvm::Function *parent = builder->GetInsertBlock()->getParent();

    llvm::BasicBlock *prev_break = break_block;
    llvm::BasicBlock *prev_cont  = cont_block;

    llvm::BasicBlock *init =
        llvm::BasicBlock::Create(*context, DEBUG_NAME("for.init"), parent);
    llvm::BasicBlock *condition =
        llvm::BasicBlock::Create(*context, DEBUG_NAME("for.cond"));
    llvm::BasicBlock *increment =
        llvm::BasicBlock::Create(*context, DEBUG_NAME("for.step"));
    cont_block = increment;
    llvm::BasicBlock *body =
        llvm::BasicBlock::Create(*context, DEBUG_NAME("for.body"));
    llvm::BasicBlock *merge =
        llvm::BasicBlock::Create(*context, DEBUG_NAME("for.end"));
    break_block = merge;

    begin_scope();

    builder->CreateBr(init);
    builder->SetInsertPoint(init);
    if (stmt->initializer)
        stmt->initializer->codegen(this);
    builder->CreateBr(condition);

    parent->insert(parent->end(), condition);
    builder->SetInsertPoint(condition);
    llvm::Value *cond = stmt->condition->codegen(this);
    builder->CreateCondBr(cond, body, merge);

    parent->insert(parent->end(), body);
    builder->SetInsertPoint(body);
    stmt->body->codegen(this);
    if (br_created)
        br_created = false;
    else
        builder->CreateBr(increment);

    parent->insert(parent->end(), increment);
    builder->SetInsertPoint(increment);
    if (stmt->increment)
        stmt->increment->codegen(this);
    builder->CreateBr(condition);

    end_scope();

    parent->insert(parent->end(), merge);
    builder->SetInsertPoint(merge);

    cont_block  = prev_cont;
    break_block = prev_break;

    return nullptr;
}

llvm::Value *compiler::Compiler::genBreakStmt(AST::Break *stmt) {
    if (break_block) {
        builder->CreateBr(break_block);
        br_created = true;
    } else {
        error::error(stmt->keyword, "No break block present.");
        errored = true;
    }
    return nullptr;
}

llvm::Value *compiler::Compiler::genContinueStmt(AST::Continue *stmt) {
    if (cont_block) {
        builder->CreateBr(cont_block);
        br_created = true;
    } else {
        error::error(stmt->keyword, "No continue block present.");
        errored = true;
    }
    return nullptr;
}

compiler::Compiler::Compiler(std::string file_name) {
    context = new llvm::LLVMContext();
    module  = new llvm::Module("top level", *context);
    module->setSourceFileName(file_name);
    builder = new llvm::IRBuilder(*context);

    llvm::Function::Create(
        llvm::FunctionType::get(
            llvm::IntegerType::getInt32Ty(*context),
            llvm::PointerType::get(llvm::Type::getInt8Ty(*context), 0), true),
        llvm::Function::ExternalLinkage, "printf", *module);

    init_lib();

    print_fmt = builder->CreateGlobalStringPtr("%f\n", DEBUG_NAME("print_fmt"),
                                               0U, module);

    llvm::Function *top_level = llvm::Function::Create(
        llvm::FunctionType::get(llvm::Type::getVoidTy(*context), false),
        llvm::Function::ExternalLinkage, "__main", *module);
    llvm::AttrBuilder attrs = llvm::AttrBuilder(*context);
    attrs.addAttribute(llvm::Attribute::NoRecurse);
    attrs.addAttribute(llvm::Attribute::MustProgress);
    top_level->addFnAttrs(attrs);

    llvm::BasicBlock *main = llvm::BasicBlock::Create(*context);
    top_level->insert(top_level->begin(), main);
    builder->SetInsertPoint(main);
}

void compiler::Compiler::print_code() {
    module->print(llvm::outs(), nullptr);
}

void compiler::Compiler::verify() {
    builder->CreateRetVoid();
    if (llvm::verifyFunction(*(builder->GetInsertBlock()->getParent()),
                             &(llvm::errs()))) {
        errored = true;
    }
    if (llvm::verifyModule(*module, &(llvm::errs()))) {
        errored = true;
    }
}

int compiler::Compiler::write(std::string file) {
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    auto target_triple = llvm::sys::getDefaultTargetTriple();
    std::string error;
    auto target = llvm::TargetRegistry::lookupTarget(target_triple, error);

    if (!target) {
        llvm::errs() << error;
        return 1;
    }

    auto CPU      = "generic";
    auto features = "";

    llvm::TargetOptions opt;
    auto target_machine = target->createTargetMachine(
        target_triple, CPU, features, opt, llvm::Reloc::PIC_);
    module->setDataLayout(target_machine->createDataLayout());
    module->setTargetTriple(target_triple);

    std::error_code ec;
    llvm::raw_fd_ostream dest(file, ec, (llvm::sys::fs::OpenFlags)0);

    llvm::legacy::PassManager pass;
    auto file_type = llvm::CodeGenFileType::ObjectFile;

    if (target_machine->addPassesToEmitFile(pass, dest, nullptr, file_type)) {
        llvm::errs() << "Target machine can't emit a file of this type.";
        return 1;
    }

    pass.run(*module);
    dest.flush();
    return 0;
}

llvm::Value *compiler::Compiler::toBool(llvm::Value *val) {
    if (val->getType()->isIntegerTy())
        return val;
    return builder->CreateFCmpONE(
        val, llvm::ConstantFP::get(*context, llvm::APFloat(0.0)),
        DEBUG_NAME("d_to_i"));
}

llvm::Value *compiler::Compiler::awayFromBool(llvm::Value *val) {
    if (val->getType()->isIntegerTy())
        return toFloat(val);
    return val;
}

llvm::Value *compiler::Compiler::toFloat(llvm::Value *val) {
    if (!val) {
        error::errored = 1;
        return nullptr;
    }
    if (val->getType()->isIntegerTy())
        return builder->CreateUIToFP(val, llvm::Type::getDoubleTy(*context),
                                     DEBUG_NAME("i_to_d"));
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

std::string compiler::Compiler::type_to_string(llvm::Type *ty) {
    switch (ty->getTypeID()) {
    case llvm::Type::IntegerTyID:
        return "b";
    case llvm::Type::DoubleTyID:
        return "d";
    }
}

std::string compiler::Compiler::gen_func_name(std::vector<llvm::Type *> tys,
                                              std::string name) {
    std::string qualified_name = "_Z" + std::to_string(name.length()) + name;
    for (auto ty : tys) {
        qualified_name += type_to_string(ty);
    }

    return qualified_name;
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

void compiler::Compiler::createDToBFunc() {
    llvm::FunctionType *toBoolTy =
        llvm::FunctionType::get(llvm::Type::getInt1Ty(*context),
                                llvm::Type::getDoubleTy(*context), false);

    std::string name =
        gen_func_name({llvm::Type::getDoubleTy(*context)}, "toBool");
    auto *func = llvm::Function::Create(
        toBoolTy, llvm::Function::ExternalLinkage, name, *module);

    auto *body = llvm::BasicBlock::Create(*context, DEBUG_NAME("entry"), func);
    builder->SetInsertPoint(body);

    builder->CreateRet(toBool(func->arg_begin()));
}

void compiler::Compiler::createBToDFunc() {
    llvm::FunctionType *toDoubleTy =
        llvm::FunctionType::get(llvm::Type::getDoubleTy(*context),
                                llvm::Type::getInt1Ty(*context), false);

    std::string name =
        gen_func_name({llvm::Type::getInt1Ty(*context)}, "toNum");
    auto *func = llvm::Function::Create(
        toDoubleTy, llvm::Function::ExternalLinkage, name, *module);

    auto *body = llvm::BasicBlock::Create(*context, DEBUG_NAME("entry"), func);
    builder->SetInsertPoint(body);

    builder->CreateRet(toFloat(func->arg_begin()));
}

void compiler::Compiler::init_lib() {
    createDToBFunc();
    createBToDFunc();
}
