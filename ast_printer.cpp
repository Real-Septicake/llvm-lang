#include "ast_printer.hpp"

#include <iostream>

void ASTPrinter::print(AST::Expr *expr) {
    expr->accept(this);
}

void ASTPrinter::print(AST::Stmt *stmt) {
    stmt->accept(this);
}

void ASTPrinter::visitAssignExpr(AST::Assign *expr) {
    std::cout << "(= ";
    std::cout << expr->name->text << " ";
    expr->value->accept(this);
    std::cout << ")";
}

void ASTPrinter::visitBinaryExpr(AST::Binary *expr) {
    std::cout << "(" << expr->op->text << " ";
    expr->left->accept(this);
    std::cout << " ";
    expr->right->accept(this);
    std::cout << ")";
}

void ASTPrinter::visitCallExpr(AST::Call *expr) {
    std::cout << "(call ";
    expr->callee->accept(this);
    for (auto arg : expr->args) {
        std::cout << " ";
        arg->accept(this);
    }
    std::cout << ")";
}

void ASTPrinter::visitGetExpr(AST::Get *expr) {
    std::cout << "(. ";
    expr->object->accept(this);
    std::cout << " " << expr->name->text << ")";
}

void ASTPrinter::visitGroupingExpr(AST::Grouping *expr) {
    std::cout << "(group ";
    expr->expression->accept(this);
    std::cout << ")";
}

void ASTPrinter::visitLogicalExpr(AST::Logical *expr) {
    std::cout << "(" << expr->op->text << " ";
    expr->left->accept(this);
    std::cout << " ";
    expr->right->accept(this);
    std::cout << ")";
}

void ASTPrinter::visitSetExpr(AST::Set *expr) {
    std::cout << "(= ";
    expr->object->accept(this);
    std::cout << " " << expr->name->text << " ";
    expr->value->accept(this);
    std::cout << ")";
}

void ASTPrinter::visitSuperExpr(AST::Super *expr) {
    std::cout << "(super " << expr->method->text << ")";
}

void ASTPrinter::visitThisExpr(AST::This *expr) {
    std::cout << "this";
}

void ASTPrinter::visitUnaryExpr(AST::Unary *expr) {
    std::cout << "(" << expr->op->text << " ";
    expr->right->accept(this);
    std::cout << ")";
}

void ASTPrinter::visitTernaryIfExpr(AST::TernaryIf *expr) {
    std::cout << "(";
    expr->condition->accept(this);
    std::cout << " then ";
    expr->then->accept(this);
    std::cout << " else ";
    expr->_else->accept(this);
    std::cout << ")";
}

void ASTPrinter::visitVariableExpr(AST::Variable *expr) {
    std::cout << expr->name->text;
}

void ASTPrinter::visitLiteralExpr(AST::Literal *expr) {
    value::printValue(expr->value);
}

void ASTPrinter::visitBlockStmt(AST::Block *stmt) {
    std::cout << "(block\n";

    for (auto statement : stmt->statements) {
        statement->accept(this);
    }

    std::cout << "end block)" << std::endl;
}

void ASTPrinter::visitProtoStmt(AST::Proto *stmt) {
    std::cout << "(extern " << stmt->name->text << " (";
    for (int i = 0; i < stmt->types.size(); i++) {
        if (i)
            std::cout << ", ";
        std::cout << stmt->types[i].second->text;
    }
    std::cout << ") : " << stmt->ret_type.second->text << " end extern)"
              << std::endl;
}

void ASTPrinter::visitExpressionStmt(AST::Expression *stmt) {
    std::cout << "(; ";
    stmt->expression->accept(this);
    std::cout << ")" << std::endl;
}

void ASTPrinter::visitFunctionStmt(AST::Function *stmt) {
    std::cout << "(fn " << stmt->name->text << " (";
    for (int i = 0; i < stmt->params.size(); i++) {
        if (i)
            std::cout << ", ";
        std::cout << stmt->params[i]->text << " : "
                  << stmt->types[i].second->text;
    }
    std::cout << ") : " << stmt->ret_type.second->text << "\n";

    for (AST::Stmt *body : stmt->body) {
        body->accept(this);
    }

    std::cout << "end fn)" << std::endl;
}

void ASTPrinter::visitClassStmt(AST::Class *stmt) {
    std::cout << "(class " << stmt->name->text;

    if (stmt->superclass != nullptr) {
        std::cout << " < ";
        print(stmt->superclass);
    }

    std::cout << "\n";

    for (AST::Function *method : stmt->methods) {
        print(method);
    }

    std::cout << "end class)" << std::endl;
}

void ASTPrinter::visitIfStmt(AST::If *stmt) {
    std::cout << "(if ";
    stmt->condition->accept(this);
    std::cout << " then ";
    stmt->thenBranch->accept(this);
    if (stmt->elseBranch != nullptr) {
        std::cout << "else ";
        stmt->elseBranch->accept(this);
    }
    std::cout << "end if)" << std::endl;
}

void ASTPrinter::visitPrintStmt(AST::Print *stmt) {
    std::cout << "(print ";
    stmt->expression->accept(this);
    std::cout << ")" << std::endl;
}

void ASTPrinter::visitReturnStmt(AST::Return *stmt) {
    std::cout << "(return";
    if (stmt->value != nullptr) {
        std::cout << " ";
        stmt->value->accept(this);
    }
    std::cout << ")" << std::endl;
}

void ASTPrinter::visitVarStmt(AST::Var *stmt) {
    std::cout << "(var " << stmt->name->text << " : "
              << stmt->type.second->text;
    if (stmt->initializer != nullptr) {
        std::cout << " = ";
        stmt->initializer->accept(this);
    }
    std::cout << ")" << std::endl;
}

void ASTPrinter::visitWhileStmt(AST::While *stmt) {
    std::cout << "(while ";
    stmt->condition->accept(this);
    std::cout << " do ";
    stmt->body->accept(this);
    std::cout << "end while)" << std::endl;
}

void ASTPrinter::visitForStmt(AST::For *stmt) {
    std::cout << "(for ";
    if (stmt->initializer != nullptr) {
        stmt->initializer->accept(this);
        // std::cout << "\b ";
    }
    std::cout << "while ";
    stmt->condition->accept(this);
    std::cout << "\n";
    stmt->body->accept(this);
    if (stmt->increment != nullptr) {
        std::cout << "then ";
        stmt->increment->accept(this);
        std::cout << "\n";
    }
    std::cout << "end for)" << std::endl;
}

void ASTPrinter::visitBreakStmt(AST::Break *stmt) {
    std::cout << "break" << std::endl;
}

void ASTPrinter::visitContinueStmt(AST::Continue *stmt) {
    std::cout << "continue" << std::endl;
}