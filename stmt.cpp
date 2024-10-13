#include "stmt.hpp"

AST::Block::Block(std::vector<Stmt *> statements) : Stmt(StmtType::BlockType) {
    this->statements = statements;
}
void AST::Block::accept(StmtVisitor *visitor) {
    visitor->visitBlockStmt(this);
}
llvm::Value *AST::Block::codegen(StmtVisitor *visitor) {
    return visitor->genBlockStmt(this);
}

AST::Expression::Expression(Expr *expression) : Stmt(StmtType::ExpressionType) {
    this->expression = expression;
}
void AST::Expression::accept(StmtVisitor *visitor) {
    visitor->visitExpressionStmt(this);
}
llvm::Value *AST::Expression::codegen(StmtVisitor *visitor) {
    return visitor->genExpressionStmt(this);
}

AST::Function::Function(Token *name, std::vector<Token *> params,
                        std::vector<std::pair<value::ValueType, Token *>> types,
                        std::pair<value::ValueType, Token *> ret_type,
                        std::vector<Stmt *> body)
    : Stmt(StmtType::FunctionType) {
    this->name     = name;
    this->params   = params;
    this->types    = types;
    this->ret_type = ret_type;
    this->body     = body;
}
void AST::Function::accept(StmtVisitor *visitor) {
    visitor->visitFunctionStmt(this);
}
llvm::Value *AST::Function::codegen(StmtVisitor *visitor) {
    return visitor->genFunctionStmt(this);
}

AST::Class::Class(Token *name, AST::Variable *superclass,
                  std::vector<AST::Function *> methods)
    : Stmt(StmtType::ClassType) {
    this->name       = name;
    this->superclass = superclass;
    this->methods    = methods;
}
void AST::Class::accept(StmtVisitor *visitor) {
    visitor->visitClassStmt(this);
}
llvm::Value *AST::Class::codegen(StmtVisitor *visitor) {
    return visitor->genClassStmt(this);
}

AST::If::If(Expr *condition, Stmt *thenBranch, Stmt *elseBranch)
    : Stmt(StmtType::IfType) {
    this->condition  = condition;
    this->thenBranch = thenBranch;
    this->elseBranch = elseBranch;
}
void AST::If::accept(StmtVisitor *visitor) {
    visitor->visitIfStmt(this);
}
llvm::Value *AST::If::codegen(StmtVisitor *visitor) {
    return visitor->genIfStmt(this);
}

AST::Print::Print(Token *keyword, Expr *expression)
    : Stmt(StmtType::PrintType) {
    this->keyword    = keyword;
    this->expression = expression;
}
void AST::Print::accept(StmtVisitor *visitor) {
    visitor->visitPrintStmt(this);
}
llvm::Value *AST::Print::codegen(StmtVisitor *visitor) {
    return visitor->genPrintStmt(this);
}

AST::Return::Return(Token *keyword, Expr *value) : Stmt(StmtType::ReturnType) {
    this->keyword = keyword;
    this->value   = value;
}
void AST::Return::accept(StmtVisitor *visitor) {
    visitor->visitReturnStmt(this);
}
llvm::Value *AST::Return::codegen(StmtVisitor *visitor) {
    return visitor->genReturnStmt(this);
}

AST::Var::Var(Token *name, std::pair<value::ValueType, Token *> type,
              Expr *initializer)
    : Stmt(StmtType::VarType) {
    this->name        = name;
    this->type        = type;
    this->initializer = initializer;
}
void AST::Var::accept(StmtVisitor *visitor) {
    visitor->visitVarStmt(this);
}
llvm::Value *AST::Var::codegen(StmtVisitor *visitor) {
    return visitor->genVarStmt(this);
}

AST::While::While(Expr *condition, Stmt *body) : Stmt(StmtType::WhileType) {
    this->condition = condition;
    this->body      = body;
}
void AST::While::accept(StmtVisitor *visitor) {
    visitor->visitWhileStmt(this);
}
llvm::Value *AST::While::codegen(StmtVisitor *visitor) {
    return visitor->genWhileStmt(this);
}

AST::For::For(Stmt *initializer, Expr *condition, Expr *increment, Stmt *body)
    : Stmt(StmtType::ForType) {
    this->initializer = initializer;
    this->condition   = condition;
    this->increment   = increment;
    this->body        = body;
}
void AST::For::accept(StmtVisitor *visitor) {
    visitor->visitForStmt(this);
}
llvm::Value *AST::For::codegen(StmtVisitor *visitor) {
    return visitor->genForStmt(this);
}

AST::Break::Break(Token *keyword) : Stmt(StmtType::BreakType) {
    this->keyword = keyword;
}
void AST::Break::accept(StmtVisitor *visitor) {
    visitor->visitBreakStmt(this);
}
llvm::Value *AST::Break::codegen(StmtVisitor *visitor) {
    return visitor->genBreakStmt(this);
}

AST::Continue::Continue(Token *keyword) : Stmt(StmtType::ContinueType) {
    this->keyword = keyword;
}
void AST::Continue::accept(StmtVisitor *visitor) {
    visitor->visitContinueStmt(this);
}
llvm::Value *AST::Continue::codegen(StmtVisitor *visitor) {
    return visitor->genContinueStmt(this);
}
