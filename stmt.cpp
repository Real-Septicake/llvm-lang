#include "stmt.hpp"

AST::Block::Block(std::vector<Stmt *> statements) : Stmt(StmtType::BlockType) {
    this->statements = statements;
}
void AST::Block::accept(StmtVisitor *visitor) {
    visitor->visitBlockStmt(this);
}

AST::Expression::Expression(Expr *expression) : Stmt(StmtType::ExpressionType) {
    this->expression = expression;
}
void AST::Expression::accept(StmtVisitor *visitor) {
    visitor->visitExpressionStmt(this);
}

AST::Function::Function(Token *name, std::vector<Token *> params,
                        std::vector<Stmt *> body)
    : Stmt(StmtType::FunctionType) {
    this->name   = name;
    this->params = params;
    this->body   = body;
}
void AST::Function::accept(StmtVisitor *visitor) {
    visitor->visitFunctionStmt(this);
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

AST::If::If(Expr *condition, Stmt *thenBranch, Stmt *elseBranch)
    : Stmt(StmtType::IfType) {
    this->condition  = condition;
    this->thenBranch = thenBranch;
    this->elseBranch = elseBranch;
}
void AST::If::accept(StmtVisitor *visitor) {
    visitor->visitIfStmt(this);
}

AST::Print::Print(Expr *expression) : Stmt(StmtType::PrintType) {
    this->expression = expression;
}
void AST::Print::accept(StmtVisitor *visitor) {
    visitor->visitPrintStmt(this);
}

AST::Return::Return(Token *keyword, Expr *value) : Stmt(StmtType::ReturnType) {
    this->keyword = keyword;
    this->value   = value;
}
void AST::Return::accept(StmtVisitor *visitor) {
    visitor->visitReturnStmt(this);
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

AST::While::While(Expr *condition, Stmt *body) : Stmt(StmtType::WhileType) {
    this->condition = condition;
    this->body      = body;
}
void AST::While::accept(StmtVisitor *visitor) {
    visitor->visitWhileStmt(this);
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

AST::Break::Break(Token *keyword) : Stmt(StmtType::BreakType) {
    this->keyword = keyword;
}
void AST::Break::accept(StmtVisitor *visitor) {
    visitor->visitBreakStmt(this);
}

AST::Continue::Continue(Token *keyword) : Stmt(StmtType::ContinueType) {
    this->keyword = keyword;
}
void AST::Continue::accept(StmtVisitor *visitor) {
    visitor->visitContinueStmt(this);
}
