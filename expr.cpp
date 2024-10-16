#include "expr.hpp"

AST::Assign::Assign(Token *name, Token *equals, Expr *value)
    : Expr(ExprType::AssignType) {
    this->name   = name;
    this->equals = equals;
    this->value  = value;
}
void AST::Assign::accept(ExprVisitor *visitor) {
    visitor->visitAssignExpr(this);
}
llvm::Value *AST::Assign::codegen(ExprVisitor *visitor) {
    return visitor->genAssignExpr(this);
}

AST::Binary::Binary(Expr *left, Token *op, Expr *right)
    : Expr(ExprType::BinaryType) {
    this->left  = left;
    this->op    = op;
    this->right = right;
}
void AST::Binary::accept(ExprVisitor *visitor) {
    visitor->visitBinaryExpr(this);
}
llvm::Value *AST::Binary::codegen(ExprVisitor *visitor) {
    return visitor->genBinaryExpr(this);
}

AST::Call::Call(Expr *callee, Token *paren, std::vector<Expr *> args)
    : Expr(ExprType::CallType) {
    this->callee = callee;
    this->paren  = paren;
    this->args   = args;
}
void AST::Call::accept(ExprVisitor *visitor) {
    visitor->visitCallExpr(this);
}
llvm::Value *AST::Call::codegen(ExprVisitor *visitor) {
    return visitor->genCallExpr(this);
}

AST::Get::Get(Expr *object, Token *name) : Expr(ExprType::GetType) {
    this->object = object;
    this->name   = name;
}
void AST::Get::accept(ExprVisitor *visitor) {
    visitor->visitGetExpr(this);
}
llvm::Value *AST::Get::codegen(ExprVisitor *visitor) {
    return visitor->genGetExpr(this);
}

AST::Grouping::Grouping(Expr *expression) : Expr(ExprType::GroupingType) {
    this->expression = expression;
}
void AST::Grouping::accept(ExprVisitor *visitor) {
    visitor->visitGroupingExpr(this);
}
llvm::Value *AST::Grouping::codegen(ExprVisitor *visitor) {
    return visitor->genGroupingExpr(this);
}

AST::Logical::Logical(Expr *left, Token *op, Expr *right)
    : Expr(ExprType::LogicalType) {
    this->left  = left;
    this->op    = op;
    this->right = right;
}
void AST::Logical::accept(ExprVisitor *visitor) {
    visitor->visitLogicalExpr(this);
}
llvm::Value *AST::Logical::codegen(ExprVisitor *visitor) {
    return visitor->genLogicalExpr(this);
}

AST::Set::Set(Expr *object, Token *name, Expr *value)
    : Expr(ExprType::SetType) {
    this->object = object;
    this->name   = name;
    this->value  = value;
}
void AST::Set::accept(ExprVisitor *visitor) {
    visitor->visitSetExpr(this);
}
llvm::Value *AST::Set::codegen(ExprVisitor *visitor) {
    return visitor->genSetExpr(this);
}

AST::Super::Super(Token *keyword, Token *method) : Expr(ExprType::SuperType) {
    this->keyword = keyword;
    this->method  = method;
}
void AST::Super::accept(ExprVisitor *visitor) {
    visitor->visitSuperExpr(this);
}
llvm::Value *AST::Super::codegen(ExprVisitor *visitor) {
    return visitor->genSuperExpr(this);
}

AST::This::This(Token *keyword) : Expr(ExprType::ThisType) {
    this->keyword = keyword;
}
void AST::This::accept(ExprVisitor *visitor) {
    visitor->visitThisExpr(this);
}
llvm::Value *AST::This::codegen(ExprVisitor *visitor) {
    return visitor->genThisExpr(this);
}

AST::Unary::Unary(Token *op, Expr *right) : Expr(ExprType::UnaryType) {
    this->op    = op;
    this->right = right;
}
void AST::Unary::accept(ExprVisitor *visitor) {
    visitor->visitUnaryExpr(this);
}
llvm::Value *AST::Unary::codegen(ExprVisitor *visitor) {
    return visitor->genUnaryExpr(this);
}

AST::TernaryIf::TernaryIf(Expr *condition, Token *question, Expr *then,
                          Token *colon, Expr *_else)
    : Expr(ExprType::TernaryIfType) {
    this->condition = condition;
    this->question  = question;
    this->then      = then;
    this->colon     = colon;
    this->_else     = _else;
}
void AST::TernaryIf::accept(ExprVisitor *visitor) {
    visitor->visitTernaryIfExpr(this);
}
llvm::Value *AST::TernaryIf::codegen(ExprVisitor *visitor) {
    return visitor->genTernaryIfExpr(this);
}

AST::Variable::Variable(Token *name) : Expr(ExprType::VariableType) {
    this->name = name;
}
void AST::Variable::accept(ExprVisitor *visitor) {
    visitor->visitVariableExpr(this);
}
llvm::Value *AST::Variable::codegen(ExprVisitor *visitor) {
    return visitor->genVariableExpr(this);
}

AST::Literal::Literal(value::Value value) : Expr(ExprType::LiteralType) {
    this->value = value;
}
void AST::Literal::accept(ExprVisitor *visitor) {
    visitor->visitLiteralExpr(this);
}
llvm::Value *AST::Literal::codegen(ExprVisitor *visitor) {
    return visitor->genLiteralExpr(this);
}
