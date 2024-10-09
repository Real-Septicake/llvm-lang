#include "expr.hpp"

AST::Assign::Assign(Token *name, Expr *value) : Expr(ExprType::AssignType) {
    this->name  = name;
    this->value = value;
}
void AST::Assign::accept(ExprVisitor *visitor) {
    visitor->visitAssignExpr(this);
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

AST::Call::Call(Expr *callee, Token *paren, std::vector<Expr *> args)
    : Expr(ExprType::CallType) {
    this->callee = callee;
    this->paren  = paren;
    this->args   = args;
}
void AST::Call::accept(ExprVisitor *visitor) {
    visitor->visitCallExpr(this);
}

AST::Get::Get(Expr *object, Token *name) : Expr(ExprType::GetType) {
    this->object = object;
    this->name   = name;
}
void AST::Get::accept(ExprVisitor *visitor) {
    visitor->visitGetExpr(this);
}

AST::Grouping::Grouping(Expr *expression) : Expr(ExprType::GroupingType) {
    this->expression = expression;
}
void AST::Grouping::accept(ExprVisitor *visitor) {
    visitor->visitGroupingExpr(this);
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

AST::Set::Set(Expr *object, Token *name, Expr *value)
    : Expr(ExprType::SetType) {
    this->object = object;
    this->name   = name;
    this->value  = value;
}
void AST::Set::accept(ExprVisitor *visitor) {
    visitor->visitSetExpr(this);
}

AST::Super::Super(Token *keyword, Token *method) : Expr(ExprType::SuperType) {
    this->keyword = keyword;
    this->method  = method;
}
void AST::Super::accept(ExprVisitor *visitor) {
    visitor->visitSuperExpr(this);
}

AST::This::This(Token *keyword) : Expr(ExprType::ThisType) {
    this->keyword = keyword;
}
void AST::This::accept(ExprVisitor *visitor) {
    visitor->visitThisExpr(this);
}

AST::Unary::Unary(Token *op, Expr *right) : Expr(ExprType::UnaryType) {
    this->op    = op;
    this->right = right;
}
void AST::Unary::accept(ExprVisitor *visitor) {
    visitor->visitUnaryExpr(this);
}

AST::Variable::Variable(Token *name) : Expr(ExprType::VariableType) {
    this->name = name;
}
void AST::Variable::accept(ExprVisitor *visitor) {
    visitor->visitVariableExpr(this);
}

AST::Literal::Literal(value::Value value) : Expr(ExprType::LiteralType) {
    this->value = value;
}
void AST::Literal::accept(ExprVisitor *visitor) {
    visitor->visitLiteralExpr(this);
}
