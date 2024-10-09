#include "ast_printer.hpp"
#include "error.hpp"
#include "parser.hpp"
#include "scanner.hpp"
#include "stmt.hpp"
#include "expr.hpp"

#include <fstream>
#include <iostream>
#include <termcolor/termcolor.hpp>
#include <vector>

class Compiler : public AST::ExprVisitor, public AST::StmtVisitor {
    public:
        void visitAssignExpr(AST::Assign *expr) override;
        void visitBinaryExpr(AST::Binary *expr) override;
        void visitCallExpr(AST::Call *expr) override;
        void visitGetExpr(AST::Get *expr) override;
        void visitGroupingExpr(AST::Grouping *expr) override;
        void visitLogicalExpr(AST::Logical *expr) override;
        void visitSetExpr(AST::Set *expr) override;
        void visitSuperExpr(AST::Super *expr) override;
        void visitThisExpr(AST::This *expr) override;
        void visitUnaryExpr(AST::Unary *expr) override;
        void visitVariableExpr(AST::Variable *expr) override;
        void visitLiteralExpr(AST::Literal *expr) override;
        void visitBlockStmt(AST::Block *stmt) override;
        void visitExpressionStmt(AST::Expression *stmt) override;
        void visitFunctionStmt(AST::Function *stmt) override;
        void visitClassStmt(AST::Class *stmt) override;
        void visitIfStmt(AST::If *stmt) override;
        void visitPrintStmt(AST::Print *stmt) override;
        void visitReturnStmt(AST::Return *stmt) override;
        void visitVarStmt(AST::Var *stmt) override;
        void visitWhileStmt(AST::While *stmt) override;
        void visitForStmt(AST::For *stmt) override;
        void visitBreakStmt(AST::Break *stmt) override;
        void visitContinueStmt(AST::Continue *stmt) override;
};

int main(int argc, char const *argv[]) {
    if (argc != 2) {
        std::cerr << termcolor::bright_red << termcolor::bold
                  << "oi, give me a file you daft cu-" << termcolor::reset
                  << std::endl
                  << "usage: " << argv[0] << " <file>" << std::endl;
        return 1;
    }
    std::ifstream file(argv[1], std::ios::binary);
    std::string source((std::istreambuf_iterator<char>{file}), {});
    if (source.length() == 0) {
        std::cerr << termcolor::bright_red << termcolor::bold
                  << "So no code? *breaks skateboard*" << termcolor::reset
                  << std::endl;
        return 0;
    }
    Scanner scanner(source);
    std::vector<Token *> toks = scanner.scanTokens();
    std::cout << "\nTokens: " << std::endl;
    for (auto tok : toks) {
        printToken(*tok);
    }
    parser::Parser parser(toks);
    std::vector<Stmt *> stmts = parser.parse();
    if (error::errored)
        return 1;
    ASTPrinter printer;
    std::cout << "\nAST:" << std::endl;
    for (auto stmt : stmts) {
        if (stmt == nullptr) {
            std::cerr << "Error parsing file" << std::endl;
            return 1;
        }
        stmt->accept(&printer);
    }
    return 0;
}

void Compiler::visitAssignExpr(AST::Assign *expr) {
    
}
