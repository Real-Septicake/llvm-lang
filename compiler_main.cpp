#include "ast_printer.hpp"
#include "compiler.hpp"
#include "error.hpp"
#include "expr.hpp"
#include "parser.hpp"
#include "scanner.hpp"
#include "stmt.hpp"

#include <fstream>
#include <iostream>
#include <termcolor/termcolor.hpp>
#include <vector>

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
#ifdef DEBUG
    std::cout << "\nTokens: " << std::endl;
    for (auto tok : toks) {
        printToken(*tok);
    }
#endif
    parser::Parser parser(toks);
    std::vector<AST::Stmt *> stmts = parser.parse();
    if (error::errored)
        return 1;
#ifdef DEBUG
    ASTPrinter printer;
    std::cout << "\nAST:" << std::endl;
    for (auto stmt : stmts) {
        if (stmt == nullptr) {
            std::cerr << "Error parsing file" << std::endl;
            return 1;
        }
        stmt->accept(&printer);
    }
#endif
    std::string filename = std::string(argv[1]).substr(
        std::string(argv[1]).find_last_of("/\\") + 1);
    filename                 = filename.substr(0, filename.find_last_of("."));
    compiler::Compiler *comp = new compiler::Compiler(filename);
    for (auto stmt : stmts) {
        stmt->codegen(comp);
    }
    if (comp->errored) {
        return 1;
    }
#ifdef DEBUG
    comp->print_code();
#endif
    comp->verify();
    if (comp->errored) {
        return 1;
    }
    if (comp->write("out.o")) {
        return 1;
    }
    return 0;
}
