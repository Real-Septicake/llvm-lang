#include "error.hpp"

#include <iostream>
#include <termcolor/termcolor.hpp>

bool error::errored = false;

void error::error(int line, std::string message) {
    report(line, "", message);
}

void error::error(Token *token, std::string message) {
    if (token->type == TokenKind::TOKEN_EOF)
        report(token->line, " at end", message);
    else
        report(token->line,
               std::string(" at '").append(token->text).append("'"), message);
}

void error::report(int line, std::string where, std::string message) {
    std::cerr << "\n"
              << termcolor::bright_red << termcolor::bold << "[line " << line
              << "] Error" << where << termcolor::reset << ": " << message
              << std::endl;
    errored = true;
}
