#ifndef ERROR_HPP
#define ERROR_HPP

#include "token.hpp"

#include <string>

/// @brief A namespace containing error utilities
namespace error {
/// @brief If an error has occurred, set if
/// @ref error::report "report" or @ref error::error "error" is called
extern bool errored;
/// @brief Reports an error on the specified line
/// @param line The line the error occurred at
/// @param message The message to display
void error(int line, std::string message);
/// @brief Reports an error at the specified token
/// @param token The token the error occurred at
/// @param message The message to display
void error(Token *token, std::string message);
/// @brief Reports an error and sets error::errored
/// @param line The line the error occurred at
/// @param where A string representing where the error occurred
/// @param message The message to display
void report(int line, std::string where, std::string message);
} // namespace error

#endif