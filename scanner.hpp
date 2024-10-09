#ifndef SCANNER_HPP
#define SCANNER_HPP

#include "token.hpp"

#include <string>
#include <vector>

/// @brief A scanner for tokenizing source code
class Scanner {
  private:
    /// @brief The original text of the source code
    std::string source;
    /// @brief The tokens generated thus far by the scanner, returned by
    /// ::scanTokens
    std::vector<Token *> tokens = std::vector<Token *>(0);
    /// @brief The beginning of the current token
    int start = 0;
    /// @brief the current character being scanned
    int current = 0;
    /// @brief Current line
    size_t line = 1;
    /// @brief Pop the next chracter from the source text
    /// @return The chracter popped
    char advance();
    /// @brief Checks if the scanner has reached the end of the source text
    /// @return True if the scanner reached the end, false otherwise
    bool isAtEnd();
    /// @brief Checks if the next character matches the provided character,
    /// popping it if it does
    /// @param expected The character to be matched
    /// @return True if the chracter is matched and popped, false otherwise
    bool match(char expected);
    /// @brief Peek the next chracter, or '\0' if the end has been reached
    /// @return The peeked chracter
    char peek();
    /// @brief Peek the character following the next character, or '\0' if the
    /// end has been reached
    /// @return The peeked character
    char peekNext();
    /// @brief Scan the next character and add it to Scanner::tokens
    void scanToken();
    /// @brief Add a token of the supplied type to Scanner::tokens, with the
    /// text covered by Scanner::start and Scanner::start
    /// @param type The type of token to add
    void addToken(TokenKind type);
    /// @brief Scan an identifier
    void identifier();
    /// @brief Scan a string
    // void string();
    /// @brief Scan a number
    void number();

  public:
    /// @brief Creates a scanner with the specified source code
    /// @param src The source code to tokenize
    Scanner(std::string src);
    /// @brief Destructor
    ~Scanner() = default;
    /// @brief Tokenizes the input code
    /// @return The list of tokens
    std::vector<Token *> scanTokens();
};

#endif