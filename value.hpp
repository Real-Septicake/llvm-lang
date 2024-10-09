#ifndef VALUE_HH
#define VALUE_HH

#include <string>
#include <vector>

/// @brief A namespace containing members used for values
namespace value {
/// @brief Types of values
enum ValueType { VAL_BOOL, VAL_NUM };

/// @brief Struct representing values
typedef struct {
    /// @brief Type of this value
    ValueType type;
    /// @brief Union of possible type representations
    union {
        /// @brief %Value of a type VAL_BOOL
        bool boolean;
        /// @brief %Value of a type VAL_NUM
        double number;
    } as;
} Value;

/// @brief Checks if the value is a bool
/// @param value The value to check
/// @return True if the value is a bool, false otherwise
inline bool isBool(Value value) {
    return value.type == VAL_BOOL;
}

/// @brief Checks if the value is a num
/// @param value The value to check
/// @return True if the value is a num, false otherwise
inline bool isNum(Value value) {
    return value.type == VAL_NUM;
}

/// @brief Acts as an unchecked cast from Value to the value's boolean
/// representation
/// @param value The Value to cast
/// @return The boolean representation
inline bool asBool(Value value) {
    return value.as.boolean;
}

/// @brief Acts as an unchecked cast from Value to the value's double
/// representation
/// @param value The value to cast
/// @return The double representation
inline double asNum(Value value) {
    return value.as.number;
}

/// @brief Creates a bool Value with the provided value
/// @param value The boolean to set the Value to
/// @return The created Value
inline Value boolVal(bool value) {
    return (Value){VAL_BOOL, {.boolean = value}};
}

/// @brief Creates a num Value with the specified value
/// @param value The number to set the Value to
/// @return The created Value
inline Value numVal(double value) {
    return (Value){VAL_NUM, {.number = value}};
}

/// @brief Checks if the two provided Value%s are equal
/// @param a The first value
/// @param b The second value
/// @return True if the two values are equal, false otherwise
bool valuesEqual(Value a, Value b);
/// @brief Prints the supplied Value
/// @param value The value to print
void printValue(Value value);
} // namespace value

#endif