#include "value.hpp"

#include <iostream>

bool value::valuesEqual(Value a, Value b) {
    if (a.type != b.type)
        return false;
    switch (a.type) {
    case VAL_BOOL:
        return asBool(a) == asBool(b);
    case VAL_NUM:
        return asNum(a) == asNum(b);
    default:
        return false;
    }
}

void value::printValue(Value value) {
    switch (value.type) {
    case VAL_BOOL:
        std::cout << (asBool(value) ? "true" : "false");
        break;
    case VAL_NUM:
        std::cout << asNum(value);
        break;
    }
}
