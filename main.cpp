#include <stdio.h>

extern "C" {
void __main();
}

int main(int argc, char const *argv[]) {
    __main();
    return 0;
}
