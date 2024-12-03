#ifndef ARG_PARSE_HPP
#define ARG_PARSE_HPP

#include <argp.h>
#include <string>
#include <vector>

struct args {
    std::string out_file;
    std::vector<std::string> files;
    bool debug;
};

struct args get_args();

error_t parse_args(int argc, char **argv);

#endif