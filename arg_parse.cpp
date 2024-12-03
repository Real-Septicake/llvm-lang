#include "arg_parse.hpp"

const char *argp_program_version = "llvm-lang 0.0";
static const char *doc           = "It's a shitty compiled language";
static const char *args_doc      = "FILE...";

static struct argp_option options[] = {
    {"out",                                    'o', "output file", 0, "The name of the outputted object file"},
    {"debug",                           'd', 0, 0, "thing"},
    {0}
};

static error_t parse_opts(int key, char *arg, struct argp_state *state) {
    struct args *arg_struct = (struct args *)state->input;
    switch (key) {
    case 'o':
        arg_struct->out_file = arg;
        break;
    case 'd':
        arg_struct->debug = true;
        break;
    case ARGP_KEY_ARG:
        arg_struct->files.push_back(std::string(arg));
        break;
    default:
        return ARGP_ERR_UNKNOWN;
    }
    return 0;
}

static struct argp argp = {options, parse_opts, args_doc, doc, 0, 0, 0};

struct args arg_struct;

args get_args() {
    return arg_struct;
}

error_t parse_args(int argc, char **argv) {
    arg_struct.out_file = "out.o";
    arg_struct.debug    = false;

    return argp_parse(&argp, argc, argv, 0, 0, &arg_struct);
    ;
}
