def isVowel(char: chr):
    return char == 'a' or char == 'e' or char == 'i' or char == 'o' or char == 'u'

def replaceAll(file, str, replace):
    with open(file, 'r') as read:
        filedata = read.read()
    
    filedata = filedata.replace(str, replace)

    with open(file, 'w') as write:
        write.write(filedata)

def defineAst(baseName: str, longName: str, types: list[tuple[str, str]], includes: list[str] = []):
    f = open(baseName.lower() + ".hpp", "w", encoding="utf-8")
    
    f.write("#ifndef " + baseName.upper() + "_HH\n")
    f.write("#define " + baseName.upper() + "_HH\n\n")

    f.write("#include <llvm/IR/Value.h>\n")
    f.write("#include \"token.hpp\"\n\n")

    for include in includes:
        f.write("#include " + include + "\n")

    f.write("/// @brief A namespace containing members used for the creation and usage of the abstract syntax tree\n")
    f.write("namespace AST {\n\n")

    f.write("/// @brief An enum representing the possible AST::" + baseName + " types\n")
    f.write("enum " + baseName.strip() + "Type {\n")
    for type in types:
        f.write(type[0].strip() + "Type,\n")
    f.write("};\n\n")

    f.write("// Forward declaration of visitor\n")
    f.write("class " + baseName + "Visitor;\n\n")

    f.write("/// @brief The base class representing " + longName + " nodes\n")
    f.write("class " + baseName + " {\n")
    f.write("public:\n")
    f.write("/// @brief The type of this node, used for quick type checks\n")
    f.write(baseName.strip() + "Type type;\n")
    f.write("/// @brief Accept an AST::" + baseName + "Visitor\n")
    f.write("/// @param visitor The visitor to accept\n")
    f.write("virtual void accept(" + baseName + "Visitor *visitor) = 0;\n")
    f.write("virtual llvm::Value *codegen(" + baseName + "Visitor *visitor) = 0;\n")
    f.write("/// @brief Create an AST::" + baseName + " with the specified type\n")
    f.write("/// @param type The type of this AST::" + baseName + " node\n")
    f.write(baseName + "(" + baseName.strip() + "Type type) : type(type) {}")
    f.write("}; // Class " + baseName + "\n\n")

    for type in types:
        f.write("/// @brief The node for a" + ('n ' if isVowel(type[0].strip().lower()[0]) else ' ') + type[0].strip().lower() + " " + longName + "\n")
        f.write("class " + type[0].strip() + ": public " + baseName + " {\n")
        f.write("public:\n")
        for field in type[1].split(","):
            f.write(field.strip() + ";\n")
        f.write(type[0].strip() + "(" + type[1].strip() + ");\n")
        f.write("void accept(" + baseName + "Visitor* visitor) override;\n")
        f.write("llvm::Value *codegen(" + baseName + "Visitor* visitor) override;\n")
        f.write("};\n\n")

    f.write("/// @brief The visitor for the AST::" + baseName + " class\n")
    f.write("class " + baseName + "Visitor {\n")
    f.write("public:\n")
    for type in types:
        f.write("/// @brief Visit the AST::" + type[0].strip() + " node\n")
        f.write("/// @param " + baseName.lower() + " The node to visit\n")
        f.write("virtual void visit" + type[0].strip() + baseName + "(AST::" + type[0].strip() + "* " + baseName.lower() + ") { return; }\n")
        f.write("virtual llvm::Value *gen" + type[0].strip() + baseName + "(AST::" + type[0].strip() + "* " + baseName.lower() + ") { return nullptr; }\n")
    f.write("}; // Visitor\n\n")

    f.write("} // namespace AST\n")
    
    f.write("\n#endif")

    f.close()

    replaceAll(baseName.lower() + ".hpp", "\"comma\"", ",")

    f = open(baseName.lower() + ".cpp", "w", encoding="utf-8")

    f.write("#include \"" + baseName.lower() + ".hpp\"\n\n")

    for type in types:
        f.write("AST::" + type[0].strip() + "::" + type[0].strip() + "(" + type[1].strip() + ") : " + baseName + "(" + baseName.strip() + "Type::" + type[0].strip() + "Type) {\n")
        for field in type[1].strip().split(","):
            f.write("this->" + field.split()[1] + " = " + field.split()[1] + ";\n")
        f.write("}\n")
        f.write("void AST::" + type[0].strip() + "::accept(" + baseName + "Visitor* visitor) {\n")
        f.write("visitor->visit" + type[0].strip() + baseName + "(this);\n")
        f.write("}\n")
        f.write("llvm::Value *AST::" + type[0].strip() + "::codegen(" + baseName + "Visitor* visitor) {\n")
        f.write("return visitor->gen" + type[0].strip() + baseName + "(this);\n")
        f.write("}\n\n")

    f.close()

    replaceAll(baseName.lower() + ".cpp", "\"comma\"", ",")

defineAst("Expr", "expression", [
    ("Assign", "Token* name, Token* equals, Expr* value"),
    ("Binary", "Expr* left, Token* op, Expr* right"),
    ("Call", "Expr* callee, Token* paren, std::vector<Expr*> args"),
    ("Get", "Expr* object, Token* name"),
    ("Grouping", "Expr* expression"),
    ("Logical", "Expr* left, Token* op, Expr* right"),
    ("Set", "Expr* object, Token* name, Expr* value"),
    ("Super", "Token* keyword, Token* method"),
    ("This", "Token* keyword"),
    ("Unary", "Token* op, Expr* right"),
    ("TernaryIf", "Expr* condition, Token* question, Expr* then, Token* colon, Expr* _else"),
    ("Variable", "Token* name"),
    ("Literal", "value::Value value")
], ["<vector>", "<utility>", "\"value.hpp\""])

defineAst("Stmt", "statement", [
    ("Block", "std::vector<Stmt*> statements"),
    ("Expression", "Expr* expression"),
    ("Function", "Token* name, std::vector<Token*> params, std::vector<std::pair<value::ValueType\"comma\"Token*>> types, std::pair<value::ValueType\"comma\"Token*> ret_type, std::vector<Stmt*> body"),
    ("Class", "Token* name, AST::Variable* superclass, std::vector<AST::Function*> methods"),
    ("If", "Token* paren, Expr* condition, Stmt* thenBranch, Stmt* elseBranch"),
    ("Print", "Token* keyword, Expr* expression"),
    ("Return", "Token* keyword, Expr* value"),
    ("Var", "Token* name, std::pair<value::ValueType\"comma\"Token*> type, Expr* initializer"),
    ("While", "Expr* condition, Stmt* body"),
    ("For", "Stmt* initializer, Expr* condition, Expr* increment, Stmt* body"),
    ("Break", "Token* keyword"),
    ("Continue", "Token* keyword")
], ["\"expr.hpp\"", "<vector>"])