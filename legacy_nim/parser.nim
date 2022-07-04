import lexer

type
    Node_type* = enum
        PROGRAM,LINES,LINE,LITERAL,EXPR,LOGIC_EXPR,FACTOR,TERM,STATEMENT,
        ARR_LITERALS, ARR_INIT, DECLARATION,ASSIGN,BREAK,CONTINUE,IO,
        STRING_LITERAL,FORMATTED_LIT,INT_LITERAL,CHAR_LITERAL,BOOL_LITERAL,FLOAT_LITERAL,TYPE,
        IF,WHILE,FOREACH,
        OPERATOR,KEYWORD,NEWLINE,ID,INDENT,DEDENT, FUNC, FUNC_CALL,METHOD_CALL,ARR_ELEMENT,
        ADD,SUB,MUL,DIV,MOD,EQUAL,BIGGER,SMALLER,EQ_BIGGER,EQ_SMALLER,NOT_EQUAL,AND,OR
        PRINT,INPUT,RETURN,PIWO,WINO,KUFEL,PARAMETERS
        EOF
        
    Node* = object 
        typ* : Node_type
        value*: string
        children* : seq[Node]
    
var
    AST_Root*: Node
    index = 0
    debug_line = 0
AST_Root.typ = PROGRAM

#TODO incerement index every time false is returned to show exactly what token is wrong

proc expr(node : var Node): bool
proc line(node: var Node): bool
proc logic_expr(node: var Node): bool
proc func_call(node: var Node): bool
proc term(node: var Node,first: bool) : bool

var deep_inc = 0
proc display_tree*(node: Node) = 
    inc deep_inc
    for i in 1 .. deep_inc: stdout.write("\t")
    echo node.typ, " ", node.value
    for c in node.children:
        display_tree(c)
    dec deep_inc

proc add_child(parent : var Node,typ : Node_type, val: string = "") =                 
    parent.children.add(Node(typ:typ,value:val))

proc add_child(parent : var Node,child : Node) = 
    parent.children.add(child)

# echo "tokens: " & $TokenList.len
proc literal(node : var Node): bool = 
    if TokenList[index].tok_type in [TOK_INT_LIT,TOK_STRING_LIT,TOK_CHAR_LIT,TOK_BOOL_LIT,TOK_FLOAT_LIT,TOK_FORMATTED_LIT]:
        let typ = Node_type(STRING_LITERAL.ord + (TokenList[index].tok_type.ord - TOK_STRING_LIT.ord))
        node.add_child(typ,TokenList[index].value)
        inc index
        return true
    else:
        dec index
        return false

proc factor(node: var Node,first: bool): bool =
    var fact = Node(typ: FACTOR)
    if TokenList[index].tok_type == TOK_ID:
        var call = false
        if TokenList[index+1].value == "[":
            call = true
            fact.add_child(ARR_ELEMENT,TokenList[index].value)
            #fact.add_child(OPERATOR,"[")
            index += 2
            if expr(fact):
                if TokenList[index].value == "]":
                    #fact.add_child(OPERATOR,"]")
                    node.add_child(fact)
                    inc index
                    return true
                else: return false
            else: return false
            
        elif TokenList[index+1].value == "(":
            #fact.add_child(OPERATOR,"(")
            call = true
            fact.add_child(FUNC_CALL,TokenList[index].value)
            index += 2
            let i = index
            var params = Node(typ: PARAMETERS)
            if func_call(params):
                if TokenList[index].value == ")":
                   # fact.add_child(OPERATOR,")")
                    fact.add_child(params)
                    node.add_child(fact)
                    inc index
                    return true
                else: return false
            else:
                index = i
                if TokenList[index].value == ")":
                    #fact.add_child(OPERATOR,")")
                    node.add_child(fact)
                    inc index
                    return true
                else: return false
        elif TokenList[index+1].value == ".":
            call = true
            fact.add_child(FUNC,"")
            if TokenList[index+2].tok_type == TOK_ID:
                fact.add_child(ID,TokenList[index+2].value)
                if TokenList[index+3].value == "(":
                    #fact.add_child(OPERATOR,"(")
                    index += 4
                    let i = index
                    if func_call(fact):
                        if TokenList[index].value == ")":
                           # fact.add_child(OPERATOR,")")
                            node.add_child(fact)
                            inc index
                            return true
                        else: return false
                    else:
                        index = i
                        if TokenList[index].value == ")":
                            #fact.add_child(OPERATOR,")")
                            node.add_child(fact)
                            inc index
                            return true
                        else: return false
            else: return false
                
        if not call: fact.add_child(ID,TokenList[index].value)  
        node.add_child(fact)
        inc index
        return true
    elif TokenList[index].value == "(":
        #fact.add_child(OPERATOR,"(")
        inc index
        let i = index
        if expr(fact):
            if TokenList[index].value == ")":
                inc index
                #fact.add_child(OPERATOR,")")
                node.add_child(fact)
                return true
        index = i
        if logic_expr(fact):
            if TokenList[index].value == ")":
                inc index
                #fact.add_child(OPERATOR,")")
                node.add_child(fact)
                return true
        else: return false
    
    if not first:
        let i = index
        if expr(fact):
            node.add_child(fact)
            return true
        if logic_expr(fact):
            node.add_child(fact)
            return true
        index = i
    
    if literal(fact):
        node.add_child(fact)
        return true
    else: return false

proc term(node: var Node,first: bool) : bool = 
    var ter = Node(typ: TERM)
    if factor(ter,first):
        if TokenList[index].value in ["*","/","%"]:
            if TokenList[index].value == "*":    ter.add_child(MUL,"")
            elif TokenList[index].value == "/":  ter.add_child(DIV,"")
            else: ter.add_child(MOD,"")
            inc index
            if factor(ter,false):
                node.add_child(ter)
                return true
            else: return false
        node.add_child(ter)
        return true
    else: return false

proc expr(node : var Node): bool = 
    var exp = Node(typ: EXPR)
    if term(exp,true):
        if TokenList[index].value in ["+","-"]:
            if TokenList[index].value == "+": exp.add_child(ADD,"")
            else: exp.add_child(SUB,"")
            inc index
            if term(exp,false):
                node.add_child(exp)
                return true
            else: dec index; return false
        node.add_child(exp)
        return true
    else:
        return false

proc logic_expr(node: var Node): bool = 
    var temp = Node(typ: LOGIC_EXPR) # FIXME another dirty workaround
    if expr(temp):
        if TokenList[index].value in [">","<",">=","<=","!=","==","albo","oraz"]:
            var logic : Node_type
            case TokenList[index].value:
                of ">": logic = BIGGER
                of "<": logic = SMALLER
                of ">=": logic = EQ_BIGGER
                of "<=": logic = EQ_SMALLER
                of "!=": logic = NOT_EQUAL
                of "==": logic = EQUAL
                of "albo": logic = OR
                of "oraz": logic = AND
                
            temp.add_child(logic,"")
            inc index
            let i = index
            if logic_expr(temp):
                node.add_child(temp)
                return true
            index = i
            if expr(temp):
                node.add_child(temp)
                return true
            else: dec index; return false
        node.add_child(temp); return true
    else: return false
        

proc assign(node: var Node) : bool = 
    
    var ass = Node(typ: ASSIGN)
    if TokenList[index].tok_type == TOK_ID:
        #ass.add_child(ID,TokenList[index].value)
        var arr = false
        inc index
        if TokenList[index].value == "[":
            ass.add_child(ARR_ELEMENT,TokenList[index-1].value)
            arr = true
            #ass.add_child(OPERATOR,"[")
            inc index
            if expr(ass):
                if TokenList[index].value == "]":
                    #ass.add_child(OPERATOR,"]")
                    inc index
                else: return false
            else: return false
        if not arr: ass.add_child(ID,TokenList[index-1].value)
        
        if TokenList[index].value == "=":
            #ass.add_child(OPERATOR,"=")
            inc index
            let i = index 
            if expr(ass):
                node.add_child(ass)
                return true
            index = i
            if logic_expr(ass):
                node.add_child(ass)
                return true
            else: return false
        else: return false
    else:
        return false
        

proc func_call(node: var Node): bool =
    if expr(node):
        if TokenList[index].value == ",":
            #node.add_child(OPERATOR,",")
            index += 1
            return func_call(node)
        return true
    else:
        return false

proc func_params(node: var Node): bool =
    if TokenList[index].tok_type == TOK_ID:
        node.add_child(ID,TokenList[index].value)
        if TokenList[index+1].value == ":":
            #node.add_child(OPERATOR,":")
            if TokenList[index+2].tok_type == TOK_TYPE and TokenList[index+2].value != "kurwa":
                node.add_child(TYPE,TokenList[index+2].value)
                if TokenList[index+3].value == ",":
                    #node.add_child(OPERATOR,",")
                    index += 4
                    return func_params(node)
                index += 3
                return true
    return false
    
proc arr_literals(node : var Node): bool =
    if expr(node):
        if TokenList[index].value == ",":
            inc index
            #node.add_child(OPERATOR,",")
            discard arr_literals(node)
            return true
        elif TokenList[index].value == "}":
            #inc index
            return true
    else:
        return false
            
            

proc arr_init(node : var Node): bool =
    var arr_init = Node(typ: ARR_INIT)
    
    if TokenList[index].value == "{":
        #arr_init.add_child(OPERATOR,"{")
        inc index
        if arr_literals(arr_init):
            # i have no idea what the fuck is going on here
            if not (TokenList[index].tok_type in [TOK_INT_LIT,TOK_STRING_LIT,TOK_CHAR_LIT,TOK_BOOL_LIT,TOK_FLOAT_LIT,TOK_FORMATTED_LIT]) and TokenList[index].value != "}": return false
        else: return false
        
        if TokenList[index].value == "}":
            #arr_init.add_child(OPERATOR,"}")
            node.add_child(arr_init)
            return true
        else:
            return false
    else:
        inc index; return false

proc declaration(node: var Node): bool = 
    var decl = Node(typ: DECLARATION)
    
    if TokenList[index].value == "piwo":
        decl.add_child(PIWO,"")
        if TokenList[index+1].tok_type == TOK_TYPE:
            decl.add_child(TYPE,TokenList[index+1].value)
            if TokenList[index+2].tok_type == TOK_ID:
                index += 2
                decl.add_child(ID,TokenList[index].value)
                # just a check if variable with type kurwa is assigned
                if TokenList[index-1].value == "kurwa":
                    if TokenList[index+1].value == "=":
                        decl.add_child(OPERATOR,"=")
                        index += 2
                        # FIXME dirty workaround
                        let i = index
                        if expr(decl): node.add_child(decl);return true
                        index = i
                        if logic_expr(decl): node.add_child(decl);return true
                        else: return false
                    else: return false
                elif TokenList[index+1].value == "=":
                        decl.add_child(OPERATOR,"=")
                        index += 2
                        let i = index
                        if expr(decl): node.add_child(decl);return true
                        index = i
                        if logic_expr(decl): node.add_child(decl);return true
                        else: return false
                inc index
                node.add_child(decl)
                return true
            else: index += 2; return false
        else:
            inc index;return false
    elif TokenList[index].value == "kufel":
        decl.add_child(KUFEL,"")
        if TokenList[index+1].tok_type == TOK_TYPE:
            let is_kurwa = TokenList[index+1].value == "kurwa"
            decl.add_child(TYPE,TokenList[index+1].value)
            if TokenList[index+2].tok_type == TOK_ID:
                decl.add_child(ID,TokenList[index+2].value)
                index += 2
                if TokenList[index+1].value == "[":
                    #decl.add_child(OPERATOR,"[")
                    index += 2 # check expression inside []
                    if not expr(decl): decl.add_child(INT_LITERAL,"0");inc index
                    if TokenList[index].value == "]":
                            #decl.add_child(OPERATOR,"]")
                            index += 1
                    else: return false
                    
                if is_kurwa and TokenList[index].value != "=": return false # error if kurwa kufel is not assigned
                if TokenList[index].value == "=":
                    index += 1
                    decl.add_child(OPERATOR,"=")
                    if arr_init(decl):
                        inc index
                        node.add_child(decl);return true
                    else:
                        return false
                else:
                    node.add_child(decl);return true
            else:
                index += 2
                return false
        else:
            index += 1
            return false;
        
    elif TokenList[index].value == "wino": # same as piwo with kurwa, just forced assignment
        decl.add_child(WINO,"")
        if TokenList[index+1].tok_type == TOK_TYPE:
            decl.add_child(TYPE,TokenList[index+1].value)
            if TokenList[index+2].tok_type == TOK_ID:
                index += 2
                decl.add_child(ID,TokenList[index].value)
                
                if TokenList[index+1].value == "=":
                        decl.add_child(OPERATOR,"=")
                        index += 2
                        let i = index
                        if expr(decl): node.add_child(decl);return true
                        index = i
                        if logic_expr(node): node.add_child(decl);return true
                        else: return false
                else: index += 1;return false
                
                inc index
                node.add_child(decl)
                return true
            else: index += 2; return false
        else:
            inc index;return false
    else: return false

# There is an error if you leave an indentation behind after statement
proc statement(node: var Node): bool = 
    var stat = Node(typ: STATEMENT)
    let i = index
    if TokenList[index].value in ["jezeli","dopoki"]:
        stat.add_child(if TokenList[index].value == "jezeli": IF else: WHILE,"")
        inc index
        if logic_expr(stat):
            if TokenList[index].value == ":":
                # index + 1 is NEWLINE
                if TokenList[index+2].tok_type == TOK_INDENT:
                    stat.add_child(INDENT,"")
                    index += 3 
                    var linenodes = Node(typ: LINE)
                    while(line(linenodes)):
                        stat.add_child(linenodes)
                        linenodes = Node(typ: LINE) # clear
                        if TokenList[index].tok_type == TOK_DEDENT: break
                    stat.add_child(DEDENT,"")
                    node.add_child(stat)
                    index += 1
                    return true
                else: return false
            else: inc index;return false
        else: return false
    index = i    
    if TokenList[index].value == "dla":
        stat.add_child(FOREACH,"dla")
        if TokenList[index+1].tok_type == TOK_ID:
            stat.add_child(ID,TokenList[index+1].value)
            if TokenList[index+2].value == "->":
                #stat.add_child(OPERATOR,"->")
                if TokenList[index+3].tok_type == TOK_ID:
                    stat.add_child(ID,TokenList[index+3].value)
                    index += 4
                    if TokenList[index].value == ":":
                    # index + 1 is NEWLINE
                        if TokenList[index+2].tok_type == TOK_INDENT:
                            stat.add_child(INDENT,"")
                            index += 3 
                            var linenodes = Node(typ: LINE)
                            while(line(linenodes)):
                                stat.add_child(linenodes)
                                linenodes = Node(typ: LINE) # clear
                                if TokenList[index].tok_type == TOK_DEDENT: break
                            stat.add_child(DEDENT,"")
                            node.add_child(stat)
                            inc index
                            return true
                    else:
                        return false
                    
                else: index += 2; return false
            else: inc index;return false
        else: return false
    if TokenList[index].value == "funkcja":
        stat.add_child(FUNC,"")
        if TokenList[index+1].tok_type == TOK_ID:
            stat.add_child(ID,TokenList[index+1].value)
            if TokenList[index+2].value == "(":
                #stat.add_child(OPERATOR,"(")
                index += 3
                let i = index
                var params = Node(typ: PARAMETERS)
                if not func_params(params): index = i
                else: stat.add_child(params)
                if TokenList[index].value == ")":
                    #stat.add_child(OPERATOR,")")
                    if TokenList[index+1].value == "->":
                        stat.add_child(OPERATOR,"->")
                        if TokenList[index+2].tok_type == TOK_TYPE and TokenList[index+2].value != "kurwa":
                            stat.add_child(TYPE,TokenList[index+2].value)
                            index += 3
                            if TokenList[index].value == ":":
                            # index + 1 is NEWLINE
                                if TokenList[index+2].tok_type == TOK_INDENT:
                                    stat.add_child(INDENT,"")
                                    index += 3 
                                    var linenodes = Node(typ: LINE)
                                    while(line(linenodes)):
                                        stat.add_child(linenodes)
                                        linenodes = Node(typ: LINE) # clear
                                        if TokenList[index].tok_type == TOK_DEDENT: break
                                    stat.add_child(DEDENT,"")
                                    node.add_child(stat)
                                    inc index
                                    return true
                            else: return false
                        else: return false
                    # if not declaring any return type
                    else:
                        index += 1
                        if TokenList[index].value == ":":
                        # index + 1 is NEWLINE
                            if TokenList[index+2].tok_type == TOK_INDENT:
                                stat.add_child(INDENT,"")
                                index += 3 
                                var linenodes = Node(typ: LINE)
                                while(line(linenodes)):
                                    stat.add_child(linenodes)
                                    linenodes = Node(typ: LINE) # clear
                                    if TokenList[index].tok_type == TOK_DEDENT: break
                                stat.add_child(DEDENT,"")
                                node.add_child(stat)
                                inc index
                                return true
        return false
    else: return false
        

proc line(node: var Node): bool =    
    inc debug_line
    let i = index
    if TokenList[index].tok_type == TOK_NEWLINE:
        inc index
        return true
    if assign(node): 
        if TokenList[index].tok_type == TOK_NEWLINE:
            #node.add_child(NEWLINE)
            inc index
            return true
        else:
            return false
    elif declaration(node):
        if TokenList[index].tok_type == TOK_NEWLINE:
            #node.add_child(NEWLINE)
            inc index
            return true
        else:
            return false
    elif statement(node):
        return true
    
    index = i
    if TokenList[index].tok_type == TOK_FLOW:
        if TokenList[index].value == "przerwij": node.add_child(BREAK,"")
        else:                                    node.add_child(CONTINUE,"")
        inc index
        if TokenList[index].tok_type == TOK_NEWLINE:
            #node.add_child(NEWLINE)
            inc index
            return true
        else: return false
        
    elif TokenList[index].value in ["zwroc","podaj","wypisz"]:
        if    TokenList[index].value == "zwroc": node.add_child(RETURN,"")
        elif  TokenList[index].value == "podaj": node.add_child(INPUT,"")
        else: node.add_child(PRINT,"")
        if TokenList[index].value[0] == 'p':
            if TokenList[index+1].tok_type == TOK_ID:
                if TokenList[index+2].value == "[":
                    node.add_child(ARR_ELEMENT,TokenList[index+1].value)
                    index += 3
                    if expr(node):
                        if TokenList[index].value == "]":
                            inc index
                            return true
                        else: return false
                    else: return false
                node.add_child(ID,TokenList[index+1].value)
                index += 2
                return true
            else: return false
        let i = index
        inc index
        
        if expr(node):
            #display_tree(node)
            if TokenList[index].tok_type == TOK_NEWLINE:
                #node.add_child(NEWLINE)
                inc index
                return true
            else: return false
        else:
            if TokenList[i].value == "zwroc":
                index = i+2
                #node.add_child(NEWLINE)
                return true
            return false
    elif TokenList[index].tok_type == TOK_ID:
        if TokenList[index+1].value == "(":
            #node.add_child(OPERATOR,"(")
            
            node.add_child(FUNC_CALL,TokenList[index].value)
            index += 2
            if TokenList[index].value == ")":
                #node.add_child(OPERATOR,")")
                inc index
                if TokenList[index].tok_type == TOK_NEWLINE:
                    #node.add_child(NEWLINE)
                    inc index
                    return true
            var params = Node(typ: PARAMETERS)
            if not func_call(params): return false
            else: node.add_child(params)
            if TokenList[index].value == ")":
                #node.add_child(OPERATOR,")")
                inc index
                if TokenList[index].tok_type == TOK_NEWLINE:
                    #node.add_child(NEWLINE)
                    inc index
                    return true
            else: return false
            
        else: return false
    else:
        return false

proc lines(node: var Node): bool = 
    var lineNode : Node
    lineNode.typ = LINE
    if line(lineNode):
        if lineNode.children.len != 0:
            node.add_child(lineNode)
        if TokenList[index].tok_type == TOK_EOF:
            node.add_child(EOF)
            return true
        return lines(node)
    else:
        return false
        

    
proc parse*(name: string) = 
    lex(name)
    if lines(AST_Root): return
    # echo "----- ERROR -----"
    # echo TokenList[index]
    # echo index
    quit("Parsing error at line number: " & $debug_line & "\nSomething wrong with: \"" & $TokenList[index].value & "\"",69)
    
