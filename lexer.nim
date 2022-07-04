import os,strutils

# FIXME C             doing something like
# do i want to fix it?
# c[] <indent> abc
# will ignore the indentation 

# TODO try using "try" keyword dumbass
# FIXME chars don't work

type
    Token_type* = enum
        TOK_EOF,UNKNOWN,TOK_WRONG,TOK_INDENT,TOK_DEDENT,TOK_NEWLINE,
        TOK_STRING_LIT,TOK_FORMATTED_LIT,TOK_INT_LIT,TOK_CHAR_LIT,TOK_BOOL_LIT,TOK_FLOAT_LIT,TOK_CLIT
        TOK_DECL,TOK_TYPE,TOK_LOOP,TOK_FLOW,TOK_IO,TOK_COND,TOK_RETURN,TOK_FUNC,TOK_ID,
        TOK_AND,TOK_OR,
        TOK_OP, TOK_MULTIOP
        TOK_STUFF
    Lex_state* = enum
        NORMAL,COMMENT,STRING,FORMATTED
        
    Token* = tuple[tok_type :Token_type,value : string]
    
const ops = "+-=*/%><^()[]{},.:\'!#"
const multi_ops = ["==","!=","+=","-=","/=","*=",">=","<=","->"]

const decl_toks = ["piwo","kufel","wino"]
const type_toks = ["int","float","char","bool","string","kurwa"]
const loop_toks = ["dopoki","dla","dopóki"]
const flow_toks = ["dalej","przerwij"]
const io_toks   = ["podaj", "wypisz"]
const cond_toks = ["jezeli","jeżeli"]
const func_toks = ["funkcja","zwroc","zwróć"]

const logic_keywords = ["albo","oraz"]

var    
    TokenList* : seq[Token]
    
    tok_idx   = 0
    
    state = NORMAL
    
    curr_indent = 0
    prev_indent = 0
    # For iterating over characters
    line_iter   = 0
    
    # Helpers
    space_indent  = 0
    string_tok    = ""
    starttok      = 0
    getting_token = false
    skip_line     = false


proc concat_string_tok(tok : var string, val : string) = tok &= multiReplace(val,("\\n","\n"),("\\t","\t"),("\\\\","\\"))

proc unpolish_string(s : string): string= 
    if s.len == 0: return ""
    var i = 0
    while i <= s.high:
        # uuugh fucking unicode
        if s[i].ord == 195:
            if s[i+1].ord == 179: result.add('o'); i += 2
        elif s[i].ord == 196:
            if s[i+1].ord == 135: result.add('c'); i += 2
        elif s[i].ord == 197:
            if s[i+1].ord == 188: result.add('z'); i += 2
        else: result.add(s[i]); inc i

proc get_type(v : string) : Token_type = 
    if v.len <= 2:
        if   v in ops:         return TOK_OP
        elif v in multi_ops:   return TOK_MULTIOP
    result = TOK_INT_LIT 
    if isDigit(v[0]) or (v[0] == '-' and isDigit(v[1])):                            # TODO Refactor this bitch, maybe some converter
        for c in (v.low + int(v[0] == '-' and isDigit(v[1]) )) .. v.high: 
            if not isDigit(v[c]) and v[c] != '.': quit("Incorrect number: " & v, 8) # checking for correct number or float
            if v[c] == '.': result = TOK_FLOAT_LIT
    else:
        # TODO nah, need to refactor it lol, but if works leave it as it is
        if v in ["tak", "nie"]: result = TOK_BOOL_LIT
        elif v in logic_keywords: result = if v == "albo": TOK_OR else: TOK_AND
        else:
            if v in decl_toks:   result = TOK_DECL
            elif v in type_toks: result = TOK_TYPE
            elif v in loop_toks: result = TOK_LOOP
            elif v in flow_toks: result = TOK_FLOW
            elif v in io_toks:   result = TOK_IO
            elif v in cond_toks: result = TOK_COND
            elif v in func_toks: result = if v == "funkcja": TOK_FUNC else: TOK_RETURN
            else: result = TOK_ID
         

proc add_token(value: string = "", typ : Token_type = UNKNOWN) = 
    var token: Token
    if typ == UNKNOWN: token.tok_type = get_type(value)
    else:              token.tok_type = typ
    
    if value in ["jeżeli","dopóki","zwróć"]: token.value = unpolish_string(value)
    else:                                    token.value = value
    TokenList.add(token)

proc lex*(filename : string) = 
    var f : File
    if not f.open(filename,fmRead):
        quit("Couldn't open file: " & paramStr(1), 1)
        
    for line in f.lines:
        let line_len = line.len
        
        curr_indent  = 0
        space_indent = 0
        line_iter    = 0
        starttok     = 0
        if line_len == 0: continue
        
        # ----------------------------- MULTILINE VALUES --------------------------------------
        if state == COMMENT:
            while line_iter <= line_len-1:
                if line[line_iter] == ']':
                    state = NORMAL
                    skip_line = true
                    break
                inc line_iter
        
        if state == COMMENT: continue
        
        if state == STRING:
            while line_iter <= line_len-1:
                if line[line_iter] == '\"':
                    concat_string_tok(string_tok, line[0 ..< line_iter])
                    add_token(string_tok,TOK_STRING_LIT)
                    string_tok = ""
                    state = NORMAL
                    skip_line = true
                    add_token(typ = TOK_NEWLINE)
                    break
                inc line_iter
        
        if state == STRING: concat_string_tok(string_tok, line); continue
        
        if state == FORMATTED:
            while line_iter <= line_len-1:
                if line[line_iter] == '\"':
                    concat_string_tok(string_tok, line[0 ..< line_iter])
                    add_token(string_tok,TOK_FORMATTED_LIT)
                    string_tok = ""
                    state = NORMAL
                    skip_line = true
                    # add_token(typ = TOK_NEWLINE)
                    break
                inc line_iter
        
        if state == FORMATTED: concat_string_tok(string_tok, line); continue
        
        if skip_line: skip_line = false; continue
        # ---------------------------------------------------------------------------------------------
        
        # Starting counting indentation in line to return INDENT or DEDENT token
        # break when encountering anything other than tab or space or reaching end of line
        while line_iter < line_len:
            if   space_indent == 4:        inc curr_indent; space_indent = 0 
            if   line[line_iter] == '\t' : inc curr_indent
            elif line[line_iter] == ' ':   inc space_indent
            else: break
            inc line_iter
        
        # If new indentation is bigger, we should return INDENT token and change previous indentation
        if curr_indent > prev_indent:
            # echo indent_op
            add_token(typ = TOK_INDENT)
            prev_indent = curr_indent
            
        # same goes here, but return DEDENT token and also do it multiple times if we fallback multiple scopes
        elif curr_indent < prev_indent:
            for i in 1 .. prev_indent - curr_indent:
                add_token(typ = TOK_DEDENT)
            prev_indent = curr_indent
        
        # If found beginning of comment, go until ] is found, if reached end of line and no ] to be seen, go to next line and do same
        starttok = line_iter
        while line_iter < line_len:
            # ------------------------------ Values contained inside one line -----------------------------------
            if state == STRING:
                if line[line_iter] == '\"':
                    concat_string_tok(string_tok,line[starttok ..< line_iter])
                    add_token(string_tok,TOK_STRING_LIT)
                    state = NORMAL
                inc line_iter
                if line_iter == line_len and string_tok.len == 0: concat_string_tok(string_tok,line[starttok ..< line_iter])
                continue
                    
            elif state == COMMENT:
                if line[line_iter] == ']': state = NORMAL; starttok = line_iter
                inc line_iter
                continue
            
            elif state == FORMATTED:
                if line[line_iter] == '\"':
                    concat_string_tok(string_tok, line[start_tok ..< line_iter])
                    add_token(string_tok,TOK_FORMATTED_LIT)
                    state = NORMAL
                inc line_iter
                if line_iter == line_len and string_tok.len == 0: concat_string_tok(string_tok,line[starttok ..< line_iter])
                continue
            
            # ----------------------------------------------------------------------------------------------------    
                
            if line[line_iter] == '\"':
                state = STRING; starttok = line_iter+1;string_tok = ""
            elif line[line_iter] == 'c' and line[if line_iter == line_len-1: 0 else: line_iter+1] == '[':
                state = COMMENT
                
            elif line[line_iter] in "\t ":
                if getting_token:
                    add_token(line[starttok ..< line_iter])
                    getting_token = false
                inc line_iter
                starttok = line_iter
                continue
            
            else:
                if line[line_iter] in ops:
                    #found char literal
                    if line[line_iter] == '\'':
                        if line[line_iter+2] == '\'' or line[line_iter + 3] == '\'': # check if its correctly closed
                            if line[line_iter+1] == '\\': # if it has \ then check for next character
                                    if line[line_iter+2]   == 'n': add_token("\n",TOK_CHAR_LIT)
                                    elif line[line_iter+2] == 't': add_token("\t",TOK_CHAR_LIT)
                                    elif line[line_iter+2] == '\\':add_token("\\",TOK_CHAR_LIT)
                                    elif line[line_iter+2] == '\'':add_token("\'",TOK_CHAR_LIT)
                                    else :quit("No special char here, not good :" & line[line_iter .. line_iter + 3],9)
                                    line_iter += 4
                                    starttok  += 4
                        
                            else:
                                add_token($line[line_iter+1],TOK_CHAR_LIT)
                                line_iter += 3
                                starttok  += 3
                        else:quit("Incorrect character value",10)
                    #found formatted string
                    if line[line_iter] == '#' and line[if line_iter == line_len-1: 0 else: line_iter+1] == '\"': # Formatted string
                        state = FORMATTED
                        line_iter += 1; start_tok = line_iter + 1  
                        string_tok = ""                  
                    # found float  
                    elif isDigit(line[if line_iter == line_len-1: 0 else: line_iter+1]) and ( line[line_iter] == '.' or line[line_iter] == '-' ): discard
                    #found some operator
                    else:
                        if getting_token:    add_token(line[starttok ..< line_iter])
                        getting_token = false
                    
                        if line[line_iter .. (if line_iter == line_len-1: line_iter-1 else: line_iter+1)] in multi_ops:
                            add_token(line[line_iter .. line_iter + 1])
                            starttok = line_iter + 2; inc line_iter
                        else:
                            add_token($line[line_iter])
                            starttok = line_iter+1
                else:
                    getting_token = true
            inc line_iter
        if getting_token: add_token(line[starttok ..< line_iter]); getting_token = false
        add_token(typ = TOK_NEWLINE)
    
    
    f.close()
    if state != NORMAL:
        echo state
        quit("Bruuuuuuuuuuuuuuuuuuuuuuh, you really didn't closed " & (if state == STRING: "string" else: "comment"),(if state == STRING: 2 else: 3))
    
    curr_indent = 0
    for i in 1 .. prev_indent - curr_indent:
        add_token(typ = TOK_DEDENT)
    add_token(typ = TOK_EOF)
