

import parser
import math,std/tables,std/os,std/strutils,std/options

if paramCount() < 1: quit("No file provided for compilation",15)

parse(paramStr(1))
when defined(display): display_tree(AST_Root)

type
    Type* = enum
        INT,FLOAT,CHAR,BOOL,STRING
    
    Value* = object of RootObj
        case typ: Type
            of INT: int_value : int64
            of FLOAT: float_value : float64
            of BOOL: bool_value : bool
            of CHAR: char_value : char
            of STRING: string_value : string

var
    scope_stack = newSeq[Table[string,Value]](2)
    arr_stack = newSeq[Table[string,seq[Value]]](2)
    
    const_stack : Table[string,Value]
    func_stack : Table[string,int]
    
    curr_scope = 0
    
    curr_line = 0 # aka current LINE node
    return_reg : Value
    
    returned = false
    printing = false # used to print array but to not use it as param
    control_flow = 0

# ------ Ugly as hell but well, works ----------
proc getFloat(n: Value): float64 =
  case n.typ:
    of FLOAT: n.float_value
    of INT: float64(n.int_value)
    of BOOL: float64(n.bool_value)
    of CHAR: float64(n.char_value)
    of STRING: parseFloat(n.string_value)

proc getInt(n: Value): int64 =
  case n.typ:
    of FLOAT: int64(n.float_value)
    of INT: n.int_value
    of BOOL: int64(n.bool_value)
    of CHAR: int64(n.char_value)
    of STRING: parseInt(n.string_value)

proc getBool(n: Value): bool =
  case n.typ:
    of FLOAT: bool(n.float_value)
    of INT: bool(n.int_value)
    of BOOL: n.bool_value
    of CHAR: n.char_value == '\0'
    of STRING: n.string_value.len == 0

proc getChar(n: Value): char =
  case n.typ:
    of FLOAT: char(n.float_value)
    of INT: char(n.int_value)
    of BOOL: char(n.bool_value)
    of CHAR: n.char_value
    of STRING: n.string_value[0]
    
proc getString(n: Value): string =
  case n.typ:
    of FLOAT: $n.float_value
    of INT: $n.int_value
    of BOOL: $n.bool_value
    of CHAR: $n.char_value
    of STRING: n.string_value
    
proc getValue(a:var Value,b: Value ) = 
    case a.typ:
    of FLOAT: a.float_value = b.getFloat
    of INT: a.int_value = b.getInt
    of BOOL: a.bool_value = b.getBool
    of CHAR: a.char_value = b.getChar
    of STRING: a.string_value = b.getString
    
proc getFromType(n: Value, t: string): Value = 
    case t:
    of "int": return Value(typ:INT,int_value: n.getInt)
    of "float": return Value(typ:FLOAT,float_value: n.getFloat)
    of "string": return Value(typ:STRING,string_value: n.getString)
    of "bool": return Value(typ:BOOL,bool_value: n.getBool)
    of "char": return Value(typ:CHAR,char_value: n.getChar)
    of "kurwa": return n

proc getFromEnum(n: Value, t: Type): Value = 
    case t:
    of INT: return Value(typ:INT,int_value: n.getInt)
    of FLOAT: return Value(typ:FLOAT,float_value: n.getFloat)
    of STRING: return Value(typ:STRING,string_value: n.getString)
    of BOOL: return Value(typ:BOOL,bool_value: n.getBool)
    of CHAR: return Value(typ:CHAR,char_value: n.getChar)

proc getDefault(t:string) : Value = 
    case t:
    of "int":   result = Value(typ: INT, int_value:0)
    of "char":  result = Value(typ: CHAR, char_value:'a')
    of "string":result = Value(typ: STRING, string_value:"")
    of "float": result = Value(typ: FLOAT, float_value:0.0)
    of "bool":  result = Value(typ: BOOL, bool_value: false)
    
proc getVariable(name: string, scope: int): Value =
    for i in countdown(scope,0): 
        if scope_stack[i].hasKey(name): return scope_stack[i][name]
    if const_stack.hasKey(name): return const_stack[name]
    quit("Trying to access variable that doesn't exist",10)
        
proc getArrayString(a: seq[Value]): Value = 
    var res :string = "["
    for e in a:
        res &= e.getString & ", "
    res[res.len - 2] = ']'
    res[res.len - 1] = '\0'
    return Value(typ:STRING,string_value: res)

# ===========================================
proc eval_expr(node: Node): Value
proc interpret_func(line : int, params: Option[seq[Value]]): Option[Value] 
proc interpret_line(node: Node):bool


proc eval_fact(node: Node): Value = 
    if node.children.len == 1: # return the literal, or the value from ID/Function
        var n = node.children[0]
        
        if n.typ == ID: # no params ID call
            if printing:
                for i in countdown(curr_scope,0): 
                    if arr_stack[i].hasKey(node.children[0].value):
                        printing = false
                        return getArrayString(arr_stack[i][node.children[0].value])
            return getVariable(node.children[0].value,curr_scope)
 
        if n.typ == FUNC_CALL: # function call without any parameters
            if func_stack.hasKey(n.value):
                let res = interpret_func(func_stack[n.value],none(seq[Value]))
                if res.isSome: returned = false;return res.get()
                else: quit("Function doesn't return anything",1)
            quit("Trying to access function that doesn't exist",10)
            

        if n.typ == EXPR: return eval_expr(node.children[0])
        
        if n.typ >= STRING_LITERAL and n.typ <= FLOAT_LITERAL: # if its literal, return literal
            case n.typ:
                of INT_LITERAL:   return Value(typ: INT,int_value: parseBiggestInt(n.value))
                of FLOAT_LITERAL: return Value(typ: FLOAT,float_value: parseFloat(n.value))
                of BOOL_LITERAL:  return Value(typ: BOOL,bool_value: n.value[0] == 't')
                of CHAR_LITERAL:  return Value(typ: CHAR,char_value: n.value[0])                 
                of STRING_LITERAL:return Value(typ: STRING,string_value: n.value)
                else:
                    # Formatted String, needs to be implemented
                    return Value(typ:INT,int_value: -1)
    
    elif node.children[0].typ == ARR_ELEMENT: # Element from array
        if const_stack.hasKey(node.children[0].value):
            if const_stack[node.children[0].value].typ == STRING:
                let idx = eval_expr(node.children[1]).getInt.abs
                if idx < const_stack[node.children[0].value].string_value.len: return Value(typ:CHAR, char_value: const_stack[node.children[0].value].string_value[idx])
                else:                                              quit("Trying to access index outside of string")
        for i in countdown(curr_scope,0): 
            if scope_stack[i].hasKey(node.children[0].value):
                if scope_stack[i][node.children[0].value].typ == STRING:
                    let idx = eval_expr(node.children[1]).getInt.abs
                    if idx < scope_stack[i][node.children[0].value].string_value.len: return Value(typ:CHAR, char_value: scope_stack[i][node.children[0].value].string_value[idx])
                    else:                                              quit("Trying to access index outside of string")
                else: quit("Trying to get element from non string variable")
        for i in countdown(curr_scope,0): 
            if arr_stack[i].hasKey(node.children[0].value):
                let idx = eval_expr(node.children[1]).getInt.abs
                if idx < arr_stack[i][node.children[0].value].len: return arr_stack[i][node.children[0].value][idx]
                else:                                              quit("Trying to access index outside of array")
                    
        quit("Trying to access array that doesn't exist")
    elif node.children[0].typ == FUNC_CALL: # Function call with parameters
        let n = node.children[0]
        if func_stack.hasKey(n.value):
            let p = node.children[1]
            var params = newSeqOfCap[Value](p.children.len)
            
            for e in p.children: params.add(eval_expr(e))
            
            let res = interpret_func(func_stack[n.value],some(params))
            
            if res.isSome: returned = false;return res.get()
            else: quit("Function doesn't return anything",1)
        quit("Trying to access function that doesn't exist",10)

    elif node.children[0].typ == METHOD_CALL: # method call
        return Value(typ:INT,int_value: -1)

proc eval_term(node: Node): Value = 
    if node.children.len == 1: return eval_fact(node.children[0])
    let n1 = eval_fact(node.children[0])
    let n2 = eval_fact(node.children[2])
    
    if n1.typ in [STRING,CHAR] or n2.typ in [STRING,CHAR]:
        quit("Can't use ( / , * , % ) operator on string",10)
    else:
        let op = node.children[1].typ
        if   op == DIV:
            if n1.typ == FLOAT or n2.typ == FLOAT:
                let n2_val = n2.getFloat
                let n1_val = n1.getFloat
                if n2_val == 0: quit("Can't divide by 0",1)
                return Value(typ:FLOAT,float_value: n1_val / n2_val)
            let n1_val = n1.getInt
            let n2_val = n2.getInt
            if n2_val == 0: quit("Can't divide by 0",1)
            return Value(typ:INT,int_value: n1_val div n2_val)
        elif op == MOD:
            if n1.typ == FLOAT or n2.typ == FLOAT:
                let n2_val = n2.getFloat
                let n1_val = n1.getFloat
                return Value(typ:FLOAT,float_value: n1_val mod n2_val)
            let n1_val = n1.getInt
            let n2_val = n2.getInt
            return Value(typ:INT,int_value: n1_val mod n2_val)
        else: # MUL
            if n1.typ == FLOAT or n2.typ == FLOAT:
                let n2_val = n2.getFloat
                let n1_val = n1.getFloat
                return Value(typ:FLOAT,float_value: n1_val * n2_val)
            let n1_val = n1.getInt
            let n2_val = n2.getInt
            return Value(typ:INT,int_value: n1_val * n2_val)
            
proc eval_expr(node: Node): Value = 
    if node.children.len == 1: return eval_term(node.children[0])
    let op = node.children[1].typ
    
    let n1 = eval_term(node.children[0])
    let n2 = eval_term(node.children[2])
    
    if op == ADD:
        if n1.typ in [STRING,CHAR] or n2.typ in [STRING,CHAR]:
            let n1_val = n1.getString
            let n2_val = n2.getString
            return Value(typ:STRING,string_value: n1_val & n2_val)
        elif n1.typ == FLOAT or n2.typ == FLOAT:
            let n1_val = n1.getFloat
            let n2_val = n2.getFloat
            return Value(typ:FLOAT,float_value: n1_val + n2_val)
        else:
            let n1_val = n1.getInt
            let n2_val = n2.getInt
            return Value(typ:INT,int_value: n1_val + n2_val)
    else: # SUB
        if n1.typ in [STRING,CHAR] or n2.typ in [STRING,CHAR]:
            quit("Can't substract from string",2)
        if n1.typ == FLOAT or n2.typ == FLOAT:
            let n1_val = n1.getFloat
            let n2_val = n2.getFloat
            return Value(typ:FLOAT,float_value: n1_val - n2_val)
        else:
            let n1_val = n1.getInt
            let n2_val = n2.getInt
            return Value(typ:INT,int_value: n1_val - n2_val)

proc eval_logic_expr(node: Node): Value =
    if node.children.len == 1: return eval_expr(node.children[0])
    let 
        n1 = eval_expr(node.children[0])
        op = node.children[1].typ
        n2 = eval_logic_expr(node.children[2])
    case op:
    of EQUAL:     return Value(typ:BOOL, bool_value: n1.getString == n2.getString) # FIXME, make it better moron, don't fucking compare two strings
    of NOT_EQUAL: return Value(typ:BOOL, bool_value: n1.getString != n2.getString)
    of BIGGER:
        if n1.typ == STRING:
            # compare sizes
            if n2.typ == STRING: return Value(typ:BOOL, bool_value: n1.string_value.len > n2.string_value.len)
            elif n2.typ in [INT,BOOL,FLOAT]:  return Value(typ:BOOL, bool_value: n1.string_value.len > n2.getInt)
        if n2.typ == STRING: return Value(typ:BOOL, bool_value: n1.getInt > n2.string_value.len)
        else: return Value(typ:BOOL, bool_value: n1.getFloat > n2.getFloat)
    of SMALLER:
        if n1.typ == STRING:
            if   n2.typ == STRING: return Value(typ:BOOL, bool_value: n1.string_value.len < n2.string_value.len)
            elif n2.typ in [INT,BOOL,FLOAT]:  return Value(typ:BOOL, bool_value: n1.string_value.len < n2.getInt)
        if n2.typ == STRING: return Value(typ:BOOL, bool_value: n1.getInt < n2.string_value.len)
        else: return Value(typ:BOOL, bool_value: n1.getFloat < n2.getFloat)
    of EQ_BIGGER:
        if n1.typ == STRING:
            if   n2.typ == STRING: return Value(typ:BOOL, bool_value: n1.string_value.len >= n2.string_value.len)
            elif n2.typ in [INT,BOOL,FLOAT]:  return Value(typ:BOOL, bool_value: n1.string_value.len >= n2.getInt)
        if n2.typ == STRING: return Value(typ:BOOL, bool_value: n1.getInt >= n2.string_value.len)
        else: return Value(typ:BOOL, bool_value: n1.getFloat >= n2.getFloat)
    of EQ_SMALLER:
        if n1.typ == STRING:
            if   n2.typ == STRING: return Value(typ:BOOL, bool_value: n1.string_value.len <= n2.string_value.len)
            elif n2.typ in [INT,BOOL,FLOAT]:  return Value(typ:BOOL, bool_value: n1.string_value.len <= n2.getInt)
        if n2.typ == STRING: return Value(typ:BOOL, bool_value: n1.getInt <= n2.string_value.len)
        else: return Value(typ:BOOL, bool_value: n1.getFloat <= n2.getFloat)
    
    of AND: return Value(typ:BOOL, bool_value: n1.getBool and n2.getBool)
    of OR:  return Value(typ:BOOL, bool_value: n1.getBool or n2.getBool)
    
    else: return Value(typ:BOOL, bool_value: false)

# Without parameters  
proc interpret_func(line : int, params: Option[seq[Value]]): Option[Value] = 
    let func_node = AST_Root.children[line].children[0]
    var func_line = if params.isSome: 4 else: 3
    inc curr_scope
    scope_stack.add(Table[string,Value]()) # create new scope
    arr_stack.add(Table[string,seq[Value]]())
    if params.isSome:
        if params.get.len != func_node.children[2].children.len div 2: quit("Wrong number of parameters given to function")
        for i in countup(0,func_node.children[2].children.len div 2 - 1):
            scope_stack[curr_scope][func_node.children[2].children[i*2].value] = getFromType(params.get()[i], func_node.children[2].children[i * 2 + 1].value)
    else:
        if func_node.children[2].typ == PARAMETERS: quit("No parameters given to function that requires them")
        
    while(func_node.children[func_line].typ != DEDENT):
        if not interpret_line(func_node.children[func_line]): echo "something went wrong";break
        if returned:
            scope_stack.del(curr_scope) # delete scope after
            dec curr_scope
            returned = false ;return some(return_reg)
        inc func_line
    returned = false
    scope_stack.del(curr_scope) # delete scope after
    arr_stack.del(curr_scope) 
    dec curr_scope
    return none(Value) # finished function without returning anything


proc interpret_line(node: Node):bool = 
    let n1 = node.children[0]
    case n1.typ:
    of BREAK:
        control_flow = 1;return true
    of CONTINUE:
        control_flow = 2;return true
    of PRINT: # Print expression
        printing = true
        stdout.write(eval_expr(node.children[1]).getString)
        printing = false # to be sure
        return true
    of INPUT:
        for i in countdown(curr_scope,0): 
            if scope_stack[i].hasKey(node.children[1].value):
                scope_stack[i][node.children[1].value] = getFromEnum(Value(typ:STRING,string_value: readLine(stdin)),scope_stack[i][node.children[1].value].typ)
                return true
        quit("Trying to assing input to variable that doesn't exist")
    of DECLARATION:
        let d = node.children[0]
        if d.children[0].typ == PIWO:
            if scope_stack[curr_scope].hasKey(d.children[2].value): quit("You can't declare same variable more than once",3)
                
            let t = d.children[1].value
            if t == "kurwa": scope_stack[curr_scope][d.children[2].value] = eval_expr(d.children[4])
            else:
                var value : Value
                if d.children.len == 3: # if not assigned, use default 
                    scope_stack[curr_scope][d.children[2].value] = getDefault(t)
                    return true
                
                case t:
                of "int": value = Value(typ: INT, int_value: eval_expr(d.children[4]).getInt)
                of "char": value = Value(typ: CHAR, char_value: eval_expr(d.children[4]).getChar)
                of "string": value = Value(typ: STRING, string_value: eval_expr(d.children[4]).getString)
                of "float": value = Value(typ: FLOAT, float_value: eval_expr(d.children[4]).getFloat)
                of "bool": value = Value(typ: BOOL, bool_value: eval_expr(d.children[4]).getBool)
                scope_stack[curr_scope][d.children[2].value] = value
                return true
            return true
        
        elif d.children[0].typ == KUFEL: # Kufel i Wino
            if arr_stack[curr_scope].hasKey(d.children[2].value): quit("You can't declare same array more than once",3)
            
            if d.children[3].value == "0": # no size of array provided
                let arr_init = d.children[5]
                var tab = newSeqOfCap[Value](arr_init.children.len)
                for i in 0 ..< arr_init.children.len:
                    tab.add(getFromType(eval_expr(arr_init.children[i]), d.children[1].value))
                arr_stack[curr_scope][d.children[2].value] = tab
            
            else:
                let size = eval_expr(d.children[3])
                if d.children.len == 4: # no initialization
                    arr_stack[curr_scope][d.children[2].value] = newSeq[Value](size.getInt)
                else:
                    let arr_init = d.children[5]
                    if arr_init.children.len == 1:
                        let value = eval_expr(arr_init.children[0])
                        var tab = newSeqOfCap[Value](size.getInt)
                        for i in 0 ..< size.getInt:
                            tab.add(value)
                        arr_stack[curr_scope][d.children[2].value] = tab
                        return true
                    if arr_init.children.len != size.getInt: quit("Size of array doesn't match initalized values")
                    var tab = newSeqOfCap[Value](size.getInt)
                    for i in 0 ..< size.getInt:
                        tab.add(getFromType(eval_expr(arr_init.children[i]), d.children[1].value))
                    arr_stack[curr_scope][d.children[2].value] = tab
            return true
        else:
            if const_stack.hasKey(d.children[2].value): quit("You can't declare same const more than once",3)
                
            let t = d.children[1].value
            if t == "kurwa": const_stack[d.children[2].value] = eval_expr(d.children[4])
            else:
                var value : Value
                case t:
                of "int": value = Value(typ: INT, int_value: eval_expr(d.children[4]).getInt)
                of "char": value = Value(typ: CHAR, char_value: eval_expr(d.children[4]).getChar)
                of "string": value = Value(typ: STRING, string_value: eval_expr(d.children[4]).getString)
                of "float": value = Value(typ: FLOAT, float_value: eval_expr(d.children[4]).getFloat)
                of "bool": value = Value(typ: BOOL, bool_value: eval_expr(d.children[4]).getBool)
                const_stack[d.children[2].value] = value
                return true
            return true
    of ASSIGN:
        let a = node.children[0]
        if a.children[0].typ == ID:
            for i in countdown(curr_scope,0): 
                if scope_stack[i].hasKey(a.children[0].value):
                    scope_stack[i][a.children[0].value].getValue(eval_expr(a.children[1]))
                    return true
            if const_stack.hasKey(a.children[0].value): quit("You can't assign to const")
            quit("You can't assign to not declared variable",4)
        elif a.children[0].typ == ARR_ELEMENT: # assigning to ARR_ELEMENT or something
            for i in countdown(curr_scope,0):
                if scope_stack[i].hasKey(a.children[0].value):
                    if scope_stack[i][a.children[0].value].typ == STRING:
                        let idx = eval_expr(a.children[1]).getInt.abs
                        if idx < scope_stack[i][a.children[0].value].string_value.len:
                            scope_stack[i][a.children[0].value].string_value[idx] = eval_expr(a.children[2]).getString[0]
                        else:        quit("Trying to assign index outside of string")        
                    else: quit("Trying to assing to element of not string")
                    return true
            for i in countdown(curr_scope,0): 
                if arr_stack[i].hasKey(a.children[0].value):
                    let idx = eval_expr(a.children[1]).getInt.abs
                    if idx < arr_stack[i][a.children[0].value].len:
                        arr_stack[i][a.children[0].value][idx] = eval_expr(a.children[2])
                    else:        quit("Trying to assign index outside of array")        
                    return true
        else:
            discard
    of STATEMENT:
        let stat = node.children[0]
        if stat.children[0].typ == WHILE:
            let start_line = 3
            var loop_line = start_line
            inc curr_scope
            scope_stack.add(Table[string,Value]()) # create new scope
            arr_stack.add(Table[string,seq[Value]]())
            if eval_logic_expr(stat.children[1]).getBool:
                while true:
                        if not interpret_line(stat.children[loop_line]):
                            echo "something went wrong";return false
                        if stat.children[loop_line].children[0].typ == RETURN and returned: break
                        if control_flow == 1: control_flow = 0;break
                        inc loop_line
                        if stat.children[loop_line].typ == DEDENT or control_flow == 2:
                            control_flow = 0
                            if not eval_logic_expr(stat.children[1]).getBool: break
                            loop_line = start_line
                            scope_stack[curr_scope].clear
                            arr_stack[curr_scope].clear
            scope_stack.del(curr_scope) # delete scope after
            arr_stack.del(curr_scope)
            dec curr_scope

        elif stat.children[0].typ == IF:
            var loop_line = 3
            inc curr_scope
            scope_stack.add(Table[string,Value]()) # create new scope
            arr_stack.add(Table[string,seq[Value]]())
            if eval_logic_expr(stat.children[1]).getBool:
                while true:
                        if not interpret_line(stat.children[loop_line]):
                            echo "something went wrong";return false
                        if stat.children[loop_line].children[0].typ == RETURN and returned: break
                        if control_flow == 1: control_flow = 0;break
                        inc loop_line
                        if stat.children[loop_line].typ == DEDENT: break
            scope_stack.del(curr_scope) # delete scope after
            arr_stack.del(curr_scope)
            dec curr_scope

        elif stat.children[0].typ == FUNC:
            if func_stack.hasKey(stat.children[1].value): quit("I don't support function overloading yet sorry",40)
            if curr_scope != 0: quit("You can't declare functions inside of a scope",67)
            func_stack[stat.children[1].value] = curr_line
        
        elif stat.children[0].typ == FOREACH:
            for i in countdown(curr_scope,0):
                let arr : bool = arr_stack[i].hasKey(stat.children[1].value)
                var str : bool = scope_stack[i].hasKey(stat.children[1].value)
                if str: str = str and scope_stack[i][stat.children[1].value].typ == STRING
                
                if arr or str:
                    var size:int
                    if arr: size = arr_stack[i][stat.children[1].value].len
                    else:   size = scope_stack[i][stat.children[1].value].string_value.len
                    if size == 0: return true
                    
                    let start_line = 4
                    var
                        loop_line = start_line
                        index = 0
                    inc curr_scope
                    scope_stack.add(Table[string,Value]()) # create new scope
                    arr_stack.add(Table[string,seq[Value]]())
                    while index < size:
                        if arr: scope_stack[curr_scope][stat.children[2].value] = arr_stack[i][stat.children[1].value][index]
                        else:   scope_stack[curr_scope][stat.children[2].value] = Value(typ:CHAR, char_value: scope_stack[i][stat.children[1].value].string_value[index])
                        if not interpret_line(stat.children[loop_line]):
                            echo "something went wrong";return false
                        if stat.children[loop_line].children[0].typ == RETURN and returned: break
                        if control_flow == 1: control_flow = 0;break
                        inc loop_line
                        inc index
                        if stat.children[loop_line].typ == DEDENT or control_flow == 2:
                            control_flow = 0
                            loop_line = start_line
                            scope_stack[curr_scope].clear
                            arr_stack[curr_scope].clear
                    scope_stack.del(curr_scope) # delete scope after
                    arr_stack.del(curr_scope)
                    dec curr_scope
                    return true

            quit("Can't iterate over array " & stat.children[1].value)
        return true
    of RETURN:
        return_reg = eval_expr(node.children[1])
        returned = true
        return true
        
    of FUNC_CALL:
        if func_stack.hasKey(node.children[0].value):
            if node.children.len == 1: discard interpret_func(func_stack[node.children[0].value],none(seq[Value]))
            else: 
                let p = node.children[1]
                var params = newSeqOfCap[Value](p.children.len)
                for e in p.children: params.add(eval_expr(e))

                discard interpret_func(func_stack[node.children[0].value],some(params))            
        else: quit("Trying to access function that doesn't exist",10)
        return true
    else:
        return true
    
    return false

while(AST_Root.children[curr_line].typ != EOF):
    if not interpret_line(AST_Root.children[curr_line]):
        echo "something went wrong"
        break
    inc curr_line
    
# ---------- Runtime functions ----------


