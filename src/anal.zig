const Node = @import("parser.zig").Node;
const std = @import("std");
const NodeType = @import("parser.zig").NodeType;

//TODO
//- Check if ELSE has IF node before it
//- Transform UTF-8 polish letters 
//- Check for correct expressions (if types are correct)
//- Transform ID's to indexes
//- Check for simple Division by 0

pub fn analyze(ast: std.ArrayList(Node)) !void {
    _ = ast;
}


fn is_literal(node: Node) bool {
    return @enumToInt(node.typ) >= @enumToInt(.STRING_LIT) and @enumToInt(node.typ) <= @enumToInt(.ARR_LIT);
}

fn is_var(node: Node) bool {
    return switch(node.typ){
        .ID, .FUNC_CALL, .ARR_ACCESS => true,
        else => false
    };
}

pub const AnalErrors = error{
    IncorrectType
};

pub fn get_expr_type(ast: std.ArrayList(Node), exp: Node) AnalErrors!NodeType {
    if(exp.children.len == 1){
        if(!is_literal(exp) and !is_var(exp)){
            return get_expr_type(ast,ast.items[exp.children.items[0]]);
        }
        else{
            return exp.typ;
        }
    }

    const lhs = ast.items[exp.children.items[0]];
    const rhs = ast.items[exp.children.items[1]];
    var l_type : NodeType = undefined;
    var r_type : NodeType = undefined;

    l_type = switch(lhs.typ){
        .ID => {
                   get_id_type(lhs.value); 
               },
        .FUNC_CALL => {
                   get_func_type(lhs.value);
               },
        .ARR_ACCESS => {
                   get_arr_type(lhs.value);
               },
        else => {
                    if(is_literal(lhs.typ)) { lhs.typ;}
                    else                    {get_expr_type(ast,lhs);}
                }
    };
    
    if({
        return !(@enumToInt(rhs.typ) >= @intToEnum(NodeType,.ADD) and @enumToInt(rhs.typ) <= @intToEnum(NodeType,.LESS_E));
    })
    {
        // Node is not an operand so can work on it
        r_type = switch(rhs.typ){
            .ID => {
                       get_id_type(rhs.value); 
                   },
            .FUNC_CALL => {
                       get_func_type(rhs.value);
                   },
            .ARR_ACCESS => {
                       get_arr_type(rhs.value);
                   },
            else => {
                    if(is_literal(rhs.typ)) { rhs.typ;}
                    else                    {get_expr_type(ast,rhs);}
                  }
        };
    }
    else{
            r_type = get_expr_type(ast,rhs);
    }
    // XXX as you can see i suck at type precedence :3
    switch(exp.typ){ // NOTE shouldn't be EXPR at this point, so we assume its some operator
        .ADD => {
            switch(l_type){
                .INT_LIT, .CHAR_LIT, .BOOL_LIT => {
                    if(r_type == .ARR_LIT) return .IncorrectType;
                    if(r_type == .STRING_LIT){ return .STRING_LIT; }
                    else if(r_type == .FLOAT_LIT){ return .FLOAT_LIT; }
                    else { return l_type; }
                },
                .FLOAT_LIT => {
                    if(r_type == .ARR_LIT) return .IncorrectType;
                    if(r_type == .STRING_LIT){ return .STRING_LIT; }
                    else                     { return .FLOAT_LIT; }
                },
                .STRING_LIT => {
                    if(r_type == .ARR_LIT) return .IncorrectType;
                    return .STRING_LIT;
                },
                .ARR_LIT => {
                    if(r_type == .ARR_LIT) return .ARR_LIT;
                    return .IncorrectType;
                },
                else => { return .IncorrectType; }
            }
        },
        .SUB => {
           switch(l_type){
                .INT_LIT, .CHAR_LIT, .BOOL_LIT => {
                    if(r_type == .ARR_LIT or r_type == .STRING_LIT) return .IncorrectType;
                    if(r_type == .FLOAT_LIT){ return .FLOAT_LIT; }
                    return l_type;
                },
                .FLOAT_LIT => {
                    if(r_type == .ARR_LIT or r_type == .STRING_LIT) return .IncorrectType;
                    return .FLOAT_LIT;
                },
                .STRING_LIT => {
                    if(r_type == .ARR_LIT or r_type == .STRING_LIT) return .IncorrectType;
                    return .STRING_LIT;
                },
                .ARR_LIT => {
                    if(r_type == .INT_LIT or r_type == .CHAR_LIT or r_type == .BOOL_LIT) return .ARR_LIT;
                    return .IncorrectType;
                },
                else => { return .IncorrectType; }
            }
        },
        .MUL => {
            switch(l_type){
                .INT_LIT, .CHAR_LIT, .BOOL_LIT => {
                    if(r_type == .ARR_LIT) return .IncorrectType;
                    if(r_type == .STRING_LIT) return .STRING_LIT;
                    if(r_type == .FLOAT_LIT) return .FLOAT_LIT;
                    return l_type;
                },
                .FLOAT_LIT => {
                    if(r_type == .ARR_LIT or r_type == .STRING_LIT) return .IncorrectType;
                    return .FLOAT_LIT;
                },
                .STRING_LIT => {
                    if(r_type == .ARR_LIT or r_type == .FLOAT_LIT) return .IncorrectType;
                    return .STRING_LIT;
                },
                .ARR_LIT => {
                    if(r_type == .INT_LIT or r_type == .CHAR_LIT or r_type == .BOOL_LIT) return .ARR_LIT;
                    return .IncorrectType;
                }
                ,else => { return .IncorrectType; }
            }
        },
        .DIV => {
           switch(l_type){
                .INT_LIT, .CHAR_LIT, .BOOL_LIT => {
                    if(r_type == .STRING_LIT or r_type == .ARR_LIT) return .IncorrectType;
                    if(r_type == .FLOAT_LIT) return .FLOAT_LIT;
                    return l_type;
                },
                .FLOAT_LIT => {
                    if(r_type == .STRING_LIT or r_type == .ARR_LIT) return .IncorrectType;
                    return .FLOAT_LIT;
                },
                .STRING_LIT => {
                    if(r_type == .FLOAT_LIT or r_type == .ARR_LIT) return .IncorrectType;
                    return .STRING_LIT;
                },
                .ARR_LIT => {
                    if(r_type == .FLOAT_LIT or r_type == .ARR_LIT) return .IncorrectType;
                    return .ARR_LIT;
                }
                ,else => { return .IncorrectType; }
            }
        },
        .MOD => {
           switch(l_type){
                .INT_LIT, .CHAR_LIT, .BOOL_LIT => {
                    if(r_type == .INT_LIT or r_type == .CHAR_LIT or r_type == .BOOL_LIT) return l_type;
                    return .IncorrectType;
                },
                .FLOAT_LIT => {
                    if(r_type == .FLOAT_LIT) return .FLOAT_LIT;
                    return .IncorrectType;
                },
                .STRING_LIT => {
                    if(r_type == .INT_LIT or r_type == .CHAR_LIT or r_type == .BOOL_LIT) return .STRING_LIT;
                    return .IncorrectType;
               },
                .ARR_LIT => {
                    if(r_type == .INT_LIT or r_type == .CHAR_LIT or r_type == .BOOL_LIT) return .ARR_LIT;
                    return .IncorrectType;
                }
                ,else => { return .IncorrectType; }
            }
        },
        .AND => {
           switch(l_type){
               .INT_LIT, .CHAR_LIT, .BOOL_LIT, .FLOAT_LIT, .STRING_LIT, .ARR_LIT => {
                   return .BOOL_LIT;
               }
               ,else => { return .IncorrectType; }
            }
        },
        .OR => {
           switch(l_type){
               .INT_LIT, .CHAR_LIT, .BOOL_LIT, .FLOAT_LIT, .STRING_LIT, .ARR_LIT => {
                   return .BOOL_LIT;
               }
               ,else => { return .IncorrectType; }
            }
        }, // NOTE didnt count for NEG and NOT cause they have one children always, so i dont think they would appear here
        else => {
            @panic("Not implemented or im stupid");
        }
    }
}


fn get_id_type(name:?[]const u8) NodeType {
    _ = name;
    @panic("ID type not implemented");
}
fn get_func_type(name:?[]const u8) NodeType{
    _ = name;
    @panic("Function type not implemented");
}
fn get_arr_type(name:?[]const u8) NodeType{
    _ = name;
    @panic("Array type not implemented");
}
