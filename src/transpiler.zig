//TODO
//- Types that can stay like they are in C: bool, int_64t, double, char(?)
//- String is just array type with char (have a function for going from UTF-8 to ASCII and when printing out, have function for that too) 
//- Have an Array type, that dynamically changes size and/or can have it predetermined
//- Use functions for expressions? (a + b becomes add(a,b))
//- Might still want to use _Generic for printing and shit, but should do in code that expressions as functions (up there ^) are fitted to types
//  so double + double is add_dd(double,double) and string + int is add_si(string,int), return type is based on precedence of given type
//  so if we add int and double, we should get a double, if we add anything to string, we will get string (concatenation) etc.
//  Writer those functions in runtime
//- Use GC (maybe this one https://github.com/mkirchner/gc) but for speed and shit, then use Boehm GC
//  or this one actually https://github.com/orangeduck/tgc
// - Math:
// - Ints are the same
// - Any calculation with float and int turns output into float
// - Any calculation with char with other than chan turns it into the other one
// - can't really calculate anything with bool, but 0 is false, !0 is true
// - String is just Array of chars
// - adding string concatenates to it (except for bool)
// - substracting int from string shrinks it ("abcd" - 2 = "abc")
// - multiplying copies string ("a" * 3 = "aaa")
// - dividing shrinks from start ("abcd" / 2 = "cd")
// - modulo works like a formatter for string ("I love {}" % ["Piwoscript"])