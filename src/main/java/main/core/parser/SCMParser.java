package main.core.parser;

public class SCMParser {

// <token> ==> <identifier> | <boolean> | <number> | <character> | <string> | ( | ) | #( | ' | `{} | , | , | .

// <delimiter> ==> <whitespace> | ( | ) | " | ;

// <whitespace> ==> <space or newline>

// <comment> ==> ; \= <all subsequent characters up to a line break>

// <atmosphere> ==> <whitespace> | <comment>

// <intertoken space> ==> <atmosphere>*

// <identifier> ==> <initial> <subsequent>* | <peculiar identifier>

// <initial> ==> <letter> | <special initial>

// <letter> ==> a | b | c | ... | z

// <special initial> ==> ! | \$ | \% | \verb"&" | * | / | : | < | = | > | ? | \verb" " | \verb"_" | \verb"^"

// <subsequent> ==> <initial> | <digit> | <special subsequent>

// <digit> ==> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
  public boolean isDigit(char c) {
    return Character.isDigit(c);
  }


// <special subsequent> ==> . | + | -

// <peculiar identifier> ==> + | - | ...

// <syntactic keyword> ==> <expression keyword> | else | => | define | unquote | unquote-splicing

// <expression keyword> ==> quote | lambda | if | set! | begin | cond | and | or | case | let | let* | letrec | do | delay | quasiquote

// <boolean> ==> #t | #f

// <character> ==> #\ <any character> | #\ <character name>

// <character name> ==> space | newline

// <string> ==> " <string element>* "

// <string element> ==> <any character other than " or \> | \" | \\

// <number> ==> <num 2> | <num 8> | <num 10> | <num 16>

//------------------------------------------------------
// Numbers
//------------------------------------------------------

// <num R> ==> <prefix R> <complex R>
//
// <complex R> ==> <real R> | <real R>  <real R> | <real R> + <ureal R> i | <real R> - <ureal R> i | <real R> + i | <real R> - i | + <ureal R> i | - <ureal R> i | + i | - i
//
// <real R> ==> <sign> <ureal R>
//
// <ureal R> ==> <uinteger R> | <uinteger R> / <uinteger R> | <decimal R>
//
// <decimal 10> ==> <uinteger 10> <suffix> | . <digit 10>+ #* <suffix> | <digit 10>+ . <digit 10>* #* <suffix> | <digit 10>+ #+ . #* <suffix>
//
// <uinteger R> ==> <digit R>+ #*
//
// <prefix R> ==> <radix R> <exactness> | <exactness> <radix R>
//
// <suffix> ==> <empty> | <exponent marker> <sign> <digit 10>+
//
// <exponent marker> ==> e | s | f | d | l
//
// <sign> ==> <empty>  | + |  -
//
// <exactness> ==> <empty> | #i | #e
//
// <radix 2> ==> #b
//
// <radix 8> ==> #o
//
// <radix 10> ==> <empty> | #d
//
// <radix 16> ==> #x
//
// <digit 2> ==> 0 | 1
//
// <digit 8> ==> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
//
// <digit 10> ==> <digit>
//
// <digit 16> ==> <digit 10> | a | b | c | d | e | f
}
