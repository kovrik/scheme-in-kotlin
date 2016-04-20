package main.expressions;


import java.util.HashMap;
import java.util.Map;

/* TODO */
// access
// bkpt
// cons-stream
// declare
// default-object?
// define-integrable
// define-macro
// define-structure
// define-syntax
// delay
// do
// fluid-let
// in-package
// lambda
// let-syntax
// letrec
// local-declare
// macro
// make-environment
// named-lambda
// quasiquote
// scode-quote
// sequence
// the-environment
// unassigned?
// using-syntax
public enum Keywords {
  DEFINE("define"),
  QUOTE("quote"),
  IF("if"),
  OR("or"),
  AND("and"),
  NOT("not"),
  BEGIN("begin"),
  NUM_EQUALITY("="),
  EQ("eq?"),
  EQV("eqv?"),
  EQUAL("equal?"),
  COND("cond"),
  ELSE("else"),
  LET("let"),
  LET_SEQ("let*"),
  CASE("case"),
  SET("set!"),
  LAMBDA("lambda"),
  LESS("<"),
  GREATER(">"),
  LESS_EQUAL("<="),
  GREATER_EQUAL(">=")
  ;

  // TODO Split KEYWORDS and FUNCTIONS

  private static final Map<String, Keywords> KEYWORDS_MAP = new HashMap<String, Keywords>(values().length);

  static {
    for (Keywords keyword : values()) {
      KEYWORDS_MAP.put(keyword.getValue(), keyword);
    }
  }

  public String value;

  Keywords(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

  public static Keywords get(String key) {
    return KEYWORDS_MAP.get(key);
  }

  @Override
  public String toString() {
    return value;
  }

}
