package core.evaluator;

public enum ExpansionContext {
  MACROEXPANSION, // TODO Should be a separate thing?
  TOP_LEVEL,
  INTERNAL_DEFINITIONS,
  EXPRESSIONS
}
