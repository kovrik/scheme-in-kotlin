package core.scm;

/**
 * Void class
 *
 * Nil result (null) is a valid result,
 * so we need this VOID class to represent actual void result
 * (same as #<unspecified> in Scheme)
 */
public enum Void implements ITyped {
  VOID {
    @Override public Type getType() { return Type.VOID; }
    @Override public String toString() { return "#<void>"; }
  }
}
