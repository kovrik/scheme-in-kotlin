package core.scm;

/**
 * Nil constant
 * Evaluates to Java's null
 */
public enum SCMNil implements ISCMClass {
  NIL;

  @Override
  public SCMClass getSCMClass() { return SCMClass.NIL; }

  @Override
  public String toString() { return "nil"; }
}
