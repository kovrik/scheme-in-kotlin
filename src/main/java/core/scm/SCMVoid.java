package core.scm;

/**
 * Void class
 *
 * Nil result (null) is a valid result,
 * so we need this VOID class to represent actual void result
 * (same as #<unspecified> in Scheme)
 */
public enum  SCMVoid implements ISCMClass {
  VOID {
    @Override
    public SCMClass getSCMClass() { return SCMClass.VOID; }

    @Override
    public String toString() { return "#<void>"; }
  }
}
