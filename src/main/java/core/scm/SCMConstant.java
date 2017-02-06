package core.scm;

public enum SCMConstant implements ISCMClass {
//  NIL("()", SCMClass.NIL),
  EOF("#<eof>", SCMClass.EOF),
  UNSPECIFIED("#<unspecified>", SCMClass.UNSPECIFIED);

  private String syntax;
  private SCMClass scmClass;

  SCMConstant(String syntax, SCMClass scmClass) {
    this.syntax = syntax;
    this.scmClass = scmClass;
  }

  @Override
  public SCMClass getSCMClass() {
    return scmClass;
  }

  @Override
  public String toString() {
    return syntax;
  }
}
