package core.scm;

public enum SCMConstant implements ISCMClass {
  EOF("#<eof>", SCMClass.EOF),
  NIL("nil", SCMClass.NIL)
  ;

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
