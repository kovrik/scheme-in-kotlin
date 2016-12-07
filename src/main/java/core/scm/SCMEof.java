package core.scm;

public enum SCMEof implements ISCMClass {
  EOF;

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.EOF;
  }

  @Override
  public String toString() {
    return "#<eof>";
  }
}
