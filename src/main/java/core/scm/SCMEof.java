package core.scm;

public final class SCMEof implements ISCMClass {

  public static final SCMEof EOF = new SCMEof();

  private SCMEof() {}

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.EOF;
  }

  @Override
  public String toString() {
    return "#<eof>";
  }
}
