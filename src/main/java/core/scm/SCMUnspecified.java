package core.scm;

public enum SCMUnspecified implements ISCMClass {
  UNSPECIFIED;

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.UNSPECIFIED;
  }

  @Override
  public String toString() {
    return "#<unspecified>";
  }
}
