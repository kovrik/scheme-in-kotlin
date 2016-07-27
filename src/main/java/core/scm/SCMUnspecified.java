package core.scm;

public final class SCMUnspecified implements ISCMClass {

  public static final SCMUnspecified UNSPECIFIED = new SCMUnspecified();

  private SCMUnspecified() {}

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.UNSPECIFIED;
  }

  @Override
  public String toString() {
    return "#<unspecified>";
  }
}
