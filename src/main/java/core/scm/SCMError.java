package core.scm;

public class SCMError extends RuntimeException implements ISCMClass {

  public SCMError(String message) {
    super(message);
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.ERROR;
  }
}
