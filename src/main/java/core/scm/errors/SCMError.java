package core.scm.errors;

public class SCMError extends RuntimeException {
  public SCMError() {
    super();
  }

  public SCMError(String message) {
    super(message);
  }
}
