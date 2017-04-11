package core.scm;

public class SCMFuture extends SCMDelay implements ISCMClass {

  public SCMFuture(Object expr) {
    super(expr);
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.FUTURE;
  }

  @Override
  protected String getName() {
    return "future";
  }
}
