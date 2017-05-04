package core.scm;

public class Error extends RuntimeException implements ITyped {

  public Error(String message) {
    super(message);
  }

  @Override
  public Type getType() {
    return Type.ERROR;
  }
}
