package core.exceptions;

public class SCMFileNotFoundException extends RuntimeException {

  public SCMFileNotFoundException(String filename) {
    super("Cannot open file: " + filename);
  }
}
