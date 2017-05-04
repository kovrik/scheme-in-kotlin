package core.exceptions;

import core.writer.Writer;

import java.util.Map;

public class ExInfoException extends RuntimeException implements IException {

  private final Map info;

  public ExInfoException(String message, Map info) {
    super(message);
    this.info = info;
  }

  public ExInfoException(String message, Map info, Throwable cause) {
    super(message, cause);
    this.info = info;
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    return null;
  }

  public Map getInfo() {
    return info;
  }

  @Override
  public String toString() {
    return "#<ex-info:{message:" + getMessage() +
           ", cause: " + Writer.write(getCause()) +
           ", data: " + Writer .write(info) + "}>";
  }
}
