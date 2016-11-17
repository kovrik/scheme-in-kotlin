package core.scm;

import java.io.IOException;
import java.io.InputStream;

public class SCMInputPort implements ISCMClass, ISCMPort {

  private final InputStream inputStream;

  private final Object lock = new Object();

  private Integer next = null;

  public SCMInputPort(InputStream inputStream) {
    this.inputStream = inputStream;
  }

  @Override
  public void close() throws IOException {
    inputStream.close();
  }

  public int read() throws IOException {
    synchronized (lock) {
      if (next != null) {
        int result = next;
        next = null;
        return result;
      } else {
        return inputStream.read();
      }
    }
  }

  // TODO Check if correctly implemented
  public int peek() throws IOException {
    synchronized (lock) {
      if (next != null) {
        return next;
      } else {
        next = inputStream.read();
        return next;
      }
    }
  }

  public InputStream getInputStream() {
    return inputStream;
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.INPUT_PORT;
  }

  @Override
  public String toString() {
    if (inputStream.equals(System.in)) {
      return "#<output-port:stdin>";
    }
    return "#<input-port:" + inputStream + ">";
  }
}
