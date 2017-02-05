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

  public int peek() throws IOException {
    synchronized (lock) {
      if (next == null) {
        next = inputStream.read();
      }
      return next;
    }
  }

  public InputStream getInputStream() {
    return inputStream;
  }

  public int available() throws IOException {
    return inputStream.available();
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.INPUT_PORT;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    SCMInputPort that = (SCMInputPort) o;
    return inputStream != null ? inputStream.equals(that.inputStream) : that.inputStream == null;
  }

  @Override
  public int hashCode() {
    return inputStream != null ? inputStream.hashCode() : 0;
  }

  @Override
  public String toString() {
    if (inputStream.equals(System.in)) {
      return "#<input-port:stdin>";
    }
    return "#<input-port:" + inputStream + ">";
  }
}
