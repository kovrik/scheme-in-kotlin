package core.scm;

import java.io.IOException;
import java.io.InputStream;

public class SCMInputPort implements ISCMClass, ISCMPort {

  private final InputStream inputStream;

  public SCMInputPort(InputStream inputStream) {
    this.inputStream = inputStream;
  }

  @Override
  public void close() throws IOException {
    inputStream.close();
  }

  public int read() throws IOException {
    return inputStream.read();
  }

  public InputStream getInputStream() {
    return inputStream;
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.INPUT_PORt;
  }

  @Override
  public String toString() {
    if (inputStream.equals(System.in)) {
      return "#<output-port:stdin>";
    }
    return "#<input-port:" + inputStream + ">";
  }
}
