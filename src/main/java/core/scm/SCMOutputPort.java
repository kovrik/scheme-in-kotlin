package core.scm;

import java.io.IOException;
import java.io.OutputStream;

public class SCMOutputPort implements ISCMClass, ISCMPort {

  private final OutputStream outputStream;

  public SCMOutputPort(OutputStream outputStream) {
    this.outputStream = outputStream;
  }

  @Override
  public void close() throws IOException {
    outputStream.close();
  }

  public void write(int b) throws IOException {
    outputStream.write(b);
  }

  public void write(String str) throws IOException {
    outputStream.write(str.getBytes());
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.OUTPUT_PORt;
  }

  @Override
  public String toString() {
    if (outputStream.equals(System.out)) {
      return "#<output-port:stdout>";
    }
    return "#<output-port:" + outputStream + ">";
  }
}
