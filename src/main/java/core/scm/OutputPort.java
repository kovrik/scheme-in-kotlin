package core.scm;

import java.io.IOException;
import java.io.OutputStream;

public class OutputPort implements ITyped, IPort {

  private static final String LS = System.getProperty("line.separator");

  private final OutputStream outputStream;

  public OutputPort(OutputStream outputStream) {
    this.outputStream = outputStream;
  }

  public OutputStream getOutputStream() {
    return outputStream;
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

  public void writeln(String str) throws IOException {
    outputStream.write((str + LS).getBytes());
  }

  public void flush() throws IOException {
    outputStream.flush();
  }

  @Override
  public Type getType() {
    return Type.OUTPUT_PORT;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    OutputPort that = (OutputPort) o;
    return outputStream != null ? outputStream.equals(that.outputStream) : that.outputStream == null;
  }

  @Override
  public int hashCode() {
    return outputStream != null ? outputStream.hashCode() : 0;
  }

  @Override
  public String toString() {
    if (outputStream.equals(System.out)) {
      return "#<output-port:stdout>";
    }
    return "#<output-port:" + outputStream + ">";
  }
}
