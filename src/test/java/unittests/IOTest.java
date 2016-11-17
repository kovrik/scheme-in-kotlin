package unittests;

import core.Main;
import core.scm.SCMInputPort;
import core.scm.SCMOutputPort;
import org.junit.Test;

import java.io.*;

import static org.junit.Assert.assertEquals;

public class IOTest extends AbstractTest {

  @Test
  public void testCurrentPorts() throws FileNotFoundException {
    assertEquals(new SCMInputPort(System.in),   eval("(current-input-port)", env));
    assertEquals(new SCMOutputPort(System.out), eval("(current-output-port)", env));

    OutputStream outputStream = new ByteArrayOutputStream(0);
    SCMOutputPort currentOutputPort = Main.getCurrentOutputPort();
    Main.setCurrentOutputPort(new SCMOutputPort(outputStream));
    assertEquals(new SCMOutputPort(outputStream), eval("(current-output-port)", env));
    Main.setCurrentOutputPort(currentOutputPort);

    InputStream inputStream = new ByteArrayInputStream("test".getBytes());
    SCMInputPort currentInputPort = Main.getCurrentInputPort();
    Main.setCurrentInputPort(new SCMInputPort(inputStream));
    assertEquals(new SCMInputPort(inputStream), eval("(current-input-port)", env));
    Main.setCurrentInputPort(currentInputPort);
  }
}
