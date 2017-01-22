package unittests;

import core.Repl;
import core.scm.SCMEof;
import core.scm.SCMInputPort;
import core.scm.SCMOutputPort;
import org.junit.Test;

import java.io.*;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;
import static org.junit.Assert.assertEquals;

public class IOTest extends AbstractTest {

  @Test
  public void testCurrentPorts() throws FileNotFoundException {
    assertEquals(new SCMOutputPort(System.out), eval("(current-output-port)", env));

    OutputStream outputStream = new ByteArrayOutputStream(0);
    SCMOutputPort currentOutputPort = Repl.getCurrentOutputPort();
    Repl.setCurrentOutputPort(new SCMOutputPort(outputStream));
    assertEquals(new SCMOutputPort(outputStream), eval("(current-output-port)", env));
    Repl.setCurrentOutputPort(currentOutputPort);

    InputStream inputStream = new ByteArrayInputStream("test".getBytes());
    SCMInputPort currentInputPort = Repl.getCurrentInputPort();
    Repl.setCurrentInputPort(new SCMInputPort(inputStream));
    assertEquals(new SCMInputPort(inputStream), eval("(current-input-port)", env));
    Repl.setCurrentInputPort(currentInputPort);
  }

  @Test
  public void testEofObject() {
    assertEquals(SCMEof.EOF, eval("eof", env));
    assertEquals(TRUE, eval("(eof-object? eof)", env));
    assertEquals(FALSE, eval("(eof-object? 0)", env));
    assertEquals(FALSE, eval("(eof-object? \"test\")", env));
  }

  @Test
  public void testPortPredicates() {
    assertEquals(TRUE, eval("(port? (current-input-port))", env));
    assertEquals(TRUE, eval("(port? (current-output-port))", env));
    assertEquals(TRUE, eval("(input-port? (current-input-port))", env));
    assertEquals(TRUE, eval("(output-port? (current-output-port))", env));

    assertEquals(FALSE, eval("(port? 1)", env));
    assertEquals(FALSE, eval("(output-port? (current-input-port))", env));
    assertEquals(FALSE, eval("(input-port? (current-output-port))", env));
  }

  // TODO close-*-port, write-char, peek-char, read-char, read, write
}
