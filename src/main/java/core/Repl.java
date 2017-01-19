package core;

import core.environment.DefaultEnvironment;
import core.environment.Environment;
import core.evaluator.Evaluator;
import core.reader.IReader;
import core.reader.Reader;
import core.reader.StringReader;
import core.scm.SCMInputPort;
import core.scm.SCMOutputPort;
import core.scm.SCMSymbol;
import core.writer.IWriter;
import core.writer.Writer;

import java.io.IOException;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static core.scm.SCMUnspecified.UNSPECIFIED;

public class Repl {

  private static final AtomicInteger SYM_COUNTER = new AtomicInteger(0);
  private static final int SYM_LIMIT = 50;

  private static final String WELCOME = "Welcome to Scheme in Java!";
  private static final String PROMPT = "> ";

  private static final Evaluator evaluator = new Evaluator();
  private static final Environment defaultEnvironment = new DefaultEnvironment();

  private static SCMInputPort currentInputPort = new SCMInputPort(System.in);
  private static final Object INPUT_PORT_LOCK = new Object();

  private static SCMOutputPort currentOutputPort = new SCMOutputPort(System.out);
  private static final Object OUTPUT_PORT_LOCK = new Object();

  private static final IWriter writer = new Writer();
  private static final IReader reader = new Reader(currentInputPort.getInputStream());

  public static void main(String[] args) throws IOException {
    /* Eval lib procedures */
    StringReader stringReader = new StringReader();
    for (String proc : defaultEnvironment.getLibraryProcedures()) {
      for (Object s : stringReader.read(proc)) {
        evaluator.macroexpandAndEvaluate(s, defaultEnvironment);
      }
    }
    repl(WELCOME, PROMPT, defaultEnvironment);
  }

  private static SCMSymbol getNextID() {
    int i = SYM_COUNTER.incrementAndGet();
    if (i == SYM_LIMIT) {
      SYM_COUNTER.set(0);
    }
    return SCMSymbol.of("$" + i);
  }

  private static void repl(String welcomeMessage, String prompt, Environment env) throws IOException {
    currentOutputPort.writeln(welcomeMessage);
    //noinspection InfiniteLoopStatement
    while (true) {
      try {
        currentOutputPort.write(prompt);
        /* Read and parse a list of S-expressions from Stdin */
        List<Object> sexps = reader.read();
        for (Object expr : sexps) {
          /* Macroexpand and then Evaluate each S-expression */
          Object result = evaluator.macroexpandAndEvaluate(expr, env);
          if (result != null && result != UNSPECIFIED) {
            /* Put result into environment */
            SCMSymbol id = getNextID();
            env.put(id, result);
            /* Print */
            currentOutputPort.writeln(id + " = " + writer.toString(result));
          }
        }
      } catch (Exception e) {
        error(e);
      }
    }
  }

  private static void error(Exception e) throws IOException {
    currentOutputPort.writeln("ERROR: " + e.getMessage());
  }

  public static SCMInputPort getCurrentInputPort() {
    synchronized (INPUT_PORT_LOCK) {
      return currentInputPort;
    }
  }

  public static void setCurrentInputPort(SCMInputPort inputPort) {
    synchronized (INPUT_PORT_LOCK) {
      currentInputPort = inputPort;
    }
  }

  public static SCMOutputPort getCurrentOutputPort() {
    synchronized (OUTPUT_PORT_LOCK) {
      return currentOutputPort;
    }
  }

  public static void setCurrentOutputPort(SCMOutputPort outputPort) {
    synchronized (OUTPUT_PORT_LOCK) {
      currentOutputPort = outputPort;
    }
  }
}
