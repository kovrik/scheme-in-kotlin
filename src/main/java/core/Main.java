package core;

import core.environment.DefaultEnvironment;
import core.environment.IEnvironment;
import core.evaluator.Evaluator;
import core.evaluator.IEvaluator;
import core.reader.IReader;
import core.reader.Reader;
import core.scm.SCMInputPort;
import core.scm.SCMOutputPort;
import core.scm.SCMSymbol;
import core.writer.IWriter;
import core.writer.Writer;

import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

import static core.scm.SCMUnspecified.UNSPECIFIED;

public class Main {

  private static final AtomicLong SYM_COUNTER = new AtomicLong(0);

  private static final String WELCOME = "Welcome to Scheme in Java!";
  private static final String PROMPT = "> ";

  private static final IWriter writer = new Writer();
  private static final IReader reader = new Reader();
  private static final IEvaluator evaluator = new Evaluator();
  private static final IEnvironment defaultEnvironment = new DefaultEnvironment();

  private static SCMInputPort currentInputPort = new SCMInputPort(System.in);
  private static final Object INPUT_PORT_LOCK = new Object();

  private static SCMOutputPort currentOutputPort = new SCMOutputPort(System.out);
  private static final Object OUTPUT_PORT_LOCK = new Object();

  private static final DateFormat DF = new SimpleDateFormat("[HH:mm:ss.S]");

  /* REPL History */
  private static final int MAX_ENTRIES = 10;
  private static final Map HISTORY = new LinkedHashMap(MAX_ENTRIES + 1, 0.75F, true) {
    public boolean removeEldestEntry(Map.Entry eldest) {
      return size() > MAX_ENTRIES;
    }
  };

  public static void main(String[] args) throws ParseException, IOException {
    /* Eval lib procedures */
    for (String proc : defaultEnvironment.getLibraryProcedures()) {
      for (Object s : reader.read(proc)) {
        evaluator.eval(s, defaultEnvironment);
      }
    }
    repl(WELCOME, PROMPT, defaultEnvironment);
  }

  private static SCMSymbol getNextID() {
    return new SCMSymbol("$" + SYM_COUNTER.incrementAndGet());
  }

  private static void repl(String welcomeMessage, String prompt, IEnvironment env) throws IOException {
    currentOutputPort.writeln(welcomeMessage);
    while (true) {
      try {
//        currentOutputPort.write(DF.format(System.currentTimeMillis()));
        currentOutputPort.write(prompt);
        currentOutputPort.flush();

        /* Read and parse a list of S-expressions from Stdin */
        List<Object> sexps = reader.read(currentInputPort.getInputStream());
        for (Object expr : sexps) {
          // TODO Macroexpand
          Object expanded = macroexpand(expr);
          /* Evaluate each S-expression */
          Object result = evaluator.eval(expanded, env);
          if (result != null && result != UNSPECIFIED) {
            /* Put result into environment */
            SCMSymbol id = getNextID();
            env.put(id, result);
            /* Print */
            currentOutputPort.writeln(id + " = " + writer.toString(result));
            currentOutputPort.flush();
            /* Store sexp in a history */
            HISTORY.put(id, expr);
          }
        }
        // TODO Proper Error handling
      } catch (Exception e) {
        error(e);
      }
    }
  }

  private static void error(Exception e) throws IOException {
    currentOutputPort.writeln("ERROR: " + e.getMessage());
    currentOutputPort.flush();
  }

  // TODO
  private static Object macroexpand(Object sexp) {
    return sexp;
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
