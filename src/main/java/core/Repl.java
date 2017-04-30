package core;

import core.environment.DefaultEnvironment;
import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.ExInfoException;
import core.exceptions.ThrowableWrapper;
import core.reader.IReader;
import core.reader.Reader;
import core.reader.StringReader;
import core.scm.*;
import core.writer.IWriter;
import core.writer.Writer;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

public class Repl {

  private static final AtomicInteger SYM_COUNTER = new AtomicInteger(0);
  private static final int SYM_LIMIT = 50;

  private static final String WELCOME = "Welcome to Scheme in Java!";
  private static final String PROMPT = "> ";

  private static final Evaluator evaluator = new Evaluator();
  private static final DefaultEnvironment defaultEnvironment = new DefaultEnvironment();

  private static SCMInputPort currentInputPort = new SCMInputPort(new BufferedInputStream(System.in));
  private static SCMOutputPort currentOutputPort = new SCMOutputPort(System.out);

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
    return SCMSymbol.intern("$" + i);
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
          /* Do not print and do not store void results */
          if (result == SCMVoid.VOID) {
            continue;
          }
          /* nil, on the other hand, is a valid result - print it, but not store it */
          if (result == null) {
            currentOutputPort.writeln(writer.toString(result));
            continue;
          }
          /* Put result into environment */
          SCMSymbol id = getNextID();
          env.put(id, result);
          /* Print */
          currentOutputPort.writeln(id + " = " + writer.toString(result));
        }
      } catch (ThrowableWrapper e) {
        /* Unwrap */
        error(e.getCause());
      } catch (Throwable e) {
        error(e);
      }
    }
  }

  private static void error(Throwable e) throws IOException {
    StringBuilder sb = new StringBuilder();
    if (e instanceof SCMError) {
      sb.append("Error: ").append(e.getMessage());
    } else if (e instanceof ExInfoException) {
      sb.append(e);
    } else {
      if (e.getMessage() == null) {
        sb.append(e.getClass().getSimpleName());
      } else {
        sb.append(e.getClass().getSimpleName()).append(": ").append(e.getMessage());
      }
      StackTraceElement[] stackTrace = e.getStackTrace();
      if (stackTrace.length > 0) {
        StackTraceElement frame = stackTrace[0];
        sb.append(" (").append(frame.getFileName()).append(':').append(frame.getLineNumber()).append(')');
      }
    }
    currentOutputPort.writeln(sb.toString());
  }

  public static SCMInputPort getCurrentInputPort() {
    return currentInputPort;
  }

  public static void setCurrentInputPort(SCMInputPort inputPort) {
    currentInputPort = inputPort;
  }

  public static SCMOutputPort getCurrentOutputPort() {
    return currentOutputPort;
  }

  public static void setCurrentOutputPort(SCMOutputPort outputPort) {
    currentOutputPort = outputPort;
  }
}
