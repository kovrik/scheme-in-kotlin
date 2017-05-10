package core;

import core.environment.DefaultEnvironment;
import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.ExInfoException;
import core.exceptions.ThrowableWrapper;
import core.reader.IReader;
import core.reader.Reader;
import core.reader.StringReader;
import core.scm.Error;
import core.scm.InputPort;
import core.scm.OutputPort;
import core.scm.Symbol;
import core.scm.Void;
import core.writer.Writer;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

public class Repl {

  private static final AtomicInteger SYM_COUNTER = new AtomicInteger(0);
  private static final int SYM_LIMIT = 25;

  private static final String WELCOME = "Welcome to Scheme in Java!";
  private static final String PROMPT = "> ";

  private static final Evaluator evaluator = new Evaluator();
  private static final DefaultEnvironment defaultEnvironment = new DefaultEnvironment();

  private static InputPort currentInputPort = new InputPort(new BufferedInputStream(System.in));
  private static OutputPort currentOutputPort = new OutputPort(System.out);

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

  private static Symbol getNextID() {
    int i = SYM_COUNTER.incrementAndGet();
    if (i == SYM_LIMIT) {
      SYM_COUNTER.set(0);
    }
    return Symbol.intern("$" + i);
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
          if (result == Void.VOID) {
            continue;
          }
          /* nil, on the other hand, is a valid result - print it, but not store it */
          if (result == null) {
            currentOutputPort.writeln(Writer.write(result));
            continue;
          }
          /* Put result into environment */
          Symbol id = getNextID();
          env.put(id, result);
          /* Print */
          currentOutputPort.writeln(id + " = " + Writer.write(result));
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
    String errorMessage;
    if (e instanceof Error) {
      errorMessage = "Error: " + e.getMessage();
    } else if (e instanceof ExInfoException) {
      errorMessage = e.toString();
    } else {
      StringBuilder sb = new StringBuilder(e.getClass().getSimpleName());
      if (e.getMessage() != null) {
        sb.append(": ").append(e.getMessage());
      }
      StackTraceElement frame = filterStackTrace(e.getStackTrace());
      if (frame != null) {
        sb.append(" (").append(frame.getFileName()).append(':').append(frame.getLineNumber()).append(')');
      }
      errorMessage = sb.toString();
    }
    currentOutputPort.writeln(errorMessage);
  }

  private static StackTraceElement filterStackTrace(StackTraceElement[] stackTraceElements) {
    for (StackTraceElement stackTraceElement : stackTraceElements) {
      if (stackTraceElement.isNativeMethod()) continue;
      String name = stackTraceElement.getClassName();
      if (name.startsWith("sun.reflect") || name.startsWith("java.lang.reflect")) continue;
      return stackTraceElement;
    }
    return null;
  }

  public static InputPort getCurrentInputPort() {
    return currentInputPort;
  }

  public static void setCurrentInputPort(InputPort inputPort) {
    currentInputPort = inputPort;
  }

  public static OutputPort getCurrentOutputPort() {
    return currentOutputPort;
  }

  public static void setCurrentOutputPort(OutputPort outputPort) {
    currentOutputPort = outputPort;
  }
}
