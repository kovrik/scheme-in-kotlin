package core

import core.environment.DefaultEnvironment
import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.ExInfoException
import core.exceptions.ThrowableWrapper
import core.reader.Reader
import core.reader.StringReader
import core.scm.*
import core.writer.Writer
import java.io.BufferedInputStream
import java.io.IOException
import java.util.concurrent.atomic.AtomicInteger

object Repl {

    private val SYM_COUNTER = AtomicInteger(0)
    private val SYM_LIMIT = 25

    private val WELCOME = "Welcome to Scheme in Java!"
    private val PROMPT = "> "

    private val evaluator = Evaluator()
    private val defaultEnvironment = DefaultEnvironment()

    @JvmStatic var currentInputPort = InputPort(BufferedInputStream(System.`in`))
    @JvmStatic var currentOutputPort = OutputPort(System.out)

    private val reader = Reader(currentInputPort.inputStream)

    class Main {
        companion object {
            @Throws(IOException::class)
            @JvmStatic fun main(args: Array<String>) {
                /* Eval lib procedures */
                val stringReader = StringReader()
                for (proc in defaultEnvironment.libraryProcedures) {
                    for (s in stringReader.read(proc)) {
                        evaluator.macroexpandAndEvaluate(s, defaultEnvironment)
                    }
                }
                repl(WELCOME, PROMPT, defaultEnvironment)
            }
        }
    }

    private val nextID: Symbol
        get() {
            val i = SYM_COUNTER.incrementAndGet()
            if (i == SYM_LIMIT) {
                SYM_COUNTER.set(0)
            }
            return Symbol.intern("$" + i)
        }

    @Throws(IOException::class)
    private fun repl(welcomeMessage: String, prompt: String, env: Environment) {
        currentOutputPort.writeln(welcomeMessage)

        while (true) {
            try {
                currentOutputPort.write(prompt)
                /* Read and parse a list of S-expressions from Stdin */
                val sexps = reader.read()
                for (expr in sexps) {
                    /* Macroexpand and then Evaluate each S-expression */
                    val result = evaluator.macroexpandAndEvaluate(expr, env)
                    /* Do not print and do not store void results */
                    if (result === Void.VOID) {
                        continue
                    }
                    /* nil, on the other hand, is a valid result - print it, but not store it */
                    if (result == null) {
                        currentOutputPort.writeln(Writer.write(result))
                        continue
                    }
                    /* Put result into environment */
                    val id = nextID
                    env.put(id, result)
                    /* Print */
                    currentOutputPort.writeln(id.toString() + " = " + Writer.write(result))
                }
            } catch (e: ThrowableWrapper) {
                /* Unwrap */
                error(e.cause ?: e)
            } catch (e: Throwable) {
                error(e)
            }
        }
    }

    @Throws(IOException::class)
    private fun error(e: Throwable) {
        val errorMessage: String
        if (e is Error) {
            errorMessage = "Error: " + e.message
        } else if (e is ExInfoException) {
            errorMessage = e.toString()
        } else {
            val sb = StringBuilder(e.javaClass.simpleName)
            if (e.message != null) {
                sb.append(": ").append(e.message)
            }
            val frame = filterStackTrace(e.stackTrace)
            if (frame != null) {
                sb.append(" (").append(frame.fileName).append(':').append(frame.lineNumber).append(')')
            }
            errorMessage = sb.toString()
        }
        currentOutputPort.writeln(errorMessage)
    }

    private fun filterStackTrace(stackTraceElements: Array<StackTraceElement>): StackTraceElement? {
        for (stackTraceElement in stackTraceElements) {
            if (stackTraceElement.isNativeMethod) continue
            val name = stackTraceElement.className
            if (name.startsWith("sun.reflect") || name.startsWith("java.lang.reflect")) continue
            return stackTraceElement
        }
        return null
    }
}
