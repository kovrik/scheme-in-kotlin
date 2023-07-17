package core

import core.environment.DefaultEnvironment
import core.environment.Environment
import core.exceptions.ExInfoException
import core.reader.FileReader
import core.reader.Reader
import core.reader.StringReader
import core.scm.Error
import core.scm.InputPort
import core.scm.OutputPort
import core.scm.Symbol
import java.io.BufferedInputStream
import java.io.File
import java.io.IOException
import java.util.concurrent.atomic.AtomicInteger

object Repl {

    private const val RES_SYM_LIMIT = 10
    private const val WELCOME = "Welcome to Scheme in Kotlin!"
    private const val PROMPT = "> "

    private val resCounter = AtomicInteger(0)
    private val environment = DefaultEnvironment()
    private val evaluator = Evaluator(environment).apply {
        with (StringReader()) {
            environment.libraryProcedures.forEach { this@apply.eval(readOne(it)) }
        }
    }

    var currentInputPort  = InputPort(BufferedInputStream(System.`in`))
    var currentOutputPort = OutputPort(System.out)

    internal val reader = Reader(currentInputPort.inputStream)

    private fun getNextResID() = resCounter.incrementAndGet().let {
        if (it == RES_SYM_LIMIT) resCounter.set(0)
        Symbol.intern("$$it")
    }

    @Throws(IOException::class)
    @JvmStatic fun main(args: Array<String>) = when {
        args.isEmpty() -> repl(WELCOME, PROMPT)
        else           -> evaluateFile(args[0])
    }

    /**
     * Read and evaluate a file and then exit
     */
    @Throws(IOException::class)
    private fun evaluateFile(filename: String) = FileReader().read(File(filename)).forEach {
        evaluator.macroexpandAndEvaluate(it)
    }

    /**
     * Run REPL and evaluate user inputs
     */
    @Throws(IOException::class)
    private fun repl(welcomeMessage: String, prompt: String) {
        currentOutputPort.writeln(welcomeMessage)
        while (true) {
            try {
                currentOutputPort.write(prompt)
                /* Read, macroexpand and then Evaluate each S-expression */
                val result = evaluator.macroexpandAndEvaluate(reader.read())
                /* Do not print and do not store void results */
                if (result === Unit) {
                    continue
                }
                /* nil, on the other hand, is a valid result - print it, but not store it */
                if (result == null) {
                    currentOutputPort.writeln(Writer.write(result))
                    continue
                }
                /* Put result into environment */
                val id = getNextResID()
                evaluator.env[id] = result
                /* Print */
                currentOutputPort.writeln("$id = ${Writer.write(result)}")
            } catch (e: Throwable) {
                /* Store caught exception in $e var */
                evaluator.env[Symbol.intern("\$e")] = e
                error(e)
            }
        }
    }

    @Throws(IOException::class)
    private fun error(e: Throwable) = when (e) {
        is Error -> e.toString()
        is ExInfoException -> e.toString()
        else -> StringBuilder(e.javaClass.simpleName).apply {
            e.message?.let { append(": $it") }
            filterStackTrace(e.stackTrace)?.let {
                append(" (").append(it.fileName ?: "NO_SOURCE_FILE").append(':').append(it.lineNumber).append(')')
            }
        }.toString()
    }.apply(currentOutputPort::writeln)

    private fun filterStackTrace(stackTraceElements: Array<StackTraceElement>) = stackTraceElements.firstOrNull {
        !it.isNativeMethod && !it.className.startsWith("sun.reflect") && !it.className.startsWith("java.lang.reflect")
    }
}
