package core.scm.specialforms

import core.Repl
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.Writer

import java.lang.management.ManagementFactory
import java.util.concurrent.TimeUnit

/**
 * Time Special Form:
 * Counts time taken for evaluation.
 * Syntax:
 * (time <expression1> ... <expression n>)
 */
object Time : SpecialForm("time") {

    override fun eval(form: List<Any?>, evaluator: Evaluator): Any? {
        if (form.size < 2) {
            throw IllegalSyntaxException(toString(), Writer.write(form))
        }

        val threadMXBean = ManagementFactory.getThreadMXBean()
        val threadId = Thread.currentThread().id
        val threadCpuTime = threadMXBean.getThreadCpuTime(threadId)

        val gcBeans = ManagementFactory.getGarbageCollectorMXBeans()
        var gcCountStart = 0L
        var gcTimeMillisStart = 0L
        for (gcBean in gcBeans) {
            gcCountStart += gcBean.collectionCount
            gcTimeMillisStart += gcBean.collectionTime
        }

        val nanos = System.nanoTime()

        (1..form.size - 2).forEach { evaluator.eval(form[it]) }
        val result = evaluator.eval(form[form.size - 1])

        var gcCountEnd = 0L
        var gcTimeMillisEnd = 0L
        for (gcBean in gcBeans) {
            gcCountEnd += gcBean.collectionCount
            gcTimeMillisEnd += gcBean.collectionTime
        }

        val cpuTime = TimeUnit.NANOSECONDS.toMillis(threadMXBean.getThreadCpuTime(threadId) - threadCpuTime)
        val realTime = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - nanos)
        val gcTimeMillis = gcTimeMillisEnd - gcTimeMillisStart
        val gcCount = gcCountEnd - gcCountStart

        Repl.currentOutputPort.writeln("CPU Time: $cpuTime ms; Real Time: $realTime ms;" +
                                       " GC Time: $gcTimeMillis ms; GC Count: $gcCount")
        return result
    }
}
