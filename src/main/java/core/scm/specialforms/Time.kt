package core.scm.specialforms

import core.Repl
import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException

import java.lang.management.ManagementFactory
import java.util.concurrent.TimeUnit

/**
 * Time Special Form:
 * Counts time taken for evaluation.
 * Syntax:
 * (time <expression1> ... <expression n>)
 */
object Time : SpecialForm("time") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (form.size < 2) {
            throw IllegalSyntaxException(toString(), form)
        }

        val threadMXBean = ManagementFactory.getThreadMXBean()
        val threadId = Thread.currentThread().id
        val threadCpuTime = threadMXBean.getThreadCpuTime(threadId)

        val gcBeans = ManagementFactory.getGarbageCollectorMXBeans()
        var gcCount = 0L
        var gcTimeMillis = 0L

        val nanos = System.nanoTime()

        for (i in 1..form.size - 2) {
            evaluator.eval(form[i], env)
        }
        val result = evaluator.eval(form[form.size - 1], env)

        for (gcBean in gcBeans) {
            gcCount += gcBean.collectionCount
            gcTimeMillis += gcBean.collectionTime
        }

        val cpuTime = TimeUnit.NANOSECONDS.toMillis(threadMXBean.getThreadCpuTime(threadId) - threadCpuTime)
        val realTime = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - nanos)

        Repl.currentOutputPort.writeln("CPU Time: $cpuTime ms; Real Time: $realTime ms;" +
                                       " GC Time: $gcTimeMillis ms; GC Count: $gcCount")
        return result
    }
}
