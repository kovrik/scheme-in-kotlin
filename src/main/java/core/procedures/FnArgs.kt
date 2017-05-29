package core.procedures

/* TODO Replace with TypeChecker OR Contracts */
class FnArgs(val min: Int = 0,
             val max: Int = Integer.MAX_VALUE,
             val mandatory: Array<Class<*>> = arrayOf<Class<*>>(),
             val rest: Class<*>? = null,
             val last: Class<*>? = null)
