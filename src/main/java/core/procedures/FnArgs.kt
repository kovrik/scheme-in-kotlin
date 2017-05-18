package core.procedures

class FnArgs(
    /* TODO Replace with TypeChecker OR Contracts */

    private val min: Int,
    /* JVM restricts max number of arguments to 255
     * If max is more than 255, then we assume that function accepts ANY number of arguments.
     * If max is less or equal to 255, then we assume that function accepts exactly up to max arguments.
     **/
    private val max: Int, private val mandatory: Array<Class<*>>, private val rest: Class<*>?, private val last: Class<*>?) {

    fun min(): Int {
        return min
    }

    fun max(): Int {
        return max
    }

    fun mandatory(): Array<Class<*>> {
        return mandatory
    }

    fun rest(): Class<*>? {
        return rest
    }

    fun last(): Class<*>? {
        return last
    }
}
