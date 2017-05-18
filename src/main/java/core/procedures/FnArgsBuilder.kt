package core.procedures

class FnArgsBuilder {

    private var min = 0
    /* JVM restricts max number of arguments to 255
     * If max is more than 255, then we assume that function accepts ANY number of arguments.
     * If max is less or equal to 255, then we assume that function accepts exactly up to max arguments.
     **/
    private var max = Integer.MAX_VALUE
    private var mandatory = arrayOf<Class<*>>()
    private var rest: Class<*>? = null
    private var last: Class<*>? = null

    fun build(): FnArgs {
        return FnArgs(min, max, mandatory, rest, last)
    }

    fun min(min: Int): FnArgsBuilder {
        this.min = min
        return this
    }

    fun max(max: Int): FnArgsBuilder {
        this.max = max
        return this
    }

    fun mandatory(mandatoryTypes: Array<Class<*>>): FnArgsBuilder {
        this.mandatory = mandatoryTypes
        return this
    }

    fun rest(restType: Class<*>): FnArgsBuilder {
        this.rest = restType
        return this
    }

    fun last(lastType: Class<*>): FnArgsBuilder {
        this.last = lastType
        return this
    }
}
