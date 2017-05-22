package core.scm

interface IMeta {

    /* Returns metadata attached to an object */
    fun meta(): Map<*, *>?
}
