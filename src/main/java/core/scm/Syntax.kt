package core.scm

data class Syntax constructor(val template: Any?) {

    override fun toString() = "#<syntax: $template>"
}