package core

import core.exceptions.IllegalSyntaxException
import core.exceptions.UndefinedIdentifierException
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Modifier
import java.math.BigDecimal
import java.math.BigInteger
import kotlin.text.isEmpty

class Reflector {

    companion object {
        private val CLASSES = listOf(Boolean::class, Byte::class, Char::class, Short::class, Int::class, Long::class, Float::class, Double::class)
        private val BOXED = CLASSES.associateBy({ it.java }, { it.javaObjectType })
        private val UNBOXED = CLASSES.associateBy({ it.javaObjectType }, { it.java })

        /* Some common classes are not in java.lang. package, but we don't want to use fully qualified name to use them */
        private val CLASS_PACKAGE_MAPPING = arrayOf(BigInteger::class.java, BigDecimal::class.java).associateBy({ it.simpleName }, { it.name })
    }

    private val name: String = "reflector"

    fun getClazz(name: String) = getClazzOrNull(name) ?: throw ClassNotFoundException("${this.name}: class not found: $name")

    fun getClazzOrNull(name: String): Class<*>? = try {
        when {
            name.contains('.') -> Class.forName(name)
            else -> Class.forName(CLASS_PACKAGE_MAPPING.getOrDefault(name, "java.lang.$name"))
        }
    } catch (e: ClassNotFoundException) {
        null
    }

    fun newInstance(clazz: String, args: Array<out Any?>): Any {
        val c = getClazz(clazz)
        val argTypes = arrayOfNulls<Class<*>>(args.size).apply {
            for (i in args.indices) {
                set(i, unboxIfPossible(args[i]))
            }
        }
        try {
            return try {
                val ctor = c.getConstructor(*argTypes)
                ctor.isAccessible = true
                ctor.newInstance(*args)
            } catch (e: NoSuchMethodException) {
                // no exact match found, try to find inexact match
                val (newArgs, newTypes) = downcastArgs(args, argTypes)
                val ctor = c.getConstructor(*newTypes)
                ctor.isAccessible = true
                ctor.newInstance(*newArgs)
            }
        } catch (ex: NoSuchMethodException) {
            throw NoSuchMethodException("${this.name}: unable to find matching constructor for class ${c.name}")
        } catch (e: IllegalAccessException) {
            throw IllegalAccessException("${this.name}: unable to access constructor for class $clazz")
        }
    }

    /* Java Interop: static fields */
    fun evalJavaStaticField(s: String): Any? = when {
        s.contains('/') -> {
            val classAndField = s.split('/').filterNot(String::isEmpty)
            if (classAndField.size < 2) {
                throw IllegalSyntaxException("${this.name}: malformed expression, expecting (Class/staticField) or (Class/staticMethod ...)")
            }
            val (className, fieldName) = classAndField
            try {
                val c = getClazz(className)
                val field = c.getField(fieldName)
                field.isAccessible = true
                if (!Modifier.isStatic(field.modifiers)) {
                    throw NoSuchFieldException("${this.name}: unable to find static field $fieldName of $className")
                }
                field.get(c)
            } catch (e: NoSuchFieldException) {
                throw NoSuchFieldException("${this.name}: unable to find static field $fieldName in class $className")
            } catch (e: IllegalAccessException) {
                throw IllegalAccessException("${this.name}: unable to access static field $fieldName in class $className")
            }
        }
        else -> throw UndefinedIdentifierException(s)
    }

    fun evalJavaMethod(method: String, args: Array<out Any?>) = when {
        method.contains('/') -> evalJavaStaticMethod(method, args)
        args.isEmpty() -> throw IllegalSyntaxException("${this.name}: malformed member expression, expecting (.member target ...)")
        method.startsWith(".-") -> evalJavaInstanceField(method, instance = args[0]!!)
        method.startsWith('.') -> evalJavaInstanceMethod(method, instance = args[0]!!, args = args.copyOfRange(1, args.size))
        else -> throw UndefinedIdentifierException(method)
    }

    /* Returns Pair(method: Method, args: Array<out Any?>) */
    private fun getMethodAndArgs(clazz: Class<*>, name: String, args: Array<out Any?>, types: Array<Class<*>?>) = try {
        Pair(clazz.getMethod(name, *types), args)
    } catch (e: NoSuchMethodException) {
        // no exact match found, try to find inexact match
        val (newArgs, newTypes) = downcastArgs(args, types)
        try {
            Pair(clazz.getMethod(name, *newTypes), newArgs)
        } catch (ex: NoSuchMethodException) {
            try {
                Pair(clazz.getMethod(name, *Array(types.size, { Object::class.java })), args)
            } catch (ex2: NoSuchMethodException) {
                throw NoSuchMethodException("${this.name}: unable to find matching method $name in class ${clazz.name}")
            }
        }
    }

    private fun downcastArgs(args: Array<out Any?>, types: Array<out Class<*>?>): Pair<Array<out Any?>, Array<out Class<*>?>> {
        val newArgs = arrayOfNulls<Any?>(args.size)
        val newTypes = arrayOfNulls<Class<*>?>(types.size)
        for (i in types.indices) {
            types[i]?.let {
                if (Number::class.java.isAssignableFrom(BOXED.getOrDefault(it, it))) {
                    // downcast to int
                    newTypes[i] = Int::class.java
                    newArgs[i] = (args[i] as Number).toInt()
                } else {
                    newTypes[i] = types[i]
                    newArgs[i] = args[i]
                }
            }
        }
        return Pair(newArgs, newTypes)
    }

    private fun unboxIfPossible(it: Any?) = it?.let { UNBOXED.getOrDefault(it.javaClass, it.javaClass) }

    /* Java Interop: instance method call: (.toString (new Object)) */
    private fun evalJavaInstanceMethod(methodName: String, instance: Any, args: Array<out Any?>) = methodName.substring(1).let {
        try {
            val argTypes = arrayOfNulls<Class<*>>(args.size).apply {
                for (i in args.indices) {
                    set(i, unboxIfPossible(args[i]))
                }
            }
            val (method, methodArgs) = getMethodAndArgs(instance.javaClass, it, args, argTypes)
            method.isAccessible = true
            method(instance, *methodArgs)
        } catch (e: IllegalAccessException) {
            throw IllegalAccessException("${this.name}: unable to access method $it of ${instance.javaClass.name}")
        } catch (e: InvocationTargetException) {
            when (e.cause) {
                null -> throw RuntimeException("${this.name}: invocation target exception")
                else -> throw e.cause as Throwable
            }
        }
    }

    /* Java Interop: instance field: (.-x (new java.awt.Point 15 4)) */
    private fun evalJavaInstanceField(field: String, instance: Any) = field.substring(2).let {
        try {
            val f = instance.javaClass.getField(it)
            f?.isAccessible = true
            f?.get(instance)
        } catch (e: IllegalAccessException) {
            throw IllegalAccessException("${this.name}: unable to access method $it of ${instance.javaClass.name}")
        } catch (e: NoSuchFieldException) {
            throw NoSuchFieldException("${this.name}: unable to find field $it of ${instance.javaClass.name}")
        }
    }

    /* Java Interop: static method call */
    private fun evalJavaStaticMethod(m: String, args: Array<out Any?>): Any? {
        val classAndMethod = m.split('/').filterNot(String::isEmpty)
        if (classAndMethod.size < 2) {
            throw IllegalSyntaxException("${this.name}: malformed expression, expecting (Class/staticField) or (Class/staticMethod ...)")
        }
        val (className, methodName) = classAndMethod
        val clazz = getClazz(className)
        val argTypes = arrayOfNulls<Class<*>>(args.size).apply {
            for (i in args.indices) {
                set(i, unboxIfPossible(args[i]))
            }
        }
        val (method, methodArgs) = getMethodAndArgs(clazz, methodName, args, argTypes)
        if (!Modifier.isStatic(method.modifiers)) {
            throw RuntimeException("${this.name}: unable to find static method $methodName of ${clazz.name}")
        }
        return try {
            method.isAccessible = true
            method(null, *methodArgs)
        } catch (e: IllegalAccessException) {
            throw IllegalAccessException("${this.name}: unable to access static method $methodName of ${clazz.name}")
        } catch (e: InvocationTargetException) {
            when (e.cause) {
                null -> throw RuntimeException("${this.name}: invocation target exception")
                else -> throw e.cause as Throwable
            }
        }
    }
}
