package com.github.ephemient.aoc2021

import com.github.ephemient.aoc2021.Day24Common.State
import org.graalvm.nativeimage.ImageInfo
import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Type
import org.objectweb.asm.commons.AnalyzerAdapter
import org.objectweb.asm.commons.GeneratorAdapter
import org.objectweb.asm.commons.Method
import org.objectweb.asm.signature.SignatureWriter
import java.lang.invoke.MethodHandles
import java.lang.invoke.MethodType

class Day24Jvm(lines: List<String>) : Day24Impl {
    private val aluClass = with(ClassWriter(ClassWriter.COMPUTE_FRAMES or ClassWriter.COMPUTE_MAXS)) {
        val implName = ALU::class.java.binaryName + "\$Impl"
        visit(
            Opcodes.V17,
            Opcodes.ACC_PUBLIC,
            implName,
            null,
            Any::class.java.binaryName,
            arrayOf(ALU::class.java.binaryName),
        )
        visitField(
            Opcodes.ACC_PRIVATE or Opcodes.ACC_FINAL,
            "visited",
            Type.getDescriptor(MutableSet::class.java),
            with(SignatureWriter()) {
                visitClassType(MutableSet::class.java.binaryName)
                with(visitTypeArgument('-')) {
                    visitClassType(State::class.java.binaryName)
                    visitEnd()
                }
                visitEnd()
                toString()
            },
            null,
        )
        with(
            visitMethod(
                Opcodes.ACC_PUBLIC,
                "<init>",
                Type.getMethodDescriptor(Type.VOID_TYPE, Type.getType(MutableSet::class.java)),
                with(SignatureWriter()) {
                    with(visitParameterType()) {
                        visitClassType(MutableSet::class.java.binaryName)
                        with(visitTypeArgument('-')) {
                            visitClassType(State::class.java.binaryName)
                            visitEnd()
                        }
                        visitEnd()
                    }
                    visitReturnType().visitBaseType('V')
                    toString()
                },
                null,
            )
        ) {
            visitCode()
            visitVarInsn(Opcodes.ALOAD, 0)
            visitMethodInsn(
                Opcodes.INVOKESPECIAL,
                Any::class.java.binaryName,
                "<init>",
                Type.getMethodDescriptor(Type.VOID_TYPE),
                false,
            )
            visitVarInsn(Opcodes.ALOAD, 0)
            visitVarInsn(Opcodes.ALOAD, 1)
            visitFieldInsn(Opcodes.PUTFIELD, implName, "visited", Type.getDescriptor(MutableSet::class.java))
            visitInsn(Opcodes.RETURN)
            visitMaxs(0, 0)
            visitEnd()
        }
        with(
            visitMethod(
                Opcodes.ACC_PUBLIC,
                ALU::solve.name,
                Type.getMethodDescriptor(Type.getType(Long::class.javaObjectType), Type.getType(Iterable::class.java)),
                null,
                null,
            )
        ) {
            visitCode()
            visitVarInsn(Opcodes.ALOAD, 0)
            visitVarInsn(Opcodes.ALOAD, 1)
            visitInsn(Opcodes.LCONST_0)
            repeat(vars.length) { visitInsn(Opcodes.ICONST_0) }
            visitMethodInsn(Opcodes.INVOKEVIRTUAL, implName, "solve", solveMethodDescriptor, false)
            visitInsn(Opcodes.ARETURN)
            visitMaxs(0, 0)
            visitEnd()
        }
        lines.foldIndexed(visitSolveMethodGenerator(implName, "solve").apply { visitCode() }) { i, generator, line ->
            generator.apply {
                if (line.startsWith("inp ")) {
                    check(line[4] in vars)
                    val endLoop = newLabel()
                    loadThis()
                    getField(Type.getObjectType(implName), "visited", Type.getType(MutableSet::class.java))
                    newInstance(Type.getType(State::class.java))
                    dup()
                    push(i)
                    vars.forEachIndexed { i, c -> if (c == line[4]) push(0) else loadArg(i + 2) }
                    invokeConstructor(
                        Type.getType(State::class.java),
                        Method.getMethod(
                            State::class.java.getDeclaredConstructor(
                                Int::class.javaPrimitiveType,
                                Int::class.javaPrimitiveType,
                                Int::class.javaPrimitiveType,
                                Int::class.javaPrimitiveType,
                                Int::class.javaPrimitiveType,
                            )
                        ),
                    )
                    invokeInterface(
                        Type.getType(Set::class.java),
                        Method.getMethod(Set::class.java.getMethod("add", Any::class.java)),
                    )
                    ifZCmp(GeneratorAdapter.EQ, endLoop)
                    val iterator = newLocal(Type.getType(Iterator::class.java))
                    val value = newLocal(Type.INT_TYPE)
                    val ret = newLocal(Type.getType(Long::class.javaObjectType))
                    loadArg(0)
                    invokeInterface(
                        Type.getType(Iterable::class.java),
                        Method.getMethod(Iterable::class.java.getMethod(Iterable<*>::iterator.name)),
                    )
                    storeLocal(iterator)
                    val continueLoop = mark()
                    loadLocal(iterator)
                    invokeInterface(
                        Type.getType(Iterator::class.java),
                        Method.getMethod(Iterator::class.java.getMethod(Iterator<*>::hasNext.name)),
                    )
                    ifZCmp(GeneratorAdapter.EQ, endLoop)
                    loadLocal(iterator)
                    invokeInterface(
                        Type.getType(Iterator::class.java),
                        Method.getMethod(Iterator::class.java.getMethod(Iterator<*>::next.name)),
                    )
                    unbox(Type.INT_TYPE)
                    storeLocal(value)
                    loadThis()
                    loadArg(0)
                    loadArg(1)
                    push(10L)
                    math(GeneratorAdapter.MUL, Type.LONG_TYPE)
                    loadLocal(value)
                    visitInsn(Opcodes.I2L)
                    math(GeneratorAdapter.ADD, Type.LONG_TYPE)
                    vars.forEachIndexed { i, c -> if (c == line[4]) loadLocal(value) else loadArg(i + 2) }
                    invokeVirtual(Type.getObjectType(implName), Method("solve$i", solveMethodDescriptor))
                    storeLocal(ret)
                    loadLocal(ret)
                    ifNull(continueLoop)
                    loadLocal(ret)
                    returnValue()
                    mark(endLoop)
                    push(null as String?)
                    returnValue()
                    endMethod()
                    return@foldIndexed visitSolveMethodGenerator(implName, "solve$i").apply { visitCode() }
                }
                val lhs = vars.indexOf(line[4])
                check(lhs >= 0)
                loadArg(lhs + 2)
                if (line.substring(6).toIntOrNull()?.also { push(it) } == null) {
                    val rhs = vars.indexOf(line[6])
                    check(rhs >= 0)
                    loadArg(rhs + 2)
                }
                when (line.substring(0, 4)) {
                    "add " -> math(GeneratorAdapter.ADD, Type.INT_TYPE)
                    "mul " -> math(GeneratorAdapter.MUL, Type.INT_TYPE)
                    "div " -> math(GeneratorAdapter.DIV, Type.INT_TYPE)
                    "mod " -> math(GeneratorAdapter.REM, Type.INT_TYPE)
                    "eql " -> {
                        val zero = newLabel()
                        val done = newLabel()
                        ifICmp(GeneratorAdapter.NE, zero)
                        push(1)
                        goTo(done)
                        mark(zero)
                        push(0)
                        mark(done)
                    }
                    else -> TODO()
                }
                storeArg(lhs + 2)
            }
        }.run {
            val end = newLabel()
            loadArg(vars.length + 1)
            ifZCmp(GeneratorAdapter.EQ, end)
            push(null as String?)
            returnValue()
            mark(end)
            loadArg(1)
            valueOf(Type.LONG_TYPE)
            returnValue()
            endMethod()
        }
        visitEnd()
        MethodHandles.lookup().defineHiddenClass(toByteArray(), true).lookupClass()
    }
    private val aluConstructor = MethodHandles.lookup()
        .findConstructor(aluClass, MethodType.methodType(Nothing::class.javaPrimitiveType, MutableSet::class.java))

    override fun part1(): Long? = (aluConstructor.invoke(CacheSet<State>(0x1000000)) as ALU).solve(9 downTo 1)

    override fun part2(): Long? = (aluConstructor.invoke(CacheSet<State>(0x1000000)) as ALU).solve(1..9)

    private interface ALU {
        fun solve(range: Iterable<Int>): Long?
    }

    class Provider : Day24Impl.Provider<Day24Jvm> {
        override fun invoke(lines: List<String>): Day24Jvm? = if (ImageInfo.inImageCode()) null else Day24Jvm(lines)

        override fun toString(): String = "Day24Jvm"
    }

    companion object {
        private const val vars = "wxyz"

        private val Class<*>.binaryName: String
            get() = name.replace('.', '/')

        private val solveMethodDescriptor = Type.getMethodDescriptor(
            Type.getType(Long::class.javaObjectType),
            Type.getType(Iterable::class.java),
            Type.LONG_TYPE,
            Type.INT_TYPE,
            Type.INT_TYPE,
            Type.INT_TYPE,
            Type.INT_TYPE,
        )

        private fun ClassVisitor.visitSolveMethodGenerator(owner: String, name: String): GeneratorAdapter =
            GeneratorAdapter(
                AnalyzerAdapter(
                    owner,
                    Opcodes.ACC_PRIVATE,
                    name,
                    solveMethodDescriptor,
                    visitMethod(Opcodes.ACC_PRIVATE, name, solveMethodDescriptor, null, null),
                ),
                Opcodes.ACC_PRIVATE,
                name,
                solveMethodDescriptor,
            )
    }
}
