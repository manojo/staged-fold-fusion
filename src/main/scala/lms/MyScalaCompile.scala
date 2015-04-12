package lms

import scala.virtualization.lms.internal._
import java.io._

import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._

import scala.tools.nsc.interpreter.AbstractFileClassLoader

trait MyScalaCompile extends ScalaCompile {
  override val codegen: ScalaCodegen { val IR: MyScalaCompile.this.type }

  def freshClass = { val className = "staged$" + compileCount; compileCount += 1; className }

  def compileAny(className: String, staticData: List[(Sym[Any], Any)], source: String): Any = {
    if (this.compiler eq null) setupCompiler()
    if (dumpGeneratedCode) println(source)

    val compiler = this.compiler
    val run = new compiler.Run
    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
    // compiler.genJVM.outputDir = fileSystem
    run.compileSources(List(new scala.reflect.internal.util.BatchSourceFile("<stdin>", source)))
    reporter.printSummary()
    if (!reporter.hasErrors) println("compilation: ok")
    else println("compilation: had errors")
    reporter.reset
    //output.reset
    val parent = this.getClass.getClassLoader
    val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)
    val cls: Class[_] = loader.loadClass(className)
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure): _*)
    cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]): _*)
  }

  override def compile[A, R](f: Exp[A] => Exp[R])
                       (implicit mA: Manifest[A], mR: Manifest[R]): A => R = {
    val className = freshClass
    val source = new StringWriter()
    val staticData = codegen.emitSource(f, className, new PrintWriter(source))

    //a bit of a hack to generate data structures
    val dataStructWriter = new StringWriter()
    codegen.emitDataStructures(new PrintWriter(dataStructWriter))

    compileAny(className, staticData, dataStructWriter.toString + source.toString).asInstanceOf[A => R]
  }

  def compile2[A, B, R](f: (Exp[A], Exp[B]) => Exp[R])
                       (implicit mA: Manifest[A], mB: Manifest[B], mR: Manifest[R]): (A, B) => R = {
    val className = freshClass
    val source = new StringWriter()
    val staticData = codegen.emitSource2(f, className, new PrintWriter(source))

    //a bit of a hack to generate data structures
    val dataStructWriter = new StringWriter()
    codegen.emitDataStructures(new PrintWriter(dataStructWriter))

    compileAny(className, staticData, dataStructWriter.toString + source.toString).asInstanceOf[(A, B) => R]
  }

  def compile2s[A, B, R](f: (Exp[A], Exp[B]) => Exp[R], source: StringWriter)
                        (implicit mA: Manifest[A], mB: Manifest[B], mR: Manifest[R]): (A, B) => R = {

    val className = freshClass
    //val source = new StringWriter()
    val staticData = codegen.emitSource2(f, className, new PrintWriter(source))
    compileAny(className, staticData, source.toString).asInstanceOf[(A, B) => R]
  }

  def compile3[A, B, C, R](f: (Exp[A], Exp[B], Exp[C]) => Exp[R])
                          (implicit mA: Manifest[A],
                           mB: Manifest[B],
                           mC: Manifest[C],
                           mR: Manifest[R]): (A, B, C) => R = {

    val className = freshClass
    val source = new StringWriter()
    val staticData = codegen.emitSource3(f, className, new PrintWriter(source))
    compileAny(className, staticData, source.toString).asInstanceOf[(A, B, C) => R]
  }

  def compile4[A, B, C, D, R](f: (Exp[A], Exp[B], Exp[C], Exp[D]) => Exp[R])
                             (implicit mA: Manifest[A],
                              mB: Manifest[B],
                              mC: Manifest[C],
                              mD: Manifest[D],
                              mR: Manifest[R]): (A, B, C, D) => R = {
    val className = freshClass
    val source = new StringWriter()
    val staticData = codegen.emitSource4(f, className, new PrintWriter(source))
    compileAny(className, staticData, source.toString).asInstanceOf[(A, B, C, D) => R]
  }

  def compile5[A, B, C, D, E, R](f: (Exp[A], Exp[B], Exp[C], Exp[D], Exp[E]) => Exp[R])
                                (implicit mA: Manifest[A],
                                 mB: Manifest[B],
                                 mC: Manifest[C],
                                 mD: Manifest[D],
                                 mE: Manifest[E],
                                 mR: Manifest[R]): (A, B, C, D, E) => R = {
    val className = freshClass
    val source = new StringWriter()
    val staticData = codegen.emitSource5(f, className, new PrintWriter(source))
    compileAny(className, staticData, source.toString).asInstanceOf[(A, B, C, D, E) => R]
  }
}
