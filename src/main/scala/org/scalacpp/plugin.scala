package org.scalacpp

import scala.tools.nsc._
import scala.tools.nsc.plugins.{ Plugin, PluginComponent }
import scala.tools.nsc.transform.{ Transform, TypingTransformers }
import java.io.Console

/**
 * @author mathi_000
 */
class ScalaCppPlugin(val global: Global) extends Plugin {
  override val name = "scalacpp"

  override val description = "scala to c++ compiler plugin"

  val component = new ScalaCppComponent(global)

  override val components = List[PluginComponent](component)
}

class ScalaCppComponent(val global: Global) extends PluginComponent with TypingTransformers with Transform {

  import global._

  override val phaseName = "scalacpp"
  override val runsAfter = List[String]("typer")
  override val runsBefore = List[String]("patmat")

  override def newPhase(prev: scala.tools.nsc.Phase) = new Phase(prev) {
    override def run(): Unit = {
      super.run()
    }
  }

  protected def newTransformer(unit: CompilationUnit): Transformer = {
    new Transformer(unit)
  }
  class Transformer(unit: global.CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: global.Tree): global.Tree = {
      //unit.source.path
      //System.err.println(tree)
      System.err.println(parseTree(tree))
      tree
    }
    implicit def termToOptionString(x: TermName) = if (x.isEmpty || "App" == x.toString) None else Some(x.toString)
    implicit def typeToOptionString(x: TypeName) = if (x.isEmpty) None else Some(x.toString)
    implicit def refToOptionString(x: RefTree) = if (!x.isDef) None else Some(x.toString)
    def parseTree(tree: global.Tree)(implicit ctx: Context = Context(Seq(), Seq())): String = {
      tree match {
        case PackageDef(ref, lst) =>
          //System.err.println("package is "+ref.name)
          lst.map(parseTree(_)(ctx.copy(_packageName = ref))).mkString("\n")
        case ClassDef(modifiers, tpeName, tparams, impl) =>
          //System.err.println("class "+tpeName)
          var visited = Seq[Tree]()
          // visit each field val/var and add it to visited
          // next do all impl
          //"[class] " + impl.body.map(parseTree(_)(parameters.copy(_clsName = tpeName, _indent = 1))).mkString("\n")
          ""
        case ModuleDef(modifiers, tpeName, impl) =>
          //System.err.println("module "+tpeName)
          /*val map = impl.collect({
            case ValDef(mods, name, tpt, rhs) => (name.toString.trim, name.toString.trim + "_")
          }).toMap
          "[module] " + impl.body.map(parseTree(_)(parameters.copy(_clsName = tpeName, _indent = 1, _variables =
          map))).mkString("\n")*/
          val map = impl.body.collect({
            case x @ ValDef(mods, name, tpt, rhs) => (name, "$" + name.toString.trim)
            case x @ DefDef(mods, name, tparams, vparams, tpt, rhs) => (name, name.toString.trim) //(name.toString.trim, name.toString.trim)
            //case ValDef(mods, name, tpt, rhs) => (name.toString.trim, name.toString.trim + "_")
            //case DefDef(mods, name, tparams, vparams, tpt, rhs) => (name.toString.trim, name.toString.trim)
          }).toMap
          val newCtx = ctx.copy(_clsName = tpeName, _names = map)
          impl.body.map(parseTree(_)(newCtx)).mkString("\n\n")
        case ValDef(mods, name, tpt, rhs) =>
          val tpe = getTypeFromScala(tpt)
          val res = (if (rhs.isEmpty) tpe.defaultValue else parseTree(rhs))
          try {
            s"${tpe.cppType} ${ctx.names(name)} = $res"
          } catch {
            case e: Throwable =>
              System.out.println(ctx.names.keys)
              throw e
          }
        case DefDef(mods, name, tparams, vparams, tpt, rhs) =>
          val tpe = getTypeFromScala(tpt)
          //val body = if(tpe.scalaType=="Unit") parseTree(rhs)
          //val body = if(tpe.scalaType=="U")
          /*System.out.println(vparams.flatten.collect({
            case ValDef(_,y,_,_) => "-"+y+"-"
            case x => x.toString
          }).mkString("["," ** ","]"))*/
          val map = (vparams.flatten.collect({
            case x @ ValDef(mods, name, tpt, rhs) => (name, "$" + name.toString.trim)
          }) ++ (rhs.collect({ case x @ ValDef(_, name, tpt, _) => (name, "$" + name.toString.trim) }))).toMap
          val args = vparams.flatten.collect({ case x @ ValDef(_, name, tpt, _) => getTypeFromScala(tpt).cppType + " $" + name.toString.trim }).mkString(",")
          val newCtx = ctx.copy(_names = map)
          val z = newCtx.names.keys
          val body = rhs match {
            case x if tpe.scalaType == "Unit" => parseTree(rhs)(newCtx) + ";"
            case Block(lh, rh) => lh.map(parseTree(_)(newCtx)).mkString(";\n") + ";\nreturn " + parseTree(rh)(newCtx) + ";\n"
            case _ => "return " + parseTree(rhs)(newCtx) + ";"
          }
          s"${tpe.cppType} ${newCtx.names(name)}($args) {\n$body\n}"
        case Block(lhs, rhs) => lhs.map(parseTree(_)).mkString(";\n") + ";\n" + parseTree(rhs)
        case Assign(lhs, rhs) => parseTree(lhs) + " = " + parseTree(rhs)
        case Apply(qlf, name) if true =>parseTree(qlf) + "(" + name.map(parseTree(_)).mkString(", ") + ")"
        case Select(qlf, name: TermName) if qlf.toString == (ctx.currentLevel + ".this") => ctx.names(name)
        case Select(qlf, name) =>
          val lhs = qlf.toString
          name.toString match {
            case "$eq$eq" => lhs + " == "
            case "$plus" => lhs + " + "
            case "$minus" => lhs + " - "
            case "*" => lhs + " * "
            case "$div" => lhs + " / "
            case "$bang$eq" => lhs + " != "
            case "$less" => lhs + " < "
            case "$less$eq" => lhs + " <= "
            case "$greater" => lhs + " > "
            case "$greater$eq" => lhs + " >= "
            case _ => qlf + " | " + name + " | " + ctx.currentLevel
          }
        case Select(qlf, name) => qlf + " | " + name + " | " + ctx.currentLevel
        case Literal(Constant(())) => ""
        case If(cond, lhs, rhs) =>
          val r = if (rhs.isEmpty) "" else s"else {${parseTree(rhs)}}"
          s"if(${parseTree(cond)}) {${parseTree(lhs)}})"
        case x @ Literal(z @ Constant(v)) => if (z.isNumeric) v.toString else z.escapedStringValue
        case x @ Ident(v:TermName) => ctx.names(v).toString 
        case x =>
          "/*" + x.toString + "-" + x.getClass.getSimpleName + "*/"
      }
    }

    /*def getMethod(v: Tree)(implicit ctx: Context): String = {
      v match {
        case Select(qualifier, name) if qualifier.toString().endsWith(".this") => ctx.variables.getOrElse(name.toString.trim, f"$name")
        case Select(qualifier, name) => f"$qualifier.$name"
        case Ident(name) => f"${parameters.variables.getOrElse(name.toString.trim, name.toString.trim)}"
      }
    }*/

    /*def convertArgList(lst: List[List[ValDef]])(implicit parameters: Context) = {
      lst.flatten.map(x => getTypeFromScala(x.tpt).cppType + " " + parameters.variables(x.name.toString)).mkString(", ")
    }*/
    def convertArgsCall(lst: List[List[Tree]]) = {
      lst.flatten.map(x => parseTree(x)).mkString(", ")
    }
    val applyModifiers: PartialFunction[Modifiers, String] = {
      case x => ""
    }
    /*val cppType: PartialFunction[String, String] = {
      case "Int" => "int"
      case "Long" => "long"
      case "Double" => "double"
      case "String" => "String"
      case "Char" => "char"
      case "Unit" => "void"
      case x => x
    }*/

    val scalaToCppTypes = Seq(
      ScalaToCppType("Int", "int", "0"),
      ScalaToCppType("Long", "long", "0"),
      ScalaToCppType("Double", "double", "0.0"),
      ScalaToCppType("String", "String", "\"\""),
      ScalaToCppType("Char", "char", "0"),
      ScalaToCppType("Unit", "void", ""),
      ScalaToCppType("Int", "int", "0"))

    def getTypeFromScala(scalaType: String): ScalaToCppType = scalaToCppTypes.find(_.scalaType == scalaType).getOrElse(ScalaToCppType(scalaType, scalaType, "null"))
    def getTypeFromScala(tree: Tree): ScalaToCppType = getTypeFromScala(tree.toString.trim)
  }

  case class Context(
      packageName: Seq[String],
      clsName: Seq[String],
      indent: Int = 0,
      names: Map[TermName, String] = Map()) {
    def copy(
      _packageName: Option[String] = None,
      _clsName: TermName = null,
      _indent: Int = 0,
      _names: Map[TermName, String] = Map()) =
      new Context(
        packageName ++ _packageName,
        (if (_clsName == null) clsName else (clsName :+ _clsName.toString())),
        indent + _indent,
        names ++ _names)

    def bol = "  " * (indent * 2)
    def methodName(name: String) = ((Seq[String]() ++ packageName ++ clsName) :+ name).map(_.replace('.', '_')).mkString("_")
    def currentLevel = (packageName ++ clsName).mkString(".")
  }

}

case class ScalaToCppType(scalaType: String, cppType: String, defaultValue: String)
