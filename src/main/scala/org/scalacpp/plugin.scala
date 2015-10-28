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
      System.err.println(parseTree(tree))
      tree
    }
    implicit def termToOptionString(x: TermName) = if (x.isEmpty || "App" == x.toString) None else Some(x.toString)
    implicit def typeToOptionString(x: TypeName) = if (x.isEmpty) None else Some(x.toString)
    implicit def refToOptionString(x: RefTree) = if (!x.isDef) None else Some(x.toString)
    def parseTree(tree: global.Tree)(implicit parameters: Parameters = Parameters(Seq(), Seq())): String = {
      tree match {
        case PackageDef(ref, lst) =>
          //System.err.println("package is "+ref.name)
          lst.map(parseTree(_)(parameters.copy(_packageName = ref))).mkString("\n")
        case ClassDef(modifiers, tpeName, tparams, impl) =>
          //System.err.println("class "+tpeName)
          impl.body.map(parseTree(_)(parameters.copy(_clsName = tpeName, _indent = 1))).mkString("\n")
        case ModuleDef(modifiers, tpeName, impl) =>
          //System.err.println("module "+tpeName)
          impl.body.map(parseTree(_)(parameters.copy(_clsName = tpeName, _indent = 1))).mkString("\n")
        case q"$mods def $name[..$tparams](...$vparams):$tpt = $rhs" => //DefDef(mods, name, tparams, vparams, tpt, rhs) =>
          //System.err.println(parameters.toString + "_" + name)
          //System.err.println(tparams.toString)
          //System.err.println(vparams.toString)
          applyModifiers(mods) + cppType(tpt.toString) + " " + parameters.methodName(name.toString) + "(" + convertArgList(vparams) + ") {\n" +
            (rhs match {
              case _ if tpt.toString.trim() equals "Unit" => parseTree(rhs)(parameters.copy(_indent = 1)) + ";" + rhs.tpe.toString + tpt.toString
              case Block(lst, tpe) => lst.map(parseTree(_)(parameters.copy(_indent = 1))).mkString(";\n") + "\nreturn " + parseTree(tpe)(parameters.copy(_indent = 1))
              case _ => "return " + parseTree(rhs)(parameters.copy(_indent = 1)) + ";"
              //case Block(lst, tpe) => lst.map(parseTree(_)(parameters.copy(_indent = 1))).mkString("\n") + "\nreturn " + parseTree(tpe)(parameters.copy(_indent = 1))
              //case _ => "****"+rhs.getClass.toString
              //case _ => parseTree(rhs)
            }) + "\n}"
        case Literal(Constant(())) => ""
        case Literal(Constant(x)) => f"$x"
        case Select(tree, name) =>
          f"$name"
        case Block(lst, tpe) =>
          lst.map(parseTree(_)(parameters.copy(_indent = 1))).mkString("\n") + "\n" + tpe.toString
        case q"$mods val $name: $tpt = $rhs" =>
          f"${cppType(tpt.toString)} $name = ${parseTree(rhs)};"
        case q"$mods var $name: $tpt = $rhs" =>
          f"${cppType(tpt.toString)} $name = ${parseTree(rhs)};"
        case Literal(x) =>
          f"$x"
        case q"$tpname.this" if tpname.toString == "App" => ""
        case q"$tpname.this" => tpname.toString()
        case q"$lhs = $rhs" => "${parseTree(lhs)} =  ${parseTree(lhs)}"

        case q"if ($cond) $lhs else $rhs" =>
          f"""if(${parseTree(cond)}) {
            ${parseTree(lhs)};
            }else{
            ${parseTree(rhs)};
            }""".stripMargin
        /*"if(" + parseTree(cond)(parameters.copy(_indent = 1)) + ") {\n" +
            parseTree(lhs)(parameters.copy(_indent = 1)) + ";\n" +
            "}else{\n" +
            parseTree(rhs)(parameters.copy(_indent = 1)) + ";\n}"*/
        case q"$method(...$args)" =>
          f"${method.getClass.toString}${method.toString()}(${convertArgsCall(args)})"
        case x =>
          "!!!" + (x.getClass.toString) + " " +
            (x.toString)
      }
    }
    def convertArgList(lst: List[List[ValDef]]) = {
      lst.flatten.map(x => cppType(x.tpt.toString) + " " + x.name.toString).mkString(", ")
    }
    def convertArgsCall(lst: List[List[Tree]]) = {
      lst.flatten.map(x => parseTree(x)).mkString(", ")
    }
    val applyModifiers: PartialFunction[Modifiers, String] = {
      case x if x.isPrivate => "private "
      case x => ""
    }
    val cppType: PartialFunction[String, String] = {
      case "Int" => "int"
      case "Long" => "long"
      case "Double" => "double"
      case "String" => "String"
      case "Char" => "char"
      case "Unit" => "void"
      case x => x
    }
  }
}

case class Parameters(
    packageName: Seq[String],
    clsName: Seq[String],
    indent: Int = 0) {
  def copy(
    _packageName: Option[String] = None,
    _clsName: Option[String] = None,
    _indent: Int = 0) =
    new Parameters(
      packageName ++ _packageName,
      clsName ++ _clsName,
      indent + _indent)

  def bol = "  " * (indent * 2)
  def methodName(name: String) = ((Seq[String]() ++ packageName ++ clsName) :+ name).map(_.replace('.', '_')).mkString("_")
}

object Parameters {

}