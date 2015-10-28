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
          impl.body.collect({
            case ValDef(mods, name, tpt, rhs) => (name.toString.trim, name.toString.trim + "_")
          }).mkString(";\n")
        case q"$mods def $name[..$tparams](...$vparams):$tpt = $rhs" => //DefDef(mods, name, tparams, vparams, tpt, rhs) =>
          //System.err.println(parameters.toString + "_" + name)
          //System.err.println(tparams.toString)
          //System.err.println(vparams.toString)
          val map = vparams.flatten.map(x => x.name.toString).map(x => (x.trim(), x.trim() + "_")).toMap
          val internalVar = parameters.copy(_variables = map)
          "[method] " + applyModifiers(mods) + getTypeFromScala(tpt).cppType + " " + parameters.methodName(name.toString) + "(" + convertArgList(vparams)(internalVar) + ") {\n" +
            (rhs match {
              case Literal(Constant(())) => "(empty)"
              case _ if tpt.toString.trim() equals "Unit" => "(void) " + parseTree(rhs)(internalVar.copy(_indent = 1)) + ";"
              case Block(lst, tpe) => "(block) " + lst.map(parseTree(_)(internalVar.copy(_indent = 1))).mkString(";\n") + "\nreturn " + parseTree(tpe)(internalVar.copy(_indent = 1))
              case _ => "(return) return " + parseTree(rhs)(internalVar.copy(_indent = 1)) + ";"
              //case Block(lst, tpe) => lst.map(parseTree(_)(parameters.copy(_indent = 1))).mkString("\n") + "\nreturn " + parseTree(tpe)(parameters.copy(_indent = 1))
              //case _ => "****"+rhs.getClass.toString
              //case _ => parseTree(rhs)
            }) + "\n}"
        case Literal(Constant(())) => "[4] "
        case Literal(Constant(x)) => f"[5] $x"
        //case x: Select if parameters.variables.contains(x.toString.trim) => f"[6a] ${getMethod(x)}"
        case x: Select => f"[6] ${getMethod(x)}"
        case Block(lst, tpe) => "[7] " + lst.map(parseTree(_)(parameters.copy(_indent = 1))).mkString("\n") + "\n" + parseTree(tpe)
        //tpe.toString
        case q"$mods val $name: $tpt = $rhs" =>
          f"[8] ${getTypeFromScala(tpt).cppType} ${parameters.getVariableName(name)} = ${parseTree(rhs)};"
        case q"$mods var $name: $tpt = $rhs" =>
          if (rhs.isEmpty) {
            val r = tpt.toString match {
              case "Long" => 0
              case "String" => "\"\""
            }
            f"${getTypeFromScala(tpt).cppType} ${parameters.getVariableName(name)} = $r;"
          } else {
            f"${getTypeFromScala(tpt).cppType} ${parameters.getVariableName(name)} = ${parseTree(rhs)};"
          }
        case Literal(x) => f"[A] $x"
        case q"$tpname.this" if tpname.toString == "App" => "[B]"
        case q"$tpname.this" => f"[C] $tpname"
        case q"$lhs = $rhs" => f"[D] ${parseTree(lhs)}=${parseTree(rhs)}"
        case q"if ($cond) $lhs else $rhs" =>
          f"""[E] if(${parseTree(cond)}) {
            ${parseTree(lhs)};
            [E]}else{
            ${parseTree(rhs)};
            }""".stripMargin
        /*"if(" + parseTree(cond)(parameters.copy(_indent = 1)) + ") {\n" +
            parseTree(lhs)(parameters.copy(_indent = 1)) + ";\n" +
            "}else{\n" +
            parseTree(rhs)(parameters.copy(_indent = 1)) + ";\n}"*/
        case q"$method(...$args)" if parameters.variables.contains(method.toString.trim) => f"${parameters.variables(method.toString.trim)}"
        case q"$method(...$args)" => f"[F] ${method.toString}(${convertArgsCall(args)})"
        case x =>
          "[H] !!!" + (x.getClass.toString) + " " + (x.toString)
      }
    }

    def getMethod(v: Tree)(implicit parameters: Parameters): String = {
      v match {
        case Select(qualifier, name) if qualifier.toString().endsWith(".this") => parameters.variables.getOrElse(name.toString.trim, f"$name")
        case Select(qualifier, name) => f"$qualifier.$name"
        case Ident(name) => f"${parameters.variables.getOrElse(name.toString.trim, name.toString.trim)}"
      }
    }

    def convertArgList(lst: List[List[ValDef]])(implicit parameters: Parameters) = {
      lst.flatten.map(x => getTypeFromScala(x.tpt).cppType + " " + parameters.variables(x.name.toString)).mkString(", ")
    }
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

  case class Parameters(
      packageName: Seq[String],
      clsName: Seq[String],
      indent: Int = 0,
      variables: Map[String, String] = Map()) {
    def copy(
      _packageName: Option[String] = None,
      _clsName: Option[String] = None,
      _indent: Int = 0,
      _variables: Map[String, String] = Map()) =
      new Parameters(
        packageName ++ _packageName,
        clsName ++ _clsName,
        indent + _indent,
        variables ++ _variables)

    def bol = "  " * (indent * 2)
    def methodName(name: String) = ((Seq[String]() ++ packageName ++ clsName) :+ name).map(_.replace('.', '_')).mkString("_")
    def getVariableName(name: String): String = variables(name.trim)
    def getVariableName(name: TermName): String = getVariableName(name.toString)
    def getVariableName(name: Name): String = getVariableName(name.toString)
  }

}

case class ScalaToCppType(scalaType: String, cppType: String, defaultValue: String)

object Parameters {

}