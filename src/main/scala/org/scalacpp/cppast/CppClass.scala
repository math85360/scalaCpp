package org.scalacpp.cppast

case class CppClass(mods: CppModifier, name: String, defs: List[CppDef]) {
  def toCpp() = {
    defs.map(_.toCpp(this)).mkString("\n\n")
  }

  def toH() = {
    defs
      .groupBy(_.mods.visibilityModifier)
      .map({ case (grp, lst) => lst.map(_.toH(this)).mkString(s"$grp:\n", "\n", "\n") })
      .mkString(s"class $name {\n", "\n", "}")
  }
}