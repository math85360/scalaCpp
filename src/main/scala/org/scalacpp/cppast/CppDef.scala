package org.scalacpp.cppast

trait CppDef {
  def mods: CppModifier

  def toCpp(cls: CppClass): String

  def toH(cls: CppClass): String
}

