package org.scalacpp.cppast

case class CppModifier(staticMod: Boolean, privateMod: Boolean) {
  def staticModifier = if(staticMod) "static" else ""
  
  def stringify() = {
    var r = Seq[String]()
    if (staticMod) r = r :+ "static"
    if (privateMod) r = r :+ "private"
    r.mkString(" ")
  }

  def visibilityModifier = if (privateMod) "private" else "public"
}
