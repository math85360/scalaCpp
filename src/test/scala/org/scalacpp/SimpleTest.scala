package org.scalacpp

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.OneInstancePerTest

/**
 * @author mathi_000
 */
class SimpleTest extends FreeSpec with Matchers with PluginRunner with OneInstancePerTest {
  "Basic sample" - {
    "should be ok" in {
      val code = """
        |object App {
        |  var last_event : Long = -3600000
        |  
        |  var lastMessage : String = _
        |  
        |  def updateStrip(data:String) {
        |    if(data.length()>0){
        |      lastMessage = data
        |      last_event = millis()
        |    }
        |  }
        |  
        |  var current: Long = _
        |  
        |  def setup() {
        |  }
        |
        |  def loop() {
        |    current = millis()
        |    val data = "test"
        |    if(data.length() > 0) updateStrip(data)
        |  }
        |  def millis() : Long = -1
        |  
        |  private def test2args(x:Long,y:Long) = x+y
        |}""".stripMargin
      compileCodeSnippet(code)
    }
  }
}