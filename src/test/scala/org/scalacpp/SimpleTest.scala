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
        |//import scala.language.implicitConversions
        |object App {
        |  //implicit def longToString(x:Long) = f"[$x]"
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
        |    val data = Message(current, "test")
        |    if(data.msg.length() > 0) updateStrip(data.msg)
        |    data match {
        |      case Message(_, "test") => updateStrip("TEeeessssttt !")
        |      case Message(tm, msg) => updateStrip(msg+" "+tm)
        |    }
        |  }
        |  def millis() : Long = -1
        |  
        |  private def test2args(x:Long,y:Long) = x+y
        |  
        |}
        |case class Message(date:Long, msg:String)""".stripMargin
      compileCodeSnippet(code)
    }
  }
}