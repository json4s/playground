import org.specs2.mutable.Specification
import java.util.Date

import org.json4s._
import org.json4s.native.JsonMethods._

case class Simple(one:Int, two:String)
case class NotSimple(one:Int,simple:Simple)

class MacroSerializerSpec extends Specification {
  def jsonPrint(value:JValue) = compact(render(value))
  
  "Serializer should do primatives" in {
    val writer = JsonWriter.ast
    val sim = Simple(1,"three")
    Serialization.asyncBuilder(sim,"simple",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "Serializer should do compound objects" in {
    val writer = JsonWriter.ast
    val notSimple = NotSimple(1,Simple(1,"two"))
    Serialization.asyncBuilder(notSimple,"simple",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }

}