import org.specs2.mutable.Specification
import java.util.Date

import org.json4s._
import org.json4s.native.JsonMethods._

case class Simple(one:Int, two:String)
case class NotSimple(one:Int,simple:Simple)

class MacroSerializerSpec extends Specification {
  sequential
  
  def jsonPrint(value:JValue) = compact(render(value))
  
  "Serializer should do primatives" in {
    val writer = JsonWriter.ast
    Serialization.serObj(453,"number",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "Serializer should do primative case classes" in {
    val writer = JsonWriter.ast
    val sim = Simple(1,"three")
    Serialization.serObj(sim,"simp",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "Serializer should do compound objects" in {
    val writer = JsonWriter.ast
    val notSimple = NotSimple(1,Simple(1,"two"))
    Serialization.serObj(notSimple,"NotSimple",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "Serialize a present Option" in {
    val writer = JsonWriter.ast
    val a:Option[Int] = Some(43)
    Serialization.serObj(a,"numberOpt",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "Serialize a present Optional complex type" in {
    val writer = JsonWriter.ast
    val a:Option[Simple] = Some(Simple(1,"twotwo"))
    Serialization.serObj(a,"SimpleOpt",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "Serialize a missing Option" in {
    val writer = JsonWriter.ast
    val a:Option[Int] = None
    Serialization.serObj(a,"missing_number",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "serialize a List of primatives" in {
    val writer = JsonWriter.ast
    val a = 4::5::6::Nil
    Serialization.serObj(a,"my_list",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "serialize a List of Simples" in {
    val writer = JsonWriter.ast
    val a = Simple(4,"four")::Simple(5,"five")::Simple(6,"six")::Nil
    Serialization.serObj(a,"my_list",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "serialize a Map[Str,Int]" in {
    val writer = JsonWriter.ast
    val a = Map("one"->1,"two"->2,"three"->3)
    Serialization.serObj(a,"my_map",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "serialize a Map[Str,Simple(Int,String)]" in {
    val writer = JsonWriter.ast
    val a = Map("one"->Simple(1,"one"),"two"->Simple(2,"two"),"three"->Simple(3,"three"))
    Serialization.serObj(a,"my_map",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
}