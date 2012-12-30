
import org.specs2.mutable.Specification

case class Junk(in:Int, in2:String)
case class ThingWithJunk(name:String, junk:Junk)
case class Crazy(name:String,thg:ThingWithJunk)

class MacroSpec extends Specification {
  "Macros" should {
    "Give me a hello world!" in {
	  Macros.hello must_== "Hello world!"
	}
	
	"Generate a Junk" in {
	  val stuff = Map("in"->"2","in2"->"cats")
	  Macros.classBuilder[Junk](stuff) must_== Junk(2,"cats")
	}
	
	"Generate a ThingWithJunk" in {
	  val expected = ThingWithJunk("Bob",Junk(2,"SomeJunk..."))
	  val stuff = Map("name"->expected.name,"junk.in"->expected.junk.in.toString,
					  "junk.in2"->expected.junk.in2)
					  
	  val result = Macros.classBuilder[ThingWithJunk](stuff)
	
	  result must_== expected
	}
	
	"Generate a 3 fold deap case class" in {
	  val expected = Crazy("crazyBob...",ThingWithJunk("Bob",Junk(2,"SomeJunk...")))
	  val stuff = Map("name"->expected.name,"thg.name"->expected.thg.name,"thg.junk.in"->expected.thg.junk.in.toString,
					  "thg.junk.in2"->expected.thg.junk.in2)
					  
	  val result = Macros.classBuilder[Crazy](stuff)
	  result must_== expected
	}
  }

}