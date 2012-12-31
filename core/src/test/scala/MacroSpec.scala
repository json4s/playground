
import org.specs2.mutable.Specification

case class Junk(in:Int, in2:String)
case class MutableJunk(var in:Int,var in2:String)
case class ThingWithJunk(name:String, junk:Junk)
case class Crazy(name:String,thg:ThingWithJunk)
case class WithOption(in:Int,opt:Option[String])
case class OptionOption(in:Option[Option[Int]])

class MacroSpec extends Specification {
  val refJunk = Junk(2,"cats")
  val refJunkDict = Map("in"->refJunk.in.toString,"in2"->refJunk.in2)
  
  "Macros" should {
    "Give me a hello world!" in {
	  Macros.hello must_== "Hello world!"
	}
	
	"Generate a Junk" in {
	  Macros.classBuilder[Junk](refJunkDict) must_== refJunk
	}
	
	"Generate a MutableJunk" in {
	  Macros.classBuilder[MutableJunk](refJunkDict) must_== MutableJunk(2,"cats")
	}
	
	"Throw ParseException with a bad map value for 'in'" in {
	  Macros.classBuilder[Junk](Map("in"->"2ffds","in2"->"cats")
		) must throwA[ParseException](message="Error parsing value 'in' to Int")
	}
	
	"Generate a recursive Option" in {
	  val expected = OptionOption(Some(Some(5)))
	  val stuff = Map("in"->"5")
	  
	  val result = Macros.classBuilder[OptionOption](stuff)
	  
	  result must_== expected
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
	
	
	"Instance a case class with an Option" in {
	  val expected = WithOption(2,Some("Pizza pockets forever!"))
	  val params = Map("in"->"2","opt"->"Pizza pockets forever!")
	  Macros.classBuilder[WithOption](params) must_== expected
	}
	
	"Instance a case class with a missing Option" in {
	  val expected = WithOption(2,None)
	  val params = Map("in"->"2")
	  Macros.classBuilder[WithOption](params) must_== expected
	}
	
  }

}