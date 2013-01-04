
import org.specs2.mutable.Specification
import java.util.Date

case class Junk(in:Int, in2:String)
case class MutableJunk(var in:Int,var in2:String)
case class ThingWithJunk(name:String, junk:Junk)
case class Crazy(name:String,thg:ThingWithJunk)
case class WithOption(in:Int,opt:Option[String])
case class OptionOption(in:Option[Option[Int]])
case class JunkWithDefault(in:Int, in2:String="Default...")
case class WithList(name:String, lst:List[Int])


case class WithDate(name:String, date: Date) {
  override def equals(in:Any) = in match {
    case that: WithDate => name == that.name && date.toString ==that.date.toString
	case _ => false
  }
}

class ClassWithDef(val in:Int=4) {
  override def toString = s"ClassWithDef(in:$in)"
  override def equals(obj:Any) = obj match {
	  case a:ClassWithDef => a.in == in
	  case _ => false
  }
}

case class ObjWithDefJunk(name:String, junk:Junk=Junk(-1,"Default"))

class MacroSpec extends Specification {
  val refJunk = Junk(2,"cats")
  val refJunkDict = Map("in"->refJunk.in.toString,"in2"->refJunk.in2)
  
  import java.text.SimpleDateFormat
  implicit val defaultDateFormat = new DateFormat("EEE MMM d HH:mm:ss zzz yyyy")
  
  
  "Macros" should {
    
	"Parse WithList" in {
	  val expected = WithList("Bob", 1::Nil)
	  val params = Map("name"->"Bob","lst0"->"1")
	  Macros.classBuilder[WithList](params) must_== expected
	}
	
	"Parse date info" in {
	  val expected = WithDate("Bob",new Date)
	  val params = Map("name"->"Bob","date"->expected.date.toString)
	  
	  Macros.classBuilder[WithDate](params) must_== expected
	}
    
	"Generate a Junk" in {
	  Macros.classBuilder[Junk](refJunkDict) must_== refJunk
	}
	
	"Generate a MutableJunk" in {
	  Macros.classBuilder[MutableJunk](refJunkDict) must_== MutableJunk(2,"cats")
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
	
	"Created ClassWithDef with param" in {
	  Macros.classBuilder[ClassWithDef](Map("in"->"1")) must_== (new ClassWithDef(1))
	}
	
	"Created ClassWithDef without param" in {
	  Macros.classBuilder[ClassWithDef](Map(""->"")) must_== (new ClassWithDef)
	}
	
	"Generate a JunkWithDefault with a value" in {
	  var expected = JunkWithDefault(refJunk.in,refJunk.in2)
	  Macros.classBuilder[JunkWithDefault](refJunkDict) must_== expected
	}
	
	"Generate a JunkWithDefault without a value" in {
	  var expected = JunkWithDefault(refJunk.in)
	  Macros.classBuilder[JunkWithDefault](Map("in"->"2")) must_== expected
	}
	"Created ObjWithDefJunk without junk" in {
	  val expected = ObjWithDefJunk("Name")
	  val map = Map("name"->"Name")
	  Macros.classBuilder[ObjWithDefJunk](map) must_== expected
	}
	
	"Created ObjWithDefJunk with provided junk" in {
	  val expected = ObjWithDefJunk("Name",Junk(2,"Provided"))
	  val map = Map("name"->"Name","junk.in"->"2","junk.in2"->"Provided")
	  Macros.classBuilder[ObjWithDefJunk](map) must_== expected
	}
	
	"Generate a recursive Option" in {
	  val expected = OptionOption(Some(Some(5)))
	  val stuff = Map("in"->"5")
	  
	  val result = Macros.classBuilder[OptionOption](stuff)
	  
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
	
	"Throw ParseException with a bad map value for 'in'" in {
	  Macros.classBuilder[Junk](Map("in"->"2ffds","in2"->"cats")
		) must throwA[ParseException](message="Error parsing value 'in' to Int")
	}
	
  }

}