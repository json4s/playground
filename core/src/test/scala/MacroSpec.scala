
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
case class WithObjList(name:String, list:List[ThingWithJunk])

case class Bill(in:Int)

class Billy[U](in:U)
case class BillyB(in:Int) extends Billy[Int](in)

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
  val refJunkDict = Map("d.in"->refJunk.in.toString,"d.in2"->refJunk.in2)
  
  import java.text.SimpleDateFormat
  implicit val defaultDateFormat = new DateFormat("EEE MMM d HH:mm:ss zzz yyyy")
  
  
  "Macros" should {
  
	"Parse WithList" in {
	  val expected = WithList("Bob", 1::4::Nil)
	  val params = Map("d.name"->"Bob","d.lst0"->"1","d.lst1"->"4")
	  Macros.classBuilder[WithList](params,"d") must_== expected
	}
	
	"parse WithObjList" in {
	//case class ThingWithJunk(name:String, junk:Junk)
      val expected = WithObjList("Bob",ThingWithJunk("Bobby",Junk(1,"one"))::ThingWithJunk("Bill",Junk(2,"two"))::Nil)
      val params = Map("d.name"->"Bob","d.list1.name"->"Bill","d.list0.name"->"Bobby","d.list0.junk.in"->"1","d.list0.junk.in2"->"one","d.list1.junk.in"->"2","d.list1.junk.in2"->"two")
      Macros.classBuilder[WithObjList](params,"d") must_== expected
    }
	
	"parse List[Bill]" in {
	//case class ThingWithJunk(name:String, junk:Junk)
      val expected = Bill(1)::Bill(3)::Nil
      val params = Map("d0.in"->"1","d0.1"->"2","d1.in"->"3","d1.d1"->"4")
      Macros.classBuilder[List[Bill]](params,"d") must_== expected
    }
	/*	// This fails to compile: complains about type parameters for List
	"parse List[List[Int]]" in {
	//case class ThingWithJunk(name:String, junk:Junk)
      val expected = (1::2::Nil)::(3::4::Nil)::Nil
      val params = Map("d0.in"->"1","d0.1"->"2","d1.in"->"3","d1.d1"->"4")
	  
      val result = Macros.classBuilder[List[List[Int]]](params,"d")
	  
	  result must_== expected
    }
	*/
	
	"parse BillyB which extends Billy[Int]" in {
	  val expected = BillyB(3)
	  val params = Map("d.in"->"3")
	  Macros.classBuilder[BillyB](params,"d") must_== expected
	}
	
	"Primatives" in {
	  val expected = 5
	  val params = Map("d"->"5")
	  Macros.classBuilder[Int](params,"d") must_== expected
	}
	
	"Parse date info" in {
	  val expected = WithDate("Bob",new Date)
	  val params = Map("d.name"->"Bob","d.date"->expected.date.toString)
	  
	  Macros.classBuilder[WithDate](params,"d") must_== expected
	}
    
	"Generate a Junk" in {
	  Macros.classBuilder[Junk](refJunkDict,"d") must_== refJunk
	}
	
	"Generate a MutableJunk" in {
	  Macros.classBuilder[MutableJunk](refJunkDict,"d") must_== MutableJunk(2,"cats")
	}
	"Generate a ThingWithJunk" in {
	  val expected = ThingWithJunk("Bob",Junk(2,"SomeJunk..."))
	  val stuff = Map("d.name"->expected.name,"d.junk.in"->expected.junk.in.toString,
					  "d.junk.in2"->expected.junk.in2)
					  
	  val result = Macros.classBuilder[ThingWithJunk](stuff,"d")
	
	  result must_== expected
	}
	
	"Generate a 3 fold deap case class" in {
	  val expected = Crazy("crazyBob...",ThingWithJunk("Bob",Junk(2,"SomeJunk...")))
	  val stuff = Map("d.name"->expected.name,"d.thg.name"->expected.thg.name,"d.thg.junk.in"->expected.thg.junk.in.toString,
					  "d.thg.junk.in2"->expected.thg.junk.in2)
					  
	  val result = Macros.classBuilder[Crazy](stuff,"d")
	  result must_== expected
	}
	
	"Created ClassWithDef with param" in {
	  Macros.classBuilder[ClassWithDef](Map("d.in"->"1"),"d") must_== (new ClassWithDef(1))
	}
	
	"Created ClassWithDef without param" in {
	  Macros.classBuilder[ClassWithDef](Map(""->""),"d") must_== (new ClassWithDef)
	}
	
	"Generate a JunkWithDefault with a value" in {
	  var expected = JunkWithDefault(refJunk.in,refJunk.in2)
	  Macros.classBuilder[JunkWithDefault](refJunkDict,"d") must_== expected
	}
	
	"Generate a JunkWithDefault without a value" in {
	  var expected = JunkWithDefault(refJunk.in)
	  Macros.classBuilder[JunkWithDefault](Map("d.in"->"2"),"d") must_== expected
	}
	
	"Created ObjWithDefJunk without junk" in {
	  val expected = ObjWithDefJunk("Name")
	  val map = Map("d.name"->"Name")
	  Macros.classBuilder[ObjWithDefJunk](map,"d") must_== expected
	}
	
	"Created ObjWithDefJunk with provided junk" in {
	  val expected = ObjWithDefJunk("Name",Junk(2,"Provided"))
	  val map = Map("d.name"->"Name","d.junk.in"->"2","d.junk.in2"->"Provided")
	  Macros.classBuilder[ObjWithDefJunk](map,"d") must_== expected
	}
	
	"Generate a recursive Option" in {
	  val expected = OptionOption(Some(Some(5)))
	  val stuff = Map("d.in"->"5")
	  
	  val result = Macros.classBuilder[OptionOption](stuff,"d")
	  
	  result must_== expected
	}
	
	"Instance a case class with an Option" in {
	  val expected = WithOption(2,Some("Pizza pockets forever!"))
	  val params = Map("d.in"->"2","d.opt"->"Pizza pockets forever!")
	  Macros.classBuilder[WithOption](params,"d") must_== expected
	}
	
	"Instance a case class with a missing Option" in {
	  val expected = WithOption(2,None)
	  val params = Map("d.in"->"2")
	  Macros.classBuilder[WithOption](params,"d") must_== expected
	}
	
	"Throw ParseException with a bad map value for 'in'" in {
	  Macros.classBuilder[Junk](Map("d.in"->"2ffds","d.in2"->"cats"),"d"
		) must throwA[ParseException](message="Error parsing value 'd.in' to Int")
	}
	
  }
}