import language.experimental.macros
import scala.reflect.macros.Context

import scala.collection.mutable.{ListBuffer, Stack}

import java.text.SimpleDateFormat
import java.util.Date

class DateFormat(fmt:String) extends SimpleDateFormat(fmt)
//implicit val defaultDateFormat = new java.text.SimpleDateFormat("EEE MMM d HH:mm:ss zzz yyyy")


class ParseException(message: String, cause: Exception) extends Exception(message, cause)

object Macros {
  
  type ParamsTpe = Map[String,String]
  
  def getArg(name:String, in:ParamsTpe) = in(name)
  
  def tester(in1:Int)(implicit in: Int) = macro testerimpl
  def testerimpl(c:Context)(in1:c.Expr[Int])(in:c.Expr[Int]):c.Expr[Int] = {
    import c.universe._
	import Flag._
	
	def cats(in:Tree):Tree = {
	  val in2 = c.Expr[Int](in)
	  reify {
	    in2.splice*in2.splice
	  }.tree
	}
	
	in
  }
  
  def classBuilder[U](params: ParamsTpe,name:String)(implicit dtf:DateFormat) = macro classbuilder[U]
  def classbuilder[U: c.WeakTypeTag](c: Context)(params: c.Expr[ParamsTpe],name:c.Expr[String])
									(dtf:c.Expr[DateFormat]):c.Expr[U] = {
    import c.universe._
	import Flag._
	
	def LIT[B](x:B) = c.Expr[B](Literal(Constant(x)))
	def CONCAT(a:c.Expr[String],b:c.Expr[String]) = reify{a.splice+b.splice}
	
	// Problem... buildObject cannot work for lists etc because the length is 
	// not known at compile time. buildObject needs compile time known names...
	// Could always just cut down the params here and pass that on...
	def rparseList(tpe:Type,name:c.Expr[String]):Tree = {
	  val TypeRef(_,_,List(argTpe)) = tpe
	  val freshNme = newTermName(c.fresh("nme$"))
	  
	  reify{
		params.splice.keys.filter{
		  _.startsWith(name.splice)	// Remove keys that are not part of the list of objects
		}.map{ k => 
		  k.subSequence(name.splice.length+1,k.length) // Remove first part and the period
		}.map{ k=>
		  k.toString.split("""\.""",2)(0)	// Basically just keep the list index
		}.toList.distinct.sorted.map{ k =>
		  c.Expr(ValDef(	// Need to use a fresh name to avoid stuff like 'val nme = nme'
			        Modifiers(), 
			        freshNme,
                    TypeTree(typeOf[String]), 
                    CONCAT(CONCAT(name,LIT(".")),c.Expr[String](Ident("k"))).tree
                  )).splice
		  // Now insert the actual build object code here
		  c.Expr(buildObject(argTpe,c.Expr[String](Ident(freshNme)))).splice 
		}
	  }.tree
	}
	
	def rparseDate(iname:c.Expr[String])   = reify { 
	  val name = iname.splice
	  
	  (dtf.splice).parse(getArg(name,params.splice))
	}
	
	def rparseInt(iname:c.Expr[String])    = reify { val name = iname.splice
	  try {
		getArg(name,params.splice).toInt
	  } catch {
	    case e: java.lang.NumberFormatException => throw new ParseException(
		s"Error parsing value '$name' to Int", e)
	  }
	}
	
	def rparseLong(iname:c.Expr[String])    = reify { val name = iname.splice
	  try {
		getArg(name,params.splice).toLong
	  } catch {
	    case e: java.lang.NumberFormatException => throw new ParseException(
		s"Error parsing value '$name' to Long", e)
	  }
	}
	
	def rparseFloat(iname:c.Expr[String])    = reify { val name = iname.splice
	  try {
		getArg(name,params.splice).toFloat
	  } catch {
	    case e: java.lang.NumberFormatException => throw new ParseException(
		s"Error parsing value '$name' to Float", e)
	  }
	}
	
	def rparseDouble(iname:c.Expr[String])    = reify { val name = iname.splice
	  try {
		getArg(name,params.splice).toDouble
	  } catch {
	    case e: java.lang.NumberFormatException => throw new ParseException(
		s"Error parsing value '$name' to Double", e)
	  }
	}
	
	def rparseString(name:c.Expr[String]) = reify{
	  getArg(name.splice,params.splice)
	}
	
	def rparseOption(tpe:Type,name:c.Expr[String]):Tree = {
	  // Cant come crom typeSymbol.typeSignature. Not the same!
	  val TypeRef(_,_,List(argTpe)) = tpe
	  reify{
	    try{  // Expr is of type argTpe
		  Some(c.Expr(buildObject(argTpe,name)).splice)
		} catch {
		  case _: java.util.NoSuchElementException => None
		}
	  }.tree
	  
	}
	
	def buildObject(tpe: Type, name:c.Expr[String]):Tree = {
	  if      (tpe =:= typeOf[Int])    { rparseInt(name).tree    }
	  else if (tpe =:= typeOf[Long])   { rparseLong(name).tree   }
	  else if (tpe =:= typeOf[Float])  { rparseFloat(name).tree  }
	  else if (tpe =:= typeOf[Double]) { rparseDouble(name).tree }
	  else if (tpe =:= typeOf[String]) { rparseString(name).tree }
	  else if (tpe =:= typeOf[Date])   { rparseDate(name).tree	 }
	  // The privlaged types
	  else if (tpe.erasure =:= typeOf[Option[Any]]) {
	    rparseOption(tpe,name)
      }
	  else if (tpe.erasure =:= typeOf[List[Any]]) {
	    rparseList(tpe,name)
	  }
	  // Must be a complex object. Hopefully it can be instanced normally
	  else {
	    val sym = tpe.typeSymbol  
	    val ctorM = tpe.member(nme.CONSTRUCTOR).asMethod
	    val ctorParams = ctorM.paramss
		
		// Create new object, and fill params with error catching expressions
		// If param takes defaults, try to find the val in map, or call 
		// default evaluation from its companion object
	    New(sym,(ctorParams(0).zipWithIndex.map { case (pSym,index) => 
		  // gen list of trees that eval to params
	      val pTpe = pSym.typeSignature
		  val compName = CONCAT(CONCAT(name,LIT(".")),LIT(pSym.name.decoded))
		  
		  if (pSym.asTerm.isParamWithDefault) {
		    //println(s"-------------------- $sym --------------------") //debug
			reify {
			  try {
			    c.Expr(buildObject(pTpe, compName)).splice // splice in another obj tree
			  } catch {
			    case e: java.util.NoSuchElementException =>
				  // Need to use the origional symbol.companionObj to get defaults
				  // Would be better to find the generated TermNames if possible
				  
				  /* Tries to throw a "partially defined" optional param error.
				   * This is not working. It catches when elements have names 
				   * that are substrings of each other such as IN and INa
				  if((params.splice).keys.map{ x=>
				    println(s"Key: $x")
				    x.startsWith(LIT(name).splice)
				  }.reduce(_&&_)) throw e
				  */
				  
				  c.Expr(Select(Ident(sym.companionSymbol),newTermName(
				    "$lessinit$greater$default$" + (index+1).toString))
				  ).splice
			  }
			}.tree
		  } else buildObject(pTpe, compName) // Required
		  
	    }):_*)
		// Here is where we need to populate mutable fields not in constructor
		
	  }
	}
	
    val tpe = weakTypeOf[U]
	
	c.Expr[U](buildObject(tpe,name))
  }
}
