import language.experimental.macros
import scala.reflect.macros.Context

import scala.collection.mutable.{ListBuffer, Stack}


class ParseException(message: String, cause: Exception) extends Exception(message, cause)

object Macros {
  def hello = macro helloimpl
  def helloimpl(c: Context):c.Expr[String] = {
    import c.universe._
  
    c.Expr[String](Literal(Constant("Hello world!")))
  }
  
  def inst[A] = macro instimpl[A]
  def instimpl[A: c.WeakTypeTag](c: Context):c.Expr[A] = {
    import c.universe._
	
	def LIT[U](x:U) = c.Expr[U](Literal(Constant(x)))
	
	val params = List(List(LIT(4).tree,LIT("Cats...").tree))
	
	val sym = weakTypeOf[A].typeSymbol
	
    val tree = New(sym,params(0):_*)
	c.Expr[A](tree)
  } 
  
  type ParamsTpe = Map[String,String]
  
  def getArg(name:String, in:ParamsTpe) = in(name)
  
  
  def classBuilder[A](params: ParamsTpe) = macro classbuilder[A]
  def classbuilder[A: c.WeakTypeTag](c: Context)(params: c.Expr[ParamsTpe]):c.Expr[A] = {
    import c.universe._
	
	def LIT[U](x:U) = c.Expr[U](Literal(Constant(x)))
	//def mkSome(t: Tree) = 
	
	def rparseInt(iname:String)    = reify { val name = LIT(iname).splice
	  try {
		getArg(name,params.splice).toInt
	  } catch {
	    case e: java.lang.NumberFormatException => throw new ParseException(
		s"Error parsing value '$name' to Int", e)
	  }
	}
	
	def rparseLong(iname:String)    = reify { val name = LIT(iname).splice
	  try {
		getArg(name,params.splice).toLong
	  } catch {
	    case e: java.lang.NumberFormatException => throw new ParseException(
		s"Error parsing value '$name' to Long", e)
	  }
	}
	
	def rparseFloat(iname:String)    = reify { val name = LIT(iname).splice
	  try {
		getArg(name,params.splice).toFloat
	  } catch {
	    case e: java.lang.NumberFormatException => throw new ParseException(
		s"Error parsing value '$name' to Float", e)
	  }
	}
	
	def rparseDouble(iname:String)    = reify { val name = LIT(iname).splice
	  try {
		getArg(name,params.splice).toDouble
	  } catch {
	    case e: java.lang.NumberFormatException => throw new ParseException(
		s"Error parsing value '$name' to Double", e)
	  }
	}
	
	def rparseString(name:String) = reify(getArg(LIT(name).splice,params.splice))
	
	def rparseOption(tpe:Type,name:String):Tree = {
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
	
	def buildObject(tpe: Type, name:String):Tree = {
	  if      (tpe =:= typeOf[Int])    { rparseInt(name).tree    }
	  else if (tpe =:= typeOf[Long])   { rparseLong(name).tree   }
	  else if (tpe =:= typeOf[Float])  { rparseFloat(name).tree  }
	  else if (tpe =:= typeOf[Double]) { rparseDouble(name).tree }
	  else if (tpe =:= typeOf[String]) { rparseString(name).tree }
	  // The privlaged types
	  else if (tpe.erasure =:= typeOf[Option[Any]]) {
	    rparseOption(tpe,name)
      }
	  // Must be a complex object. Hopefully it can be instanced normally
	  else {
	    val sym = tpe.typeSymbol  
	    val ctorM = tpe.member(nme.CONSTRUCTOR).asMethod
	    val ctorParams = ctorM.paramss
		
		/* Will need to work here to deal with mutable objects
		 * This tree will have to become part of a more complex
		 * expression that tests for each param and applies it.
		 * This same idea must be applied to optional params.
		 */
		
	    New(sym,(ctorParams(0).map { sym => 
		  // gen list of trees that eval to params
	      val tpe = sym.typeSignature
		  val compName = if(name != "") {
		    name + "." + sym.name.decoded
		  } else (sym.name.decoded) 
		  
		  buildObject(tpe, compName)
	    }):_*)
	  }
	}
	
    val tpe = weakTypeOf[A]
	c.Expr[A](buildObject(tpe,""))
  }
}
