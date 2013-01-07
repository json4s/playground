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
	
	// For building objects that take type parameters
	def typeArgumentTree(t: c.Type): c.Tree = t match {
      case TypeRef(_, _, typeArgs @ _ :: _) => AppliedTypeTree(
            Ident(t.typeSymbol), typeArgs map (t=>typeArgumentTree(t)) )
      case _                                => Ident(t.typeSymbol.name)
    }
	
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
	
	// The really heavyweight function. Most of the magic happens in the last else statement
	def buildObject(tpe: Type, name:c.Expr[String]):Tree = {
	  // simple types
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
		// Create new object, and fill params with error catching expressions
		
		val TypeRef(_,sym:Symbol,tpeArgs:List[Type]) = tpe
		val ctorParams = tpe.member(nme.CONSTRUCTOR).asMethod.paramss
		
	    New(typeArgumentTree(tpe),ctorParams.map{_.zipWithIndex.map{
		  case (pSym,index) => // Change out the types if it has type parameters
		    val pTpe = pSym.typeSignature.substituteTypes(sym.asClass.typeParams,tpeArgs)
			(pTpe,pSym,index)  // Substituted tpe, the symbol, and index
		  }.map { case (pTpe,pSym,index) =>
		  // gen list of trees that eval to this types params
	      val compName = CONCAT(CONCAT(name,LIT(".")),LIT(pSym.name.decoded))
		  
		  // If param has defaults, try to find the val in map, or call 
		  // default evaluation from its companion object
		  if (pSym.asTerm.isParamWithDefault) {
			reify {
			  try {
			    c.Expr(buildObject(pTpe, compName)).splice // splice in another obj tree
			  } catch {
			    case e: java.util.NoSuchElementException =>
				  // Need to use the origional symbol.companionObj to get defaults
				  // Would be better to find the generated TermNames if possible
				  c.Expr(Select(Ident(sym.companionSymbol),newTermName(
				    "$lessinit$greater$default$" + (index+1).toString))
				  ).splice
			  }
			}.tree
		  } else buildObject(pTpe, compName) // Required
		  
	    }})	// Using the New(Tree,List(List(Tree))) constructor
		// Here is where we need to populate mutable fields not in constructor
		
	  }
	}
	
	val tpe = weakTypeOf[U]
	c.Expr[U](buildObject(tpe,name))
  }
}
