import language.experimental.macros
import scala.reflect.macros.Context

import scala.collection.mutable.{ListBuffer, Stack}


object Macros {
  def hello = macro helloimpl
  def helloimpl(c: Context):c.Expr[String] = {
    import c.universe._
  
    c.Expr[String](Literal(Constant("Hello world!")))
  }
  
  
  
  type ParamsTpe = Map[String,String]
  
  def getArg(name:String, in:ParamsTpe) = in(name)
  
  def classBuilder[A](params: ParamsTpe) = macro classbuilder[A]
  def classbuilder[A: c.WeakTypeTag](c: Context)(params: c.Expr[ParamsTpe]):c.Expr[A] = {
    import c.universe._
	
	def LIT[U](x:U) = c.Expr[U](Literal(Constant(x)))
	
	def rparseInt(name:String)    = reify(getArg(LIT(name).splice,params.splice).toInt)
	def rparseLong(name:String)   = reify(getArg(LIT(name).splice,params.splice).toLong)
	def rparseFloat(name:String)  = reify(getArg(LIT(name).splice,params.splice).toFloat)
	def rparseDouble(name:String) = reify(getArg(LIT(name).splice,params.splice).toDouble)
	def rparseString(name:String) = reify(getArg(LIT(name).splice,params.splice))
	
	def buildParams(syms:List[List[Symbol]],name:String):List[Tree] = {
	  syms(0) map { sym =>
	    val tpe = sym.typeSignature
		val compName = if(name != "") {
		  name + "." + sym.name.decoded
		} else (sym.name.decoded) 
		
		if      (tpe =:= typeOf[Int])    { rparseInt(compName).tree    }
		else if (tpe =:= typeOf[Long])   { rparseLong(compName).tree   }
		else if (tpe =:= typeOf[Float])  { rparseFloat(compName).tree  }
		else if (tpe =:= typeOf[Double]) { rparseDouble(compName).tree }
		else if (tpe =:= typeOf[String]) { rparseString(compName).tree }
		else                             { buildObject(tpe, compName)  }
	  }
	  
	}
	
	def buildObject(tpe: Type, name:String):Tree = {
	  val ctorM = tpe.member(nme.CONSTRUCTOR).asMethod
	  val ctorParams = ctorM.paramss
	  Apply(Select(Ident(tpe.typeSymbol.companionSymbol),"apply"),buildParams(ctorParams,name))
	}
	
    val tpe = weakTypeOf[A]
	val tree = buildObject(tpe,"")
	c.Expr[A](tree)
  }
}