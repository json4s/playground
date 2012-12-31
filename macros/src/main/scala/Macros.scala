import language.experimental.macros
import scala.reflect.macros.Context

import scala.collection.mutable.{ListBuffer, Stack}


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
	
	def rparseInt(name:String)    = reify(getArg(LIT(name).splice,params.splice).toInt)
	def rparseLong(name:String)   = reify(getArg(LIT(name).splice,params.splice).toLong)
	def rparseFloat(name:String)  = reify(getArg(LIT(name).splice,params.splice).toFloat)
	def rparseDouble(name:String) = reify(getArg(LIT(name).splice,params.splice).toDouble)
	def rparseString(name:String) = reify(getArg(LIT(name).splice,params.splice))
	
	def tryFindTree(ty:Tree,els: Tree): Tree = {
	  Try(
        ty, 
        List(
	      CaseDef(
		    Typed(
		      Ident(nme.WILDCARD),
		      Select(Select(Ident("java"),newTermName("util")),newTermName("NoSuchElementException"))
            ),
		    EmptyTree,
		    els
		  )
	    ), 
	    EmptyTree
	  )
	}
	
	def rparseOption(tpe:Type,name:String):Tree = {
	  val TypeRef(_,_,List(argTpe)) = tpe // Why did this quit working?
	  
	  val onErr: Tree = Select(Ident("scala"), newTermName("None"))
	  
	  val ty = Apply(Select(Select(Ident("scala"), newTermName("Some")), newTermName("apply")),  
	    List(buildObject(argTpe,name)))
		
	  tryFindTree(ty,onErr)
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
	  else {
	    val sym = tpe.typeSymbol  
	    val ctorM = tpe.member(nme.CONSTRUCTOR).asMethod
	    val ctorParams = ctorM.paramss
	    New(sym,(ctorParams(0).map { sym =>
	      val tpe = sym.typeSignature
		  val compName = if(name != "") {
		    name + "." + sym.name.decoded
		  } else (sym.name.decoded) 
		
		  // Handle privlaged types such as options now..
		  buildObject(tpe, compName)
	    }):_*)
	  }
	}
	
    val tpe = weakTypeOf[A]
	c.Expr[A](buildObject(tpe,""))
  }
}
