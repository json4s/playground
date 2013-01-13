import language.experimental.macros
import scala.reflect.macros.Context

import org.json4s.JsonWriter

class WriterStack[T](var current:JsonWriter[T]) {
  
  def startArray() = {
    current = current.startArray()
    current
  }
  def endArray() = {
    current = current.endArray()
    current
  }
  def startObject() = {
    current = current.startObject()
    current
  }
  def endObject() = {
    current = current.endObject()
    current
  }
  
  def primative(value:Any) = { 
    current = value match {
      case a:Int        => current.int(a)
      case a:String     => current.string(a)
      case a:Float      => current.float(a)
      case a:Double     => current.double(a)
      case a:Boolean    => current.boolean(a)
      case a:Long       => current.long(a)
      case a:Byte       => current.byte(a)
      case a:BigInt     => current.bigInt(a)
      case a:Short      => current.short(a)
      case a:BigDecimal => current.bigDecimal(a)
    }
    current
  }
  
  def startField(name: String) = {
    current = current.startField(name)
    current
  }
  /*
  def addJValue(jv: JValue) = {
    current = current.addJValue(jv)
    current
  }
  */
  def result = current.result
}

// Intended to be the serialization side of the class builder
object Serialization {
  type Writer = JsonWriter[_]
  
  def asyncBuilder[U](obj:U,name:String, writer: Writer) = macro asyncimpl[U]
  def asyncimpl[U:c.WeakTypeTag](c: Context)(obj: c.Expr[U], name:c.Expr[String],
                      writer: c.Expr[Writer]):c.Expr[Unit] = {
                      
    import c.universe._
    val helpers = new MacroHelpers[c.type](c)
    import helpers._
    
    // Will help manage the JsonWriters for us instead of having to keep track
    // as we go down the tree
    val Block(writerStackDef::Nil,_) = reify{
      val writerStack = new WriterStack(writer.splice)
    }.tree
    val writerStack = c.Expr[WriterStack[_]](Ident("writerStack"))
    
    val primativeTypes = typeOf[Int]::typeOf[String]::typeOf[Float]::typeOf[Double]::
                          typeOf[Boolean]::typeOf[Long]::typeOf[Byte]::typeOf[BigInt]::
                          typeOf[Short]::typeOf[BigDecimal]::Nil
    
    // Writer should already be in startObject state
    def dumpObject(tpe:Type,path:Tree,name:c.Expr[String]):c.Tree = {
      
      val TypeRef(_,sym:Symbol,tpeArgs:List[Type]) = tpe
      // get fields
      val fields = getVars(tpe):::getVals(tpe)
      val fieldTrees = fields map { pSym => 
        val pTpe = pSym.typeSignature.substituteTypes(sym.asClass.typeParams,tpeArgs)
        val fieldName = pSym.name.decoded.trim    // Do I need to trim here?
        val fieldPath = Select(path,newTermName(fieldName))
        
        
        if(primativeTypes.contains(pTpe)) { // Must be primative
          reify{
            writerStack.splice.startField(LIT(fieldName).splice)
            writerStack.splice.primative(c.Expr(fieldPath).splice)
          }.tree
        } else if(pTpe.erasure =:= typeOf[List[Any]]) {
          EmptyTree   // TODO
        } else if(pTpe.erasure =:= typeOf[Map[Any,Any]]) {
          EmptyTree   // TODO
        } else if(pTpe.erasure =:= typeOf[Option[Any]]) {
          EmptyTree   // TODO
        } else {
          reify{
            writerStack.splice.startField(LIT(fieldName).splice)
            writerStack.splice.startObject()
            c.Expr(dumpObject(pTpe,fieldPath,LIT(fieldName))).splice
          }.tree
        }
      }
      
      // Return add all the blocks for each field and pop this obj off the stack
      Block(fieldTrees,reify{writerStack.splice.endObject()}.tree)
    }
    val stuff = Block(
      writerStackDef::reify{
        writerStack.splice.startObject()  // Does this do anything?
      }.tree::dumpObject(weakTypeOf[U],obj.tree,name)::Nil,
      Select(Ident("scala"), newTermName("Unit"))
    )
    println(s"----------------------- Debug: Generated Code -----------------------\n $stuff")
    c.Expr[Unit](stuff)
  }
  
}