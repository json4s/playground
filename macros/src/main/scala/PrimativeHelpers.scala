import java.util.Date
import scala.reflect.macros.Context


object PrimativeHelpers {

// TODO: fix use realy formatters
  val defaultDateFormat = new java.text.SimpleDateFormat("EEE MMM d HH:mm:ss zzz yyyy")
  
  // Some helpers to make things a little more simple in the generated code
  def getInt(name:String,in:Any):Int = in match {
    case n:Long => n.asInstanceOf[Int]
    case n:Int => n
    case s:String => s.toInt
    case e => throw new ParseException(s"Error converting item '$name' to Int. Value: $e",null)
  }
  
  def getLong(name:String,in:Any):Long = in match {
    case n:Long => n
    case n:Int => n.asInstanceOf[Long]
    case s:String => s.toLong
    case e => throw new ParseException(s"Error converting item '$name' to Long. Value: $e",null)
  }
  
  def getFloat(name:String,in:Any):Float = in match {
    case n:Float => n
    case n:Double => n.asInstanceOf[Float]
    case n:Long =>   n.asInstanceOf[Float]
    case n:Int =>    n.asInstanceOf[Float]
    case s:String => s.toFloat
    case e => throw new ParseException(s"Error converting item '$name' to Float. Value: $e",null)
  }
  
  def getDouble(name:String,in:Any):Double = in match {
    case n:Double => n
    case n:Float => n.asInstanceOf[Double]
    case n:Long =>   n.asInstanceOf[Double]
    case n:Int =>    n.asInstanceOf[Double]
    case s:String => s.toDouble
    case e => throw new ParseException(s"Error converting item '$name' to Double. Value: $e",null)
  }
  
  def getString(name:String,in:Any):String = in match {
    case s:String => s
    case s => s.toString
  }
  
  def getDate(name:String, in:Any):Date = in match {
    case s:Date => s
    case s:String => defaultDateFormat.parse(s)
    case e => throw new ParseException(s"Error converting item '$name' to Date. Value: $e",null)
  }
  
}

