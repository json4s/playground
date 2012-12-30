
case class Junk(in:Int, in2:String)



object Main {

  val stuff = Map("in"->"2","in2"->"cats")
  
  def main(args: Array[String]) {
    println(Macros.hello)
	println(Macros.classBuilder[Junk](stuff))
  }
}
