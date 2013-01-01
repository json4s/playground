
case class Junk(in:Int, in2:String)



object Main {

  val stuff = Map("in"->"2","in2"->"cats")
  
  def main(args: Array[String]) {
    println("Hello world. Macro reconstruction rocks.")
	println(Macros.classBuilder[Junk](stuff))
  }
}
