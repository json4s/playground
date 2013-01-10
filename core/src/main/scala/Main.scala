
object Main {
  
  def main(args: Array[String]) {
    //Benchmark.run
    val data = Map("dd"->"10220")
    Macros.asyncBuilder[Int](data,"dd")(f=>println(s"Got the number $f back!")) 
  }
}
