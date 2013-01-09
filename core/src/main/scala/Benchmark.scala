import java.util.concurrent.atomic.AtomicLong
import java.util.Date

case class Project(name: String, startDate: Date, lang: Option[Language], teams: List[Team]) extends Serializable
case class Language(name: String, version: Double) extends Serializable
case class Team(role: String, members: List[Employee]) extends Serializable
case class Employee(name: String, experience: Int) extends Serializable

case class ThingWithJunk(name:String, junk:Junk)
case class Crazy(name:String,thg:ThingWithJunk,thg2:ThingWithJunk,thg3:ThingWithJunk,thg4:ThingWithJunk,thg5:ThingWithJunk)
case class Junk(in1:Int, in2:String)



object Benchmark {
  val counter = new AtomicLong(0)
  def project = {
    val c = counter.incrementAndGet()
    val project = Project("test"+c, new Date, Some(Language("Scala"+c, 2.75+c)), List(
      Team("QA"+c, List(Employee("John Doe"+c, 5+c.toInt), Employee("Mike"+c, 3+c.toInt))),
      Team("Impl"+c, List(Employee("Mark"+c, 4+c.toInt), Employee("Mary"+c, 5+c.toInt), Employee("Nick Noob"+c, 1+c.toInt)))))
    val data = Map(
      ("name"-> project.name) ::
      ("startDate"->project.startDate.toString) ::
        ("lang.name"->project.lang.get.name) ::
        ("lang.version"->project.lang.get.version.toString) ::
      
        project.teams.zipWithIndex.flatMap{ case(team,i) =>
          ("teams."+i.toString+".role"->team.role)::
            team.members.zipWithIndex.flatMap { case (member,j) =>
              "teams."+i.toString+".members."+j.toString+".name" ->member.name ::
              "teams."+i.toString+".members."+j.toString+".experience"->member.experience.toString :: Nil
            }
        } :_*
    )
    data.map{ case (k,v) => ("dd."+k,v) }
  }
  
  def run = {
    println(s"Hello world. Macro reconstruction rocks.")
    val data = project
    val iterations = 2000000
    val counter = new Stopwatch
    
    val expected = Crazy("crazyBob...",ThingWithJunk("Bob",Junk(2,"SomeJunk...")),ThingWithJunk("Bob",Junk(2,"SomeJunk...")),ThingWithJunk("Bob",Junk(2,"SomeJunk...")),ThingWithJunk("Bob",Junk(2,"SomeJunk...")),ThingWithJunk("Bob",Junk(2,"SomeJunk...")))
    val stuff = Map("d.name"->expected.name,
                "d.thg.name"->expected.thg.name,
                "d.thg.junk.in1"->expected.thg.junk.in1.toString,
                "d.thg.junk.in2"->expected.thg.junk.in2.toString,
                
                "d.thg2.name"->expected.thg2.name,
                "d.thg2.junk.in1"->expected.thg2.junk.in1.toString,
                "d.thg2.junk.in2"->expected.thg2.junk.in2.toString,
                "d.thg3.name"->expected.thg3.name,
                "d.thg3.junk.in1"->expected.thg3.junk.in1.toString,
                "d.thg3.junk.in2"->expected.thg3.junk.in2.toString,
                
                "d.thg4.name"->expected.thg4.name,
                "d.thg4.junk.in1"->expected.thg4.junk.in1.toString,
                "d.thg4.junk.in2"->expected.thg4.junk.in2.toString,
                "d.thg5.name"->expected.thg5.name,
                "d.thg5.junk.in1"->expected.thg5.junk.in1.toString,
                "d.thg5.junk.in2"->expected.thg5.junk.in2.toString)
            
    val result = Macros.classBuilder[Crazy](stuff,"d")
    
    var i = 0
    while(i < iterations/5) {
      //val a = Macros.classBuilder[Project](data,"dd")
      val b = Macros.classBuilder[Crazy](stuff,"d")
      i+=1
    }
    counter.start
    i = 0
    while(i < iterations) {
      //val a = Macros.classBuilder[Project](data,"dd")
      val b = Macros.classBuilder[Crazy](stuff,"d")
      i+=1
    }
    counter.stop
    
    println(s"Took ${counter.getElapsedTime*0.001} seconds for ${iterations/1000}K iterations.")
  }
}

class Stopwatch {

  private var startTime = -1L
  private var stopTime = -1L
  private var running = false

  def start(): Stopwatch = {
    startTime = System.currentTimeMillis()
    running = true
    this
  }

  def stop(): Stopwatch = {
    stopTime = System.currentTimeMillis()
    running = false
    this 
  }

  def isRunning(): Boolean = running

  def getElapsedTime() = {
    if (startTime == -1) {
      0
    }
    if (running) {
      System.currentTimeMillis() - startTime
    }
    else {
      stopTime - startTime
    }
  }

  def reset() {
    startTime = -1
    stopTime = -1
    running = false
  }
}