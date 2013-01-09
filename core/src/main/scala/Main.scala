import java.util.Date

case class Project(name: String, startDate: Date, lang: Option[Language], teams: List[Team]) extends Serializable
case class Language(name: String, version: Double) extends Serializable
case class Team(role: String, members: List[Employee]) extends Serializable
case class Employee(name: String, experience: Int) extends Serializable


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

import java.util.concurrent.atomic.AtomicLong

object Main {
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
  
  def main(args: Array[String]) {
    println(s"Hello world. Macro reconstruction rocks.")
    val data = project
    val iterations = 200000
    val counter = new Stopwatch
    
    var i = 0
    while(i < iterations) {
      val a = Macros.classBuilder[Project](data,"dd")
      i+=1
    }
    counter.start
    i = 0
    while(i < iterations) {
      val a = Macros.classBuilder[Project](data,"dd")
      i+=1
    }
    counter.stop
    
    println(s"Took ${counter.getElapsedTime*0.001} seconds for ${iterations/1000}K iterations.")
  }
}
