package playground

import scala.util.Try

import java.util.Date
import java.text.SimpleDateFormat

class DateFormat(fmt:String) extends SimpleDateFormat(fmt)

object MapConverter {

  lazy val defaultDateFormat = new DateFormat("EEE MMM d HH:mm:ss zzz yyyy")

  def mapStoMapAny(in: Map[String,String]): Map[String,Any] = in.map{ case (k,v) =>
    
      
    val vv = if (v.contains('.')) {
               Try{v.toDouble                  } orElse
               Try{ v                          }
             } else {
               Try{v.toLong                    } orElse 
               Try{ defaultDateFormat.parse(v) } orElse 
               Try{ v                          }
             }
    (k,vv.get)
  }

}