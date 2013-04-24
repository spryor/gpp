package gpp.app

import gpp.util._

class Convert {
  val version = "0.1.0"
}

/**
 * The Convert_Stanford object contains the main method
 * which reads from the Stanford datafile and coverts it
 * into a xml file to be used in the sentiment analysis
 */

object Convert_Stanford extends Convert {

  def main(args: Array[String]) {
    val opts = ConvertOpts(args)
    if(opts.version()) {
      println("Version " + version)
      System.exit(0)
    }
    val stanfordData = getData(opts.filename().mkString)
    val marshalled =
      <dataset>
      { stanfordData.map { data =>
        <item label={getPolarity(data._1)}>
          <content>{data._2}</content>
        </item>
      }}
      </dataset>

    println(marshalled.mkString)
  }

  def getData(filename:String) = {
    io.Source.fromFile(filename)
      .getLines
      .map(line => {
          val tokens = line.split(";;")
          (tokens(0),tokens(tokens.length-1))
          })
      .toArray
  }

  def getPolarity(num:String) = {
    num match {
      case "0" => "negative"
      case "2" => "neutral"
      case _ => "positive"
    }
  }
}

/**
 * The Convert_Emoticon object contains the main method
 * which reads from the emoticon datafiles and coverts them
 * into a xml file to be used in the sentiment analysis
 */
object Convert_Emoticon extends Convert {

  def main(args: Array[String]) {
    val opts = ConvertOpts(args)
    if(opts.version()) {
      println("Version " + version)
      System.exit(0)
    }
    val files = IndexedSeq("happy.txt","neutral.txt","sad.txt")
    val dataDir = opts.filename().mkString + "/"
    val twitterData = getData(dataDir+files(0)) ++ getData(dataDir+files(1)) ++ getData(dataDir+files(2)) 
    val marshalled = 
      <dataset>
      { twitterData.map { data =>
        <item label={data._1}>
          <content>{data._2}</content>
        </item>
      }}
      </dataset>
    println(marshalled.mkString)
  }

  def getData(filename:String) = {
    val sentiment = getSentiment(filename)
    val output = io.Source.fromFile(filename)
      .getLines
      .map(line => {
        val text = line.split("\t")(2).mkString
        (sentiment, text)
        })
      .toSet
    output
  }

  def getSentiment(filename:String):String = {
    filename match {
      case f if f.endsWith("happy.txt") => "positive"
      case f if f.endsWith("sad.txt") => "negative"
      case _ => "neutral"
    }
  }
}

object ConvertOpts {

  import org.rogach.scallop._

  def apply(args: Array[String]) = new ScallopConf(args) {
    banner("""
Application for Converting.

For usage see below:
""")
    val version = opt[Boolean]("version", noshort=true, default=Some(false), descr="Show version of this program")
    val filename = trailArg[String]("filename", required=false, descr = "The input filename.")

  }
}
