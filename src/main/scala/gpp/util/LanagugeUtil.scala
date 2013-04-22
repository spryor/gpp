package gpp.util

object Resource {
  import java.util.zip.GZIPInputStream
  import java.io.DataInputStream

  /**
   * Read in a file as a Source, ensuring that the right thing
   * is done for gzipped files.
   */
  def asSource(location: String) = {
    val stream = this.getClass.getResourceAsStream(location)
    if (location.endsWith(".gz"))
      io.Source.fromInputStream(new GZIPInputStream(stream))
    else
      io.Source.fromInputStream(stream)
  
  }

  def asStream(location: String) = {
    val stream = this.getClass.getResourceAsStream(location)
    val stream2 = if (location.endsWith(".gz")) new GZIPInputStream(stream) else stream
    new DataInputStream(stream2)
  }
}

object English {
  lazy val getWordLabel = """.*word1=([a-z]+).*priorpolarity=([a-z]+).*""".r
  lazy val polarityLexicon = getPolarityLexicon()

  def getPolarityLexicon() = {
    val mpqa = Resource.asSource("/lang/eng/lexicon/polarityLexicon.gz")
    .getLines
    .map{case getWordLabel(word, label) => 
      val numLabel = label match {
        case "negative" => -1
        case "positive" => 1
        case _ => 0
      }
      (word, numLabel)
    }
    .toMap
    .withDefaultValue(0)

    val posWords = getLexicon("positive-words.txt.gz").map(word => (word, 1)).toMap
    val negWords = getLexicon("negative-words.txt.gz").map(word => (word, -1)).toMap
    mpqa ++ posWords ++ negWords
  }

  def getLexicon(filename: String) = 
    Resource.asSource("/lang/eng/lexicon/"+filename)
      .getLines
      .filterNot(_.startsWith(";")) // filter out comments
      .toSet
}
