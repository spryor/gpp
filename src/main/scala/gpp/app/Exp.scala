package gpp.app

import gpp.util.English
import java.io.File
import nak.util.ConfusionMatrix
import nak.NakContext._
import nak.core._
import nak.data._
import nak.liblinear._  
import nak.util.ConfusionMatrix
import chalk.lang.eng.{PorterStemmer,Twokenize}
import cmu.arktweetnlp.Tagger
import cmu.arktweetnlp.Tagger._
import scala.collection.JavaConversions._

/**
 * The Exp object contains the main method for the Exp
 * functionality. It serves the purpose of selecting 
 * functions based on command line arguments.
 */
object Exp {  
 
  val version = "0.1.0"

  def main(args: Array[String]) {
    val opts = ExpOpts(args)

    if(opts.version()) {
      println("Version " + version)
      System.exit(0)
    }

    val (goldLabels, predictedLabels, items) = opts.method() match {
      case "majority" => MajorityMethod(opts.train(), opts.eval())
      case "lexicon" => LexiconMethod(opts.eval())
      case _ => LiblinearMethod(opts.method(), opts.train(), opts.eval(), opts.cost(), opts.extended())
    }

    val confusionMatrix = ConfusionMatrix(goldLabels, predictedLabels, items)

    if(!opts.detailed()) println(confusionMatrix)
    else println(confusionMatrix.detailedOutput)
  }

}

/**
 * The Method class acts as the base class for the Method 
 * objects.
 */
class Method {

  /**
   * A simple helper function for reading XML files
   *
   * @param path - The path to the XML file to be read
   */
  def readXML(path: String) = scala.xml.XML.loadFile(path)

  def readXMLFiles(paths: List[String]) =
    paths.flatMap(path => (readXML(path) \ "item")) 
}

/**
 * The MajorityMethod object contains the functionality for 
 * running Majority label based sentiment analysis.
 */
object MajorityMethod extends Method {
  /**
   * A function to use the majority method for SA.
   * 
   * @param trainFile - The path to the training xml file
   * @param evalFile - The path to the evaluation xml file
   */
  def apply(trainFiles: List[String], 
            evalFiles: List[String]):(Seq[String], Seq[String], Seq[String])  = {
    val trainData = readXMLFiles(trainFiles)
    val evalData = readXMLFiles(evalFiles)
   
    val topLabel = trainData
                     .map(item => (item \ "@label").text)
                     .groupBy(l => l)
                     .mapValues(_.length)
                     .reduceLeft((a, b) => if(a._2 > b._2) a else b)
                     ._1
    
    val goldLabels = evalData.map{item => (item \ "@label").text}
    val predictedLabels = goldLabels.map(_ => topLabel)
    (goldLabels, predictedLabels, evalData.map(_.text.trim))
  }
}

/**
 * The LexiconMethod object contains the functionality for 
 * running lexicon based sentiment analysis. 
 */
object LexiconMethod extends Method {
  /**
   * A function to use the lexicon method for SA.
   * 
   * @param evalFile - The path to the evaluation xml file
   */
  def apply(evalFiles: List[String]):(Seq[String], Seq[String], Seq[String])  = {
    val evalData = readXMLFiles(evalFiles)
    val goldLabels = evalData.map{item => (item \ "@label").text}
    val predictedLabels = evalData.map(item => classifyTweet(item.text))
    (goldLabels, predictedLabels, evalData.map(_.text.trim))
  }

  /**
   * The label input function simply takes a string and 
   * returns a polarity label
   *
   * @param input - The string to be labeled.
   */
  def classifyTweet(input: String) = {
    val labelAssignment = Twokenize(input)
      .map(English.polarityLexicon)
      .sum
      
    if(labelAssignment > 0) "positive" 
    else if(labelAssignment < 0)  "negative"
    else "neutral"
  }
}

/**
 * The LiblinearMethod object contains the functionality for 
 * running various liblinear classifiers for sentiment analysis. 
 */
object LiblinearMethod extends Method {

  import java.util.List

  //A featurizer using simple bag-of-words features
  val SimpleFeaturizer = new Featurizer[String, String] {
    def apply(input: String) = input
      .replaceAll("""([\?!\";\|\[\]])""", " $1 ") 
      .trim
      .split("\\s+")
      .map(tok => FeatureObservation("word="+tok))
  }

  lazy val Stemmer = new PorterStemmer
  lazy val Tagger = {
    val model = new Tagger
    model.loadModel("/cmu/arktweetnlp/model.20120919")
    model
  }
  
  //A featurizer using lowercase bag-of-words features
  //combined with lexicon based polarity features.
  val ExtendedFeaturizer = new Featurizer[String, String] {
    def apply(input: String) = {
      val words = input
        .replaceAll("""([\?!\";:,\|\[\]])""", " ") 
        .trim
        .toLowerCase
        .replaceAll("(.)\\1{2,}", "$1")
        .split("\\s+") 
	 
      val wordFeatures = words
        .filterNot(English.stopwords)
	.map(tok => FeatureObservation("word="+Stemmer(tok)))

      val polarityFeature = 
        Array(FeatureObservation("lexicalPolarity="+LexiconMethod.classifyTweet(input)))

      val tags = 
        asScalaBuffer(Tagger.tokenizeAndTag(input))
          .toList
	  .map(_.tag)

      val tagFeatures = tags
        .map(tag => FeatureObservation("tag="+tag))

      wordFeatures ++ 
      polarityFeature ++ 
      tagFeatures
    }
  }
  
  /**
   * A function to use L2-regularized logistic regression
   * for SA.
   * 
   * @param method - The type of classifier to use
   * @param trainFiles - The path to the training xml file
   * @param evalFiles - The path to the evaluation xml file
   * @param costParam - The cost for value for the model.
   * @param extended - A boolean value for whether or not to
   *                   use extended features.
   */
  def apply(method: String,
            trainFiles: List[String], 
            evalFiles: List[String], 
            costParam: Double, 
            extended: Boolean):(Seq[String], Seq[String], Seq[String]) = {

    val rawExamples = readRaw(trainFiles)
    val config = LiblinearConfig(solverType=Solver(method), cost=costParam)    
    val classifier = trainClassifier(config, 
                                     if(extended) ExtendedFeaturizer else SimpleFeaturizer,
                                     rawExamples)

    def maxLabelLiblinear = maxLabel(classifier.labels) _

    val comparisons = for (ex <- readRaw(evalFiles).toList) yield 
      (ex.label, maxLabelLiblinear(classifier.evalRaw(ex.features)), ex.features)
      
    comparisons.unzip3
  }

  /**
   * A function to convert raw XML files to Example objects
   *
   * @param filename - The name of the file in the resources
   *                   folder containing the data to read. 
   */
  def readRaw(filenames: List[String]) = 
    filenames.flatMap(filename => 
      for(item <- (readXML(filename) \ "item")) 
        yield Example((item \ "@label").text, item.text.trim)
    )
}

object ExpOpts {

  import org.rogach.scallop._

  def apply(args: Array[String]) = new ScallopConf(args) {
    banner("""
Classification application.

For usage see below:
""")
    
    val methodTypes = Set("majority", "lexicon") ++ Solver.solverTypes
    val train = opt[List[String]]("train", short='t', descr="The files containing training events.")
    val eval = opt[List[String]]("eval", short='e', required=true, descr="The cost parameter C. Bigger values means less regularization (more fidelity to the training set). (default = 1.0)")
    val method = opt[String]("method", short='m', default=Some("L2R_LR"), validate = methodTypes, descr="The type of solver to use. Possible values: majority, lexicon, or any liblinear solver type. (default = L2R_LR)")
    val cost = opt[Double]("cost", short='c', default=Some(1.0), descr="The cost parameter C. Bigger values means less regularization (more fidelity to the training set). (default = 1.0)")
    val extended = opt[Boolean]("extended", short='x', default=Some(false), descr="Use extended features.")
    val detailed = opt[Boolean]("detailed", short='d', default=Some(false))
    val version = opt[Boolean]("version", noshort=true, default=Some(false), descr="Show version of this program")
    val help = opt[Boolean]("help", noshort = true, descr = "Show this message")
    val verbose = opt[Boolean]("verbose", short='v')
  }
}


