
import java.util.Properties

import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations.CollapsedCCProcessedDependenciesAnnotation 
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations.CollapsedDependenciesAnnotation
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations.BasicDependenciesAnnotation 
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefClusterIdAnnotation
import edu.stanford.nlp.ling.IndexedWord
import edu.stanford.nlp.semgraph.SemanticGraph
import edu.stanford.nlp.semgraph.SemanticGraphEdge

import scala.collection.JavaConversions._

val props = new Properties
props.put("annotators", "tokenize, ssplit, pos, parse")
val pipeline = new StanfordCoreNLP(props)

val document = new Annotation(args(0))
pipeline.annotate(document)

for (sentence <- document.get(classOf[SentencesAnnotation])) {
	println(sentence.get(classOf[CollapsedDependenciesAnnotation]))
}
