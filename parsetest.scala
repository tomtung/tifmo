import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.trees.PennTreebankLanguagePack

val lp = LexicalizedParser.loadModel("edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz")
val tlp = new PennTreebankLanguagePack
val gsf = tlp.grammaticalStructureFactory

val sent = "American accused of espionage"

val parse = lp.apply(sent)
val gs = gsf.newGrammaticalStructure(parse)
val dct = gs.typedDependenciesCollapsedTree

println(dct)
