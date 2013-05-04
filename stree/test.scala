import tifmo.knowledge.SemRole
import tifmo.knowledge.EnWord
import tifmo.stree.InferMgr
import tifmo.stree.WORD
import tifmo.stree.Subsume
import tifmo.stree.Qtfier
import tifmo.stree.PreNode
import tifmo.stree.STree

val love = new EnWord("love", "O", true)
val like = new EnWord("like", "O", true)

val boyNode = new PreNode
boyNode.word = new EnWord("boy", "O", true)
boyNode.outr = SemRole.ARG
boyNode.qtfier = Qtfier.ALL

val girlNode = new PreNode
girlNode.word = new EnWord("girl", "O", true)
girlNode.outr = SemRole.ARG

val loveNode = new PreNode
loveNode.word = love
loveNode.addChild(SemRole.SBJ, boyNode, 2)
loveNode.addChild(SemRole.OBJ, girlNode, 1)

val tstree = new STree(boyNode :: girlNode :: loveNode :: Nil)

val likeNode = new PreNode
likeNode.word = like
likeNode.addChild(SemRole.SBJ, boyNode, 1)
likeNode.addChild(SemRole.OBJ, girlNode, 2)

val hstree = new STree(boyNode :: girlNode :: likeNode :: Nil)

val im = new InferMgr(hstree)
im.addHypernym(love, like)
im.addPremise(tstree)

im.trace(_ => 0.0, 1.0)
