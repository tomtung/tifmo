SCALAC = scalac
SCALADOC = scaladoc
SCALA = JAVA_OPTS="-Xmx2g" scala

TARGET = \
	tifmo/mylib/oneFromEach.class \
	tifmo/mylib/listPartitions.class \
	tifmo/mylib/listCoverings.class \
	tifmo/knowledge/SemRole.class \
	tifmo/inference/IETerm.class \
	tifmo/inference/IEngine.class \
	tifmo/inference/IEBasic.class \
	tifmo/knowledge/WordInfo.class \
	tifmo/stree/Term.class \
	tifmo/stree/AtomSt.class \
	tifmo/stree/Qtfier.class \
	tifmo/stree/STreeNode.class \
	tifmo/stree/PreNode.class \
	tifmo/stree/STree.class \
	tifmo/stree/SelNum.class \
	tifmo/stree/Schema.class \
	tifmo/stree/Align.class \
	tifmo/stree/InferMgr.class \
	tifmo/resource/WordNet.class \
	tifmo/knowledge/StopWords.class \
	tifmo/resource/NgramDist.class \
	tifmo/knowledge/EnWord.class \
	tifmo/proc/preProcEnglish.class \
	tifmo/proc/mkSTreeEnglish.class
	

all: $(TARGET)

clean:
	rm -rf tifmo/*

##################################################

tifmo/mylib/oneFromEach.class: mylib/oneFromEach.scl
	$(SCALAC) $<

tifmo/mylib/listPartitions.class: mylib/listPartitions.scl
	$(SCALAC) $<

tifmo/mylib/listCoverings.class: mylib/listCoverings.scl
	$(SCALAC) $<

tifmo/knowledge/SemRole.class: knowledge/SemRole.scl
	$(SCALAC) $<

tifmo/inference/IETerm.class: inference/IETerm.scl
	$(SCALAC) $<

tifmo/inference/IEngine.class: inference/IEngine.scl
	$(SCALAC) $<

tifmo/inference/IEBasic.class: inference/IEBasic.scl
	$(SCALAC) $<

tifmo/knowledge/WordInfo.class: knowledge/WordInfo.scl
	$(SCALAC) $<

tifmo/stree/Term.class: stree/Term.scl
	$(SCALAC) $<

tifmo/stree/AtomSt.class: stree/AtomSt.scl
	$(SCALAC) $<

tifmo/stree/Qtfier.class: stree/Qtfier.scl
	$(SCALAC) $<

tifmo/stree/STreeNode.class: stree/STreeNode.scl
	$(SCALAC) $<

tifmo/stree/PreNode.class: stree/PreNode.scl
	$(SCALAC) $<

tifmo/stree/STree.class: stree/STree.scl
	$(SCALAC) $<

tifmo/stree/SelNum.class: stree/SelNum.scl
	$(SCALAC) $<

tifmo/stree/Schema.class: stree/Schema.scl
	$(SCALAC) $<

tifmo/stree/Align.class: stree/Align.scl
	$(SCALAC) $<

tifmo/stree/InferMgr.class: stree/InferMgr.scl
	$(SCALAC) $<

tifmo/resource/WordNet.class: resource/WordNet.scl
	$(SCALAC) $<

tifmo/knowledge/StopWords.class: knowledge/StopWords.scl
	$(SCALAC) $<

tifmo/resource/NgramDist.class: resource/NgramDist.scl
	$(SCALAC) $<

tifmo/knowledge/EnWord.class: knowledge/EnWord.scl
	$(SCALAC) $<

tifmo/proc/preProcEnglish.class: proc/preProcEnglish.scl
	$(SCALAC) $<

tifmo/proc/mkSTreeEnglish.class: proc/mkSTreeEnglish.scl
	$(SCALAC) $<

####################################################

scaladoc:
	$(SCALADOC) -d scaladoc mylib/*.scl inference/*.scl stree/*.scl

test:
	$(SCALA) test.scala

test2:
	$(SCALA) test2.scala

rte:
	$(SCALA) rte.scala

.PHONY: scaladoc clean test test2 rte
