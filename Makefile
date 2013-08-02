CLASSPATH = "extern"
SCALAC = scalac
SCALACEX = scalac -classpath $(CLASSPATH)
SCALADOC = scaladoc -classpath $(CLASSPATH)
SCALA = JAVA_OPTS="-Xmx2g" scala -classpath $(CLASSPATH)

TARGET_CORE = \
	tifmo/mylib/oneFromEach.class \
	tifmo/mylib/listPartitions.class \
	tifmo/knowledge/SemRole.class \
	tifmo/inference/IETerm.class \
	tifmo/inference/IEPred.class \
	tifmo/inference/Trigger.class \
	tifmo/inference/IEngine.class \
	tifmo/knowledge/WordInfo.class \
	tifmo/inference/IEBasic.class \
	tifmo/stree/Term.class \
	tifmo/stree/AtomSt.class \
	tifmo/stree/Qtfier.class \
	tifmo/stree/STreeNode.class \
	tifmo/stree/PreNode.class \
	tifmo/stree/STree.class \
	tifmo/stree/SelNum.class \
	tifmo/stree/SelSup.class \
	tifmo/stree/Schema.class \
	tifmo/stree/Align.class \
	tifmo/stree/TraceInfo.class \
	tifmo/stree/InferMgr.class

TARGET_RESOURCE = \
	tifmo/resource/EnStopWords.class \
	tifmo/resource/EnWordNet.class \
	tifmo/resource/EnNgramDist.class \
	tifmo/resource/EnPolarity.class \
	tifmo/resource/EnDownward.class

TARGET_KNOWLEDGE = \
	tifmo/mylib/longestCommSeq.class \
	tifmo/knowledge/EnWord.class \
	tifmo/knowledge/EnConfiFunc.class
	
TARGET_PROC = \
	tifmo/proc/preProcEnglish.class \
	tifmo/proc/mkSTreeEnglish.class \
	tifmo/proc/addknowEnglish.class \
	tifmo/proc/featureEnglish.class \
	tifmo/proc/linearClassifier.class
	
TARGET_CLASSES = $(TARGET_CORE) $(TARGET_RESOURCE) $(TARGET_KNOWLEDGE) $(TARGET_PROC)

all: $(TARGET_CLASSES)

tifmo/mylib/%.class: mylib/%.scl
	$(SCALAC) $<

tifmo/inference/IETerm.class tifmo/inference/IEPred.class tifmo/inference/Trigger.class tifmo/inference/IEngine.class: inference/IETerm.scl inference/IEPred.scl inference/Trigger.scl inference/IEngine.scl
	$(SCALAC) $^

tifmo/inference/%.class: inference/%.scl
	$(SCALAC) $<

tifmo/stree/%.class: stree/%.scl
	$(SCALAC) $<

tifmo/knowledge/SemRole.class: knowledge/SemRole.scl
	$(SCALAC) $<

tifmo/knowledge/WordInfo.class: knowledge/WordInfo.scl
	$(SCALAC) $<

tifmo/knowledge/%.class: knowledge/%.scl
	$(SCALACEX) $<

tifmo/resource/%.class: resource/%.scl
	$(SCALACEX) $<

tifmo/proc/%.class: proc/%.scl
	$(SCALACEX) $<

#########################################################################

clean:
	rm -rf tifmo/*

scaladoc:
	$(SCALADOC) -d scaladoc mylib/*.scl inference/*.scl stree/*.scl knowledge/*.scl resource/*.scl proc/*.scl *.scl

test:
	$(SCALA) test.scala

.PHONY: clean scaladoc test
