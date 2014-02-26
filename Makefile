SCALAC=scalac
SCALADOC=scaladoc

CORE=\
	tifmo/dcstree/SemRole.class \
	tifmo/dcstree/Quantifier.class \
	tifmo/dcstree/WordBase.class \
	tifmo/dcstree/Executor.class \
	tifmo/dcstree/Selection.class \
	tifmo/dcstree/Relation.class \
	tifmo/dcstree/Denotation.class \
	tifmo/dcstree/Statement.class \
	tifmo/dcstree/DCSTreeEdge.class \
	tifmo/dcstree/DCSTreeNode.class \
	tifmo/dcstree/Declarative.class \
	mylib/misc/oneFromEach.class \
	mylib/misc/listPartitions.class \
	mylib/misc/listCoverings.class \
	tifmo/inference/Dimension.class \
	tifmo/inference/IEngineCore.class \
	tifmo/inference/FuncComplement.class \
	tifmo/inference/FuncDIall.class \
	tifmo/inference/FuncDIno.class \
	tifmo/inference/FuncSingle.class \
	tifmo/inference/FuncNegation.class \
	tifmo/inference/IEDump.class \
	tifmo/inference/IEngine.class \
	tifmo/onthefly/AEngine.class \
	tifmo/onthefly/Path.class \
	tifmo/onthefly/PathAlignment.class \
	tifmo/onthefly/alignPaths.class \
	

core: $(CORE)

tifmo/%.class: src/tifmo/%.scl
	$(SCALAC) $<
	
mylib/misc/%.class: src/mylib/misc/%.scl
	$(SCALAC) $<
	
tifmo/dcstree/DCSTreeNode.class: src/tifmo/dcstree/DCSTreeNode.scl src/tifmo/dcstree/Ref.scl src/tifmo/dcstree/Context.scl
	$(SCALAC) $^
	
tifmo/inference/IEngineCore.class: src/tifmo/inference/IEngineCore.scl src/tifmo/inference/Term.scl src/tifmo/inference/Finder.scl src/tifmo/inference/Debug_RuleTrace.scl src/tifmo/inference/IEPred.scl src/tifmo/inference/IEFunction.scl src/tifmo/inference/RuleArg.scl src/tifmo/inference/Trigger.scl src/tifmo/inference/RulesQuick.scl src/tifmo/inference/RulesLight.scl src/tifmo/inference/RulesHeavy.scl
	$(SCALAC) $^

#################################

clean:
	rm -rf tifmo mylib scaladoc

scaladoc:
	$(SCALADOC) -d scaladoc src/tifmo/dcstree/*.scl src/tifmo/inference/*.scl src/tifmo/onthefly/*.scl

.PHONY: clean scaladoc
