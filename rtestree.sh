#!/bin/sh

SCALA="JAVA_OPTS='-Xmx2g' scala -classpath 'extern'"

eval $SCALA mkSTreeData.scala RTE_dev > output/RTE_dev.stree.txt 2> output/log2RTE_dev &
eval $SCALA mkSTreeData.scala RTE_dev2 > output/RTE_dev2.stree.txt 2> output/log2RTE_dev2 &
eval $SCALA mkSTreeData.scala RTE2_dev > output/RTE2_dev.stree.txt 2> output/log2RTE2_dev &
eval $SCALA mkSTreeData.scala RTE3_dev > output/RTE3_dev.stree.txt 2> output/log2RTE3_dev &
eval $SCALA mkSTreeData.scala RTE_test > output/RTE_test.stree.txt 2> output/log2RTE_test &
eval $SCALA mkSTreeData.scala RTE2_test > output/RTE2_test.stree.txt 2> output/log2RTE2_test &
eval $SCALA mkSTreeData.scala RTE3_test > output/RTE3_test.stree.txt 2> output/log2RTE3_test &
eval $SCALA mkSTreeData.scala RTE5_dev > output/RTE5_dev.stree.txt 2> output/log2RTE5_dev &
eval $SCALA mkSTreeData.scala RTE5_test > output/RTE5_test.stree.txt 2> output/log2RTE5_test &

exit 0
