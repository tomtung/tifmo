#!/bin/sh

SCALA="JAVA_OPTS='-Xmx2g' scala -classpath 'extern'"

eval $SCALA mkTraceData.scala RTE_dev > output/RTE_dev.trace.txt 2> output/log2RTE_dev &
eval $SCALA mkTraceData.scala RTE_dev2 > output/RTE_dev2.trace.txt 2> output/log2RTE_dev2 &
eval $SCALA mkTraceData.scala RTE2_dev > output/RTE2_dev.trace.txt 2> output/log2RTE2_dev &
eval $SCALA mkTraceData.scala RTE3_dev > output/RTE3_dev.trace.txt 2> output/log2RTE3_dev &
eval $SCALA mkTraceData.scala RTE_test > output/RTE_test.trace.txt 2> output/log2RTE_test &
eval $SCALA mkTraceData.scala RTE2_test > output/RTE2_test.trace.txt 2> output/log2RTE2_test &
eval $SCALA mkTraceData.scala RTE3_test > output/RTE3_test.trace.txt 2> output/log2RTE3_test &
eval $SCALA mkTraceData.scala RTE5_dev > output/RTE5_dev.trace.txt 2> output/log2RTE5_dev &
eval $SCALA mkTraceData.scala RTE5_test > output/RTE5_test.trace.txt 2> output/log2RTE5_test &

exit 0
