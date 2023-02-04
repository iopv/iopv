#!/bin/bash
#set -xi
##########################Step4: Mvn test java change commit##############################
#mvn test -fn -Dtest=Test***.java
#rundir: hadoopTest
#input : refineMapping**.txt
#output: output.test
DIR_HADOOP=hadoop/hadoop-common-project
Mvntest()
{
MVN_TEST=combination_sample.csv
OUT_RUNTIME=runtime.out
OUT_PERF=performance.out
OUT_TEST=test.log
FAILED_TEST=fail.test
CONFIG_FILE=hadoop-common/src/main/resources/core-default.xml
JAR_CONFIG=modifyConfig.jar
lines=($(cat ${MVN_TEST}))

for line in "${lines[@]:1}"
do
	#get commit
	IFS=', ' read -r -a array <<< "$line"
	echo ${line}
  cd $DIR_HADOOP
	Child_COMMIT=${array[1]}
	Parent_COMMIT=${array[2]}
	TEST_JAVA=${array[3]}
#	echo $Hadoop_COMMIT
  for COMMIT in $Child_COMMIT $Parent_COMMIT
  do
    mvn clean 1> /dev/null 2> /dev/null
	  git reset --hard $COMMIT
	  (mvn install -DskipTests -DskipShade)

	  test_log=`mvn test -fn -Dtest=$TEST_JAVA -DfailIfNoTests=false 2>&1`
    test_result=`echo "${test_log}" | grep BUILD`
    echo "${test_result}" >> ../../$OUT_TEST
    if [ "$test_result" != "[INFO] BUILD SUCCESS" ]
    then
      echo ${test_result}
      echo ${line} >> ../../$FAILED_TEST
      continue
    fi

    executionTime=`(echo "${test_log}" | grep "Time elapsed:")`
    echo $executionTime
    if [ "$executionTime" = "" ]
    then
      echo "Error no time elapsed found."
      echo ${line} >> ../../$FAILED_TEST
      echo "Error no time elapsed found." >> ../$FAILED_TESTS
      continue
    fi

	  cp ../../perfsys.py perfsys.py
	  cp ../../$JAR_CONFIG $JAR_CONFIG
  	echo ${line},${COMMIT} >> ../$OUT_RUNTIME
    echo ${line},${COMMIT} >> ../$OUT_PERF

	  # Options and values
    OPTION1_NAME=${array[4]}
    OPTION2_NAME=${array[6]}
    OPTION3_NAME=${array[8]}
    OPTION4_NAME=${array[10]}
    OPTION1_VALUE=${array[5]}
    OPTION2_VALUE=${array[7]}
    OPTION3_VALUE=${array[9]}
    OPTION4_VALUE=${array[11]}

    echo $OPTION1_NAME, OPTION2_NAME, OPTION3_NAME, OPTION4_NAME >> ../$OUT_RUNTIME
    echo $OPTION1_NAME, OPTION2_NAME, OPTION3_NAME, OPTION4_NAME >> ../$OUT_PERF

    echo $OPTION1_VALUE, $OPTION2_VALUE, $OPTION3_VALUE, $OPTION4_VALUE >> ../$OUT_RUNTIME
    echo $OPTION1_VALUE, $OPTION2_VALUE, $OPTION3_VALUE, $OPTION4_VALUE >> ../$OUT_PERF

    java -jar $JAR_CONFIG $CONFIG_FILE $OPTION1_NAME $OPTION1_VALUE
    java -jar $JAR_CONFIG $CONFIG_FILE $OPTION2_NAME $OPTION2_VALUE
    java -jar $JAR_CONFIG $CONFIG_FILE $OPTION3_NAME $OPTION3_VALUE
    java -jar $JAR_CONFIG $CONFIG_FILE $OPTION4_NAME $OPTION4_VALUE

    pkill -9 java
		mvn test -fn -Dtest=$TEST_JAVA #1> /dev/null 2> /dev/null
		TestExecution=`(mvn test -fn -Dtest=$TEST_JAVA | grep 'Time elapsed:' | cut -d ' ' -f4)`
		# execute the test 30 times
    for i in $( seq 1 30 )
    do
      echo $i" test"
      echo $TestExecution
      (python perfsys.py $TestExecution &)
      TestTime=`(mvn test -fn -Dtest=$TEST_JAVA | grep 'Time elapsed:')`
      echo $TestTime >> ../../$OUT_RUNTIME

      pid=`ps -ef | grep '[p]ython perfsys.py' | awk '{ print $2 }'`
      if [ "$pid" != "" ]
      then
        kill $pid
      fi
      pkill -9 java
    done
	done
	cd ../..
done
}
#call function Mvntest
Mvntest
