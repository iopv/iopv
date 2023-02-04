#!/bin/bash
#set -x
##########################ant test java change commit##############################

DIR_CASSANDRA=cassandra
Anttest()
{
TEST_FILE=combination_sample.csv
OUT_RUNTIME=runtime.out
OUT_PERF=performance.out
OUT_TEST=test.log
FAILED_TEST=fail.test
# modify config
CONFIG_FILE=conf/cassandra.yaml
MODIFY_CONFIG=modifyConfig.py
# capture perf
PERF_MONITOR=perfsys.py
lines=($(cat ${TEST_FILE}))

for line in "${lines[@]:1}"  # skip the first line
do
	IFS=', ' read -r -a array <<< "$line"
	echo ${line}
  cd $DIR_CASSANDRA
	Child_COMMIT=${array[1]}
#	Parent_COMMIT=`git rev-parse ${Child_COMMIT}^`
	Parent_COMMIT=${array[2]}
	TEST_JAVA=${array[3]}
	TEST_JAVA=${TEST_JAVA##*.}

  for COMMIT in $Child_COMMIT $Parent_COMMIT
  do
      ant clean 1> /dev/null 2> /dev/null
      git reset --hard $COMMIT
      ant build > /dev/null

      build_log=`ant test -Dtest.name=$TEST_JAVA 2>&1`
      build_result=`echo "${build_log}" | grep BUILD`
      echo "${build_result}" >> ../$OUT_TEST
      # if the test is failed, the skip it.
      if [ "$build_result" != "BUILD SUCCESSFUL" ]
      then
        echo ${build_result}
        echo ${line},${COMMIT} >> ../$FAILED_TEST
        continue
      fi

      executionTime=`(echo "${build_log}" | grep "Time elapsed:")`
      echo $executionTime
      if [ "$executionTime" = "" ]
      then
         echo "Error no time elapsed found."
         echo ${line},${COMMIT} >> ../$FAILED_TEST
         echo "Error no time elapsed found." >> ../$FAILED_TEST
         continue
      fi

      echo ${line},${COMMIT} >> ../$OUT_RUNTIME
      echo ${line},${COMMIT} >> ../$OUT_PERF
      pkill -9 java
      cp ../$PERF_MONITOR $PERF_MONITOR
      cp ../$MODIFY_CONFIG $MODIFY_CONFIG

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
      (python $MODIFY_CONFIG $CONFIG_FILE $OPTION1_NAME $OPTION1_VALUE $OPTION2_NAME $OPTION2_VALUE $OPTION3_NAME $OPTION3_VALUE $OPTION4_NAME $OPTION4_VALUE)

      build_log=`ant test -Dtest.name=$TEST_JAVA 2>&1`
      TestExecutionTime=`(echo "${build_log}" | grep "Time elapsed:")`

      echo $TestExecutionTime >> ../$OUT_PERF
      #esercise the test 30 times
      for i in $( seq 1 30 )
      do
          (python $PERF_MONITOR "JUnitTestRunner" "${TestExecutionTime}" &)
          build_log=`ant test -Dtest.name=$TEST_JAVA 2>&1`
          TestTime=`(echo "${build_log}" | grep "Time elapsed:")`
          echo $TestTime >> ../$OUT_RUNTIME

          pid=`ps -ef | grep '[p]ython perfsys.py' | awk '{ print $2 }'`
          if [ "$pid" != "" ]
          then
            kill $pid
          fi
          pkill -9 java
      done
	done
	cd ..
done
}
#call function Mvntest
Anttest
