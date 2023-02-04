import psutil
import commands
import time
import sys

psutillog="../performance.out"
# filelog=open(psutillog,'a')

def getPid(processname):
	cmd = "jps | grep '%s' " % processname
    	info = commands.getoutput(cmd)
    	infos = info.split()
    	if len(infos) > 1:
    	    return infos[0]
    	else:
    	    return -1

def perf(process_id,runtime):
    try:
        p = psutil.Process(process_id)
        waittime = runtime - 4
        if(waittime < 0):
            waittime = runtime - (runtime / 4)
        time.sleep(waittime)
        # the process exists
        with open(psutillog, 'a') as f:
            f.write(str(p.cpu_times()) + '\n' + str(p.memory_full_info()) +
                    '\n' + str(p.io_counters()) + '\n')
    except Exception as e:
        with open(psutillog, 'a') as f:
            f.write("Error: {}\n".format(str(e)))

if __name__ == "__main__":
    processname = sys.argv[1]
    runtime = sys.argv[2]
    print processname,runtime
    if ":" in runtime:
        second = runtime.split('Time elapsed: ')[1].split(' s')[0]
        try:
            runtime = float(second)
            # print(runtime)
        except Exception as e:
            print("Error in Python: {}\n".format(str(e)))
            with open(psutillog, 'a') as f:
                f.write("Error: {}\n".format(str(e)))
            sys.exit(1)
        for second in range(1, 15):
            pid = getPid(processname)
            if(pid != -1):
                perf(int(pid), runtime)
                break
            time.sleep(0.3)
    else:
        with open(psutillog, 'a') as f:
            f.write("Error no : : {}\n".format(str(runtime)))
        sys.exit(1)
