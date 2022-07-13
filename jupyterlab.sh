#!/bin/bash

#SBATCH --job-name=jupyter
#SBATCH --partition=pe2
#SBATCH --time=7-00:00:00
#SBATCH --cpus-per-task=24
#SBATCH --mem=64G
#SBATCH --output=/gpfs/commons/home/khadi/jupyter.log

# module unload python
# module load python/3.9.7


set -a
export thisport=8888
export notebook_directory="sadlfksdfglkhasdlfkhsdfglk_34243987562938472938476_/sdflgkhasdflkjasdglkhsdflkjhdsalkjh123498340957_"
export browser=false
export core=false
export datarate=10000000 ## don't set this too high
export max_size=24000000000

while (( "$#" )); do
    case "$1" in
        -p|--port)
            thisport=$2
            shift
	    shift
            ;;
        -n|--ndir)
            notebook_directory=$2
            shift
	    shift
            ;;
        -b|--browser)
            browser=true
            shift
            ;;
        -c|--core)
            core=true
            shift
            ;;
	--max_size)
	    max_size=$2
	    shift
	    shift
	    ;;
	--datarate)
	    datarate=$2
	    shift
	    shift
	    ;;
        --)
            shift
            break
            ;;
        -*|--*=)
        echo "Error: Unsupported flag $1" >&2
        exit 1
        ;;
        *)
            break
            ;;
    esac
done

dt=$(date +%Y%m%d_%H%M%S)

# host_name=$(cat /etc/hosts | awk '{if (NR == 6) print $2}')

host_name=$(hostname)

log_file=jupyter_${host_name}_${dt}.log

[ -d ${notebook_directory} ] && addon="--notebook-dir ${notebook_directory}"
${core} && addon="${addon} --core-mode"
addon="${addon} --ServerApp.max_buffer_size=${max_size} --NotebookApp.max_buffer_size=${max_size} --ServerApp.iopub_data_rate_limit=${datarate} --NotebookApp.iopub_data_rate_limit=${datarate}"

cat /etc/hosts | tee ${log_file}

echo "see log file"
echo "${log_file}"
if ${browser}; then
    set -x
    jupyter lab --ip=0.0.0.0 --port=${thisport} \
	    ${addon} \
	    2>&1 | tee -a ${log_file}
    set +x
else
    set -x
    jupyter lab --ip=0.0.0.0 --port=${thisport} \
	    ${addon} \
	    --no-browser \
	    2>&1 | tee -a ${log_file}
    set +x
fi

