{
    # .bash_profile

    # # Get the aliases and functions
    # if [ -f ~/.bashrc ]; then
    #	source ~/.bashrc
    # fi

    # User specific environment and startup programs

    # stty -ixon
    # stty -ixoff

    export XTERM=xterm-256color
    export TERM=xterm-256color
    export LOCKPRG='/bin/true'
    export LD_LIBRARY_PATH=/gpfs/commons/groups/imielinski_lab/Software/libtiff/lib:/gpfs/commons/home/mimielinski/software/libfftw3/lib:$LD_LIBRARY_PATH
    export LD_LIBRARY_PATH=/gpfs/commons/groups/imielinski_lab/Software/protobuf/lib:~/modules/Snowman:$LD_LIBRARY_PATH:~/modules/Snowman/lib
    export PATH=/gpfs/commons/groups/imielinski_lab/Software/mosh2/mosh-1.3.2/bin:/gpfs/commons/groups/imielinski_lab/Software/protobuf/bin:~/software/libtiff/bin:~/software/libtiff/lib:~/.aspera/connect/bin:~/software/xsel-1.2.0/bin:~/software/gsl-2.4:~/lab/.local/bin/:~/git/dapars/:~/lab/git/MACS/bin/:~/scripts/:~/modules/Snowman/:~/modules/STAR:/nethome/mimielinski/Software/sshpass-1.05:~/Software/sratoolkit.2.5.7-centos_linux64/bin:~/Software/HMMCopy/HMMcopy/bin/:~/Software/UCSC/:~/software/emacs-25.2/bin:$PATH:$HOME/bin:$HOME/dev/scripts/:$HOME/Scripts/bash:~/lab/Software/CPLEX/CPLEX_Studio/cplex/bin/x86-64_linux/:${HOME}/scripts_kh
    export SKI_DB_ROOT="~/DB/"
    export SKI_SOFTWARE_ROOT="~/Software/"
    export GIT_HOME="~/git"
    # export DEFAULT_BSGENOME="/data/research/mski_lab/DB/UCSC/hg19.broad.chrom.sizes"
    export DEFAULT_BSGENOME="~/DB/UCSC/hg19.broad.chrom.sizes"
    # export DISPLAY=:0.0
    # export DISPLAY=localhost:12.0
    # export DISPLAY="" # the default
    export GENCODE_DIR=~/DB/GENCODE

    export CPLEX_DIR=/gpfs/commons/home/khadi/lab/software/CPLEX/CPLEX_Studio

    export R_DATATABLE_NUM_THREADS=1

    export EDITOR="emacs -nw --quick"

    export C_INCLUDE_PATH=/gpfs/commons/groups/imielinski_lab/Software/libtiff/include:$C_INCLUDE_PATH

    export protobuf_CFLAGS="-I$HOME/software/protobuf/include"
    export protobuf_LIBS=$HOME/software/protobuf/lib/libprotobuf.a
    # export protobuf_LIBS=$HOME/software/protobuf/lib

    # IFS=$' \t\n' ## default IFS value for bash

    # module load gcc/4.9.2
    module load gcc/8.2.0 ## only for rstan
    # module load gcc/6.2.0
    # module load bwa/0.7.12
    # module load perl/5.10.0
    module load jags/4.3.0
    module unload samtools
    module load samtools/1.3.1
    #module load python/2.7.8
    #module load python/2.7.11
    #module load python/2.7.10
    module load bcftools
    module load vcftools

    module load mpi
    module load openmpi/2.0.2

    module load bedops
    module load tabix

    module load java/1.8

    # export TMP=~/lab/home/khadi/tmp
    # export TMPDIR=~/lab/home/khadi/tmp

    if [ "$(hostname)" = "mskilab01.c.nygenome.org" ] || [ "$(hostname)" = "mskilab02.c.nygenome.org" ]
    then
	path_to_tmpdir="/scratch/$(whoami)"
    else
	path_to_tmpdir="/tmp"
    fi


    # if [ -d /scratch ]
    # then
    #     path_to_tmpdir="/scratch/$(whoami)"
    # else
    #     path_to_tmpdir="/tmp"
    # fi

    mkdir -p ${path_to_tmpdir}
    export TMP=${path_to_tmpdir}
    export TMPDIR=${path_to_tmpdir}



    #export PYTHONPATH="/gpfs/commons/groups/imielinski_lab/lib/python-2.7.8/site-packages:$PYTHONPATH"
    #export PYTHONPATH=~/software/deepTools-2.0/lib/python3.5/site-packages:$PYTHONPATH
    # export PYTHONPATH=~/software/python/2.7.8:$PYTHONPATH

    if [ $( grep 'CentOS Linux release 7' /etc/redhat-release | wc -l ) -eq "1" ]
    then
	# export R_LIBS="~/lab/lib/R-3.4.1_alt:~/lab/lib/R-3.4.1/"
	# export R_LIBS="~/lab/lib/R-3.4.1"
	# export R_LIBS="~/lab/lib/R-3.5.1"
	export R_LIBS="~/lab/lib/R-3.6.1"
	echo "Centos 7 detected: R_LIBS=${R_LIBS}" 1>&2
	module load bedtools/2.27.1 ## centos 7 version
	module remove python/2.7.8  # default load, python/3.5.1
	module load python/3.5.1    # default load, python/3.5.1
	# module load R/3.4.1
	module unload R
	# module load R/3.5.1
	module load R/3.6.1
	#    export R_LIBS="/data/research/mski_lab/Software/R7"
	# module load R/3.3.0  # changed to 3.3.0 from 3.1.3
	# module load R/3.3.0
	# export R_LIBS='~/lab/lib/R-3.3.0/'
	# module load R/3.4.1
	# module load $
	# export R_LIBS='~/lab/lib/R-3.4.1/'
    else
	export PATH=~/software/bedtools/bedtools2/bin/:$PATH
	R_LIBS=~/software/anaconda3/lib/R/library
	# export R_LIBS="~/lab/Software/R"
	module unload R
	# module load R/3.3.0
	export PATH=/gpfs/commons/groups/imielinski_lab/Software/anaconda3/bin:$PATH
	# export PATH="/gpfs/commons/groups/imielinski_lab/Software/miniconda3/bin:$PATH"
	echo "Centos 6 detected:  R_LIBS=${R_LIBS}" 1>&2
	# export R_LIBS=~/software/anaconda3/lib/R/library
	# export R_LIBS=~/lab/Software/R:~/lab/lib/R-miniconda3:~/lab/lib/R-3.3.0-Centos6
	export PYTHONPATH=""
	# export R_LIBS="~/lab/lib/R-miniconda3"
	#    export R_LIBS='~/lab/lib/R-3.3.0-Centos6'
	# export PYTHONPATH=/gpfs/commons/groups/imielinski_lab/lib/python-3.5.1-CentOS6/site-packages
    fi


    alias jupyter_run="xvfb-run jupyter notebook --notebook-dir=~/notebooks"


    # Finished adapting your PATH environment variable for use with MacPorts.

    # PS1='\s:\h:\w \! \$ '
    # PS1='\s:\!:\h:\n\w\n \$ '
    PS1='\n\s:\!:\h:\n\w\n \$ '

    alias mskilab="ssh mskilab"
    alias picard="java -jar ~/modules/BWAMem/picard.jar"
    alias lsh="ls -lh"
    alias lss="ls -lSrh"
    alias lst="ls -lhrt"
    alias lsd="ls -ld */"
    alias duh="du -h --max-depth 1"
    alias michor="ssh marcin@sphinx.dfci.harvard.edu"
    alias eris="ssh -Y mbi1@rgs03.research.partners.org"
    alias cga3="ssh -Y marcin@cga3.broadinstitute.org"
    alias cga2="ssh -Y marcin@cga2.broadinstitute.org"
    alias copper="ssh -Y marcin@copper.broadinstitute.org"
    alias gold="ssh -Y marcin@gold.broadinstitute.org"
    alias kras="ssh -Y marcin@cga-kras.broadinstitute.org"
    alias tp53="ssh -Y marcin@cga-tp53.broadinstitute.org"
    alias rb1="ssh -Y marcin@cga-rb1.broadinstitute.org"
    alias cdk="ssh -Y marcin@cga-cdkn2a.broadinstitute.org"
    alias wcmc="ssh -Y mai9037@aristotle.med.cornell.edu"
    alias penn="ssh pennstation"
    alias harlem="ssh harlem"
    alias sr="screen -r -d"
    alias sd="screen -d -r"
    alias dcc="sftp ImielinskM@dccsftp.nci.nih.gov"
    alias rf="readlink -f"
    alias TIME="~/Software/time/time -v"
    alias igv='java -Xmx4g -jar ~/software/IGV_2.3.25/igv.jar'
    alias vncspawn="vncserver -geometry 2560x1440"
    alias vnckill="vncserver -kill"
    alias ec="emacsclient -nw"
    # alias ecs="emacsclient -nw -s"
    alias ew="emacs -nw"
    # alias ewd="emacs -nw --daemon=\"daemon\""
    alias getip="ifconfig | grep \"inet \" | grep -Fv 127.0.0.1 | awk '{print \$2}'"
    alias p3="source ~/scripts/module_load_python3"
    alias p2="source ~/scripts/module_load_python2"

    # for i in $( screen -ls | grep "Attached" | awk '{print $1}' );
    # do
    #     screen -D $i
    # done

    # function emd {
    #     nm="$1"
    #     emacs -nw --daemon="$nm"
    # }



    # if $(screen -ls | grep -q "Detached");
    # then screen -RR;
    # else echo "No screen to attach";
    # fi

    # trap ~/detach_all.sh EXIT

    ########## functions

    get_ext() {
	filename=$(basename -- "${1}")
	extension="${filename##*.}"
	filename="${filename%.*}"
	# echo 'vcf_ext=${extension}'
	echo ${extension}
    }

    get_fn() {
	fn=`readlink -f $1`
	filename=$(basename -- "$1")
	echo "`dirname "${fn}"`/${filename}"
    }

    tolower() {
	echo $1 | tr '[:upper:]' '[:lower:]'
    }


    get_time() {
	date +%Y%m%d_%H%M%S
    }

    lsa() {
	ls -alh $@
    }

    gcom() {
	git commit -a -m "$@"
    }

    parse() {
	echo "$@" | xargs | xargs
    }

    export -f parse
    export -f get_ext
    export -f get_fn
    export -f lsa
    export -f gcom
    export -f get_time
    export -f tolower
    export PATH=${PATH}:/gpfs/commons/home/khadi/edirect
}
