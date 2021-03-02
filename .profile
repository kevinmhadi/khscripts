export XTERM=xterm-256color
export TERM=xterm-256color
export LOCKPRG='/bin/true'
export LD_LIBRARY_PATH=/gpfs/commons/groups/imielinski_lab/Software/libtiff/lib:/gpfs/commons/home/mimielinski/software/libfftw3/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=/gpfs/commons/groups/imielinski_lab/Software/protobuf/lib:~/modules/Snowman:$LD_LIBRARY_PATH:~/modules/Snowman/lib
export PATH=/gpfs/commons/groups/imielinski_lab/Software/mosh2/mosh-1.3.2/bin:/gpfs/commons/groups/imielinski_lab/Software/protobuf/bin:~/software/libtiff/bin:~/software/libtiff/lib:~/.aspera/connect/bin:~/software/xsel-1.2.0/bin:~/software/gsl-2.4:~/lab/.local/bin/:~/git/dapars/:~/lab/git/MACS/bin/:~/scripts/:~/modules/Snowman/:~/modules/STAR:/nethome/mimielinski/Software/sshpass-1.05:~/Software/sratoolkit.2.5.7-centos_linux64/bin:~/Software/HMMCopy/HMMcopy/bin/:~/Software/UCSC/:~/software/emacs-25.2/bin:$PATH:$HOME/bin:$HOME/dev/scripts/:$HOME/Scripts/bash:~/lab/Software/CPLEX/CPLEX_Studio/cplex/bin/x86-64_linux/:${HOME}/scripts_kh
export SKI_DB_ROOT="~/DB/"
export SKI_SOFTWARE_ROOT="~/Software/"
export GIT_HOME="~/git"
export DEFAULT_BSGENOME="~/DB/UCSC/hg19.broad.chrom.sizes"
# export DISPLAY="" # the default
export GENCODE_DIR=~/DB/GENCODE

export CPLEX_DIR=/gpfs/commons/home/khadi/lab/software/CPLEX/CPLEX_Studio

export R_DATATABLE_NUM_THREADS=1
export R_REMOTES_NO_ERRORS_FROM_WARNINGS=TRUE

export C_INCLUDE_PATH=/gpfs/commons/groups/imielinski_lab/Software/libtiff/include:$C_INCLUDE_PATH

export protobuf_CFLAGS="-I$HOME/software/protobuf/include"
export protobuf_LIBS=$HOME/software/protobuf/lib/libprotobuf.a
# export protobuf_LIBS=$HOME/software/protobuf/lib


MANPATH=$MANPATH:$HOME/share/man


PATH="/gpfs/commons/home/khadi/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/gpfs/commons/home/khadi/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/gpfs/commons/home/khadi/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/gpfs/commons/home/khadi/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/gpfs/commons/home/khadi/perl5"; export PERL_MM_OPT;


# IFS=$' \t\n' ## default IFS value for bash


if [ "$(hostname)" = "mskilab01.c.nygenome.org" ] || [ "$(hostname)" = "mskilab02.c.nygenome.org" ]
then
    path_to_tmpdir="/scratch/$(whoami)"
else
    path_to_tmpdir="/tmp"
fi


mkdir -p ${path_to_tmpdir}
export TMP=${path_to_tmpdir}
export TMPDIR=${path_to_tmpdir}



if [ $( grep 'CentOS Linux release 7' /etc/redhat-release | wc -l ) -eq "1" ]
then
    export R_LIBS="~/lab/lib/R-3.6.1"
    echo "Centos 7 detected: R_LIBS=${R_LIBS}" 1>&2
else
    export PATH=~/software/bedtools/bedtools2/bin/:$PATH
    R_LIBS=~/software/anaconda3/lib/R/library
    export PATH=/gpfs/commons/groups/imielinski_lab/Software/anaconda3/bin:$PATH
    echo "Centos 6 detected:  R_LIBS=${R_LIBS}" 1>&2
    export PYTHONPATH=""
fi

export PYTHONUNBUFFERED=1


# if [ "$BASH" ]; then
#     . ~/.bashrc
# fi
