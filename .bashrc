## only do if interactive shell
[[ $- == *i* ]] || return 0

# Source global definitions
# if [ -f /etc/bashrc ]; then
# 	. /etc/bashrc
# fi

export PS1='\n\s:\!:\h:\n\w\n \$ '

module load gcc/8.2.0 ## only for rstan
module load jags/4.3.0
module unload samtools
module load samtools/1.3.1
module load bcftools
module load vcftools
module load mpi
module load openmpi/2.0.2
module load bedops
module load tabix
module load java/1.8

if [ $( grep 'CentOS Linux release 7' /etc/redhat-release | wc -l ) -eq "1" ]
then
    module load bedtools/2.27.1 ## centos 7 version
    module remove python/2.7.8  # default load, python/3.5.1
    module load python/3.5.1    # default load, python/3.5.1
    module unload R
    module load R/3.6.1
else
    module unload R
fi

export EDITOR="emacs -nw --quick"

alias jupyter_run="xvfb-run jupyter notebook --notebook-dir=~/notebooks"
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



########## functions

get_ext() {
    filename=$(basename -- "${1}")
    extension="${filename##*.}"
    # filename="${filename%.*}"
    # echo 'vcf_ext=${extension}'
    echo ${extension}
}


get_fn_noext() {
    # filename=$(basename -- "${1}")
    filename="${1}"
    filename="${filename%.*}"
    echo ${filename}
}


# get_fn_noext() {
#     filename=$(basename -- "${1}")
#     # extension="${filename##*.}"
#     filename="${filename%.*}"
#     # echo 'vcf_ext=${extension}'
#     echo ${filename}
# }


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

gitrv() {
    git remote -v
}
giturl() {
    git remote set-url $1 $2
}

lsat() {
    ls -alhrt $@
}
## git remote set-url origin https://hostname/USERNAME/REPOSITORY.git

export -f parse
export -f get_ext
export -f get_fn
export -f lsa
export -f gcom
export -f get_time
export -f tolower
export -f giturl
export -f gitrv
export -f lsat
export -f get_fn_noext

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('/nfs/sw/anaconda3/anaconda3-10.19/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
#     eval "$__conda_setup"
# else
#     if [ -f "/nfs/sw/anaconda3/anaconda3-10.19/etc/profile.d/conda.sh" ]; then
#         . "/nfs/sw/anaconda3/anaconda3-10.19/etc/profile.d/conda.sh"
#     else
#         export PATH="/nfs/sw/anaconda3/anaconda3-10.19/bin:$PATH"
#     fi
# fi
# unset __conda_setup
# <<< conda initialize <<<

