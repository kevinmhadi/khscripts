## only do if interactive shell
[[ $- == *i* ]] || return 0

export PATH=$(pwd)/score-client-5.8.1/bin:$PATH

# Source global definitions
# if [ -f /etc/bashrc ]; then
# 	. /etc/bashrc
# fi

# set -a
stty -ixon -ixoff
export PS1='\n\s:\!:\H:\n\w\n \$ '

# module load gcc/8.2.0 ## only for rstan
module load gcc/9.2.0 ## only for rstan
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
# module load vim

if [ $( grep 'CentOS Linux release 7' /etc/redhat-release | wc -l ) -eq "1" ]
then
    module load bedtools/2.27.1 ## centos 7 version
    module remove python/2.7.8  # default load, python/3.5.1
    module load python/3.5.1    # default load, python/3.5.1
    module unload R
    # module load R/3.6.1
    module load R/4.0.2
else
    module unload R
fi


alias jupyter_run="xvfb-run jupyter notebook --notebook-dir=~/notebooks"
alias mskilab="ssh mskilab"
alias picard="java -jar ~/modules/BWAMem/picard.jar"
alias lsh="ls -lh"
alias lss="ls -lSrh"
alias lst="ls -lhrt"
alias lsd="ls -ld */"
alias duh="du -h --max-depth 1"
alias wcmc="ssh -Y keh2019@aristotle.med.cornell.edu"
alias penn="ssh pennstation"
alias harlem="ssh harlem"
alias sr="screen -r -d"
alias sd="screen -d -r"
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

gitcommit() {
    git commit -a -m "$@"
}

parse() {
    echo "$@" | xargs | xargs
}

gitremote() {
    git remote -v
}
gitseturl() {
    # git remote set-url $1 $2
    git remote set-url $@
}
gitsetremote() {
    git remote set-url $@
}

lsat() {
    ls -alhrt $@
}

gitsetupstream() {
    git push --set-upstream $@
}

getallgitbranches() {
    git branch -r | grep -v '\->' | while read remote; do git branch --track "${remote#origin/}" "$remote"; done
    git fetch --all
}

gitgetbranches() {
    git branch -r | grep -v '\->' | while read remote; do git branch --track "${remote#origin/}" "$remote"; done
    git fetch --all
}


gitfastforward() {
    if [ $# -eq 0 ]; then
	BRANCH="master"
    else
	BRANCH="$1"
    fi
    git merge --ff-only origin/${BRANCH}
}


trimspaces() {
    echo $1 | sed 's/[[:space:]]\{2,\}\|[	]\+/ /g'
}


trimspaces2() {
    echo $1 | sed 's/\\[[:space:]]\+/ /g' | sed "s/[[:space:]]\{2,\}\|[$(printf '\t')]\+/ /g"
}
## git remote set-url origin https://hostname/USERNAME/REPOSITORY.git

# echo $(printf '%(%Y%m%d%H%M%S)T\n' -1)

chmodv() {
    per=$1; shift
    chmod -v ${per} $@
}

scp_hop() {
    local_path=$1
    jump_host=$2
    host=$3
    destination_path=$4
    # scp -o ProxyCommand="ssh $1
    set -x
    scp -o ProxyCommand="ssh $jump_host nc $host 22" $local_path $host:$destination_path
    set +x
}

export -f parse
export -f get_ext
export -f get_fn
export -f lsa
export -f gcom
export -f get_time
export -f tolower
export -f gitseturl
export -f gitremote
export -f gitsetremote
export -f gitcommit
export -f lsat
export -f get_fn_noext
export -f getallgitbranches
export -f gitgetbranches
export -f gitsetupstream
export -f gitfastforward
export -f trimspaces
export -f trimspaces2
export -f scp_hop
export -f chmodv

# # >>> conda initialize >>>
# # !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('/gpfs/commons/groups/imielinski_lab/Software/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
#     eval "$__conda_setup"
# else
#     if [ -f "/gpfs/commons/groups/imielinski_lab/Software/anaconda3/etc/profile.d/conda.sh" ]; then
#         . "/gpfs/commons/groups/imielinski_lab/Software/anaconda3/etc/profile.d/conda.sh"
#     else
#         export PATH="/gpfs/commons/groups/imielinski_lab/Software/anaconda3/bin:$PATH"
#     fi
# fi
# unset __conda_setup
# # <<< conda initialize <<<[w

