[[ $- == *i* ]] || return 0

# Source global definitions
# if [ -f /etc/bashrc ]; then
# 	. /etc/bashrc
# fi

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

gitrv() {
    git remote -v
}
giturl() {
    git remote set-url $1 $2
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
