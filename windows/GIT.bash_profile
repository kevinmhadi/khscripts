{
    if [ -n "$INSIDE_EMACS" ]; then
    export PS1='\[\033[32m\]\u@\h \[\033[33m\]\w\[\033[36m\]`__git_ps1`\[\033[0m\]\n$ '
fi

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
}
