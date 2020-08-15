# .bashrc

#export DISPLAY=:0.0

# Source global definitions
# if [ -f /etc/bashrc ]; then
# 	. /etc/bashrc
# fi

# User specific aliases and functions

# PATH="/nethome/mimielinski/perl5/bin${PATH+:}${PATH}"; export PATH;
# PERL5LIB="/nethome/mimielinski/perl5/lib/perl5${PERL5LIB+:}${PERL5LIB}"; export PERL5LIB;
# PERL_LOCAL_LIB_ROOT="/nethome/mimielinski/perl5${PERL_LOCAL_LIB_ROOT+:}${PERL_LOCAL_LIB_ROOT}"; export PERL_LOCAL_LIB_ROOT;
# PERL_MB_OPT="--install_base \"/nethome/mimielinski/perl5\""; export PERL_MB_OPT;
# PERL_MM_OPT="INSTALL_BASE=/nethome/mimielinski/perl5"; export PERL_MM_OPT;

source ~/.bash_profile



# added by Miniconda3 4.1.11 installer
#export PATH="/gpfs/commons/home/mimielinski/Software/miniconda3/bin:$PATH"

# export PATH=/gpfs/commons/groups/imielinski_lab/Software/anaconda3/bin:$PATH
MANPATH=$MANPATH:$HOME/share/man

PATH="/gpfs/commons/home/khadi/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/gpfs/commons/home/khadi/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/gpfs/commons/home/khadi/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/gpfs/commons/home/khadi/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/gpfs/commons/home/khadi/perl5"; export PERL_MM_OPT;
