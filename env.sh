# Source this before running satools

export SATOOLS="/home/mjuric/projects/satools"
export SDSSAST_WORKSPACE="$SATOOLS/workspace"
export PERL5LIB="$SATOOLS/perl_modules/lib64/perl5:$SATOOLS/perl_modules/lib/perl5"
export CONF_SDSS=~/.conf.sdss
export PATH="$PATH:$SATOOLS/scripts:$SATOOLS/bin"

"$@"

