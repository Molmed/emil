#!/bin/sh

# Emil version 1.3.2 introduced new coding style guidelines and renamed many
# functions and arguments. This script can be used to convert R-scripts written
# for emil v < 1.3.2 to the new standard. The script is unfortunatelly not able
# to fix all possible problems, but can still be of good help in most cases.

if [ $# -eq 0 ]
  then
    echo "No files given"
fi

sed -i 's/\.checkpoint\.dir\b/.checkpoint_dir/g' $1
sed -i 's/\.parallel\.cores\b/.cores/g' $1
sed -i 's/\.return\.errors\b/.return_error/g' $1
sed -i 's/\.save=list/.save=c/g' $1
sed -i 's/\bbatch\.model\b/evaluate/g' $1
sed -i 's/"crossval"/"crossvalidation"/g' $1
sed -i 's/\bemil\.fit\./fit_/g' $1
sed -i 's/\bemil\.predict\./predict_/g' $1
sed -i 's/\bemil\.vimp\./importance_/g' $1
sed -i 's/\berror\.fun\b/error_fun/g' $1
sed -i 's/\bevaluate\.modeling\b/evaluate/g' $1
sed -i 's/\bfalse\.triggers\b/false_triggers/g' $1
sed -i 's/\bfit\.fun\b/fit_fun/g' $1
sed -i 's/fit=TRUE/model=TRUE/g' $1
sed -i 's/\bfrac\b/fraction/g' $1
sed -i 's/\bget\.debug\.flags\b/get_debug_flags/g' $1
sed -i 's/\bindex\.fit\b/index_fit/g' $1
sed -i 's/\bindex\.test\b/index_test/g' $1
sed -i 's/\bis\.blank\b/is_blank/g' $1
sed -i 's/\bmodeling\.FUN\b/modeling_fun/g' $1
sed -i 's/\bmodeling\.procedure\b/modeling_procedure/g' $1
sed -i 's/\bmodeling\.result\b/modeling_result/g' $1
sed -i 's/\bna\.fill\b/na_fill/g' $1
sed -i 's/\bnice\.require\b/nice_require/g' $1
sed -i 's/\bnrep\b/nrepeat/g' $1
sed -i 's/\bparam\b/parameter/g' $1
sed -i 's/\bprob\b/probability/g' $1
sed -i 's/\bpre\.impute\.knn\b/pre_impute_knn/g' $1
sed -i 's/\bpre\.impute\.median\b/pre_impute_median/g' $1
sed -i 's/\bpred\b/prediction/g' $1
sed -i 's/\bpredict\.fun\b/predict_fun/g' $1
sed -i 's/\breset\.warn\.once\b/reset_notification/g' $1
sed -i 's/\bset\.debug\.flags\b/set_debug_flags/g' $1
sed -i 's/\bto\.factor\b/to_factor/g' $1
sed -i 's/\bvimp\.fun\b/importance_fun/g' $1
sed -i 's/\bvimp\b/importance/g' $1
