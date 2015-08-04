#!/bin/sh

# Emil version 1.3.2 introduced new coding style guidelines and renamed many
# functions and arguments. This script can be used to convert R-scripts written
# for emil v < 1.3.2 to the new standard. The script is unfortunatelly not able
# to fix all possible problems, but can still be of good help in most cases.

if [ $# -eq 0 ]
  then
    echo "No myfiles given"
fi

for myfile in "$@"
do
	sed -i 's/\.checkpoint\.dir\b/.checkpoint_dir/g' $myfile
	sed -i 's/\.parallel\.cores\b/.cores/g' $myfile
	sed -i 's/\.return\.errors\b/.return_error/g' $myfile
	sed -i 's/\.save=list/.save=c/g' $myfile
	sed -i 's/\bbatch\.model\b/evaluate/g' $myfile
	sed -i 's/"crossval"/"crossvalidation"/g' $myfile
	sed -i 's/\bemil\.fit\./fit_/g' $myfile
	sed -i 's/\bemil\.predict\./predict_/g' $myfile
	sed -i 's/\bemil\.vimp\./importance_/g' $myfile
	sed -i 's/\berror\.fun\b/error_fun/g' $myfile
	sed -i 's/\bevaluate\.modeling\b/evaluate/g' $myfile
	sed -i 's/\bfalse\.triggers\b/false_triggers/g' $myfile
	sed -i 's/\bfit\.fun\b/fit_fun/g' $myfile
	sed -i 's/fit=TRUE/model=TRUE/g' $myfile
	sed -i 's/\bfrac\b/fraction/g' $myfile
	sed -i 's/\bget\.debug\.flags\b/get_debug_flags/g' $myfile
	sed -i 's/\bindex\.fit\b/index_fit/g' $myfile
	sed -i 's/\bindex\.test\b/index_test/g' $myfile
	sed -i 's/\bis\.blank\b/is_blank/g' $myfile
	sed -i 's/\bmodeling\.FUN\b/modeling_fun/g' $myfile
	sed -i 's/\bmodeling\.procedure\b/modeling_procedure/g' $myfile
	sed -i 's/\bmodeling\.result\b/modeling_result/g' $myfile
	sed -i 's/\bna\.fill\b/na_fill/g' $myfile
	sed -i 's/\bnice\.axis\b/nice_axis/g' $myfile
	sed -i 's/\bnice\.box\b/nice_box/g' $myfile
	sed -i 's/\bnice\.require\b/nice_require/g' $myfile
	sed -i 's/\bnrep\b/nrepeat/g' $myfile
	sed -i 's/\([^@]\)\bparam\b/\1parameter/g' $myfile
	# The @ group is added to avoid interfering with the roxygen keyword "@param".
	sed -i 's/\bprob\b/probability/g' $myfile
	sed -i 's/\bpre\.impute\.knn\b/pre_impute_knn/g' $myfile
	sed -i 's/\bpre\.impute\.median\b/pre_impute_median/g' $myfile
	sed -i 's/\bpre\.process\b/pre_process/g' $myfile
	sed -i 's/\bpred\b/prediction/g' $myfile
	sed -i 's/\bpredict\.fun\b/predict_fun/g' $myfile
	sed -i 's/\breset\.warn\.once\b/reset_notification/g' $myfile
	sed -i 's/\bset\.debug\.flags\b/set_debug_flags/g' $myfile
	sed -i 's/\bto\.factor\b/to_factor/g' $myfile
	sed -i 's/\bvimp\.fun\b/importance_fun/g' $myfile
	sed -i 's/\bvimp\b/importance/g' $myfile
done
# function calls to `integer.events` must manually be replaced with
# appropriate calls to `dichotomize()`.
