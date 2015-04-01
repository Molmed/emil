#!/bin/sh

# Emil version 1.3.2 introduced new coding style guidelines and renamed many
# functions and arguments. This script can be used to convert R-scripts written
# for emil v < 1.3.2 to the new standard.

sed -i 's/\.checkpoint\.dir\b/.checkpoint_dir/g' *
sed -i 's/\.parallel\.cores\b/.cores/g' *
sed -i 's/\.return\.errors\b/.return_error/g' *
sed -i 's/\bbatch\.model\b/batch_model/g' *
sed -i 's/\bemil\.fit\./fit_/g' *
sed -i 's/\bemil\.predict\./predict_/g' *
sed -i 's/\bemil\.vimp\./importance_/g' *
sed -i 's/\berror\.fun\b/error_fun/g' *
sed -i 's/\bevaluate\.modeling\b/evaluate/g' *
sed -i 's/\bfalse\.triggers\b/false_triggers/g' *
sed -i 's/\bfit\.fun\b/fit_fun/g' *
sed -i 's/\bfrac\b/fraction/g' *
sed -i 's/\bget\.debug\.flags\b/get_debug_flags/g' *
sed -i 's/\bindex\.fit\b/index_fit/g' *
sed -i 's/\bindex\.test\b/index_test/g' *
sed -i 's/\bis\.blank\b/is_blank/g' *
sed -i 's/\bmodeling\.FUN\b/modeling_fun/g' *
sed -i 's/\bmodeling\.result\b/modeling_result/g' *
sed -i 's/\bna\.fill\b/na_fill/g' *
sed -i 's/\bpre\.impute\.knn\b/pre_impute_knn/g' *
sed -i 's/\bpre\.impute\.median\b/pre_impute_median/g' *
sed -i 's/\bpred\b/prediction/g' *
sed -i 's/\bpredict\.fun\b/predict_fun/g' *
sed -i 's/\breset\.warn\.once\b/reset_notification/g' *
sed -i 's/\bset\.debug\.flags\b/set_debug_flags/g' *
sed -i 's/\bto\.factor\b/to_factor/g' *
sed -i 's/\bvimp\.fun\b/importance_fun/g' *
sed -i 's/\bvimp\b/importance/g' *
