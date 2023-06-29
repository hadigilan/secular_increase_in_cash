

#
#	required packages
#

library(DoubleML)
library(mlr3)
library(mlr3learners)
remotes::install_github("mlr-org/mlr3extralearners")
library(mlr3extralearners)
library(data.table)


#
#	Learners for DML
#


ml_g_boost_d1_s0.1<- lrn("regr.gbm", distribution ='gaussian', interaction.depth = 1, shrinkage = 0.1, cv.folds = 10)
ml_m_boost_d1_s0.1<- lrn("regr.gbm", distribution ='gaussian', interaction.depth = 1, shrinkage = 0.1, cv.folds = 10)						


ml_g_lasso<- lrn("regr.cv_glmnet", s = "lambda.min", alpha=1, nfolds=10, family='gaussian')
ml_m_lasso<- lrn("regr.cv_glmnet", s = "lambda.min", alpha=1, nfolds=10, family='gaussian')


ml_g_tree_cp0.01<- lrn("regr.rpart", cp=0.01)				# cp stands for "complexity parameter" / default value for CV is 10 (xval=10)
ml_m_tree_cp0.01<- lrn("regr.rpart", cp=0.01)



#
#	Data sets for DML
#

data_DML<- scale(final_data.w[,c('ln_netcash',  'cash.asset',
                                 'size', 'mtb', 'lev', 'div', 'cfo', 'volcfo_ind', 'nwc', 'acq', 'capex',							
                                 'salegro', 'zscore', 'efftax', 'roa', 'volprof_firm', 'netdebt', 'netequ', 'netinv', 'saleppe', 'salepiv', 'overinv', 'volequi_firm', 'volequi_ind', 'neginc', 'forinc', 'dominc',
                                 'rd', 'intang', 'tang', 'taxcost', 'carry', 'mature', 'diversity', 'relation', 'foresale')])


#
#	separate causal effect (RD for example)
#


data_lnnetcash_rd<- DoubleMLData$new(data = as.data.table(data_DML), y_col = 'ln_netcash', 
                                     x_cols = c('size', 'mtb', 'lev', 'div', 'cfo', 'volcfo_ind', 'nwc', 'acq', 'capex',								
                                                'salegro', 'zscore', 'efftax', 'roa', 'volprof_firm', 'netdebt', 'netequ', 'netinv', 'saleppe', 'salepiv', 'overinv', 'volequi_firm', 'volequi_ind', 'neginc', 'forinc', 'dominc'),
                                     d_cols = 'rd')
boost_d1_s0.1_cross4_dml2_lnnetcash_rd<- DoubleMLPLR$new(data=data_lnnetcash_rd, ml_g=ml_g_boost_d1_s0.1, ml_m=ml_m_boost_d1_s0.1, score="partialling out", n_rep=100, n_folds=4, dml_procedure="dml2")
boost_d1_s0.1_cross4_dml2_lnnetcash_rd$fit()
boost_d1_s0.1_cross4_dml2_lnnetcash_rd$summary()


lasso_cross4_dml2_lnnetcash_rd<- DoubleMLPLR$new(data=data_lnnetcash_rd, ml_g=ml_g_lasso, ml_m=ml_m_lasso, score="partialling out", n_rep=100, n_folds=4, dml_procedure="dml2")
lasso_cross4_dml2_lnnetcash_rd$fit()
lasso_cross4_dml2_lnnetcash_rd$summary()


tree_cp0.01_cross4_dml2_lnnetcash_rd<- DoubleMLPLR$new(data=data_lnnetcash_rd, ml_g=ml_g_tree_cp0.01, ml_m=ml_m_tree_cp0.01, score="partialling out", n_rep=100, n_folds=4, dml_procedure="dml2")
tree_cp0.01_cross4_dml2_lnnetcash_rd$fit()
tree_cp0.01_cross4_dml2_lnnetcash_rd$summary()




#
#	simultaneous causal effects (AFTER controlling for the effect of the other drivers)	
#

data_lnnetcash_simul<- DoubleMLData$new(data = as.data.table(data_DML), use_other_treat_as_covariate=TRUE, 
                                        y_col = 'ln_netcash', x_cols = c('size', 'mtb', 'lev', 'div', 'cfo', 'volcfo_ind', 'nwc', 'acq', 'capex',								
                                                                         'salegro', 'zscore', 'efftax', 'roa', 'volprof_firm', 'netdebt', 'netequ', 'netinv', 'saleppe', 'salepiv', 'overinv', 'volequi_firm', 'volequi_ind', 'neginc', 'forinc', 'dominc'),
                                        d_cols = c('rd', 'intang', 'tang', 'taxcost', 'carry', 'mature', 'diversity', 'relation', 'foresale' ))

boost_d1_s0.1_cross4_dml2_lnnetcash_simul<- DoubleMLPLR$new(data=data_lnnetcash_simul, ml_g=ml_g_boost_d1_s0.1, ml_m=ml_m_boost_d1_s0.1, score="partialling out", n_rep=100, n_folds=4, dml_procedure="dml2")
boost_d1_s0.1_cross4_dml2_lnnetcash_simul$fit()
boost_d1_s0.1_cross4_dml2_lnnetcash_simul$summary()
boost_d1_s0.1_cross4_dml2_lnnetcash_simul$bootstrap(method = "normal", n_rep_boot = 1000)		# for joint inference
boost_d1_s0.1_cross4_dml2_lnnetcash_simul$p_adjust(method = "romano-wolf")				  	      # Romano-Wolf adjusting
round(boost_d1_s0.1_cross4_dml2_lnnetcash_simul$p_adjust(method = "BY"), 3)					        # Benjamini-Yekutieli adjusting
round(boost_d1_s0.1_cross4_dml2_lnnetcash_simul$p_adjust(method = "bonferroni"), 3)			    # Bonferroni adjusting


lasso_cross4_dml2_lnnetcash_simul<- DoubleMLPLR$new(data=data_lnnetcash_simul, ml_g=ml_g_lasso, ml_m=ml_m_lasso, score="partialling out", n_rep=100, n_folds=4, dml_procedure="dml2")
lasso_cross4_dml2_lnnetcash_simul$fit()
lasso_cross4_dml2_lnnetcash_simul$summary()								
lasso_cross4_dml2_lnnetcash_simul$bootstrap(method = "normal", n_rep_boot = 1000)		
lasso_cross4_dml2_lnnetcash_simul$p_adjust(method = "romano-wolf")				
round(lasso_cross4_dml2_lnnetcash_simul$p_adjust(method = "BY"), 3)			
round(lasso_cross4_dml2_lnnetcash_simul$p_adjust(method = "bonferroni"), 3)

tree_cp0.01_cross4_dml2_lnnetcash_simul<- DoubleMLPLR$new(data=data_lnnetcash_augment_simul, ml_g=ml_g_tree_cp0.01, ml_m=ml_m_tree_cp0.01, score="partialling out", n_rep=100, n_folds=4, dml_procedure="dml2")
tree_cp0.01_cross4_dml2_lnnetcash_simul$fit()
tree_cp0.01_cross4_dml2_lnnetcash_simul$summary()
tree_cp0.01_cross4_dml2_lnnetcash_simul$bootstrap(method = "normal", n_rep_boot = 1000)		
tree_cp0.01_cross4_dml2_lnnetcash_simul$p_adjust(method = "romano-wolf")				
round(tree_cp0.01_cross4_dml2_lnnetcash_simul$p_adjust(method = "BY"), 3)				
round(tree_cp0.01_cross4_dml2_lnnetcash_simul$p_adjust(method = "bonferroni"), 3)		

