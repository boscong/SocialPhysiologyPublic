# -*- coding: utf-8 -*-

# RFECV
from sklearn.feature_selection import RFECV
splitter = KFold(n_splits=1)

# (1)RF
RF_rfecv = RFECV(estimator=RFgrid,          # 学习器
                 min_features_to_select=20, # 最小选择的特征数量
                 step=1,                 # 移除特征个数
                 cv=splitter,  # 交叉验证次数
                 scoring='neg_mean_squared_error',     # 学习器的评价标准
                 verbose = 1, n_jobs = -1
                 ).fit(X, y)

RF_X_RFECV = RF_rfecv.transform(X) # 全部特征和有效特征
print("RFECV特征选择结果——————————————————————————————————————————————————")
print("有效特征个数 : %d" % RF_rfecv.n_features_) # 8162
print("全部特征等级 : %s" % list(RF_rfecv.ranking_))

min(abs(RF_rfecv.cv_results_['mean_test_score'])) # 0.08130169208861156
RF_mean_score = pd.DataFrame(RF_rfecv.cv_results_['mean_test_score'], columns = ["negMSE"])
RF_mean_score.to_csv('E:/Rfiles/R workstation/RF_mean_score.csv', index=None)
RF_rank = pd.DataFrame({"id":X.columns,
                        "ranking":RF_rfecv.ranking_, 
                        "support":RF_rfecv.support_})
RF_rank.to_csv('E:/Rfiles/R workstation/RF_rank.csv', index=None)

# (2)GBDT
GBDT_rfecv = RFECV(estimator=GBDTgrid,          # 学习器
                 min_features_to_select=20, # 最小选择的特征数量
                 step=1,                 # 移除特征个数
                 cv=splitter,  # 交叉验证次数
                 scoring='neg_mean_squared_error',     # 学习器的评价标准
                 verbose = 1, n_jobs = -1
                 ).fit(X, y)

print("有效特征个数 : %d" % GBDT_rfecv.n_features_) # 14459
min(abs(GBDT_rfecv.cv_results_['mean_test_score'])) # 0.08024809475313434
GBDT_mean_score = pd.DataFrame(GBDT_rfecv.cv_results_['mean_test_score'], columns = ["negMSE"])
GBDT_mean_score.to_csv('E:/Rfiles/R workstation/GBDT_mean_score.csv', index=None)
GBDT_rank = pd.DataFrame({"id":X.columns,
                        "ranking":GBDT_rfecv.ranking_, 
                        "support":GBDT_rfecv.support_})
GBDT_rank.to_csv('E:/Rfiles/R workstation/GBDT_rank.csv', index=None)

# (3)XGB
XGB_rfecv = RFECV(estimator=XGBgrid,          # 学习器
                 min_features_to_select=20, # 最小选择的特征数量
                 step=1,                 # 移除特征个数
                 cv=splitter,  # 交叉验证次数
                 scoring='neg_mean_squared_error',     # 学习器的评价标准
                 verbose = 1, n_jobs = -1
                 ).fit(X, y)

print("有效特征个数 : %d" % XGB_rfecv.n_features_) # 317
min(abs(XGB_rfecv.cv_results_['mean_test_score'])) # 0.11885431778138544
XGB_mean_score = pd.DataFrame(XGB_rfecv.cv_results_['mean_test_score'], columns = ["negMSE"])
XGB_mean_score.to_csv('E:/Rfiles/R workstation/XGB_mean_score.csv', index=None)
XGB_rank = pd.DataFrame({"id":X.columns,
                        "ranking":XGB_rfecv.ranking_, 
                        "support":XGB_rfecv.support_})
XGB_rank.to_csv('E:/Rfiles/R workstation/XGB_rank.csv', index=None)

# (4)LGB
LGB_rfecv = RFECV(estimator=LGBgrid,          # 学习器
                 min_features_to_select=20, # 最小选择的特征数量
                 step=1,                 # 移除特征个数
                 cv=splitter,  # 交叉验证次数
                 scoring='neg_mean_squared_error',     # 学习器的评价标准
                 verbose = 1, n_jobs = -1
                 ).fit(X, y)

print("有效特征个数 : %d" % LGB_rfecv.n_features_) # 158
min(abs(LGB_rfecv.cv_results_['mean_test_score'])) # 0.0886265130868133
LGB_mean_score = pd.DataFrame(LGB_rfecv.cv_results_['mean_test_score'], columns = ["negMSE"])
LGB_mean_score.to_csv('E:/Rfiles/R workstation/LGB_mean_score.csv', index=None)
LGB_rank = pd.DataFrame({"id":X.columns,
                        "ranking":LGB_rfecv.ranking_, 
                        "support":LGB_rfecv.support_})
LGB_rank.to_csv('E:/Rfiles/R workstation/LGB_rank.csv', index=None)


















