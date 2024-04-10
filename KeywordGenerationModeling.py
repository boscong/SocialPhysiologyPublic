
##############################
# 1 词向量训练
##############################
import numpy as np
import pandas as pd
from gensim.models import word2vec
import logging

logging.basicConfig(format='%(asctime)s:%(levelname)s: %(message)s', level=logging.INFO)
sentences=word2vec.Text8Corpus(
    u"E:\Rfiles\R workstation\gradedDiagnosisTreatment\Epil_docspace.txt") # Influ_docspace

W2Vmodel = word2vec.Word2Vec(sentences,  # default window=5, vector_size=100, alpha=0.025
                             min_count=5, 
                             vector_size=300,
                             window=5,
                             workers=-1) 

len(W2Vmodel.wv.vectors)
W2Vmodel.wv.get_vector(u"epilepsia") # influenza
W2Vmodel.wv.get_index(u"epilepsia")
y = W2Vmodel.wv.similar_by_key("epilepsia", topn=20) 
print (u"和【epilepsia】最相关的词有：\n", y)

# 保存模型，以便重用
W2Vmodel.save("W2Vmodel.model")
# 对应的加载方式
W2Vmodel = word2vec.Word2Vec.load("W2Vmodel.model")

##############################
# 2 模型构建与参数优化
##############################
from sklearn.model_selection import KFold, cross_val_score
from sklearn.metrics import r2_score, mean_squared_error
import xgboost as xgb
import lightgbm as lgb
from sklearn.ensemble import RandomForestRegressor
from sklearn.ensemble import GradientBoostingRegressor
# from sklearn.ensemble import AdaBoostRegressor
# from sklearn.neighbors import KNeighborsRegressor
# from sklearn.svm import SVR

# 构建基础模型: XGBoost/Lightgbm/RF/SVR(先用0.1高学习率调参,后降低)
# (1)RF:n_estimators_range/max_features_range/max_depth_range
# n_estimators_range=[int(x) for x in np.linspace(start=40,stop=400,num=20)]
# max_features_range=['auto','sqrt']
# max_depth: [3, 5, 6, 7, 9, 12, 15, 17, 25]
RFmodel = RandomForestRegressor(n_estimators=100,
                                max_features='auto',
                                max_depth=3) # default=None,建议从3开始增加

# (2)GBDT:learning rate/n_estimators/max_depth/max_features
# 'n_estimators':range(20,81,10)
# 'max_depth':range(3,14,2)
# 'max_features':range(7,20,2)
GBDTmodel = GradientBoostingRegressor()

# (3)XGB:max_depth/learning_rate/n_estimators/reg_lambda
# max_depth:[3, 5, 6, 7, 9, 12, 15, 17, 25]
# learning_rate:[0.01, 0.015, 0.025, 0.05, 0.1]
# lambda:[0, 0.1, 0.5, 1]
XGBmodel = xgb.XGBRegressor(max_depth=3,       # the max feature, default=6
                            learning_rate=0.1, # learning rate, default=0.3,每棵树的预测结果都要乘以这个学习率
                            n_estimators=100,  # number of trees, default=100
                            reg_alpha=0,       # default=0 控制模型复杂程度的权重值的L1正则项参数,参数值越大,模型越不容易过拟合。
                            reg_lambda=1)      # regularization parameter, default=1，控制模型复杂度的权重值的L2正则化项参数,参数值越大,模型越不容易过拟合。
# (4)Lightgbm:max_depth/num_leaves
# max_depth: [3, 5, 6, 7, 9, 12, 15, 17, 25]
# num_leaves:range(50, 170, 30)  # 50，80，110，140 
# reg_alpha：[0, 0.01~0.1, 1]
# reg_lambda ：[0, 0.1, 0.5, 1]
# LGB用leaf-wise算法,故在调节树复杂程度时用num_leaves而非max_depth
# 大致换算关系num_leaves = 2^(max_depth)，但值设置应小于2^(max_depth)否则可能会过拟合
LGBmodel = lgb.LGBMRegressor(num_leaves=31,
                             learning_rate=0.1, 
                             n_estimators=100, 
                             max_depth=-1) # -1表示不做限制，合理的设置可以防止过拟合

##############################
# 3 误差分析与特征筛选
##############################
# 1.误差计算函数
def AverageMSE(X, y, k, baseModel):
    # X_train,X_test,y_train,y_test = train_test_split(X, y, test_size=0.3, random_state=0)
    # 全样本交叉验证
    # 创建k折交叉验证对象
    kf = KFold(n_splits=k, shuffle=True, random_state=1)
    cv_results = cross_val_score(baseModel, X, y, cv=kf,
                                 scoring='neg_mean_squared_error', # 评估的指标负mse,也'accuracy'
                                 n_jobs=-1)                        # 设置为-1调用所有的cpu
    return cv_results.mean()

# 2.根据相关性引入特征构建模型
def nVarMSE(n, X, y, k, baseModel):
    MSE = []

    for i in range(n):
        temp = abs(AverageMSE(X=X.iloc[:,0:1+i], y=y, k=k, 
                              baseModel=baseModel)) # 负mse取正,注意iloc
        MSE.append(temp)
        print("\rProcessing progress: %.2f%%" % ((i+1)/n*100), end="") # i从0开始的
        
    return pd.DataFrame({'nVAR': list(range(1, n+1, 1)), 'MSE': MSE})

##############################
# 4 相关性排序
##############################
'''
# 1.数据结构变换
X_nvec = len(W2Vmodel.wv.vectors)-1
X_similar = W2Vmodel.wv.similar_by_key(u"epilepsia", topn=X_nvec) 
X_similar = pd.DataFrame(X_similar, columns=['Keyword', 'Similarity'])
X_similar["order"] = X_similar.index
X_similar.to_csv('E:/Rfiles/R workstation/Epil_Xsimilar.csv', index=None)

# 2.按照相关性排序（先取绝对值,不管正负只管大小）
X_similar['Similarity'] = abs(X_similar['Similarity'])
X_similar_sort = X_similar.sort_values('Similarity', ascending=False)
X_similar_sort.to_csv('E:/Rfiles/R workstation/Epil_Xsim_nodir.csv', index=None)
'''

# 提取词-词向量（标准化）生成字典，转化为数据框
word_vector_dict = {}
for word in W2Vmodel.wv.index_to_key:
    word_vector_dict[word] = list(W2Vmodel.wv.get_vector(word))
w2v_df = pd.DataFrame(word_vector_dict)
w2v_df.to_csv('E:/Rfiles/R workstation/w2v_df.csv', index=None)

word_id = pd.DataFrame(w2v_df.columns, columns=["feature"])
w2v_df.columns = list(range(0,17844,1))

from sklearn import preprocessing # 标准化
w2v_df = pd.DataFrame(preprocessing.minmax_scale(w2v_df))

# 怎么对应id和feature：R语言表连接

# w2v_df = pd.DataFrame(W2Vmodel.wv.get_normed_vectors()).T
w2v_df.to_csv('E:/Rfiles/R workstation/w2v_df_before.csv', index=None)
y = w2v_df["4158"] # W2Vmodel.wv.get_index("epilepsia")
X = w2v_df.drop("4158", axis=1)

w2v_df = pd.read_csv('E:/Rfiles/R workstation/w2v_df_before.csv')

'''
# 修改回跟相似度计算未排序顺序
X.columns = list(range(0,X.shape[1],1))
# 重排列的顺序，跟相似度排序一样
X.columns = X_similar["order"] 
X1 = X[X_similar_sort["order"]] 
'''

# 重排列的顺序，跟shap排序一样
import shap
def ShapFea(X, baseModel):
    explainer = shap.Explainer(baseModel)
# 获取训练集data各个样本各个特征的SHAP值。
    shap_v = explainer(X)
    shap_feature = pd.DataFrame(
        np.abs(shap_v.values[0]), index = X.columns, columns=["shap_value"]
        ).sort_values(by="shap_value", ascending=False)
    return shap_feature

# (1)RF
# 加入排序后建模
XRF = X[ShapFea(X, RFgrid).index] 
Epil_RF_Shap= nVarMSE(n=17842, X=XRF, y=y, k=3, baseModel=RFgrid)
# 求MSE最小值处的特征数n，即XRF的前n列index
RFmMSE_ind_shap = np.argmin(Epil_RF_Shap["MSE"])
# nVar_MinMSE = Epil_RF_Shap["nVAR"][np.argmin(Epil_RF_Shap["MSE"])]
RFmMSE_fea_shap = pd.DataFrame(XRF.columns[0:RFmMSE_ind_shap+1], columns=["feature"])
Epil_RF_Shap.to_csv('E:/Rfiles/R workstation/Epil_RF_Shap.csv', index=None)
RFmMSE_fea_shap.to_csv('E:/Rfiles/R workstation/RFmMSE_fea_shap.csv', index=None)

# 未加入排序后建模
Epil_RF=nVarMSE(n=17842, X=X, y=y, k=3, baseModel=RFgrid) # 4822 0.00324
Epil_RF.to_csv('E:/Rfiles/R workstation/Epil_RF.csv', index=None)

# (2)GBDT
XGBDT = X[ShapFea(X, GBDTgrid).index]
Epil_GBDT_Shap= nVarMSE(n=17842, X=XGBDT, y=y, k=3, baseModel=GBDTgrid)
GBDTmMSE_ind_shap = np.argmin(Epil_GBDT_Shap["MSE"])
GBDTmMSE_fea_shap = pd.DataFrame(XGBDT.columns[0:GBDTmMSE_ind_shap+1], columns=["feature"])
Epil_GBDT_Shap.to_csv('E:/Rfiles/R workstation/Epil_GBDT_Shap.csv', index=None)
GBDTmMSE_fea_shap.to_csv('E:/Rfiles/R workstation/GBDTmMSE_fea_shap.csv', index=None)

Epil_GBDT=nVarMSE(n=17842, X=X, y=y, k=3, baseModel=GBDTgrid) # 4822 0.00324
Epil_GBDT.to_csv('E:/Rfiles/R workstation/Epil_GBDT.csv', index=None)

# (3)XGB
XXGB = X[ShapFea(X, XGBgrid).index] 
Epil_XGB_Shap= nVarMSE(n=17842, X=XXGB, y=y, k=3, baseModel=XGBgrid)
XGBmMSE_ind_shap = np.argmin(Epil_XGB_Shap["MSE"])
XGBmMSE_fea_shap = pd.DataFrame(XXGB.columns[0:XGBmMSE_ind_shap+1], columns=["feature"])
Epil_XGB_Shap.to_csv('E:/Rfiles/R workstation/Epil_XGB_Shap.csv', index=None)
XGBmMSE_fea_shap.to_csv('E:/Rfiles/R workstation/XGBmMSE_fea_shap.csv', index=None)

Epil_XGB=nVarMSE(n=17842, X=X, y=y, k=3, baseModel=XGBgrid) # 4822 0.00324
Epil_XGB.to_csv('E:/Rfiles/R workstation/Epil_XGB.csv', index=None)

# (4)LGB
XLGB = X[ShapFea(X, LGBgrid).index] 
Epil_LGB_Shap= nVarMSE(n=17842, X=XLGB, y=y, k=3, baseModel=LGBgrid)
LGBmMSE_ind_shap = np.argmin(Epil_LGB_Shap["MSE"])
LGBmMSE_fea_shap = pd.DataFrame(XLGB.columns[0:LGBmMSE_ind_shap+1], columns=["feature"])
Epil_LGB_Shap.to_csv('E:/Rfiles/R workstation/Epil_LGB_Shap.csv', index=None)
LGBmMSE_fea_shap.to_csv('E:/Rfiles/R workstation/LGBmMSE_fea_shap.csv', index=None)

Epil_LGB=nVarMSE(n=17842, X=X, y=y, k=3, baseModel=LGBgrid) # 4822 0.00324
Epil_LGB.to_csv('E:/Rfiles/R workstation/Epil_LGB.csv', index=None)
###############################################
###############################################
###############################################
###############################################

# (1)RF
XRF = pd.DataFrame(RFgrid.feature_importances_,index = X.columns, 
                    columns=["importance"]).sort_values(by="importance", ascending=False) 
XRF = X[XRF.index]
Epil_RF_Tree= nVarMSE(n=12000, X=XRF, y=y, k=3, baseModel=RFgrid)
# 求MSE最小值处的特征数n，即XRF的前n列index
RFmMSE_ind_Tree = np.argmin(Epil_RF_Tree["MSE"])
# nVar_MinMSE = Epil_RF_Tree["nVAR"][np.argmin(Epil_RF_Tree["MSE"])]
RFmMSE_fea_Tree = pd.DataFrame(XRF.columns[0:RFmMSE_ind_Tree+1], columns=["feature"])
Epil_RF_Tree.to_csv('E:/Rfiles/R workstation/Epil_RF_Tree.csv', index=None)
RFmMSE_fea_Tree.to_csv('E:/Rfiles/R workstation/RFmMSE_fea_Tree.csv', index=None)

# (2)GBDT
XGBDT = pd.DataFrame(GBDTgrid.feature_importances_,index = X.columns, 
                    columns=["importance"]).sort_values(by="importance", ascending=False) 
XGBDT = X[XGBDT.index]
Epil_GBDT_Tree= nVarMSE(n=12000, X=XGBDT, y=y, k=3, baseModel=GBDTgrid)
GBDTmMSE_ind_Tree = np.argmin(Epil_GBDT_Tree["MSE"])
GBDTmMSE_fea_Tree = pd.DataFrame(XGBDT.columns[0:GBDTmMSE_ind_Tree+1], columns=["feature"])
Epil_GBDT_Tree.to_csv('E:/Rfiles/R workstation/Epil_GBDT_Tree.csv', index=None)
GBDTmMSE_fea_Tree.to_csv('E:/Rfiles/R workstation/GBDTmMSE_fea_Tree.csv', index=None)

# (3)XGB
XXGB = pd.DataFrame(XGBgrid.feature_importances_,index = X.columns, 
                    columns=["importance"]).sort_values(by="importance", ascending=False)
XXGB = X[XXGB.index]
Epil_XGB_Tree= nVarMSE(n=12000, X=XXGB, y=y, k=3, baseModel=XGBgrid)
XGBmMSE_ind_Tree = np.argmin(Epil_XGB_Tree["MSE"])
XGBmMSE_fea_Tree = pd.DataFrame(XXGB.columns[0:XGBmMSE_ind_Tree+1], columns=["feature"])
Epil_XGB_Tree.to_csv('E:/Rfiles/R workstation/Epil_XGB_Tree.csv', index=None)
XGBmMSE_fea_Tree.to_csv('E:/Rfiles/R workstation/XGBmMSE_fea_Tree.csv', index=None)

# (4)LGB
XLGB = pd.DataFrame(LGBgrid.feature_importances_,index = X.columns, 
                    columns=["importance"]).sort_values(by="importance", ascending=False) 
XLGB = X[XLGB.index]
Epil_LGB_Tree= nVarMSE(n=12000, X=XLGB, y=y, k=3, baseModel=LGBgrid)
LGBmMSE_ind_Tree = np.argmin(Epil_LGB_Tree["MSE"])
LGBmMSE_fea_Tree = pd.DataFrame(XLGB.columns[0:LGBmMSE_ind_Tree+1], columns=["feature"])
Epil_LGB_Tree.to_csv('E:/Rfiles/R workstation/Epil_LGB_Tree.csv', index=None)
LGBmMSE_fea_Tree.to_csv('E:/Rfiles/R workstation/LGBmMSE_fea_Tree.csv', index=None)

##############################
# 5 可视化与评价
##############################
import matplotlib.pyplot as plt
plt.plot(Epil_RF['nVAR'], Epil_RF['MSE'])
plt.plot(Epil_GBDT_Shap['nVAR'], Epil_GBDT_Shap['MSE'])


XLGB = pd.DataFrame(LGBgrid.feature_importances_, 
                    index = X.columns, 
                    columns=["importance"]).sort_values(by="importance", 
                                                        ascending=False)
import matplotlib.pyplot as plt
plt.plot(XLGB["importance"])
plt.plot(Epil_GBDT_Shap['nVAR'], Epil_GBDT_Shap['MSE'])
