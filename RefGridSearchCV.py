###################
# 参数优化
###################

###################
# dataset split
###################
from sklearn.model_selection import train_test_split
# X, y = w2v_df.drop(y_index, axis=1), w2v_df[y_index]
X_train,X_test,y_train,y_test = train_test_split(X,y,test_size = 0.3,random_state = 0)
X_train.shape
X_train.to_csv("E:\\Rfiles\\R workstation\\Disabled\\X_train.csv", index=None)
X_test.to_csv("E:\\Rfiles\\R workstation\\Disabled\\X_test.csv", index=None)
y_train.to_csv("E:\\Rfiles\\R workstation\\Disabled\\y_train.csv", index=None)
y_test.to_csv("E:\\Rfiles\\R workstation\\Disabled\\y_test.csv", index=None)


X_train = pd.read_csv("E:\\Rfiles\\R workstation\\Disabled\\X_train.csv")
X_test = pd.read_csv("E:\\Rfiles\\R workstation\\Disabled\\X_test.csv")
y_train = pd.read_csv("E:\\Rfiles\\R workstation\\Disabled\\y_train.csv")
y_test = pd.read_csv("E:\\Rfiles\\R workstation\\Disabled\\y_test.csv")

###################
# RandomizedSearchCV - GridSearhCV
###################
from sklearn.model_selection import RandomizedSearchCV
from sklearn.model_selection import GridSearchCV
import time

# 1.RF:n_estimators_range/max_features_range/max_depth_range
# (1)RandomizedSearchCV
RFmodel = RandomForestRegressor(n_estimators=100,
                                max_features='auto',
                                max_depth=3) # default=None,建议从3开始增加
random_params_group = {
    'n_estimators': [int(x) for x in np.linspace(start=40,stop=400,num=20)],
    'max_depth': [3, 5, 6, 7, 9, 12, 15, 17, 25],
    'max_features': ['auto','sqrt']
    }
random_model =RandomizedSearchCV(
    estimator=RFmodel, 
    param_distributions = random_params_group,
    n_iter = 100, scoring = 'neg_mean_squared_error',
    verbose = 2, n_jobs = -1, cv = 3, random_state = 0)
random_model.fit(X_train,y_train) # 35.5min
# 观察模型的训练过程，这里设置了n_iter=100，交叉验证的折数=3，所以该模型要迭代300次
random_model.best_params_
# {'n_estimators': 134, 'max_features': 'sqrt', 'max_depth': 7}

RFrandom = RandomForestRegressor(
    n_estimators = 134,
    max_features = 'sqrt',
    min_samples_leaf = 7)
RFrandom.fit(X_train,y_train)
predictions = RFrandom.predict(X_test)
MSEr = mean_squared_error(y_test,predictions)
print('模型预测误差:', MSEr) # 0.002876689247735428
RMSEr = np.sqrt(mean_squared_error(y_test,predictions))
print('模型预测误差:', RMSEr) # 0.053634776476978334

# (2)GridSearhCV
param_grid = {
    'n_estimators':[120,130,140],
    'max_depth':[7,8],
    'max_features':['sqrt']}
RFgrid = RandomForestRegressor()
grid = GridSearchCV(
    RFgrid,param_grid = param_grid,
    scoring = 'neg_mean_squared_error',cv = 3,n_jobs = -1)

start_time = time.time()
grid.fit(X_train,y_train)
end_time = time.time()
print('模型训练用时:{}'.format(end_time - start_time)) # 10.29852032661438
grid.best_params_ # {'max_depth': 8, 'max_features': 'sqrt', 'n_estimators': 130}

RFgrid = RandomForestRegressor(
    n_estimators = 130,
    max_features = 'sqrt',
    max_depth = 8)
RFgrid=RFgrid.fit(X,y)


predictions = RFgrid.predict(X_test)
MSEg = mean_squared_error(y_test,predictions)
print('模型预测误差:', MSEg) # 0.0027896038743714835
print('模型的提升效果:{}'.format(round(100*(MSEr-MSEg)/MSEr),2),'%')
RMSEg = np.sqrt(mean_squared_error(y_test,predictions))
print('模型预测误差:', RMSEg) # 0.052816700714560766
print('模型的提升效果:{}'.format(round(100*(RMSEr-RMSEg)/RMSEr),2),'%')

# 2.GBDT:learning rate/n_estimators/max_depth/max_features
# (1)RandomizedSearchCV
# learning_rate:[0.01, 0.015, 0.025, 0.05, 0.1]  默认0.1,先用0.1快速调其他参数后再微调
GBDTmodel = GradientBoostingRegressor(learning_rate=0.1)

random_params_group = {
    'n_estimators':range(20,81,10),
    'max_depth':range(3,14,2),
    'max_features':range(7,20,2)}
random_model =RandomizedSearchCV(
    estimator=GBDTmodel, 
    param_distributions = random_params_group,
    n_iter = 100, scoring = 'neg_mean_squared_error',
    verbose = 2, n_jobs = -1, cv = 3, random_state = 0)
random_model.fit(X_train,y_train) # 12.1s
# 观察模型的训练过程，这里设置了n_iter=100，交叉验证的折数=3，所以该模型要迭代300次
random_model.best_params_
# {'n_estimators': 50, 'max_features': 7, 'max_depth': 7}

GBDTrandom = GradientBoostingRegressor(
    learning_rate=0.1,
    n_estimators=50,
    max_features=7,
    max_depth=7)
GBDTrandom.fit(X_train,y_train)
predictions = GBDTrandom.predict(X_test)
MSEr = mean_squared_error(y_test,predictions)
print('模型预测误差:', MSEr) # 0.0029622498862253085
RMSEr = np.sqrt(mean_squared_error(y_test,predictions))
print('模型预测误差:', RMSEr) # 0.05442655497296617

# (2)GridSearhCV
param_grid = {
    'n_estimators':[int(x) for x in np.linspace(start=41,stop=59,num=5)],
    'max_depth':[6, 7, 8],
    'max_features':[4, 5 , 6, 7, 8]}
GBDTgrid = GradientBoostingRegressor(learning_rate=0.1)
grid = GridSearchCV(
    GBDTgrid,param_grid = param_grid,
    scoring = 'neg_mean_squared_error',cv = 3,n_jobs = -1)

start_time = time.time()
grid.fit(X_train,y_train)
end_time = time.time()
print('模型训练用时:{}'.format(end_time - start_time)) # 9.707756996154785
grid.best_params_ # {'max_depth': 7, 'max_features': 6, 'n_estimators': 41}

GBDTgrid = GradientBoostingRegressor(
    learning_rate=0.1,
    n_estimators=41,
    max_features=6,
    max_depth=7)
GBDTgrid = GBDTgrid.fit(X,y)

predictions = GBDTgrid.predict(X_test)
MSEg = mean_squared_error(y_test,predictions)
print('模型预测误差:', MSEg) # 0.002736670265763171
print('模型的提升效果:{}'.format(round(100*(MSEr-MSEg)/MSEr),4),'%') # 8 %
RMSEg = np.sqrt(mean_squared_error(y_test,predictions))
print('模型预测误差:', RMSEg) # 0.05231319399313304
print('模型的提升效果:{}'.format(round(100*(RMSEr-RMSEg)/RMSEr),4),'%') # 4 %

# 3.XGB:max_depth/learning_rate/n_estimators/reg_lambda
# (1)RandomizedSearchCV
# learning_rate:[0.01, 0.015, 0.025, 0.05, 0.1]  先用0.1快速调其他参数后再微调
XGBmodel = xgb.XGBRegressor(max_depth=3,       # the max feature, default=6
                            learning_rate=0.1, # learning rate, default=0.3,每棵树的预测结果都要乘以这个学习率
                            n_estimators=100,  # number of trees, default=100
                            reg_alpha=0,       # default=0 控制模型复杂程度的权重值的L1正则项参数,参数值越大,模型越不容易过拟合。
                            reg_lambda=1)      # regularization parameter, default=1，控制模型复杂度的权重值的L2正则化项参数,参数值越大,模型越不容易过拟合。
random_params_group = {
    'reg_lambda':[0, 0.1, 0.5, 1],
    'n_estimators': [int(x) for x in np.linspace(start=50,stop=200,num=10)],
    'max_depth': [3, 5, 6, 7, 9, 12, 15, 17, 25]}
random_model =RandomizedSearchCV(
    estimator=XGBmodel, 
    param_distributions = random_params_group,
    n_iter = 100, scoring = 'neg_mean_squared_error',
    verbose = 2, n_jobs = -1, cv = 3, random_state = 0)
random_model.fit(X_train,y_train) # 23.1min
# 观察模型的训练过程，这里设置了n_iter=100，交叉验证的折数=3，所以该模型要迭代300次
random_model.best_params_
# {'reg_lambda': 0.1, 'n_estimators': 150, 'max_depth': 7}

XGBrandom = xgb.XGBRegressor(
    reg_lambda = 0.1,
    n_estimators = 150,
    max_depth = 7)
XGBrandom.fit(X_train,y_train)
predictions = XGBrandom.predict(X_test)
MSEr = mean_squared_error(y_test,predictions)
print('模型预测误差:', MSEr) # 0.0034334972
RMSEr = np.sqrt(mean_squared_error(y_test,predictions))
print('模型预测误差:', RMSEr) # 0.058596052

# (2)GridSearhCV
from sklearn.model_selection import GridSearchCV
import time
 
param_grid = {
    'n_estimators':[int(x) for x in np.linspace(start=140,stop=160,num=5)],
    'max_depth':[7,8],
    'learning_rate':[0.01, 0.015, 0.025, 0.05, 0.1]}
XGBgrid = xgb.XGBRegressor(reg_lambda = 0.1)

grid = GridSearchCV(
    XGBgrid,param_grid = param_grid,
    scoring = 'neg_mean_squared_error',cv = 3,n_jobs = -1)

start_time = time.time()
grid.fit(X_train,y_train)
end_time = time.time()
print('模型训练用时:{}'.format(end_time - start_time)) # 1167.964703321457
grid.best_params_ # {'learning_rate': 0.05, 'max_depth': 7, 'n_estimators': 160}

XGBgrid = xgb.XGBRegressor(
    learning_rate = 0.1,
    reg_lambda = 0.1,
    n_estimators = 160,
    max_depth = 7)
XGBgrid=XGBgrid.fit(X,y)

predictions = XGBgrid.predict(X_test)
MSEg = mean_squared_error(y_test,predictions)
print('模型预测误差:', MSEg) # 0.0027379840112898646/0.002688908
print('模型的提升效果:{}'.format(round(100*(MSEr-MSEg)/MSEr),4),'%') # 20 %/ 22 %
RMSEg = np.sqrt(mean_squared_error(y_test,predictions))
print('模型预测误差:', RMSEg) # 0.052325749027509055/ 0.05185468
print('模型的提升效果:{}'.format(round(100*(RMSEr-RMSEg)/RMSEr),4),'%') # 11 %/ 12 %

# 4.Lightgbm:max_depth/num_leaves
# (1)RandomizedSearchCV
LGBmodel = lgb.LGBMRegressor(learning_rate=0.1) 
random_params_group = {
    'n_estimators': [int(x) for x in np.linspace(start=50,stop=300,num=10)],
    'max_depth': [3, 5, 6, 7, 9, 12, 15, 17, 25],
    'num_leaves': [int(x) for x in np.linspace(start=20,stop=200,num=10)], 
    'reg_lambda':[0, 0.1, 0.5, 1]}

random_model =RandomizedSearchCV(
    estimator=LGBmodel, 
    param_distributions = random_params_group,
    n_iter = 100, scoring = 'neg_mean_squared_error',
    verbose = 2, n_jobs = -1, cv = 3, random_state = 0)

random_model.fit(X_train,y_train) # 19.5min
# 观察模型的训练过程，这里设置了n_iter=100，交叉验证的折数=3，所以该模型要迭代300次
random_model.best_params_
# {'reg_lambda': 1, 'num_leaves': 100, 'n_estimators': 50, 'max_depth': 12}

LGBrandom = lgb.LGBMRegressor(reg_lambda=1, num_leaves=100, 
                             n_estimators=50, max_depth=12,
                             learning_rate=0.1) 
LGBrandom.fit(X_train,y_train)
predictions = LGBrandom.predict(X_test)
MSEr = mean_squared_error(y_test,predictions)
print('模型预测误差:', MSEr) # 0.0031135623888584947 # dis:0.0983399625039679
RMSEr = np.sqrt(mean_squared_error(y_test,predictions))
print('模型预测误差:', RMSEr) # 0.05579930455533021 # dis:0.3135920319522929

# (2)GridSearhCV
param_grid = {
    'n_estimators': [30, 35, 40, 45, 50, 55],
    'max_depth': [10, 12, 14],
    'num_leaves': [90, 95, 100, 105, 110]}
LGBgrid = lgb.LGBMRegressor(reg_lambda=1, learning_rate=0.1) 
grid = GridSearchCV(
    LGBgrid,param_grid = param_grid,
    scoring = 'neg_mean_squared_error',cv = 3,n_jobs = -1)

start_time = time.time()
grid.fit(X_train,y_train)
end_time = time.time()
print('模型训练用时:{}'.format(end_time - start_time)) # 449.05058550834656 # dis:1086.9350244998932
grid.best_params_ 
# {'max_depth': 10, 'n_estimators': 35, 'num_leaves': 90}
# dis：{'max_depth': 10, 'n_estimators': 45, 'num_leaves': 90}

LGBgrid = lgb.LGBMRegressor(reg_lambda=1, num_leaves=90, 
                             n_estimators=45, max_depth=10,
                             learning_rate=0.1) 
LGBgrid = LGBgrid.fit(X,y)

predictions = LGBgrid.predict(X_test)
MSEg = mean_squared_error(y_test,predictions)
print('模型预测误差:', MSEg) # 0.0030998838558306584 # dis:0.09855895848151551
print('模型的提升效果:{}'.format(round(100*(MSEr-MSEg)/MSEr),2),'%') 
RMSEg = np.sqrt(mean_squared_error(y_test,predictions))
print('模型预测误差:', RMSEg) # 0.0556766006131001 # dis:0.3139410111494124
print('模型的提升效果:{}'.format(round(100*(RMSEr-RMSEg)/RMSEr),2),'%')
