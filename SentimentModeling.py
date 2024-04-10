# -*- coding: utf-8 -*-

# 1.首先导入本次实验用到的第三方库
from sklearn.model_selection import train_test_split
import numpy as np
import pandas as pd
import jieba as jb
# from sklearn.externals import joblib
from gensim.models.word2vec import Word2Vec
from sklearn import metrics

# 2. 加载数据，使用jieba将数据进行分词，将正反样本拼接，然后创建全是0和全是1的向量拼接起来作为标签
data = pd.read_csv('E:/Pyfiles/2014sentiment/w2v_op_new.csv')
doc = data.doc
xtemp = doc.apply(lambda x: jb.lcut(x))
# # 去除空格和空元素
for i in range(0, data.shape[0], 1):
    xtemp[i] = [x for x in xtemp[i] if x and not isinstance(x, str) or (isinstance(x, str) and x.strip())]
x = xtemp
y = data.emo1.astype('category')

#拆分训练集和测试集
x_train,x_test,y_train,y_test = train_test_split(x,y,test_size=0.1, random_state=1)
x_train.shape

# 3.定义生成每一个句子vec的函数
def build_vector(text,size,w2v):
    #创建一个指定大小的数据空间
    vec = np.zeros(size).reshape((1,size))
    #count是统计有多少词向量
    count = 0
    #循环所有的词向量进行求和
    for w in text:
        try:
            vec += w2v.wv.get_vector(w)
            count +=1
        except:
            continue
    #循环完成后求均值
    if count!=0:
        vec/=count
    return vec

# 4 .计算词向量
#初始化模型和词表
w2v = Word2Vec(vector_size=300,workers=-1)
w2v.build_vocab(x_train)
# 训练并建模
w2v.train(x_train,total_examples=w2v.corpus_count, epochs=10)

w2v.wv.get_index(u"向往")
w2v.wv.similar_by_key("残疾", topn=10) 

#获取train_vecs
train_vecs = np.concatenate([build_vector(z,300,w2v) for z in x_train])
#保存处理后的词向量
np.save('E:/Pyfiles/2014sentiment/train_vecs.npy',train_vecs)
#保存模型
w2v.save("E:/Pyfiles/2014sentiment/w2v_model.pkl")
w2v.train(x_test,total_examples=w2v.corpus_count, epochs=10)
test_vecs = np.concatenate([build_vector(z,300,w2v) for z in x_test])
np.save('E:/Pyfiles/2014sentiment/test_vecs.npy',test_vecs)

# 5.参数调优
from sklearn.model_selection import KFold, cross_val_score
from sklearn.metrics import r2_score, mean_squared_error
from sklearn.model_selection import RandomizedSearchCV
from sklearn.model_selection import GridSearchCV
import time
import lightgbm as lgb
# Lightgbm:max_depth/num_leaves

param_grid = {
    # 'max_depth': [10, 11, 12],
    'num_leaves': [130, 131, 132]}
LGBgrid = lgb.LGBMClassifier(reg_lambda=1, learning_rate=0.1) 

grid = GridSearchCV(
    LGBgrid,param_grid = param_grid,
    scoring = 'neg_mean_squared_error',cv = 3,n_jobs = -1)

start_time = time.time()
grid.fit(train_vecs,y_train)
end_time = time.time()

print('模型训练用时:{}'.format(end_time - start_time)) # S 518.877295255661  E 649.2472672462463
grid.best_params_ 
# {'max_depth': 10, 'num_leaves': 132}

LGBgrid = lgb.LGBMClassifier(reg_lambda=1, 
                            learning_rate=0.1,
                            num_leaves=132,
                            max_depth=10)

LGBgrid.fit(train_vecs, y_train)
predictions = LGBgrid.predict(test_vecs)
auc_better = metrics.accuracy_score(y_test,predictions)

print('模型预测误差:', auc_better) # 0.7267402568145979
print('模型的提升效果:{}'.format(round(100*(auc_better-auc_raw)/auc_raw),2),'%') # 7 %

# 6.模型与评估
# 创建R模型
LGBgrid = lgb.LGBMClassifier(reg_lambda=1, 
                            learning_rate=0.1,
                            num_leaves=132,max_depth=10)
#训练模型
LGBgrid.fit(train_vecs,y_train)

y_pred = LGBgrid.predict(test_vecs)
#模型评估
auc_raw = metrics.accuracy_score(y_test, y_pred) # 0.7433333333333333
print('准确率:', metrics.accuracy_score(y_test, y_pred)) #预测准确率输出
#计算宏平均、微平均、加权平均精确率输出
print('宏平均精确率:',metrics.precision_score(y_test,y_pred,average='macro')) 
print('微平均精确率:', metrics.precision_score(y_test, y_pred, average='micro')) 
print('加权平均精确率:', metrics.precision_score(y_test, y_pred, average='weighted')) 
#计算宏平均、微平均、加权平均召回率输出
print('宏平均召回率:',metrics.recall_score(y_test,y_pred,average='macro'))
print('微平均召回率:',metrics.recall_score(y_test,y_pred,average='micro'))
print('加权平均召回率:',metrics.recall_score(y_test,y_pred,average='weighted')) 
#计算宏平均、微平均、加权平均f1-score输出
print('宏平均F1-score:',metrics.f1_score(y_test,y_pred,labels=[0,1],average='macro'))
print('微平均F1-score:',metrics.f1_score(y_test,y_pred,labels=[0,1],average='micro'))
print('加权平均F1-score:',metrics.f1_score(y_test,y_pred,labels=[0,1],average='weighted')) 
print('混淆矩阵输出:\n',metrics.confusion_matrix(y_test,y_pred))#混淆矩阵输出
print('分类报告:\n', metrics.classification_report(y_test, y_pred))#分类报告

# 7.预测
disab_data = pd.read_csv('E:/Pyfiles/2014sentiment/disab_op.csv')
disab_doc = disab_data.doc
disab_xtemp = disab_doc.apply(lambda x: jb.lcut(x))
# # 去除空格和空元素
for i in range(0, disab_data.shape[0], 1):
    disab_xtemp[i] = [x for x in disab_xtemp[i] if x and not isinstance(x, str) or (isinstance(x, str) and x.strip())]
disab_x = disab_xtemp

#初始化模型和词表
disab_w2v = Word2Vec(vector_size=300,workers=-1)
disab_w2v.build_vocab(disab_x)
# 训练并建模
disab_w2v.train(disab_x,total_examples=disab_w2v.corpus_count, epochs=10)

#获取train_vecs
disab_vecs = np.concatenate([build_vector(z,300,disab_w2v) for z in disab_x])
#保存处理后的词向量
#np.save('E:/Pyfiles/2014sentiment/disab_vecs.npy',disab_vecs)
#保存模型
#disab_w2v.save("E:/Pyfiles/2014sentiment/disab_w2v_model.pkl")

disab_y_pred = LGBgrid.predict(disab_vecs)
disab_y_pred_df = pd.DataFrame({"emo1": disab_y_pred})
disab_y_pred_df.to_csv('E:/Pyfiles/2014sentiment/disab_emo1_pred_df.csv', index=None)





