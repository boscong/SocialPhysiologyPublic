# -*- coding: utf-8 -*-

###########################
# import tomotopy as tp
###########################
import pandas as pd
import datetime
import tomotopy as tp
import os
os.getcwd() # 当前工作目录
os.chdir("E:/Pyfiles/disable_data") # 设置工作目录
tp.isa # 如果 tp.isa 返回 None，则训练过程可能需要很长时间

def FindTopic(docs, min_k=1, max_k=30, min_df=10, seed=2017):
    # docs：文档
    # min_df：词语最少出现在min_df 个文档中
    
    import matplotlib.pyplot as plt
    scores = []
    j = 1  # 进度条参数
    n = len(range(min_k, max_k))
    start_time = datetime.datetime.now()
    for k in range(min_k, max_k):
        #seed随机种子，保证在大邓这里运行结果与你运行的结果一样
        mdl = tp.LDAModel(min_df=min_df, k=k, seed=seed)
        
        for words in docs:
            if words:
                mdl.add_doc(words)
        mdl.train(20)
        coh = tp.coherence.Coherence(mdl)
        scores.append(coh.get_score())
        end_time = datetime.datetime.now()
        print("\rProcessing progress: %.2f%%;" % (j/n*100), " Generate topics: ", k,"; Elapse: ", end_time-start_time, end="") # i从0开始的
        j=j+1
    #x = list(range(min_k, max_k - 1))  # 区间最右侧的值。注意：不能大于max_k
    #print(x)
    #print()
    plt.plot(range(min_k, max_k), scores)
    plt.xlabel("Number of Topics")
    plt.ylabel("Coherence")
    plt.show()
    return(scores)
  
# 2017
disab_space = pd.read_csv("disab_space2017.csv")
ndoc = len(disab_space.doc)
for i in range(0, ndoc):
    temp = disab_space.doc[i] 
    disab_space.doc[i] = temp.split()

coh_score2017 = FindTopic(docs= disab_space.doc, min_k=1, max_k=500, min_df=10, seed=2017)
coh_score2017_df = pd.DataFrame({"ntopics": range(1,500), "coherence":coh_score2017})
coh_score2017_df.to_csv('E:/Rfiles/R workstation/coh_score2017_df.csv', index=None)

# 2018
disab_space = pd.read_csv("disab_space2018.csv")
ndoc = len(disab_space.doc)
for i in range(0, ndoc):
    temp = disab_space.doc[i] 
    disab_space.doc[i] = temp.split()
    
coh_score2018 = FindTopic(docs= disab_space.doc, min_k=1, max_k=500, min_df=10, seed=2018)
coh_score2018_df = pd.DataFrame({"ntopics": range(1,500), "coherence":coh_score2018})
coh_score2018_df.to_csv('E:/Rfiles/R workstation/coh_score2018_df.csv', index=None)

# 2019
disab_space = pd.read_csv("disab_space2019.csv")
ndoc = len(disab_space.doc)
for i in range(0, ndoc):
    temp = disab_space.doc[i] 
    disab_space.doc[i] = temp.split()
    
coh_score2019 = FindTopic(docs= disab_space.doc, min_k=1, max_k=500, min_df=10, seed=2019)
coh_score2019_df = pd.DataFrame({"ntopics": range(1,500), "coherence":coh_score2019})
coh_score2019_df.to_csv('E:/Rfiles/R workstation/coh_score2019_df.csv', index=None)

# 2020
disab_space = pd.read_csv("disab_space2020.csv")
ndoc = len(disab_space.doc)
for i in range(0, ndoc):
    temp = disab_space.doc[i] 
    disab_space.doc[i] = temp.split()
    
coh_score2020 = FindTopic(docs= disab_space.doc, min_k=1, max_k=500, min_df=10, seed=2020)
coh_score2020_df = pd.DataFrame({"ntopics": range(1,500), "coherence":coh_score2020})
coh_score2020_df.to_csv('E:/Rfiles/R workstation/coh_score2020_df.csv', index=None)

# 2021
disab_space = pd.read_csv("disab_space2021.csv")
ndoc = len(disab_space.doc)
for i in range(0, ndoc):
    temp = disab_space.doc[i] 
    disab_space.doc[i] = temp.split()
    
coh_score2021 = FindTopic(docs= disab_space.doc, min_k=1, max_k=500, min_df=10, seed=2021)
coh_score2021_df = pd.DataFrame({"ntopics": range(1,500), "coherence":coh_score2021})
coh_score2021_df.to_csv('E:/Rfiles/R workstation/coh_score2021_df.csv', index=None)

# 2022
disab_space = pd.read_csv("disab_space2022.csv")
ndoc = len(disab_space.doc)
for i in range(0, ndoc):
    temp = disab_space.doc[i] 
    disab_space.doc[i] = temp.split()
    
coh_score2022 = FindTopic(docs= disab_space.doc, min_k=1, max_k=500, min_df=10, seed=2022)
coh_score2022_df = pd.DataFrame({"ntopics": range(1,500), "coherence":coh_score2022})
coh_score2022_df.to_csv('E:/Rfiles/R workstation/coh_score2022_df.csv', index=None)

# 2023
disab_space = pd.read_csv("disab_space2023.csv")
ndoc = len(disab_space.doc)
for i in range(0, ndoc):
    temp = disab_space.doc[i] 
    disab_space.doc[i] = temp.split()
    
coh_score2023 = FindTopic(docs= disab_space.doc, min_k=1, max_k=500, min_df=10, seed=2023)
coh_score2023_df = pd.DataFrame({"ntopics": range(1,500), "coherence":coh_score2023})
coh_score2023_df.to_csv('E:/Rfiles/R workstation/coh_score2023_df.csv', index=None)

# 情感分类结果主题分析
# 正向
disab_space = pd.read_csv("E:/Pyfiles/2014sentiment/lda_pos.csv")
ndoc = len(disab_space.doc)
for i in range(0, ndoc):
    temp = disab_space.doc[i] 
    disab_space.doc[i] = temp.split()
    
coh_score_pos = FindTopic(docs= disab_space.doc, min_k=1, max_k=100, min_df=5, seed=2024)
coh_score_pos_df = pd.DataFrame({"ntopics": range(1,100), "coherence":coh_score_pos})
coh_score_pos_df.to_csv('E:/Rfiles/R workstation/coh_score_pos_df.csv', index=None)
# 负向
disab_space = pd.read_csv("E:/Pyfiles/2014sentiment/lda_neg.csv")
ndoc = len(disab_space.doc)
for i in range(0, ndoc):
  temp = disab_space.doc[i] 
disab_space.doc[i] = temp.split()

coh_score_neg = FindTopic(docs= disab_space.doc, min_k=1, max_k=100, min_df=5, seed=2025)
coh_score_neg_df = pd.DataFrame({"ntopics": range(1,100), "coherence":coh_score_neg})
coh_score_neg_df.to_csv('E:/Rfiles/R workstation/coh_score_neg_df.csv', index=None)

# 初始化LDA
mdl = tp.LDAModel(k=2, min_df=5, seed=2023)
for words in disab_space['doc']:
    #确认words 是 非空词语列表
    if words:
        mdl.add_doc(words=words)
# 训练
mdl.train()
# 查看每个topic feature words
for k in range(mdl.k):
    print('Top 10 words of topic #{}'.format(k))
    print(mdl.get_topic_words(k, top_n=10))
    print('\n')
# 合并主体呈数据框
comb_df = pd.DataFrame()
for k in range(mdl.k):
    theme_word=pd.DataFrame(mdl.get_topic_words(k, top_n=100))[0]
    comb_df = pd.concat([comb_df, theme_word], axis=1)
    
comb_df.to_csv('E:/Pyfiles/disable_data/comb_df2023.csv', index=None)


    
    
    
    
    
    
    
    
    
