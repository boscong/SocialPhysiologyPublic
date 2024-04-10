#################################
# 0 目录
#################################
# 1 文本预处理（文档层级和词层级）
# 2 V2W输入

#################################
# tbl格式转为词向量输入格式
Tbl2Word2Vec <- function(tbldata, word, pid) {
  # tbl data with colnames: id and word
  
  segcn_lst <- with(tbldata, split(word, pid))
  
  nseg <- length(segcn_lst)
  doc_space <- vector("list", nseg)
  
  for (j in seq(nseg)) {
    
    for (i in seq(segcn_lst[[j]])) {
      doc_space[[j]] <- paste(doc_space[[j]], list(segcn_lst[[j]][i]))
    }
    cat('\rProcessing progress: ', round(j/nseg*100), "%  ", sep = "")
  }
  return(doc_space %>% unlist %>% paste0(sep = " "))
}

# 读写路径
paths <- "E:/Rfiles/R workstation/gradedDiagnosisTreatment/"

##############################

# 1 文本预处理（Influenza）
##############################
# 1.Influenza去重和生成筛选变量
Influenza <- read_csv(paste0(paths, "Influenza.csv"))
# Influenza[duplicated(Influenza), ] %>% DT::datatable()
Influ_clean <- Influenza %>% select(-keyword) %>% distinct %>% 
  mutate(absNChar = str_length(abstract)) # 54 rep (NA), 19946

# 2.Influenza变量处理和筛选
#（1）摘要缺失值删除
sapply(Influ_clean, function(x) sum(is.na(x)))               # nrow(Influ_clean)-2970
Influ_na <- dplyr::filter(Influ_clean, !is.na(abstract)) 
Influ_na$title[is.na(Influ_na$title)] <- "syntax error" # rep("syntax error", sum(is.na(Influ_na$title)))
sapply(Influ_na, function(x) sum(is.na(x)))
#（2）文献筛选
Influ_select <- dplyr::filter(Influ_na, absNChar > 50)       # 根据频数分布,50以下为无效(实验保守设置)
Influ_select <- Influ_select[!str_detect(Influ_select$abstract, 
                                    "^(\\[).*(\\]\\.)$"), ]  # 筛掉特殊模式'[].'的无效文献
#（3）lowercase conversation
Influ_select$abstract <- str_to_lower(Influ_select$abstract) 
#（4）小于500字数就识别并筛掉含有correction(修正内容)
Influ_less <- dplyr::filter(Influ_select, absNChar < 500)
Index_cor <- with(Influ_less,
                  str_detect(abstract, "correction") | str_detect(title, "correction|erratum|corrigendum")) 
# 正则匹配遇NA仍是NA,若没转化为小写需忽略大小写regex("correction", ignore_case = TRUE)

Influ_seled <- anti_join(Influ_select, Influ_less[Index_cor, ], by = "pid") %>% # 18,307
  dplyr::filter(is.na(year) | year > 2018)                                      # 数据年份筛选2019以后
# Influ_seled[!duplicated(Influ_seled$pid), ] 
# Influ_seled$year %>% table

# write_csv(Influ_seled[c("pid", "abstract")], paste0(paths, "Influ_data.csv"))
# write.table(Influ_seled["abstract"], paste0(paths, "Influ_data.txt"), 
#             row.names = FALSE, 
#             col.names = FALSE,
#             fileEncoding = "UTF-8")

#（5）文本降噪——词层级（N-gram）
# 词条化
library(tidytext)
Influ_proc <- Influ_seled[c("pid", "abstract")] %>% 
  unnest_tokens(word, abstract)
# sum(Influ_proc$word == "")

# # 提取词干并还原（停用词典已含各种形态,不需要还原）
# library(SnowballC)
# Influ_proc$word <- wordStem(Influ_proc$word)

del_allnum <- Influ_proc[str_detect(Influ_proc$word, "^(\\d{1,}.*\\d{1,})$")|
                           str_detect(Influ_proc$word, "^(\\d{1,})$"),]        # 数字
del_no_ennum <- Influ_proc[str_detect(Influ_proc$word, "[^0-9a-zA-Z\\'\\.]"),] # 单词/数字/非英文字符(单位/希腊字母) 
del_no_term <- Influ_proc[str_detect(Influ_proc$word,                          # 文章惯用语
                                     "^(title|abstract|keyword|background|introduction|
                                     method|result|conclusion|discussion|study|analy|
                                     paper|article|review|find)"),] 
  
Influ_del <- Influ_proc %>%
  anti_join(del_allnum, by = "word") %>%   # 删除数词\\d{1,}.*\\d{1,}
  anti_join(del_no_ennum, by = "word") %>% # 删除非英文和数字词
  anti_join(del_no_term, by = "word") %>%  # 删除文章惯用语
  anti_join(stop_words, by = "word") %>%   # 删除停用词
  anti_join(tibble("word" = c("studies","studied","found","data",
                              "cm","ci")))  # 删除自定义停用词

Influ_del <- dplyr::filter(Influ_del, str_length(word) > 3) # 删除单词中字符数不足3的
# del_no_term %>% group_by(word) %>% summarise(n = n()) %>% arrange(desc(n)) %>% DT::datatable()
##############################

# 2 V2W输入（Influenza）
##############################
Influ_docspace <- Tbl2Word2Vec(Influ_del)
write.table(Influ_docspace, paste0(paths, "Influ_docspace.txt"), 
            row.names = FALSE, 
            col.names = FALSE,
            fileEncoding = "UTF-8")
##############################

# 3 result（Influenza）
##############################
# 词向量相关性
Influ_Xsimilar <- read_csv("Influ_Xsimilar.csv")
Influ_Xsim_nodir <- read_csv("Influ_Xsim_nodir.csv")
Influ_Xsimilar %>% DT::datatable()
Influ_Xsim_nodir %>% DT::datatable()
# 结果评估
Influ_XGB <- read_csv("Influ_XGB.csv")
w2v_df_select %>% slice(which.min(R2))
##############################


##############################

# 1 文本预处理（Epilepsia）
##############################

#（1）Epilepsia去重和生成筛选变量
Epilepsia <- read_csv(paste0(paths, "Epilepsia.csv"))
# Epilepsia[duplicated(Epilepsia), ] %>% DT::datatable()
Epil_clean <- Epilepsia %>% select(-keyword) %>% distinct %>% 
  mutate(absNChar = str_length(abstract)) # 54 rep (NA), 19888

#（2）Epilepsia变量处理和筛选
# 摘要缺失值删除
sapply(Epil_clean, function(x) sum(is.na(x)))         # nrow(Epil_clean)-2970
Epil_na <- dplyr::filter(Epil_clean, !is.na(abstract)) 
Epil_na$title[is.na(Epil_na$title)] <- "syntax error" # rep("syntax error", sum(is.na(Epil_na$title)))
sapply(Epil_na, function(x) sum(is.na(x)))
# 文献筛选
Epil_select <- dplyr::filter(Epil_na, absNChar > 70)       # 根据词频分布,70以下为无效(实验保守设置)
Epil_select <- Epil_select[!str_detect(Epil_select$abstract, 
                                       "^(\\[).*(\\]\\.)$"), ]  # 筛掉特殊模式'[].'的无效文献
#（3）lowercase conversation
Epil_select$abstract <- str_to_lower(Epil_select$abstract) 
#（4）小于500字数就识别并筛掉含有correction(修正内容)
Epil_less <- dplyr::filter(Epil_select, absNChar < 500)
Index_cor <- with(Epil_less,
                  str_detect(abstract, "correction") | str_detect(title, "correction")) 
# 正则匹配遇NA仍是NA,若没转化为小写需忽略大小写regex("correction", ignore_case = TRUE)

Epil_seled <- anti_join(Epil_select, Epil_less[Index_cor, ], by = "pid") %>%  # 18271
  dplyr::filter(is.na(year) | year > 2018)                                    # 数据年份筛选2019以后
# Epil_seled[!duplicated(Epil_seled$pid), ] 
# Epil_seled$year %>% table
# write_csv(Epil_seled[c("pid", "abstract")], paste0(paths, "Epil_data.csv"))
# write.table(Epil_seled["abstract"], paste0(paths, "Epil_data.txt"), 
#             row.names = FALSE, 
#             col.names = FALSE,
#             fileEncoding = "UTF-8")
#（5）文本降噪——词层级（N-gram）
# 词条化
library(tidytext)
Epil_proc <- Epil_seled[c("pid", "abstract")] %>% 
  unnest_tokens(word, abstract)
# sum(Epil_proc$word == "")

# # 提取词干并还原（停用词典已含各种形态,不需要还原）
# library(SnowballC)
# Epil_proc$word <- wordStem(Epil_proc$word)

del_allnum <- Epil_proc[str_detect(Epil_proc$word, "^(\\d{1,}.*\\d{1,})$")|
                          str_detect(Epil_proc$word, "^(\\d{1,})$"),]        # 数字
del_no_ennum <- Epil_proc[str_detect(Epil_proc$word, "[^0-9a-zA-Z\\'\\.]"),] # 单词/数字/非英文字符(单位/希腊字母) 
del_no_term <- Epil_proc[str_detect(Epil_proc$word,                          # 文章惯用语
                                    "^(title|abstract|keyword|background|introduction|
                                     method|result|conclusion|discussion|study|analy|
                                     paper|article|review|find)"),] 

Epil_del <- Epil_proc %>%
  anti_join(del_allnum, by = "word") %>%   # 删除数词\\d{1,}.*\\d{1,}
  anti_join(del_no_ennum, by = "word") %>% # 删除非英文和数字词
  anti_join(del_no_term, by = "word") %>%  # 删除文章惯用语
  anti_join(stop_words, by = "word") %>%   # 删除停用词
  anti_join(tibble("word" = c("studies","studied","found","data",
                              "cm","ci")))  # 删除自定义停用词

Epil_del <- dplyr::filter(Epil_del, str_length(word) > 3) 
# 删除单词中字符数不足3的
# del_no_term %>% group_by(word) %>% summarise(n = n()) %>% arrange(desc(n)) %>% DT::datatable()
##############################

# 2 V2W输入（Epilepsia）
##############################
Epil_docspace <- Tbl2Word2Vec(Epil_del)
write.table(Epil_docspace, paste0(paths, "Epil_docspace.txt"), 
            row.names = FALSE, 
            col.names = FALSE,
            fileEncoding = "UTF-8")


Epil_docspace <- read.table(paste0(paths, "Epil_docspace.txt"),
                            fileEncoding = "UTF-8")

##############################

# 3 result（Epilepsia）
##############################
theme_bold <- function() {
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold", size = 8), ## hjust = 1
        axis.title.y = element_text(face = "bold", size = 8), 
        axis.text.x = element_text(),  
        axis.text.y = element_text())
  }
  

# 1.RF
# (1)结果评估
Epil_RF <- read_csv("Epil_RF.csv")
pRF <- ggplot(Epil_RF, aes(nVAR, MSE)) +
  geom_point(color = "#4DBBD5FF", size = 0.8) + 
  geom_line(color = "#3C5488FF") +
  geom_vline(xintercept = which.min(Epil_RF$MSE), 
             color = "#DC0000FF", linetype = "dashed") +
  theme_bw() +
  labs(x = "Number of Keyword", y = "Average MSE for RF") +
  theme_bold() 
pRF
ggsave("Epil_RF.png",width = 12,height = 6, units = "cm", dpi = 600)

Epil_RF %>% slice(which.min(MSE))
# (2)Shap结果评估
Epil_RF_Shap <- read_csv("Epil_RF_Shap.csv")

Epil_RF_Shap <- read_csv("Epil_RF_Tree.csv")

pRF_Shap <- ggplot(Epil_RF_Shap, aes(nVAR, MSE)) +
  geom_point(color = "#4DBBD5FF", size = 0.8) + 
  geom_line(color = "#3C5488FF") +
  geom_vline(xintercept = which.min(Epil_RF_Shap$MSE), 
             color = "#DC0000FF", linetype = "dashed") +
  theme_bw() +
  labs(x = "Number of Keyword", y = "Average MSE for SHAP-based RF") +
  theme_bold()
pRF_Shap
ggsave("Epil_RF_Shap.png",width = 12,height = 6, units = "cm", dpi = 600)

Epil_RF_Shap %>% slice(which.min(MSE))

# 3.GBDT
# (1)结果评估
Epil_GBDT <- read_csv("Epil_GBDT.csv")
pGBDT <- ggplot(Epil_GBDT, aes(nVAR, MSE)) +
  geom_point(color = "#4DBBD5FF", size = 0.8) + 
  geom_line(color = "#3C5488FF") +
  geom_vline(xintercept = which.min(Epil_GBDT$MSE), 
             color = "#DC0000FF", linetype = "dashed") +
  theme_bw() +
  
  labs(x = "Number of Keyword", y = "Average MSE for GBDT") +
  theme_bold()
pGBDT
ggsave("Epil_GBDT.png",width = 12,height = 6, units = "cm", dpi = 600)

Epil_GBDT %>% slice(which.min(MSE))
# (2)Shap结果评估
Epil_GBDT_Shap <- read_csv("Epil_GBDT_Shap.csv")

Epil_GBDT_Shap <- read_csv("Epil_GBDT_Tree.csv")

pGBDT_Shap <- ggplot(Epil_GBDT_Shap, aes(nVAR, MSE)) +
  geom_point(color = "#4DBBD5FF", size = 0.8) + 
  geom_line(color = "#3C5488FF") +
  geom_vline(xintercept = which.min(Epil_GBDT_Shap$MSE), 
             color = "#DC0000FF", linetype = "dashed") +
  theme_bw() +
  labs(x = "Number of Keyword", y = "Average MSE for SHAP-based GBDT") +
  theme_bold()
pGBDT_Shap
ggsave("Epil_GBDT_Shap.png",width = 12,height = 6, units = "cm", dpi = 600)

Epil_GBDT_Shap %>% slice(which.min(MSE))

# 4.XGB
# (1)结果评估
Epil_XGB <- read_csv("Epil_XGB.csv")
pXGB <- ggplot(Epil_XGB, aes(nVAR, MSE)) +
  geom_point(color = "#4DBBD5FF", size = 0.8) + 
  geom_line(color = "#3C5488FF") +
  geom_vline(xintercept = which.min(Epil_XGB$MSE), 
             color = "#DC0000FF", linetype = "dashed") +
  theme_bw() +
  labs(x = "Number of Keyword", y = "Average MSE for XGB") +
  theme_bold() 
pXGB
ggsave("Epil_XGB.png",width = 12,height = 6, units = "cm", dpi = 600)

Epil_XGB %>% slice(which.min(MSE))
# (2)Shap结果评估
Epil_XGB_Shap <- read_csv("Epil_XGB_Shap.csv")

Epil_XGB_Shap <- read_csv("Epil_XGB_Tree.csv")

pXGB_Shap <- ggplot(Epil_XGB_Shap, aes(nVAR, MSE)) +
  geom_point(color = "#4DBBD5FF", size = 0.8) + 
  geom_line(color = "#3C5488FF") +
  geom_vline(xintercept = which.min(Epil_XGB_Shap$MSE), 
             color = "#DC0000FF", linetype = "dashed") +
  theme_bw() +
  labs(x = "Number of Keyword", y = "Average MSE for SHAP-based XGB") +
  theme_bold()
pXGB_Shap
ggsave("Epil_XGB_Shap.png",width = 12,height = 6, units = "cm", dpi = 600)

Epil_XGB_Shap %>% slice(which.min(MSE))

# 5.LGB
# (1)结果评估
Epil_LGB <- read_csv("Epil_LGB.csv")
pLGB <- ggplot(Epil_LGB, aes(nVAR, MSE)) +
  geom_point(color = "#4DBBD5FF", size = 0.8) + 
  geom_line(color = "#3C5488FF") +
  geom_vline(xintercept = which.min(Epil_LGB$MSE), 
             color = "#DC0000FF", linetype = "dashed") +
  theme_bw() +
  labs(x = "Number of Keyword", y = "Average MSE for LGBM") +
  theme_bold()
pLGB
ggsave("Epil_LGB.png",width = 12,height = 6, units = "cm", dpi = 600)

Epil_LGB %>% slice(which.min(MSE))
# (2)Shap结果评估
Epil_LGB_Shap <- read_csv("Epil_LGB_Shap.csv")

Epil_LGB_Shap <- read_csv("Epil_LGB_Tree.csv")

pLGB_Shap <- ggplot(Epil_LGB_Shap, aes(nVAR, MSE)) +
  geom_point(color = "#4DBBD5FF", size = 0.8) + 
  geom_line(color = "#3C5488FF") +
  geom_vline(xintercept = which.min(Epil_LGB_Shap$MSE), 
             color = "#DC0000FF", linetype = "dashed") +
  theme_bw() +
  labs(x = "Number of Keyword", y = "Average MSE for SHAP-based LGBM") +
  theme_bold()
pLGB_Shap
ggsave("Epil_LGB_Shap.png",width = 12,height = 6, units = "cm", dpi = 600)

Epil_LGB_Shap %>% slice(which.min(MSE))

##############################

# 4 关键词池抽取
##############################

w2v_df <- read_csv('E:/Rfiles/R workstation/w2v_df.csv')
word_id <- tibble(id = 1:ncol(w2v_df)-1,word = colnames(w2v_df)) # Python的索引从0开始
# word_id %>% write_csv('E:/Rfiles/R workstation/word_id.csv')

# 1.RF
RFmMSE_fea_shap <- read_csv('E:/Rfiles/R workstation/RFmMSE_fea_shap.csv')
RFmMSE_fea_shap <- dplyr::transmute(RFmMSE_fea_shap, "id" = as.integer(feature))
RF_shap <- left_join(RFmMSE_fea_shap, word_id, by = "id")

RFmMSE_fea_Tree <- read_csv('E:/Rfiles/R workstation/RFmMSE_fea_Tree.csv')
RFmMSE_fea_Tree <- dplyr::transmute(RFmMSE_fea_Tree, "id" = as.integer(feature))
RF_tree <- left_join(RFmMSE_fea_Tree, word_id, by = "id")

RF_rank <- read_csv('E:/Rfiles/R workstation/RF_rank.csv')
RF_tree_el <- left_join(RF_rank, word_id, by = "id") %>%
  dplyr::filter(support == TRUE)
RF_tree_el_rank <- left_join(RF_tree, RF_tree_el, by = "id") 
RF_tree_el_rank_NA <- RF_tree_el_rank[!is.na(RF_tree_el_rank$word.y), ] %>% select(id, word = word.x)

# 2.GBDT
GBDTmMSE_fea_shap <- read_csv('E:/Rfiles/R workstation/GBDTmMSE_fea_shap.csv')
GBDTmMSE_fea_shap <- dplyr::transmute(GBDTmMSE_fea_shap, "id" = as.integer(feature))
GBDT_shap <- left_join(GBDTmMSE_fea_shap, word_id, by = "id")

GBDTmMSE_fea_Tree <- read_csv('E:/Rfiles/R workstation/GBDTmMSE_fea_Tree.csv')
GBDTmMSE_fea_Tree <- dplyr::transmute(GBDTmMSE_fea_Tree, "id" = as.integer(feature))
GBDT_tree <- left_join(GBDTmMSE_fea_Tree, word_id, by = "id")

GBDT_rank <- read_csv('E:/Rfiles/R workstation/GBDT_rank.csv')
GBDT_tree_el <- left_join(GBDT_rank, word_id, by = "id") %>%
  dplyr::filter(support == TRUE)
GBDT_tree_el_rank <- left_join(GBDT_tree, GBDT_tree_el, by = "id")
GBDT_tree_el_rank_NA <- GBDT_tree_el_rank[!is.na(GBDT_tree_el_rank$word.y), ] %>% select(id, word = word.x)

# 3.XGB
XGBmMSE_fea_shap <- read_csv('E:/Rfiles/R workstation/XGBmMSE_fea_shap.csv')
XGBmMSE_fea_shap <- dplyr::transmute(XGBmMSE_fea_shap, "id" = as.integer(feature))
XGB_shap <- left_join(XGBmMSE_fea_shap, word_id, by = "id")

XGBmMSE_fea_Tree <- read_csv('E:/Rfiles/R workstation/XGBmMSE_fea_Tree.csv')
XGBmMSE_fea_Tree <- dplyr::transmute(XGBmMSE_fea_Tree, "id" = as.integer(feature))
XGB_tree <- left_join(XGBmMSE_fea_Tree, word_id, by = "id")

XGB_rank <- read_csv('E:/Rfiles/R workstation/XGB_rank.csv')
XGB_tree_el <- left_join(XGB_rank, word_id, by = "id") %>%
  dplyr::filter(support == TRUE)
XGB_tree_el_rank <- left_join(XGB_tree, XGB_tree_el, by = "id")
XGB_tree_el_rank_NA <- XGB_tree_el_rank[!is.na(XGB_tree_el_rank$word.y), ] %>% select(id, word = word.x)

# 4.LGB
LGBmMSE_fea_shap <- read_csv('E:/Rfiles/R workstation/LGBmMSE_fea_shap.csv')
LGBmMSE_fea_shap <- dplyr::transmute(LGBmMSE_fea_shap, "id" = as.integer(feature))
LGB_shap <- left_join(LGBmMSE_fea_shap, word_id, by = "id")

LGBmMSE_fea_Tree <- read_csv('E:/Rfiles/R workstation/LGBmMSE_fea_Tree.csv')
LGBmMSE_fea_Tree <- dplyr::transmute(LGBmMSE_fea_Tree, "id" = as.integer(feature))
LGB_tree <- left_join(LGBmMSE_fea_Tree, word_id, by = "id")

LGB_rank <- read_csv('E:/Rfiles/R workstation/LGB_rank.csv')
LGB_tree_el <- left_join(LGB_rank, word_id, by = "id") %>%
  dplyr::filter(support == TRUE)
LGB_tree_el_rank <- left_join(LGB_tree, LGB_tree_el, by = "id")
LGB_tree_el_rank_NA <- LGB_tree_el_rank[!is.na(LGB_tree_el_rank$word.y), ] %>% select(id, word = word.x)

##############################

# 5 余弦相似度
##############################
library(lsa)

SimKeyWord <- function(kwPool, modelName = NULL) {
  
  sim <- apply(w2v_df[kwPool$word], 2, 
                  function(x) cosine(x, w2v_df[["epilepsia"]])) %>% as_tibble() 
  sim_kw <- kwPool %>% 
    add_column("similarity" = abs(sim$value)) %>% 
    # arrange(desc(similarity)) %>%
    # add_column("model" = rep(modelName, nrow(kwPool)), .before = 1)
  
  return(sim_kw)
  }

# 1.RF
RF_none <- GBDT_none <- XGB_none <- LGB_none <- SimKeyWord(word_id[1:7, ])
mean(RF_none$similarity)
cumsum(RF_none$similarity)

RF_shap_sim <- SimKeyWord(RF_shap[1:7, ]) # Shap
mean(RF_shap_sim$similarity)
cumsum(RF_shap_sim$similarity)

RF_tree_sim <- SimKeyWord(RF_tree[1:7, ]) # tree
mean(RF_tree_sim$similarity)
cumsum(RF_tree_sim$similarity)

RF_tree_el_sim <- SimKeyWord(RF_tree_el_rank_NA[1:7, ]) # rfecv
mean(RF_tree_el_sim$similarity)
cumsum(RF_tree_el_sim$similarity)

# 2.GBDT
GBDT_shap_sim <- SimKeyWord(GBDT_shap[1:7, ])
mean(GBDT_shap_sim$similarity)

GBDT_tree_sim <- SimKeyWord(GBDT_tree[1:7, ])
mean(GBDT_tree_sim$similarity)

GBDT_tree_el_sim <- SimKeyWord(GBDT_tree_el_rank_NA[1:7, ])
mean(GBDT_tree_el_sim$similarity)

# 3.XGB
XGB_shap_sim <- SimKeyWord(XGB_shap[1:7, ])
mean(XGB_shap_sim$similarity)
cumsum(XGB_shap_sim$similarity)

XGB_tree_sim <- SimKeyWord(XGB_tree[1:7, ])
mean(XGB_tree_sim$similarity)
cumsum(XGB_tree_sim$similarity)

XGB_tree_el_sim <- SimKeyWord(XGB_tree_el_rank_NA[1:7, ])
mean(XGB_tree_el_sim$similarity)
cumsum(XGB_tree_el_sim$similarity)

# 4.LGB
LGB_shap_sim <- SimKeyWord(LGB_shap[1:7, ])
mean(LGB_shap_sim $similarity)
cumsum(LGB_shap_sim$similarity)

LGB_tree_sim <- SimKeyWord(LGB_tree[1:7, ])
mean(LGB_tree_sim$similarity)
cumsum(LGB_tree_sim$similarity)

LGB_tree_el_sim <- SimKeyWord(LGB_tree_el_rank_NA[1:7, ])
mean(LGB_tree_el_sim$similarity)
cumsum(LGB_tree_el_sim$similarity)


# sim_none <- rbind(RF_none, GBDT_none,
#                     XGB_none, LGB_none)
# similarity <- rbind(RF_sim_kw, GBDT_sim_kw,
#                     XGB_sim_kw, LGB_sim_kw)
# 
# similarity %>% DT::datatable()
# write_csv(similarity, "similarity_neg.csv")
# write_csv(similarity, "similarity_pos.csv")
# write_csv(similarity, "similarity.csv")
# 
# # 平均相似度
# sim_avg <- similarity %>% 
#   group_by(model) %>% 
#   summarise(sim_avg = mean(similarity))
# 
# sim_avg_none <- sim_none %>% 
#   group_by(model) %>% 
#   summarise(sim_avg = mean(similarity))

# 累计相似分数
library(ggsci)
npg <- pal_npg("nrc")(10)

theme_bold <- function() {
  theme(legend.position = c(.35, .875),
        legend.text = element_text(size = 7), 
        legend.direction = "vertical",
        legend.background = element_rect(fill = "transparent"), # 图例背景透明
        legend.key.size = unit(.3, "cm"), # 图例key大小
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold", size = 8), ## hjust = 1
        axis.title.y = element_text(face = "bold", size = 8), 
        axis.text.x = element_text(),  
        axis.text.y = element_text(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())}

ggplot(RF_none, aes(x=1:7, y=cumsum(similarity))) + 
  geom_line(aes(color = "#4DBBD5FF")) +
  geom_line(data = RF_shap_sim, aes(color = "#B09C85FF")) +
  geom_line(data = RF_tree_sim, aes(color ="#E64B35FF")) +
  geom_line(data = RF_tree_el_sim, aes(color = "#F39B7FFF"), linetype = "dashed") +
  theme_bw() +
  # labs(x = "Number of Keywords", y = "Cumulative similarity score for RF") +
  labs(x = "关键词数量", y = "基于RF的累积相似度得分") +
  guides(color = guide_legend(title = NULL)) +
  scale_color_discrete(labels = c("RFI", "SHAP-based + RFI",
                                  "Tree-based + RFI", "Tree-based + RFE")) + 
  theme_bold() +
  scale_x_continuous(limits=c(1,7)) +
  scale_y_continuous(limits=c(0,1.25))
ggsave("RF_sim_cum.png",width = 6,height = 6, units = "cm", dpi = 600)


ggplot(GBDT_none, aes(x=1:7, y=cumsum(similarity))) + 
  geom_line(aes(color = "#4DBBD5FF")) +
  geom_line(data = GBDT_shap_sim, aes(color = "#B09C85FF")) +
  geom_line(data = GBDT_tree_sim, aes(color ="#E64B35FF")) +
  geom_line(data = GBDT_tree_el_sim, aes(color = "#F39B7FFF"), linetype = "dashed") +
  theme_bw() +
  # labs(x = "Number of Keywords", y = "Cumulative similarity score for GBDT") +
  labs(x = "关键词数量", y = "基于GBDT的累积相似度得分") +
  guides(color = guide_legend(title = NULL)) +
  scale_color_discrete(labels = c("RFI", "SHAP-based + RFI",
                                  "Tree-based + RFI", "Tree-based + RFE")) + 
  theme_bold() +
  scale_x_continuous(limits=c(1,7)) +
  scale_y_continuous(limits=c(0,1.25))
ggsave("GBDT_sim_cum.png",width = 6,height = 6, units = "cm", dpi = 600)


ggplot(XGB_none, aes(x=1:7, y=cumsum(similarity))) + 
  geom_line(aes(color = "#4DBBD5FF")) +
  geom_line(data = XGB_shap_sim, aes(color = "#B09C85FF")) +
  geom_line(data = XGB_tree_sim, aes(color ="#E64B35FF")) +
  geom_line(data = XGB_tree_el_sim, aes(color = "#F39B7FFF"), linetype = "dashed") +
  theme_bw() +
  # labs(x = "Number of Keywords", y = "Cumulative similarity score for XGB") +
  labs(x = "关键词数量", y = "基于XGB的累积相似度得分") +
  guides(color = guide_legend(title = NULL)) +
  scale_color_discrete(labels = c("RFI", "SHAP-based + RFI",
                                  "Tree-based + RFI", "Tree-based + RFE")) + 
  theme_bold() +
  scale_x_continuous(limits=c(1,7)) +
  scale_y_continuous(limits=c(0,1.25))
ggsave("XGB_sim_cum.png",width = 6,height = 6, units = "cm", dpi = 600)


ggplot(LGB_none, aes(x=1:7, y=cumsum(similarity))) + 
  geom_line(aes(color = "#4DBBD5FF")) +
  geom_line(data = LGB_shap_sim, aes(color = "#B09C85FF")) +
  geom_line(data = LGB_tree_sim, aes(color ="#E64B35FF")) +
  geom_line(data = LGB_tree_el_sim, aes(color = "#F39B7FFF"), linetype = "dashed") +
  theme_bw() +
  # labs(x = "Number of Keywords", y = "Cumulative similarity score for LGBM") +
  labs(x = "关键词数量", y = "基于LGBM的累积相似度得分") +
  guides(color = guide_legend(title = NULL)) +
  scale_color_discrete(labels = c("RFI", "SHAP-based + RFI",
                                  "Tree-based + RFI", "Tree-based + RFE")) + 
  theme_bold() +
  scale_x_continuous(limits=c(1,7)) +
  scale_y_continuous(limits=c(0,1.25))
ggsave("LGB_sim_cum.png",width = 6,height = 6, units = "cm", dpi = 600)
  
  # annotate("text", x=3.5, y=0.42, label="SHAP-based LGBM", size=3) + 
  # annotate("text", x=6, y=0.2, label="LGBM", size=3) + # 图上添加文本

                                                









