
##############################
# 1 训练数据库构建
##############################

# 1.读取数据
require(rvest)   # 解析

datapath <- "E:/Pyfiles/2014sentiment"
datalist <- dir(datapath, full.names = TRUE)[str_detect(dir(datapath), "xml$")]
out <- vector("list", length(datalist))

for (i in seq_along(datalist)) {
  
  # 读取数据构建数据库
  out[[i]] <- read_html(datalist[i])
  
  # 进度条
  cat('\rProcessing progress: ', round(i/length(datalist)*100), "%  ", sep = "")
}

# 2014年
# 2.数据提取
nsample <- length(out)
weibo_tbldata <- vector("list", length = nsample)
for (j in seq(nsample)) {
  
  weibo_sample <- out[[j]]
  
  weiboID <- weibo_sample %>% 
    html_elements("weibo") %>% 
    html_attr("id")
  
  n <- length(weiboID)
  data_tbl <- vector("list")
  for (i in seq(n)) {
    
    xpaths <- paste0("//weibo[@id=", weiboID[i] , "]/sentence")
    
    sentenceID <- weibo_sample %>% 
      html_nodes(xpath = xpaths) %>% 
      html_attr("id")
    sentence <- weibo_sample %>% 
      html_nodes(xpath = xpaths) %>% 
      html_text(trim = TRUE) 
    opinionated <- weibo_sample %>% 
      html_nodes(xpath = xpaths) %>% 
      html_attr("opinionated")
    emo1 <- weibo_sample %>% 
      html_nodes(xpath = xpaths) %>%  
      html_attr("emotion-1-type")
    emo2 <- weibo_sample %>% 
      html_nodes(xpath = xpaths) %>%  
      html_attr("emotion-2-type")
    keyexp <- weibo_sample %>% 
      html_nodes(xpath = xpaths) %>%  
      html_attr("keyexpression1")
    
    # 构建数据库
    data_tbl[[i]] <- tibble(
      weiboID = weiboID[i], # 自动补齐(根据最长)
      sentenceID = sentenceID,
      sentence = sentence,
      opinionated = opinionated,
      emo1 = emo1,
      emo2 = emo2,
      keyexp = keyexp)
  }
  weibo_tbldata[[j]] <- bind_rows(data_tbl)
}

weibo_tbl2014  <- bind_rows(weibo_tbldata)
write_csv(weibo_tbl2014, "E:/Pyfiles/2014sentiment/weibo_tbl2014.csv")


# 2012年
# 2.数据提取
nsample <- length(out)
weibo_tbldata <- vector("list", length = nsample)
for (j in seq(nsample)) {
  
  weibo_sample <- out[[j]]

  weiboID <- weibo_sample %>% 
    html_elements("weibo") %>% 
    html_attr("id")
  
  n <- length(weiboID)
  data_tbl <- vector("list")
  for (i in seq(n)) {
    
    xpaths <- paste0("//weibo[@id=", weiboID[i] , "]/sentence")
    
    sentenceID <- weibo_sample %>% 
      html_nodes(xpath = xpaths) %>% 
      html_attr("id")
    sentence <- weibo_sample %>% 
      html_nodes(xpath = xpaths) %>% 
      html_text(trim = TRUE) 
    opinionated <- weibo_sample %>% 
      html_nodes(xpath = xpaths) %>% 
      html_attr("opinionated")
    polarity <- weibo_sample %>% 
      html_nodes(xpath = xpaths) %>%  
      html_attr("polarity")
    
    # 构建数据库
    data_tbl[[i]] <- tibble(
      weiboID = weiboID[i], # 自动补齐(根据最长)
      sentenceID = sentenceID,
      sentence = sentence,
      opinionated = opinionated,
      polarity = polarity)
  }
  weibo_tbldata[[j]] <- bind_rows(data_tbl)
}

weibo_tbl  <- bind_rows(weibo_tbldata)

write_csv(weibo_tbl, "E:/Pyfiles/sentiment/weibo_tbl.csv")

##############################
# 2 训练数据整合
##############################

weibo_tbl <- read_csv("E:/Pyfiles/sentiment/weibo_tbl.csv")
weibo_tbl %>% DT::datatable()

weibo_tbl2014 <- read_csv("E:/Pyfiles/2014sentiment/weibo_tbl2014.csv")

tail(weibo_tbl2014)
weibo_tbl2014$sentenceID %>% is.na() %>% sum()

##############################
# 3 文本分词与格式转换
##############################

# 读写路径
paths <- "E:/Pyfiles/2014sentiment/"

# tbl格式转为词向量输入格式
Tbl2Word2Vec <- function(datasets,
                         paths) {
  # datasets with column: id and words
  
  segcn_lst <- with(datasets, split(words, id))
  
  nseg <- length(segcn_lst)
  doc_space <- vector("list", nseg)
  
  for (j in seq(nseg)) {
    
    for (i in seq(segcn_lst[[j]])) {
      doc_space[[j]] <- paste(doc_space[[j]], list(segcn_lst[[j]][i]))
    }
    cat('\rProcessing progress: ', round(j/nseg*100), "%  ", sep = "")
  }
  # disab_space <- doc_space %>% unlist %>% paste0(sep = " ")
  # 
  # write.table(doc_space, paste0(paths, "disab_space", weiboyears, ".txt"),
  #             row.names = FALSE,
  #             col.names = FALSE,
  #             fileEncoding = "UTF-8")
  doc_space <- doc_space %>% unlist %>% paste0(sep = " ")
  w2v_space <- tibble(id = unique(datasets$id), doc = doc_space) # id可能不是连续的！数据清洗删掉了
  
  write_csv(w2v_space, paste0(paths, "w2v_space.csv"))
  
  return(w2v_space)
  }

weibo_tbl2014 <- read_csv("E:/Pyfiles/2014sentiment/weibo_tbl2014.csv")
weibo2014_pre <- weibo_tbl2014

# （1）预处理：编码不一致不能直接分词：有的只有符号，有的只有数字
weibo2014_pre$sentence <- str_replace_all(weibo2014_pre$sentence, 
                                          "[[:punct:]0-9a-zA-Z\\s$~+^<>=|]+", " ")

weibo2014_col <- weibo2014_pre %>% distinct %>% 
  mutate(nChar = str_length(sentence)) %>% 
  dplyr::filter(nChar > 1)
# dplyr::filter(weibo2014_col, nChar == 2) %>% DT::datatable()
write_csv(weibo2014_col, "E:/Pyfiles/2014sentiment/weibo2014_pre.csv")

# （2）分词
library(jiebaR)
wk <- worker()
segmentcn <- weibo2014_col$sentence %>%
  lapply(segment, jiebar = wk) 

weibo2014_add <- weibo2014_col %>% 
  add_column(id = 1:nrow(weibo2014_col), .before = 1)
weibo2014_seg <- with(weibo2014_add, tibble(
    id = as.integer(rep(id, unlist(lapply(segmentcn, length)))),
    weiboID = as.factor(rep(weiboID, unlist(lapply(segmentcn, length)))),
    sentenceID = as.factor(rep(sentenceID, unlist(lapply(segmentcn, length)))),
    opinionated = as.factor(rep(opinionated, unlist(lapply(segmentcn, length)))),
    emo1 = as.factor(rep(emo1, unlist(lapply(segmentcn, length)))),
    emo2 = as.factor(rep(emo2, unlist(lapply(segmentcn, length)))),
    keyexp = as.factor(rep(keyexp , unlist(lapply(segmentcn, length)))),
    nChar = as.factor(rep(nChar, unlist(lapply(segmentcn, length)))),
    words = unlist(segmentcn)))

# （3）文本降噪
stopword <- read_delim("./TextMining/stopword_jieba_tmcn_web.txt", delim = "\n")

weibo2014_no_short <- weibo2014_seg %>%       # 43,494,686 x 3
  dplyr::filter(str_length(words) >= 2) %>%    ## 过滤词长小于2
  anti_join(stopword, by = "words")         ## 过滤停用词

weibo2014_no_freq <- weibo2014_no_short %>%                  
  group_by(words) %>%
  summarise(freq = n()) %>% 
  arrange(desc(freq)) 
# quantile(disab_freq$freq, .75)
del_no_less <- dplyr::filter(weibo2014_no_freq, freq < mean(freq)) # 去除频数小于频数均值的词

weibo2014_anti <- weibo2014_no_short %>% 
  anti_join(del_no_less, by = "words") %>%   
  anti_join(stopword, by = "words") %>%
  dplyr::filter(str_length(words) >= 2) 

write_csv(weibo2014_anti, paste0(paths, "weibo2014_anti.csv"))

# （4）转换W2V格式，表格链接
w2v_space <- Tbl2Word2Vec(weibo2014_anti, paths)

# 以防weiboID和sentenceID丢失，采用原始分词id生成进行表连接
weibo2014_sel <- select(weibo2014_add, id, weiboID, sentenceID, opinionated, emo1, emo2, keyexp, nChar)

w2v_space_tbl <- left_join(w2v_space, weibo2014_sel, by = "id") %>% 
  distinct()

w2v_space_tbl

# 检查weiboID和sentenceID是否丢失
w2v_space_tbl$weiboID %>% is.na() %>% sum()
w2v_space_tbl$sentenceID %>% is.na() %>% sum()
w2v_space_tbl$opinionated %>% is.na() %>% sum()
w2v_space_tbl$emo1 %>% is.na() %>% sum()
w2v_space_tbl$emo2 %>% is.na() %>% sum()
# 检查是否有无效字段
w2v_space_tbl %>% dplyr::filter(str_length(doc) < 5)

# 编码
sum(w2v_space_tbl$opinionated == "Y")
sum(w2v_space_tbl$opinionated == "N")
w2v_space_tbl$opinionated[w2v_space_tbl$opinionated == "Y"] <- "1"
w2v_space_tbl$opinionated[w2v_space_tbl$opinionated == "N"] <- "0"

write_csv(w2v_space_tbl, paste0(paths, "w2v_space_tbl.csv"))

write_csv(w2v_space_tbl["doc"], paste0(paths, "w2v_space_doc.csv"), col_names = F)

w2v_space_tbl[1:1000,] %>% DT::datatable()


w2v_space_tbl <- read_csv("E:/Pyfiles/2014sentiment/w2v_space_tbl.csv")

# 平衡一下样本
sum(w2v_space_tbl$opinionated == 1)
sum(w2v_space_tbl$opinionated == 0)

not_opn_index <- which(w2v_space_tbl$opinionated == 0)
not_opn_index_sel <- sample(not_opn_index, sum(w2v_space_tbl$opinionated == 1))
w2v_space_new <- rbind(w2v_space_tbl[w2v_space_tbl$opinionated == 1, ], 
                       w2v_space_tbl[not_opn_index_sel, ])
sum(w2v_space_new$opinionated == 1)
sum(w2v_space_new$opinionated == 0)

write_csv(w2v_space_new, "E:/Pyfiles/2014sentiment/w2v_space_new.csv")
write_csv(w2v_space_new["doc"], "E:/Pyfiles/2014sentiment/w2v_space_new_doc.csv", col_names = F)


# 情感标签处理
w2v_space_new <- read_csv("E:/Pyfiles/2014sentiment/w2v_space_new.csv")
w2v_op <- w2v_space_new[w2v_space_new$opinionated == 1, ]
# 检测缺失值
w2v_op$emo1 %>% is.na() %>% sum()
table(w2v_op$emo1)
# 编码
sum(w2v_op$emo1 == "anger" | w2v_op$emo1 == "disgust" | 
      w2v_op$emo1 == "fear" | w2v_op$emo1 == "sadness" | w2v_op$emo1 == "surprise")
sum(w2v_op$emo1 == "happiness" | w2v_op$emo1 == "like")
w2v_op$emo1[w2v_op$emo1 == "happiness" | w2v_op$emo1 == "like"] <- "1"
w2v_op$emo1[w2v_op$emo1 == "anger" | w2v_op$emo1 == "disgust" | 
              w2v_op$emo1 == "fear" | w2v_op$emo1 == "sadness" | w2v_op$emo1 == "surprise"] <- "0"

write_csv(w2v_op, "E:/Pyfiles/2014sentiment/w2v_op.csv")
write_csv(w2v_op["doc"], "E:/Pyfiles/2014sentiment/w2v_op_doc.csv", col_names = F)


# 平衡一下样本
w2v_op <- read_csv("E:/Pyfiles/2014sentiment/w2v_op.csv")
sum(w2v_op$emo1 == 1)
sum(w2v_op$emo1 == 0)

not_emo_index <- which(w2v_op$emo1 == 0)
not_emo_index_sel <- sample(not_emo_index, sum(sum(w2v_op$emo1 == 1)))
w2v_op_new <- rbind(w2v_op[(w2v_op$emo1 == 1), ], 
                    w2v_op[not_emo_index_sel, ])
sum(w2v_op_new$emo1 == 1)
sum(w2v_op_new$emo1 == 0)

write_csv(w2v_op_new, "E:/Pyfiles/2014sentiment/w2v_op_new.csv")
write_csv(w2v_op_new["doc"], "E:/Pyfiles/2014sentiment/w2v_op_new_doc.csv", col_names = F)




