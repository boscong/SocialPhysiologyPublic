library(entropy)

# Entropy Weight
EntropyWeight <- function(data_frame,index_type=1) {
  # index_type=1 means pos,index_type=-1 means neg.
  
  # max-min scale
  data_maxmin <- apply(data_frame, 2,
                       function(x) {
                         if (index_type > 0) {
                           (x-min(x))/(max(x)-min(x))
                         } else {
                           (max(x)-x)/(max(x)-min(x))
                         }
                       })
  n <- nrow(data_maxmin)
  # entropy of information
  data_entropy <- apply(data_maxmin, 2, entropy)
  
  # entropy weight 
  entropy_weight <- (1-data_entropy)/(length(data_entropy)-sum(data_entropy))
  
  return(entropy_weight)
}

EntropyWeight(iris[1:4])


index <- read_csv("E:/Pyfiles/2014sentiment/index.csv")

#（1）权重确定
# max-min scale
# 负向指标 x34/35/50
# 适度指标 x12/13
data_pos <- apply(index[c(-12,-13,-34,-35,-50)], 2,  function(x) (x-min(x))/(max(x)-min(x))) %>% as_tibble()
data_neg <- apply(index[c(34,35,50)], 2,  function(x) (max(x)-x)/(max(x)-min(x))) %>% as_tibble()
data_neu <- apply(index[c(12,13)], 2,  function(x) 1-abs(x-mean(x))/max(mean(x)-min(x), max(x)-mean(x))) %>% as_tibble()
data_index <- cbind(data_pos*0.998+0.002,data_neu*0.998+0.002,data_neg*0.998+0.002)

data_pop <- apply(data_index , 2, function(x) x/sum(x)) %>% as_tibble()
data_entropy <- apply(data_pop, 2, function(x) -1/log(5)*sum(x*log(x))) 
entropy_weight <- (1-data_entropy)/(length(data_entropy)-sum(data_entropy)) 

weight_tbl <- tibble(varis=names(entropy_weight), weight= entropy_weight)

write_csv(weight_tbl, "E:/Pyfiles/2014sentiment/entropy_weight.csv")

#（2）综合评价
# TOPSIS
data_order <- cbind(data_index[1:11], data_index[46:47], data_index[12:31], 
                    data_index[48:49], data_index[32:45],data_index[50])
weight_order <- rbind(weight_tbl[1:11, ], weight_tbl[46:47, ], weight_tbl[12:31, ], 
                      weight_tbl[48:49, ], weight_tbl[32:45, ],weight_tbl[50, ])
# 长宽数据转换
# weight_sp <- spread(data = weight_order, 
#        key = varis, ## key需将变量值拓展为字段的变量
#        value = weight) 
# 构造加权决策矩阵
decision <- apply(data_order, 2, function(x) x/sum(x^2)) %>% as_tibble()

weight_decision <- vector("list", length = 50)
for (i in 1:50) {
  weight_decision[[i]] <- weight_order[[i, 2]]*decision[i] 
}
decision <- bind_cols(weight_decision)
# weight_order %>% DT::datatable() 
# weight_sp %>% DT::datatable() 
# weight_order[[50, 2]]*decision[50]

# 确定正负理想值
dis_max <- lapply(decision, function(x) max(x)) %>% as_tibble()
dis_min <- lapply(decision, function(x) min(x)) %>% as_tibble()
dis_pos <- apply(decision, 1,  function(x) sqrt(sum((x-dis_max)^2))) %>% as_tibble()
dis_neg <- apply(decision, 1,  function(x) sqrt(sum((x-dis_min)^2))) %>% as_tibble()

topsis_score <- tibble(year = 2017:2021, 
                       score = (dis_neg/(dis_pos+dis_neg))$value)

write_csv(topsis_score, "E:/Pyfiles/2014sentiment/topsis_score.csv")

p_topsis_score <- ggplot(topsis_score , aes(year, score )) +
  geom_point(color = "#4DBBD5FF", size = 0.8) + 
  geom_line(color = "#3C5488FF") +
  theme_bw() +
  scale_x_continuous(breaks = 2017:2024) +
  labs(x = "年份", y = "残疾人事业综合发展状况") +
  theme_bold() 
p_topsis_score
ggsave("topsis_score.png", width = 12,height = 6, units = "cm", dpi = 600)


#（3）一级指标
weight_order <- rbind(weight_tbl[1:11, ], weight_tbl[46:47, ], weight_tbl[12:31, ], 
                      weight_tbl[48:49, ], weight_tbl[32:45, ],weight_tbl[50, ])


TPScoreIndex <- function(data_order =  data_order,   # 顺序指标值
                         weight_order = weight_order, # 顺序权重值
                         index_range = 1:5) { # 指标范围
  
  weight_st <- weight_order$weight[index_range]/sum(weight_order$weight[index_range])
  # 构造加权决策矩阵
  decision <- apply(data_order[index_range], 2, function(x) x/sum(x^2)) %>% as_tibble()
  
  n <- length(index_range)
  weight_decision <- vector("list", length = n)
  for (i in 1:n) {
    weight_decision[[i]] <- weight_order[[i, 2]]*decision[i] 
  }
  decision <- bind_cols(weight_decision)
  # weight_order %>% DT::datatable() 
  # weight_sp %>% DT::datatable() 
  # weight_order[[50, 2]]*decision[50]
  
  # 确定正负理想值
  dis_max <- lapply(decision, function(x) max(x)) %>% as_tibble()
  dis_min <- lapply(decision, function(x) min(x)) %>% as_tibble()
  dis_pos <- apply(decision, 1,  function(x) sqrt(sum((x-dis_max)^2))) %>% as_tibble()
  dis_neg <- apply(decision, 1,  function(x) sqrt(sum((x-dis_min)^2))) %>% as_tibble()
  
  topsis_score <- tibble(year = 2017:2021, 
                         score = (dis_neg/(dis_pos+dis_neg))$value)
  return(topsis_score)
}

first_index <- tibble(kangfu=TPScoreIndex(data_order =  data_order,
                                          weight_order = weight_order, 
                                          index_range = 1:5)$score,
                      jiaoyu=TPScoreIndex(data_order =  data_order,
                                          weight_order = weight_order, 
                                          index_range = 6:9)$score,
                      jiuye=TPScoreIndex(data_order =  data_order,
                                        weight_order = weight_order, 
                                        index_range = 10:18)$score,
                      shehuibaozhang=TPScoreIndex(data_order =  data_order,
                                                  weight_order = weight_order, 
                                                  index_range = 19:21)$score,
                      wentixuanchuan=TPScoreIndex(data_order =  data_order,
                                                  weight_order = weight_order, 
                                                  index_range = 22:30)$score,
                      weiquan=TPScoreIndex(data_order =  data_order,
                                           weight_order = weight_order, 
                                           index_range = 31:38)$score,
                      zuzhi=TPScoreIndex(data_order =  data_order,
                                         weight_order = weight_order, 
                                         index_range = 39:42)$score,
                      fuwusheshi=TPScoreIndex(data_order =  data_order,
                                   weight_order = weight_order, 
                                   index_range = 43:46)$score,
                      shehuihudong=TPScoreIndex(data_order =  data_order,
                                                weight_order = weight_order, 
                                                index_range = 47:50)$score)
write_csv(first_index,  "E:/Pyfiles/2014sentiment/first_index.csv")

snd_index <- tibble(kangfufuwu=TPScoreIndex(data_order =  data_order,
                                        weight_order = weight_order, 
                                        index_range = 1:2)$score,
                    kangfujianshe=TPScoreIndex(data_order =  data_order,
                                            weight_order = weight_order, 
                                            index_range = 3:5)$score,
                    gaojizhongdengjiaoyu=TPScoreIndex(data_order =  data_order,
                                        weight_order = weight_order, 
                                        index_range = 6:7)$score,
                    gaodengjiaoyu=TPScoreIndex(data_order =  data_order,
                                                      weight_order = weight_order, 
                                                      index_range = 8:9)$score,
                    chengxiangjiuye=TPScoreIndex(data_order =  data_order,
                                       weight_order = weight_order, 
                                       index_range = 10:11)$score,
                    jiuyejiegou=TPScoreIndex(data_order =  data_order,
                                       weight_order = weight_order, 
                                       index_range = 12:13)$score,
                    mangrenanmo=TPScoreIndex(data_order =  data_order,
                                       weight_order = weight_order, 
                                       index_range = 14:15)$score,
                    jiuyebangfu=TPScoreIndex(data_order =  data_order,
                                       weight_order = weight_order, 
                                       index_range = 16:18)$score,
                    shehuibaoxian=TPScoreIndex(data_order =  data_order,
                                                weight_order = weight_order, 
                                                index_range = 19:20)$score,
                    tuoyangfuwu=TPScoreIndex(data_order =  data_order,
                                               weight_order = weight_order, 
                                               index_range = 21)$score,
                    wenhua=TPScoreIndex(data_order =  data_order,
                                                weight_order = weight_order, 
                                                index_range = 22:24)$score,
                    tiyu=TPScoreIndex(data_order =  data_order,
                                                weight_order = weight_order, 
                                                index_range = 25:27)$score,
                    xuanchuan=TPScoreIndex(data_order =  data_order,
                                                weight_order = weight_order, 
                                                index_range = 28:30)$score,
                    fazhiweiquan=TPScoreIndex(data_order =  data_order,
                                         weight_order = weight_order, 
                                         index_range = 31:35)$score,
                    fazhijiuzhu=TPScoreIndex(data_order =  data_order,
                                         weight_order = weight_order, 
                                         index_range = 36:38)$score,
                    zuzhijianshe=TPScoreIndex(data_order =  data_order,
                                       weight_order = weight_order, 
                                       index_range = 39:41)$score,
                    zhiyuanzhezhucan=TPScoreIndex(data_order =  data_order,
                                       weight_order = weight_order, 
                                       index_range = 42)$score,
                    fuwusheshitouru=TPScoreIndex(data_order =  data_order,
                                            weight_order = weight_order, 
                                            index_range = 43:44)$score,
                    fuwusheshichanchu=TPScoreIndex(data_order =  data_order,
                                            weight_order = weight_order, 
                                            index_range = 45:46)$score,
                    shehuiguanzhu=TPScoreIndex(data_order =  data_order,
                                              weight_order = weight_order, 
                                              index_range = 47:48)$score,
                    shehuifankui=TPScoreIndex(data_order =  data_order,
                                              weight_order = weight_order, 
                                              index_range = 49:50)$score)
write_csv(snd_index,  "E:/Pyfiles/2014sentiment/snd_index.csv")

















