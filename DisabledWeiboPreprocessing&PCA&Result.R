#############################
# 0 微博爬虫
#############################
library(lubridate)

search_key_list = list('残疾','残障','伤残','截肢',
                       '瘫痪','假肢','义肢','无障碍','认知障碍',
                       '精神障碍','运动障碍')


datalist = list('disability','handicapped','disabled','amputated',
                'paralyzed','prosthetics','artificial_limbs','accessibility','cognitive_impair',
                'mental_impair','movement_disorders')


#############################
# 1 数据库构建
#############################

DataCombine <- function(datapath) {
  # 单关键词基础数据库整合
  
  datalist <- dir(datapath, full.names = TRUE)[str_detect(dir(datapath), "csv$")]
  out <- vector("list", length(datalist))
  for (i in seq_along(datalist)) {
    
    # 读取数据构建数据库
    tryCatch({
      # # 数据编码异常
      
      out[[i]] <- read_csv(datalist[i], locale=locale(encoding="GBK"),
                           col_types = cols(
                             `加入微博时间` = col_character(),
                             `发布时间` = col_character(),
                             `爬取时间` = col_character())) %>% distinct
      names(out[[i]]) <- c("keyword","post","link","like","repost","comment","image_video_link","post_time","post_id",
                           "post_account","account_type","follower","account_s_follower",
                           "author_profile","ip_affiliation","gender","all_post",
                           "tag","image_video_type","blogger_category","company","university",
                           "registration_time","credit","crawl_time","terminal")

    }, error = function(e) {
      out[[i]] <- read.csv(datalist[i]) %>% distinct %>% as_tibble # 数据编码异常处理
      names(out[[i]]) <- c("keyword","post","link","like","repost","comment","image_video_link","post_time","post_id",
                           "post_account","account_type","follower","account_s_follower",
                           "author_profile","ip_affiliation","gender","all_post",
                           "tag","image_video_type","blogger_category","company","university",
                           "registration_time","credit","crawl_time","terminal")
    })
    
    # 进度条,按需设定try catch
    cat('\rProcessing progress: ', round(i/length(datalist)*100), "%  ", sep = "")
  }
  
  return(out)
  
}

SaveFun = function(datapath = "E:/Pyfiles/disable_data/",
                   datanames) {
  # 保存数据
  pathtemp = paste0(datapath, datanames)
  datatemp <- bind_rows(DataCombine(pathtemp)) %>% distinct()
  write_csv(datatemp, paste0(pathtemp, "/",datanames, '.csv'))
  
  return(datatemp)
}

# 合并数据，并写入硬盘
disability <- SaveFun(datanames = "disability")
handicapped <- SaveFun(datanames = "handicapped")
disabled <- SaveFun(datanames = "disabled")
amputated <- SaveFun(datanames = "amputated")
paralyzed <- SaveFun(datanames = "paralyzed")
prosthetics <- SaveFun(datanames = "prosthetics")
artificial_limbs <- SaveFun(datanames = "artificial_limbs")
accessibility <- SaveFun(datanames = "accessibility")
cognitive_impair <- SaveFun(datanames = "cognitive_impair")
mental_impair <- SaveFun(datanames = "mental_impair")
movement_disorders <- SaveFun(datanames = "movement_disorders")

sample_n(movement_disorders, 1000) %>% DT::datatable(.)

# 多关键词数据库整合 
datapath  <-  "E:/Pyfiles/disable_data/"
datalist  <-  list('disability','handicapped','disabled','amputated','paralyzed',
                   'prosthetics','artificial_limbs','accessibility','cognitive_impair',
                   'mental_impair','movement_disorders')

for (i in seq_along(datalist)) {
  assign(datalist[[i]], read_csv(paste0(datapath, datalist[[i]], "/", datalist[[i]], ".csv"),
                                 col_types = cols(
                                   post_time = col_character(),
                                   registration_time = col_character(),
                                   crawl_time = col_character())))
  # 进度条
  cat('\rProcessing progress: ', round(i/length(datalist)*100), "%  ", sep = "")
  
}

disab_dbase <- rbind(disability,handicapped,disabled,amputated,paralyzed,
                     prosthetics,artificial_limbs,accessibility,cognitive_impair,
                     mental_impair,movement_disorders) # A tibble: 2,676,033 x 26

# '残疾','残障','伤残','截肢','瘫痪','假肢','义肢','无障碍','认知障碍','精神障碍','运动障碍'
#############################
# 2 数据清洗
#############################
# 1.文档层级

# 去重和生成新变量(post字符数可以等内容修正后生成)
disab_rep <- distinct(disab_dbase) %>% # 去重，减轻下一步去重压力
  .[!duplicated(.$post), ]          # 根据post去重，1,018,591 x 27
disab_rep$post <- str_remove_all(disab_rep$post, "\\s") # 去除所有类型空格,后面需要对字符计数（空格也算1个字符）

disab_newvar <- mutate(disab_rep,
                      post_url = str_count(post, "http"), # 生成post中url数量
                      post_hot = str_count(post, "\\[超话\\]"), # 生成post中超话标签数量
                      post_label = str_count(post, "#.+?#") , # 生成post中标签数量,注意正则表达式+必须含字符1个以上（含超话）
                      post_emoji = str_count(post, "\\[[\u4e00-\u9fa50-9a-zA-Z]{1,8}\\]") - post_hot) # 生成post中表情包数量

# disab_newvar %>% sample_n(10000) %>% DT::datatable()
# sapply(disab_clean, function(x) sum(is.na(x)))

# 无效微博内容删除和筛选
disab_newvar$post <- disab_newvar$post %>% 
  str_remove_all("\\[[\u4e00-\u9fa50-9a-zA-Z]{1,8}\\]") %>% # 去除表情包和[超话]标签[XX/XXX]
  str_remove_all("http[[:punct:]0-9a-zA-Z\\s$~+^<>=|]{1,}")  # 去除URL

disab_lesschar <- mutate(disab_newvar, post_char = str_length(post)) %>% # 生成post字符数
  dplyr::filter(post_char > 2) # 保留字符数>2
# disab_lesschar %>% sample_n(1000) %>% DT::datatable()


# 噪声微博删除
DisabUnrelate <- function(disab_lesschar,
                          keyword_char = NULL,
                          pattern_flt = NULL) {
  
  if (!is.null(keyword_char)) (
    if (str_length(keyword_char) > 3) (
      # 关键词为4个字,保留>4个字符的微博
      disab_lesschar <- dplyr::filter(disab_lesschar, post_char > 4)
    )
  )
  
  retweet_list <- str_extract_all(disab_lesschar$post, "//@") # 多个//@符号的表示转发，不要评论只要原微博
  retweet_bool <- lapply(retweet_list, function(x) length(x) > 1) %>% unlist()
  disab_lesschar <- disab_lesschar[!retweet_bool, ] %>% .[!duplicated(.$post), ] # 再去重
  disab_lesschar$post <- str_remove(disab_lesschar$post, "//@.*?:") # 去除//@[id]:模式转发账号
  
  # 无关微博删除
  if (is.null(keyword_char)) (
    tempdata <- disab_lesschar
  ) else tempdata <- dplyr::filter(disab_lesschar, keyword == keyword_char)
  
  
  pattern_flt <- paste0("转发微博|转微博|转发|转载|分享视频 http|分享图片|阅读分享|", # 转发分享类
                        
                        "交通瘫痪|交通压力|全城瘫痪|疫情瘫痪|微博瘫痪|系统瘫痪|网络瘫痪|手机瘫痪|全网瘫痪|脑残|",# 迷惑类
                        "电脑瘫痪|服务器瘫痪|软件瘫痪|静若瘫痪|身高残废|睡眠瘫痪|睡眠幻觉|睡眠障碍|累到瘫痪|", 
                        "官网瘫痪|一度瘫痪|半瘫痪状态|电路瘫痪|城市瘫痪|经济瘫痪|陷入瘫痪|企业瘫痪|餐饮瘫痪|",
                        "服务瘫痪|就业瘫痪|瘫痪状态|处于瘫痪|进入瘫痪|“截肢”|码头瘫痪|累瘫痪|机场瘫痪|车站瘫痪|",
                        
                        "膝跳反应|残疾的爱情|残疾队友|残障队友|脑瘫逻辑|想截肢|截肢算了|通信瘫痪|差点瘫痪|",
                        "假肢靴|光腿神器|骑士靴|假肢隆胸|不是假肢|不假肢|堪比假肢|算是假肢|以下都是假肢|",
                        "灵魂不可能有义肢|同款义肢|灵魂没有义肢|闻香识女人|无障碍物|社交无障碍|有点认知障碍|",
                        "下颌运动障碍|睡眠运动障碍|肌抽跃|无障碍交流|",
                        "残疾一样|残障一样|伤残一样|义肢一样|假肢一样|截肢一样|瘫痪一样|精神障碍一样|运动障碍一样|认知障碍一样|",
                        "像残疾|像残障|像伤残|像截肢|像假肢|像义肢|像精神障碍|像瘫痪|像认知障碍|像运动障碍|",
                        
                        "门票|动物园|残疾动物|动物救助|野生动物|流浪动物|流浪狗|流浪小狗|流浪猫|流浪小猫|", # 残疾动物类
                        "狗狗|猫猫|领养代替购|缉毒犬|残障鱼|残疾狗|残疾猫|残疾野生|猫咪|柴犬|狗子|短腿猫|大象|", 
                        
                        "经济学人|考研英语|阅读理解|四六级|英语四级|英语六级|雅思|托福|英语听力|每日英语|", # 学习类
                        "三国演义|水浒|扇贝打卡|百词斩|打卡活动|模拟考题|高校招生考试|天天打卡|",
                        
                        "书单|小说|连载|故事情节|男主|女主|美剧|韩剧|英剧|影视解说|金鸡奖|大结局|杀青|娱乐圈|", # 娱乐消遣类
                        "脑残粉|娱乐新闻|恋与制作人|我们的歌|再见爱人|时代少年团|穿越剧|禁忌游戏|彩蛋|",
                        "魔鬼的体温|这就是街舞|心动欲燃|长相思|电视剧|霹雳布袋戏|南风知我意|网剧|柳舟记|",
                        "阳光普照|本草纲目|目黑莲|藤萝为枝|剧情|追剧|刷剧|剧集|番外篇|混剪|剪辑|神功|",
                        "进击的巨人|上映|武侠|外卖小哥和伪残疾|读书看我置顶|残疾反派|环球影城|深海长眠|",
                        "权力的游戏|行尸走肉|僵尸|以家人之名|我和我的时光少年|偶像剧|演技|金马奖|配角|",
                        "演员请就位|六公主|寂静之地|美国大片|漫威|复仇者联盟|外婆的新世界|内地综艺|内娱|",
                        "tvb|TVB|马得福|苦水村|山海情|我的春日纪实|公主宝贝|敖丙|遇见璀璨的你|煎牛排|",
                        "辛波斯卡|来风至|金牌解说|电影|昆仑神宫|谢谢你医生|海贼王|你的名字|扫黑风暴|",
                        "奇迹暖暖|高岛屋|kimrhodes|披荆斩棘|叶问蹲|安欣|爱丽丝|小花仙|良言写意|厉择良|",
                        "纪舒晚陆曜景|血观音|赛博朋克|女武神|鹅岭二厂|唐探|哈利波特|王牌部队|",
                        
                        "权志龙|蔡少芬|周棋洛|成毅|张予曦|丁程鑫|贺峻霖|刘浩存|傅云深|李炎谁|肖春生|", # 艺人明星类 
                        "马振桓|齐思钧|钱嘉乐|龚俊|陈炜|赵丽颖|亦舒|林雨晗|陈铭生|钟楚曦|张云龙|罗晋|王一博|",
                        "王菲|谢霆锋|谢娜|王俊凯|刘畊宏|王心凌|施柏宇|张翰|郑爽|邓超|姚晨|刘宪华|田鸿熊|", 
                        "Angelababy|杨颖|angelababy|马斯佳|杏鲍菇|藤本树|千颂伊|泽禹|李敏镐|",
                        
                        "道德经|克夫|佛教|菩提|菩萨|法师|修行|寺庙|吉凶|断财官|八字|禅理|算命|阴德|", # 宗教信仰类
                        
                        "王者|妲己|兰陵王|原神|第五人格|狼队对战|赛点|明日方舟|东皇|公孙离|对抗路|打野|反野|守野区|", # 游戏类
                        "游走支援|蹲一波|打龙|主宰|斩杀|塞尔达|switch|迷雾侦探|游戏安利|机动神脑|人偶师|", 
                        
                        "欧莱雅|美妆|谈恋爱|无障碍浏览 |刍狗|走狗|狗日|小日本|老色胚|狗仔|", # 不常见无关类
                        
                        "#股票|#基金|#今日看盘|美股|期货|同花顺|招聘面试|深圳招聘|", # 经济类
                        
                        "把你打成残|残废光头|小矮人|残障迅猛龙|脑子残疾|牛皮凉席|双十一|双11|光棍节|肥宅|宅男|", # 常见网络类
                        "宅女|圣母|婊子|猥琐|小学生必背|cosplay|漫展|哈哈哈|哥你的腿不是|黑心卤蛋|呜呜呜|不到170|",
                        "营销大师|次元|姐弟恋|恋爱脑|霸道总裁|狗蛋|死残废|死脑残|单身狗|死直男|普信男|普信女|不到一米七|",
                        "卧槽|草泥马|草你妈|艹你妈|槽尼玛|爷青回|你个老六|栓Q|小趴菜|芭比Q了|啊对对对|挚友语音版|",
                        "城野医生|万物狂欢|",
                        
                        "人民至上|生命至上|第一杯奶茶|神兽|光盘行动|双循环|内卷|好家伙|百年未有之大变局|赶考|", # 年度热词类
                        "打工人|双减|碳达峰|碳中和|野性消费|理性消费|车险综合改革|觉醒年代|打鸡血|踔厉奋发|",
                        "新赛道|雪糕刺客|沉浸式|华为HDC|核酸检测通告|核酸检测通知|防控方案|疫情防控指挥部",
                        pattern_flt)
  tempdata = tempdata[!str_detect(tempdata$post, pattern_flt), ] 
  
  return(tempdata)
}

data_unr <- DisabUnrelate(disab_lesschar) # 843,217 x 31
# data_unr %>% sample_n(10000) %>% DT::datatable()
write_csv(data_unr, "E:/Pyfiles/disable_data/data_unr.csv")

####################################################################
data_unr <- read_csv("E:/Pyfiles/disable_data/data_unr.csv",
                     col_types = cols(post_time = col_character(),
                                      registration_time = col_character(),
                                      crawl_time = col_character()))

# 保留微博
DisabFilter <- function(disab_data,
                        keyword_char = NULL,
                        pattern_flt = NULL) {
  
  if (is.null(keyword_char)) (
    tempdata <- disab_data
  ) else tempdata <- dplyr::filter(disab_data, keyword == keyword_char)
  
  pattern_flt = paste0("残疾|残障|伤残|残废|瘫痪|躯干麻痹|四肢瘫|扶残|助残|残缺|身残|",
                       "截肢|假肢|义肢|截指|残肢|肢残|独臂|独腿|独肢|轮椅|腿缺失|臂缺失|指缺失|钢腿|",
                       
                       "无障碍|认知障碍|精神障碍|心理障碍|精神疾病|",
                       "身体缺陷|行动不便|四肢毫无知觉|自理能力缺陷|运动障碍|",
                       
                       "手指|脚趾|双手|双臂|双腿|双眼|双耳|双脚|上肢|下肢|",
                       "失明|盲人|盲道|视障|听障|耳聋|听力损失|听力损伤|听力障碍|助听器|言语障碍|",
                       
                       "聋哑|哑巴|失语|运动性构音障碍|器质性构音障碍|发声障碍|儿童言语发育迟滞|",
                       "精神发育不全|智力迟滞|智力损害|智力明显衰退|智障|智力缺陷|有缺陷|",
                       
                       "植物人|脑瘫|偏瘫|截瘫|渐冻症|渐冻人|肌萎缩症|肌肉萎缩|肌无力|骨折|",
                       
                       "痴呆|阿尔兹海默|阿尔茨海默|双相情感障碍|双相障碍|创伤后应激障碍|PTSD|ptsd|",
                       "小儿麻痹|精神分裂|自闭症|认知功能障碍|记忆功能障碍|记忆障碍|",
                       
                       "孤独谱系障碍|ASD|ads|",
                       "截掉|知觉|摔残|致残|断指|断肢|断臂|断腿|断手|断脚|",
                       
                       "残奥|残联|地震|导盲犬|战争|战后|车祸|医院|医生", 
                       pattern_flt)
  tempdata = tempdata[str_detect(tempdata$post, pattern_flt), ] 
  
  return(tempdata)
}

data_fil <- DisabFilter(data_unr) # 808,944 x 31
write_csv(data_fil, "E:/Pyfiles/disable_data/data_fil.csv")

####################################################################
data_fil <- read_csv("E:/Pyfiles/disable_data/data_unr.csv",
                     col_types = cols(post_time = col_character(),
                                      registration_time = col_character(),
                                      crawl_time = col_character()))


disability_flt <- dplyr::filter(data_fil, keyword == "残疾")
# disability_flt %>% sample_n(1000) %>% DT::datatable()

handicapped_flt <- dplyr::filter(data_fil, keyword == "残障")
# handicapped_flt %>% sample_n(1000) %>% DT::datatable()

disabled_flt <- dplyr::filter(data_fil, keyword == "伤残")
# disabled_flt %>% sample_n(1000) %>% DT::datatable()

amputated_flt <- dplyr::filter(data_fil, keyword == "截肢")
# amputated_flt %>% sample_n(1000) %>% DT::datatable()
amputated_flt = amputated_flt[!str_detect(amputated_flt$post, paste0("去截肢|快截肢|截肢吧|截肢嘛")), ]

paralyzed_flt <- dplyr::filter(data_fil, keyword == "瘫痪")
# paralyzed_flt %>% sample_n(1000) %>% DT::datatable()
paralyzed_flt = paralyzed_flt[!str_detect(paralyzed_flt$post, paste0("基本瘫痪|全部瘫痪|直接瘫痪|瘫痪了|瘫痪吗|已瘫痪|疼瘫痪|追星")), ]

prosthetics_flt <- dplyr::filter(data_fil, keyword == "假肢")
# prosthetics_flt %>% sample_n(1000) %>% DT::datatable()
prosthetics_flt = prosthetics_flt[!str_detect(prosthetics_flt$post, 
                                              paste0("Day|陪玩|OOTD|3DS|呀呀呀|哇哇哇|拜登|教练语录|还有假肢|整个假肢|假肢公司|安个假肢|民心工程|抠的假肢|问题来了")), ] 


artificial_limbs_flt <- dplyr::filter(data_fil, keyword == "义肢")
# artificial_limbs_flt %>% sample_n(1000) %>% DT::datatable()
artificial_limbs_flt = artificial_limbs_flt[!str_detect(artificial_limbs_flt$post,
                                                        paste0(
                                                          "游戏背景|完璧归赵|银魂|你我的义肢|外表设计|普鲁修卡|莉可|王牌部队|水花|萨摩|",
                                                          
                                                          "原哥|金毛|林克|冬兵|空鹤|米夏|好耶|挑战赛|Luna|寿屋|零距离指导|打游戏|意淫|GPT|",
                                                          
                                                          "北京买电车|体外的义肢|动画|宜野座|美队|毕设")), ] 

accessibility_flt <- dplyr::filter(data_fil, keyword == "无障碍")
# accessibility_flt %>% sample_n(1000) %>% DT::datatable()
accessibility_flt = accessibility_flt[!str_detect(accessibility_flt$post, 
                                                  paste0("斩|热词|棒棒棒|英语|法典|英文|波士顿|追星|老师推荐|学校推荐|演员|后现代主义|整形|美容|百年征程|漫画|阴阳|蔡徐坤|修文县|清净|秋裤|",
                                                         
                                                         "沟通无障碍|无障碍沟通|交流无障碍|车辆无障碍|无障碍对话|语言无障碍|无障碍演绎|",
                                                         
                                                         "完全无障碍|无障碍舒适|无障碍挑战|音乐无障碍|无障碍运行|AC建筑|欢迎词|猥亵|方言|日语|卖房|买房")), ] 


cognitive_impair_flt <- dplyr::filter(data_fil, keyword == "认知障碍")
# cognitive_impair_flt %>% sample_n(1000) %>% DT::datatable()
cognitive_impair_flt = cognitive_impair_flt[!str_detect(cognitive_impair_flt$post,
                                                        paste0(
                                                          "抖音|肥仔|追星|安瑞|泌尿专科|消费者认知障碍|傻子|国籍认知障碍|中风患者与照料者手册|陈露")), ] 

mental_impair_flt <- dplyr::filter(data_fil, keyword == "精神障碍")
# mental_impair_flt %>% sample_n(1000) %>% DT::datatable()
mental_impair_flt = mental_impair_flt[!str_detect(mental_impair_flt$post, paste0("精神障碍护理学|巨婴|中国音乐剧|综治宣传月")), ]


movement_disorders_flt <- dplyr::filter(data_fil, keyword == "运动障碍")
# movement_disorders_flt %>% sample_n(1000) %>% DT::datatable()
movement_disorders_flt = movement_disorders_flt[!str_detect(movement_disorders_flt$post, paste0("室内设计|小阳爱分享|学习的本质")), ]


disab_flt <- rbind(disability_flt,handicapped_flt, disabled_flt,amputated_flt, paralyzed_flt,
                   prosthetics_flt,artificial_limbs_flt,accessibility_flt,cognitive_impair_flt,
                   mental_impair_flt,movement_disorders_flt) # 761,822 x 31

disab_flt$post_char <- str_length(disab_flt$post) # 前面删除过内容,更新一次post字符数
disab_flt <- disab_flt[!str_detect(disab_flt$post_time, "(2023-10)|(2023-11)"), ] %>% 
  dplyr::filter(post_char > 10)   # 744,447 x 31,根据词频分布,10以下为无效(实验保守设置)

# disab_flt %>% sample_n(1000) %>% DT::datatable()
write_csv(disab_flt, "E:/Pyfiles/disable_data/disab_flt.csv")


# 微博发布时间分布情况（结果显示很均匀）
disab_flt <- read_csv("E:/Pyfiles/disable_data/disab_flt.csv",
                      col_types = cols(
                        post_time = col_character(),
                        registration_time = col_character(),
                        crawl_time = col_character()))

library(lubridate)
time_dis <- tibble(year_post = disab_flt$post_time %>% str_extract("^\\d{4}"),
                   id = rep(1,length(year_post)))

ggplot(time_dis, aes(id, fill=year_post)) +
  geom_bar(position = "stack")+
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none")+
  coord_flip()+
  scale_y_reverse()+
  labs(x=NULL, y=NULL)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave("weibo_year.png",width = 15, height = 2, units = "cm", dpi = 600)

teemp_d <- time_dis %>% group_by(year_post) %>% 
  summarise(freq=n()) %>% 
  with(., add_column(., perc=freq/sum(freq)*100 %>% round(2)))
teemp_d$freq/sum(teemp_d$freq)*100 # 6.610947 15.080993 15.486798 15.408350 15.921886 17.893819 13.597207

# S <- c(15.610947,15.080993,15.486798,15.408350,15.921886,15.893819,15.597207)
# ks.test(S, "punif")


# 微博热度分析（结合url）
disab_flt <- read_csv("E:/Pyfiles/disable_data/disab_flt.csv",
                      col_types = cols(
                        post_time = col_character(),
                        registration_time = col_character(),
                        crawl_time = col_character()))
library(lubridate)
hot_select <- mutate(disab_flt, 
                        like, repost, comment, post_time = ymd((str_extract(disab_flt$post_time, "\\d{4}-\\d{2}-\\d{2}"))), 
                        follower, author_profile = str_length(author_profile), all_post, 
                        registration_time = ymd((str_extract(disab_flt$registration_time, "\\d{4}-\\d{2}-\\d{2}"))), 
                        crawl_time = ymd((str_extract(disab_flt$crawl_time, "\\d{4}-\\d{2}-\\d{2}"))), 
                        post_url, post_hot, post_label, post_emoji, post_char)

sapply(hot_select, function(x) sum(is.na(x)))
hot_select <- hot_select[!(is.na(hot_select$registration_time)|is.na(hot_select$author_profile)),]

# follower,tweeting_rate,tweeting_growing,profile_fullness
# wordiness,timeliness,url_fullness,label_rate,emoji_rate
# retweet_rate
# like_rate, comment_rate
hot_select %>% head %>% DT::datatable()

weibo_hot <- mutate(hot_select,
                    year = str_extract(post_time, "^\\d{4}") %>% as.numeric(),
                    follower_rate = follower/(all_post+1), 
                    tweeting_rate = all_post/as.double.difftime(crawl_time-registration_time),
                    # tweeting_growing = as.double.difftime(post_time-registration_time),
                    profile_fullness = author_profile/max(author_profile)*100,
                    
                    wordiness = post_char/max(post_char)*100,
                    url_fullness = post_url/max(post_url)*100,
                    label_rate = post_label/post_char*100,
                    emoji_rate = post_emoji/post_char*100,
                    
                    retweet_rate = repost/(follower+1)*10000,
                    
                    like_rate = like/(follower+1)*10000,
                    comment_rate = comment/(follower+1)*10000) %>% 
  select(year, follower_rate,tweeting_rate,profile_fullness,
         wordiness,url_fullness,label_rate,emoji_rate, 
         retweet_rate,
         like_rate, comment_rate)

sapply(weibo_hot, function(x) sum(is.na(x)))
weibo_hot %>% group_by(year) %>% summarise(freq=n())

write_csv(weibo_hot, "E:/Pyfiles/disable_data/weibo_hot.csv")
# weibo_hot %>% DT::datatable()

# 描述性统计
weibo_desc <- describe(weibo_hot)
write_csv(weibo_desc, "E:/Pyfiles/disable_data/weibo_desc.csv")


# "keyword","post","link","like","repost","comment","image_video_link","post_time","post_id",
# "post_account","account_type","follower","account_s_follower",
# "author_profile","ip_affiliation","gender","all_post",
# "tag","image_video_type","blogger_category","company","university",
# "registration_time","credit","crawl_time","terminal"

# 权重确定与综合得分计算
library(psych) 

weibo_hot<- read_csv("E:/Pyfiles/disable_data/weibo_hot.csv")
weibo_hot_scal <- sapply(weibo_hot[-1], scale)

KMO(weibo_hot_scal)
cortest.bartlett(cor(weibo_hot_scal),nrow(weibo_hot_scal))	

fa_model <- fa.parallel(weibo_hot_scal, fa= "pc", n.iter = 100)

fa_eigenValue <- tibble(factor_eigen = fa_model$fa.values,
                        component_eigen =  fa_model$pc.values,
                        simulated_eigen =  fa_model$pc.sim)

theme_bold <- function() {
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold", size = 7.8), ## hjust = 1
        axis.title.y = element_text(face = "bold", size = 7.8), 
        axis.text.x = element_text(),  
        axis.text.y = element_text())}

ggplot(fa_eigenValue, aes(x=1:10, y = component_eigen)) +
  geom_point(color = "#4DBBD5FF", size = 0.8) + 
  geom_line(color = "#3C5488FF") +
  #geom_hline(yintercept = 1, color ="#E64B35FF", linetype = "dashed") +
  theme_bw() +
  labs(x = "主成分数量", y = "主成分特征根") +
  guides(color = guide_legend(title = NULL)) +
  theme_bold() +
  scale_x_continuous(breaks = 1:10)
ggsave("fa_eigenValue.png",width = 12,height = 6, units = "cm", dpi = 600)

pca_hot <- principal(weibo_hot_scal, nfactors= ncol(weibo_hot_scal),rotate= "none", scores = TRUE)

# 特征向量（coefficient for SPSS）
# （1）方法1
eigen_vec <- sweep(pca_hot$weights, 2, sqrt(pca_hot$values), "*") %>% as_tibble()
write_csv(eigen_vec, "E:/Pyfiles/disable_data/eigen_vec.csv")
# （1）方法2
pca_hot$weights %*% diag(sqrt(pca_hot$values))
# （1）方法3
EigenVec <- function(PCA=pca_hot, component=1) {
  (PCA$loadings[,component] / sqrt(PCA$values[component])) %>% round(3)}
EigenVec()
EigenVec(component=2) 
EigenVec(component=3) 

# 主成分权重(系数)
pca_var <- pca_hot$Vaccounted[4, ][1:10]
pcaScore_weight <- pca_var/sum(pca_var)

# 综合得分
pca_hot$score %>% as_tibble %>% write_csv("E:/Pyfiles/disable_data/pca_score.csv")

pca_notYear_score <- as_tibble(pca_hot$score[,1:7]*pcaScore_weight)  %>% # 
  mutate(Z=PC1+PC2+PC3+PC4+PC5+PC6+PC7)
describe(pca_notYear_score$Z)

#画样本概率密度图
ks.test(x=jitter(pca_notYear_score$Z,0.000001),y='pnorm',alternative='two.sided')


pca_cp_score <- as_tibble(pca_hot$score*pcaScore_weight) %>% # as_tibble(pca_hot$score[,1:7]*pcaScore_weight) 
  add_column(year = weibo_hot$year, .before = 1) %>% 
  mutate(Z=PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10) %>% 
  select(year, Z) %>% 
  group_by(year) %>% 
  summarise(avgZ = mean(Z),
            tol = sum(Z))
write_csv(pca_cp_score, "E:/Pyfiles/disable_data/pca_cp_score .csv")

ggplot(pca_cp_score, aes(x=year, y = avgZ)) +
  geom_point(color = "#4DBBD5FF", size = 0.8) + 
  geom_line(color = "#3C5488FF") +
  #geom_hline(yintercept = 1, color ="#E64B35FF", linetype = "dashed") +
  theme_bw() +
  labs(x = "年份", y = "残疾人社会关注广度") +
  guides(color = guide_legend(title = NULL)) +
  theme_bold() +
  scale_x_continuous(breaks = 2017:2023)
ggsave("pca_cp_score_full.png",width = 12,height = 6, units = "cm", dpi = 600)
  
# 最终综合评价得分指数与指标的线性组合
vec_coff <- vector("list", 10)
for (i in 1:10) {
  vec_coff[i] = eigen_vec[, i]*pcaScore_weight[i]
}

x_weight <- vec_coff %>% as.data.frame() %>% as_tibble() %>% apply(1, sum)

# 尝试计算各个二级指标的年份趋势
weibo_year_nd <- dplyr::transmute(weibo_hot, year, 
                                  weibo_hot[2]*x_weight[1],
                                  weibo_hot[3]*x_weight[2],
                                  weibo_hot[4]*x_weight[3],
                                  weibo_hot[5]*x_weight[4],
                                  weibo_hot[6]*x_weight[5],
                                  weibo_hot[7]*x_weight[6],
                                  weibo_hot[8]*x_weight[7],
                                  weibo_hot[9]*x_weight[8],
                                  weibo_hot[10]*x_weight[9],
                                  weibo_hot[11]*x_weight[10])

weibo_year_nd %>% group_by(year) %>% 
  summarise(x1=mean(follower_rate),
            x2=mean(tweeting_rate),
            x3=mean(profile_fullness),
            x4=mean(wordiness),
            x5=mean(url_fullness),
            x6=mean(label_rate),
            x7=mean(emoji_rate),
            x8=mean(retweet_rate),
            x9=mean(like_rate),
            x10=mean(comment_rate)) %>% write_csv("E:/Pyfiles/disable_data/weibo_year_x.csv")

weibo_year_nd %>% group_by(year) %>% 
  summarise(y1=mean(follower_rate+tweeting_rate+profile_fullness),
            y2=mean(wordiness+url_fullness+label_rate+emoji_rate),
            y3=mean(retweet_rate),
            y4=mean(like_rate+comment_rate)) %>% write_csv("E:/Pyfiles/disable_data/weibo_year_nd.csv")



# 2.词层级（1）+ 数据可视
disab_flt <- read_csv("E:/Pyfiles/disable_data/disab_flt.csv",
                      col_types = cols(post_time = col_character(),
                                       registration_time = col_character(),
                                       crawl_time = col_character()))
sapply(disab_flt, function(x) sum(is.na(x)))
disab_flt %>% sample_n(1000) %>% DT::datatable()

# 分词
library(jiebaR)
library(lubridate)
wk <- worker()
segmentcn <- disab_flt$post %>%
  lapply(segment, jiebar = wk) 

disab_seg <- disab_flt %>% 
  add_column(id = seq(nrow(.)), .before = 1) %>%
  with(tibble(id = as.factor(rep(id , unlist(lapply(segmentcn, length)))),
              keyword = as.factor(rep(keyword , unlist(lapply(segmentcn, length)))),
              words = unlist(segmentcn))) # 76,735,693

# 文本降噪
stopword <- read_delim("./TextMining/stopword_jieba_tmcn_web.txt", delim = "\n") 

disab_seg$words <- disab_seg$words %>%
  str_replace_all("ncp|2019-ncov|ncov|covid-19|covid|covid19|sars-cov-2|新型冠状肺炎|新型肺炎|新型冠状病毒肺炎", "新冠肺炎") %>% 
  str_replace_all("sars|sars-cov|非典", "非典型肺炎") %>%
  str_replace_all("mers|mers-cov", "中东呼吸综合征") %>%
  str_replace_all("新型病毒|新型冠状病毒", "新冠病毒") %>% 
  str_replace_all("ebola|埃博拉病毒|伊波拉病毒", "埃博拉") %>%
  str_replace_all("h9n2|h7n9|h7n7|h7n3|h7n2|h7n1|h5n8|h5n6|h5n1|h3n2|h1n1", "禽流感") %>%
  str_replace_all("hpv", "人乳头状瘤病毒") %>%
  str_replace_all("PTSD|ptsd", "创伤后应激障碍") %>%
  str_replace_all("ai", "人工智能") %>%
  str_replace_all("icu|重症加强护理病房|加强监护病房综合治疗室|深切治疗部", "重症监护室") %>% 
  str_replace_all("app|application", "手机软件") %>%
  str_replace_all("ASD|ads", "孤独谱系障碍") 
# str_replace_all("[a-zA-Z0-9[:punct:][:blank:]\\s\\+\\<\\>~=\\|]+", " ")
# disab_seg %>% group_by(words) %>% summarise(freq = n()) %>% arrange(desc(freq)) %>%  View()

disab_seg$words <- disab_seg$words %>% str_remove_all("[[:punct:]0-9a-zA-Z\\s$~+^<>=|]+")

disab_tidy <- disab_seg %>%       # 43,806,069 x 3
  dplyr::filter(str_length(words) >= 2) %>% ## 过滤词长小于2
  anti_join(stopword, by = "words")         ## 过滤停用词

disab_freq <- disab_tidy %>%                  
  group_by(words) %>%
  summarise(freq = n()) %>% 
  arrange(desc(freq)) 
# quantile(disab_freq$freq, .75)
del_less <- dplyr::filter(disab_freq, freq < mean(freq)) # 过滤频数小于频数均值的词

disab_anti <- disab_tidy %>% 
  anti_join(del_less, by = "words") %>%   
  anti_join(stopword, by = "words")         # 40,323,285 x 3

write_csv(disab_anti, "E:/Pyfiles/disable_data/disab_anti.csv")

# 数据可视化
library(igraph)
disab_anti %>% group_by(keyword) %>% summarize(freq=n())

# 数据结构准备
net_themes <- disab_anti %>% 
  select(keyword, words) %>% 
  group_by(keyword, words) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  arrange(desc(freq))

net_themes$keyword <- as.character(net_themes$keyword)
net_keywords_sep <- spread(data = net_themes, 
                           key = words,   ## key需将变量值拓展为字段的变量
                           value = freq)  ## value需分散的值
net_keywords_sep[is.na(net_keywords_sep)] <- 0
cor_mat <- cor(t(as.matrix(net_keywords_sep[, -1]))) # 计算自协方差矩阵cov(x, x)=方差矩阵var(x)
cor_mat[cor_mat < .05 ] <- 0
diag(cor_mat) <- 0

# 网络要素准备
graph <- graph.adjacency(cor_mat, weighted=TRUE, mode = "lower")
graph <- delete.edges(graph, E(graph)[ weight < 0.05])

E(graph)$edge.width <- E(graph)$weight*12
V(graph)$label <- unique(net_keywords_sep$keyword) %>% as.character()
V(graph)$size <- colSums(t(as.matrix(net_keywords_sep[, -1]))) * 0.000005

# 网络图绘制
set.seed(6)
plot(graph, 
     edge.width = E(graph)$edge.width, 
     edge.color = "#E64B35FF", 
     vertex.color = "#E64B35FF", 
     label.family = "STXINGKA", # STXINGKA
     vertex.frame.color = NA,
     vertex.label.color = "black", 
     vertex.label.cex = 5, 
     lwd = 0.6)

# 2.词层级（2）+ 数据整合去重
disab_flt <- read_csv("E:/Pyfiles/disable_data/disab_flt.csv",
                      col_types = cols(post_time = col_character(),
                                       registration_time = col_character(),
                                       crawl_time = col_character())) # 744,447 x 31
disab_no_sel <- disab_flt %>% 
  select(-keyword) %>% 
  distinct() %>% 
  .[!duplicated(.$post), ] # 739,049 x 30

# 分词
library(jiebaR)
wk <- worker()
segmentcn <- disab_no_sel$post %>%
  lapply(segment, jiebar = wk) 

disab_no_seg <- disab_no_sel %>% 
  add_column(id = seq(nrow(.)), .before = 1) %>%
  with(tibble(id = as.factor(rep(id , unlist(lapply(segmentcn, length)))),
              years = as.factor(rep(post_time , unlist(lapply(segmentcn, length)))),
              words = unlist(segmentcn))) # 46,440,969 x 3

# 抽取年份
disab_no_seg$years <- str_extract(disab_no_seg$years, "^\\d{4}") %>% as.numeric()

# 文本降噪
stopword <- read_delim("./TextMining/stopword_jieba_tmcn_web.txt", delim = "\n") 
disab_no_seg$words <- disab_no_seg$words %>% # 同义词转换
  str_replace_all("ncp|2019-ncov|ncov|covid-19|covid|covid19|sars-cov-2|新型冠状肺炎|新型肺炎|新型冠状病毒肺炎", "新冠肺炎") %>% 
  str_replace_all("sars|sars-cov|非典", "非典型肺炎") %>%
  str_replace_all("mers|mers-cov", "中东呼吸综合征") %>%
  str_replace_all("新型病毒|新型冠状病毒", "新冠病毒") %>% 
  str_replace_all("ebola|埃博拉病毒|伊波拉病毒", "埃博拉") %>%
  str_replace_all("h9n2|h7n9|h7n7|h7n3|h7n2|h7n1|h5n8|h5n6|h5n1|h3n2|h1n1", "禽流感") %>%
  str_replace_all("hpv", "人乳头状瘤病毒") %>%
  str_replace_all("PTSD|ptsd", "创伤后应激障碍") %>%
  str_replace_all("ai", "人工智能") %>%
  str_replace_all("icu|重症加强护理病房|加强监护病房综合治疗室|深切治疗部", "重症监护室") %>% 
  str_replace_all("app|application", "手机软件") %>%
  str_replace_all("ASD|ads", "孤独谱系障碍") 
# disab_seg %>% group_by(words) %>% summarise(freq = n()) %>% arrange(desc(freq)) %>%  View()

disab_no_seg$words <- disab_no_seg$words %>% str_remove_all("[[:punct:]0-9a-zA-Z\\s$~+^<>=|]+")

disab_no_tidy <- disab_no_seg %>%       # 43,494,686 x 3
  dplyr::filter(str_length(words) >= 2) %>% ## 过滤词长小于2
  anti_join(stopword, by = "words")         ## 过滤停用词

disab_no_freq <- disab_no_tidy %>%                  
  group_by(words) %>%
  summarise(freq = n()) %>% 
  arrange(desc(freq)) 
# quantile(disab_freq$freq, .75)
del_no_less <- dplyr::filter(disab_no_freq, freq < mean(freq)) # 去除频数小于频数均值的词

disab_no_anti <- disab_no_tidy %>% 
  anti_join(del_no_less, by = "words") %>%   
  anti_join(stopword, by = "words") %>%         # 40,043,490 x 3
  dplyr::filter(str_length(words) >= 2)  ## 过滤词长小于2
# disab_no_anti$years <- as.numeric(disab_no_anti$years)

write_csv(disab_no_anti, "E:/Pyfiles/disable_data/disab_no_anti.csv")
#############################
# 5 格式转换
#############################
disab_no_anti <- read_csv("E:/Pyfiles/disable_data/disab_no_anti.csv")

# tbl格式转为词向量输入格式
Tbl2Word2Vec <- function(datasets = disab_no_anti, 
                         weiboyears = 2017,
                         paths = "E:/Pyfiles/disable_data/") {
  # datasets with column: id and words
  tbldata <- dplyr::filter(datasets, years == weiboyears)
  
  segcn_lst <- with(tbldata, split(words, id))
  
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
  disab_space <- tibble(id = 1:length(doc_space), doc = doc_space)

  write_csv(disab_space, paste0(paths, "disab_space", weiboyears, ".csv"))
  
  return(disab_space)
}

disab_space2017 <- Tbl2Word2Vec(weiboyears = 2017)
disab_space2018 <- Tbl2Word2Vec(weiboyears = 2018)
disab_space2019 <- Tbl2Word2Vec(weiboyears = 2019)
disab_space2020 <- Tbl2Word2Vec(weiboyears = 2020)
disab_space2021 <- Tbl2Word2Vec(weiboyears = 2021)
disab_space2022 <- Tbl2Word2Vec(weiboyears = 2022)
disab_space2023 <- Tbl2Word2Vec(weiboyears = 2023)

#############################
# 6 LDA
#############################
theme_bold <- function() {
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold", size = 7.8), ## hjust = 1
        axis.title.y = element_text(size = 7.8), 
        axis.text.x = element_text(),  
        axis.text.y = element_text())}

PlotCoh <- function(dataset = coh_score2017_df, savename = "coh_score2017_df") {
  
  yearof_weibo <- str_extract(savename, "\\d{4}")
  ylabs_name <- paste0(yearof_weibo, "年残疾人相关微博主题一致性")
  
  pRF <- ggplot(dataset, aes(ntopics, coherence)) +
    geom_point(color = "#4DBBD5FF", size = 0.8) + 
    geom_line(color = "#3C5488FF") +
    geom_vline(xintercept = which.max(dataset$coherence), 
               color = "#DC0000FF", linetype = "dashed") +
    theme_bw() +
    labs(x = "主题数量", y = ylabs_name) +
    theme_bold() 
  
  filename <- paste0(savename, ".png")
  ggsave(filename,width = 12,height = 6, units = "cm", dpi = 600)
  
  return(pRF)
}

coh_score2017_df <- read_csv('E:/Rfiles/R workstation/coh_score2017_df.csv')
coh_score2017_df %>% slice(which.max(coherence))
PlotCoh(coh_score2017_df, "coh_score2017_df")

coh_score2018_df <- read_csv('E:/Rfiles/R workstation/coh_score2018_df.csv')
coh_score2018_df %>% slice(which.max(coherence))
PlotCoh(coh_score2018_df, "coh_score2018_df")

coh_score2019_df <- read_csv('E:/Rfiles/R workstation/coh_score2019_df.csv')
coh_score2019_df %>% slice(which.max(coherence))
PlotCoh(coh_score2019_df, "coh_score2019_df")

coh_score2020_df <- read_csv('E:/Rfiles/R workstation/coh_score2020_df.csv')
coh_score2020_df %>% slice(which.max(coherence))
PlotCoh(coh_score2020_df, "coh_score2020_df")

coh_score2021_df <- read_csv('E:/Rfiles/R workstation/coh_score2021_df.csv')
coh_score2021_df %>% slice(which.max(coherence))
PlotCoh(coh_score2021_df, "coh_score2021_df")

coh_score2022_df <- read_csv('E:/Rfiles/R workstation/coh_score2022_df.csv')
coh_score2022_df %>% slice(which.max(coherence))
PlotCoh(coh_score2022_df, "coh_score2022_df")

coh_score2023_df <- read_csv('E:/Rfiles/R workstation/coh_score2023_df.csv')
coh_score2023_df %>% slice(which.max(coherence))
PlotCoh(coh_score2023_df, "coh_score2023_df")

# 情感分类结果主题分析
coh_score_pos_df <- read_csv('E:/Rfiles/R workstation/coh_score_pos_df.csv')
coh_score_pos_df[-1,] %>% slice(which.max(coherence))


# 年主题数
coh_score <- rbind(coh_score2017_df %>% slice(which.max(coherence)) %>% add_column("year" = 2017,.before = 1),
                   coh_score2018_df %>% slice(which.max(coherence)) %>% add_column("year" = 2018,.before = 1),
                   coh_score2019_df %>% slice(which.max(coherence)) %>% add_column("year" = 2019,.before = 1),
                   coh_score2020_df %>% slice(which.max(coherence)) %>% add_column("year" = 2020,.before = 1),
                   coh_score2021_df %>% slice(which.max(coherence)) %>% add_column("year" = 2021,.before = 1),
                   coh_score2022_df %>% slice(which.max(coherence)) %>% add_column("year" = 2022,.before = 1),
                   coh_score2023_df %>% slice(which.max(coherence)) %>% add_column("year" = 2023,.before = 1))

Pcoh <- ggplot(coh_score, aes(year, ntopics)) +
  geom_point(color = "#4DBBD5FF", size = 0.8) + 
  geom_line(color = "#3C5488FF") +
  theme_bw() +
  scale_x_continuous(breaks = 2017:2024) +
  labs(x = "年份", y = "残疾人社会关注深度") +
  theme_bold() 
Pcoh
ggsave("Pcoh.png", width = 12,height = 6, units = "cm", dpi = 600)


# 各年份主题与主题-词网络
library(visNetwork)
library(ggsci)  # ggsci提取16进制颜色
library(scales) # show_col

comb_df2017 <- read_csv('E:/Pyfiles/disable_data/comb_df2017.csv')
df2017_graph <- comb_df2017[1:10, ]
names(df2017_graph) <- c("残疾关怀", "健康护理", "伤残赔偿与残疾人权益", "康复与就业服务","社区关爱与志愿者服务","无障碍设施与服务")

comb_df2018 <- read_csv('E:/Pyfiles/disable_data/comb_df2018.csv')
df2018_graph <- comb_df2018[1:10, ]
names(df2018_graph) <- c("伤残康复与赔偿", "残疾人服务与社会融合")

comb_df2019 <- read_csv('E:/Pyfiles/disable_data/comb_df2019.csv')
df2019_graph <- comb_df2019[1:10, ]
names(df2019_graph) <- c("残疾人社区服务与支持", "截肢康复与家庭关爱", "瘫痪孩子的希望", "残疾人康复与医疗服务","精神健康与社会支持", 
                         "残障儿童公益服务与社会关爱", "伤残赔偿与法律维权", "医疗治疗与身体健康", "事故伤害与医疗治疗","残疾人服务与社会保障",
                         "无障碍城市建设与管理", "工伤赔偿与保障措施", "神经疾病治疗与研究", "意外保险与医疗保障", "糖尿病治疗与健康管理", "健康科技发展与无障碍技术")

comb_df2020 <- read_csv('E:/Pyfiles/disable_data/comb_df2020.csv')
df2020_graph <- comb_df2020[1:10, ]
names(df2020_graph) <- c("疾病治疗与健康管理", "残疾人社区服务与建设")

comb_df2021 <- read_csv('E:/Pyfiles/disable_data/comb_df2021.csv')
df2021_graph <- comb_df2021[1:10, ]
names(df2021_graph) <- c("伤残赔偿与法律支持","神经功能障碍研究与治疗","残疾人服务与发展保障")

comb_df2022 <- read_csv('E:/Pyfiles/disable_data/comb_df2022.csv')
df2022_graph <- comb_df2022[1:10, ]
names(df2022_graph) <- c("身心健康与功能障碍", "社区健康服务与疫情防控", "工伤赔偿与保障制度", "残疾人生活与关爱", "无障碍社会建设与公益事业","疾病治疗与健康影响")

comb_df2023 <- read_csv('E:/Pyfiles/disable_data/comb_df2023.csv')
df2023_graph <- comb_df2023[1:10, ]
names(df2023_graph) <- c("伤残赔偿与社会支持","疾病治疗与康复研究")

# color
show_col(pal_npg("nrc")(10))
show_col(pal_npg("nrc", alpha = 0.6)(10))
npg <- pal_npg("nrc")(10)
aaas <- pal_aaas(alpha = 1)(10)
scales::show_col(npg)

PlotNet <- function(netdata,   # tbl: ntheme X nword
                    netcenter = 2, # which themes use as the center
                    nword = 9, # nrow(netdata)+1
                    ntheme = 1, # ncol(netdata)
                    font_size = 16,
                    netgroup = npg[7])
                    # 若要按颜色区分主题,需要算nword（公共词数量），如netgroup = c(npg[1:6], rep(npg[1:6], each = 10), aaas[1])
{
  # net graph
  
  base_chain <- tibble(from = rep(names(netdata), each = nword),
                       to = netdata %>% as.matrix() %>% as.vector())
  
  nodes <- tibble(id = 1:length(unique(c(base_chain$from, base_chain$to))),
                  label = unique(c(base_chain$from, base_chain$to)),
                  font.size = font_size,
                  font.color = "black",
                  group = netgroup,  
                  shadow = F,
                  shape = c(rep("ellipse", ntheme), rep("text", length(id)-ntheme)),
                  color = netgroup)
  edges <- base_chain %>% 
    left_join(y = nodes %>% select(label, id), by = c("from" = "label")) %>% 
    mutate(from = id) %>% 
    select(-id) %>% 
    left_join(y = nodes %>% select(label, id), by = c("to" = "label")) %>% 
    mutate(to = id) %>% 
    select(-id) %>%
    mutate(smooth = T, shadow = F) %>% 
    bind_rows(tibble(from = rep(netcenter, ntheme),
                     to = rep(ntheme:1, 1),
                     smooth = T,
                     shadow = T,
                     color = rep("#FFFFFF00", ntheme)))
  visNetwork(nodes = nodes, edges = edges) %>% 
    visNodes(value = 0.05) %>% 
    visEdges(arrows = "to", length = 1) %>%
    visOptions(highlightNearest = TRUE)
}

PlotNet(netdata = df2017_graph,
        netcenter = 1,
        nword = 10, 
        ntheme = 6,
        netgroup = npg[1])

PlotNet(netdata = df2018_graph,
        netcenter = 1,
        nword = 10, 
        ntheme = 2,
        netgroup = npg[2])

PlotNet(netdata = df2019_graph,
        netcenter = 4,
        nword = 10, 
        ntheme = 16,
        netgroup = npg[5])

PlotNet(netdata = df2020_graph,
        netcenter = 1,
        nword = 10, 
        ntheme = 2,
        netgroup = npg[3])

PlotNet(netdata = df2021_graph,
        netcenter = 1,
        nword = 10, 
        ntheme = 3,
        netgroup = npg[6])

PlotNet(netdata = df2022_graph,
        netcenter = 1,
        nword = 10, 
        ntheme = 6,
        netgroup = npg[7])

PlotNet(netdata = df2023_graph,
        netcenter = 1,
        nword = 10, 
        ntheme = 2,
        netgroup = npg[10])

##############################
# 7 微博句子拆分
##############################
disab_flt <- read_csv("E:/Pyfiles/disable_data/disab_flt.csv",
                      col_types = cols(post_time = col_character(),
                                       registration_time = col_character(),
                                       crawl_time = col_character())) # 744,447 x 31

# sentence split
SenSplit <- function(x) str_split(x, "[。？！]")
SenSplitLen <- function(x) lapply(str_split(x, "[。？！]"), length)

disab_sen <- disab_flt %>% 
  select(2) %>% 
  add_column(weiboID = seq(nrow(.)), .before = 1) %>% # 新增id列
  with(tibble(weiboID = as.factor(rep(weiboID, unlist(SenSplitLen(post)))),
              sentenceID = unlist(apply(matrix(unlist(SenSplitLen(post))), 1, seq)), 
              sentence = unlist(SenSplit(post))))

##############################
# 8 句子清洗、分词、降噪和格式转换
##############################
# （1）清洗
# 预处理：编码不一致不能直接分词，有的只有符号，有的只有数字
disab_sen$sentence <- str_replace_all(disab_sen$sentence, "[[:punct:]0-9a-zA-Z\\s$~+^<>=|]+", " ")
disab_senClean <- dplyr::filter(disab_sen, str_length(sentence) > 5) %>% distinct
# # 确定清洗字符数目标准：当字符数为4时，对应句子数量开始趋于平缓
# ntest <- vector("list")
# for (i in 1:10) {
#   ntest[[i]] <- dplyr::filter(disab_sen, str_length(post_sen) < i) %>% nrow
# }
# tbltest <- tibble(id = 1:10, n =  ntest %>% unlist)
# ggplot(tbltest, aes(x = n, y = id)) + 
#   geom_point() +
#   geom_line()

# （2）分词
library(jiebaR)
wk <- worker()
segmentcn <- disab_senClean$sentence %>%
  lapply(segment, jiebar = wk) 

disab_add <- disab_senClean %>% 
  add_column(id = 1:nrow(disab_senClean), .before = 1)

disab_weiboSeg <- with(disab_add, tibble(
  id = as.integer(rep(id, unlist(lapply(segmentcn, length)))),
  weiboID = as.factor(rep(weiboID, unlist(lapply(segmentcn, length)))),
  sentenceID = as.factor(rep(sentenceID, unlist(lapply(segmentcn, length)))),
  words = unlist(segmentcn)))

# （3）文本降噪
stopword <- read_delim("./TextMining/stopword_jieba_tmcn_web.txt", delim = "\n")

weiboSeg_no_short <- disab_weiboSeg %>%       # 43,494,686 x 3
  dplyr::filter(str_length(words) >= 2) %>%    ## 过滤词长小于2
  anti_join(stopword, by = "words")         ## 过滤停用词

weiboSeg_no_freq <- weiboSeg_no_short %>%                  
  group_by(words) %>%
  summarise(freq = n()) %>% 
  arrange(desc(freq)) 
# quantile(disab_freq$freq, .75)
del_no_less <- dplyr::filter(weiboSeg_no_freq, freq < mean(freq)) # 去除频数小于频数均值的词

weiboSeg_anti <- weiboSeg_no_short %>% 
  anti_join(del_no_less, by = "words") %>%   
  anti_join(stopword, by = "words") %>%
  dplyr::filter(str_length(words) >= 2) 

write_csv(weiboSeg_anti, paste0(paths, "weiboSeg_anti.csv"))

# （4）转换W2V格式，表格链接
disab_W2Vspace <- Tbl2Word2Vec(weiboSeg_anti, paths)

# 以防weiboID和sentenceID丢失，采用原始分词id生成进行表连接
weiboSeg_sel <- select(disab_add, id, weiboID, sentenceID)

disab_W2Vspace_tbl <- left_join(disab_W2Vspace, weiboSeg_sel, by = "id") %>% 
  distinct()

disab_W2Vspace_tbl

# 检查weiboID和sentenceID是否丢失
disab_W2Vspace_tbl$weiboID %>% is.na() %>% sum()
disab_W2Vspace_tbl$sentenceID %>% is.na() %>% sum()
# 检查是否有无效字段
disab_W2Vspace_tbl %>% dplyr::filter(str_length(doc) < 4)

write_csv(disab_W2Vspace_tbl, paste0(paths, "disab_W2Vspace_tbl.csv"))
write_csv(disab_W2Vspace_tbl["doc"], paste0(paths, "disab_W2Vspace_doc.csv"), col_names = F)

##############################
# 9 观点句抽取与情感分类
##############################

disab_W2Vspace_tbl <- read_csv("E:/Pyfiles/2014sentiment/disab_W2Vspace_tbl.csv") # 3,552,318 x 4
disab_y_pred_df <- read_csv("E:/Pyfiles/2014sentiment/disab_y_pred_df.csv")

disab_op <- disab_W2Vspace_tbl %>%  # 1,701,901 x 5
  add_column(disab_y_pred_df) %>% 
  dplyr::filter(opinionated == 1)
write_csv(disab_op, "E:/Pyfiles/2014sentiment/disab_op.csv")
write_csv(disab_op["doc"], "E:/Pyfiles/2014sentiment/disab_op_doc.csv", col_names = F)


# 情感分类结果
disab_op <- read_csv("E:/Pyfiles/2014sentiment/disab_op.csv") # 3,552,318 x 4
disab_emo1_pred_df <- read_csv("E:/Pyfiles/2014sentiment/disab_emo1_pred_df.csv")

disab_emo <- disab_op %>%  # 1,701,901 x 5
  add_column(disab_emo1_pred_df)
write_csv(disab_emo, "E:/Pyfiles/2014sentiment/disab_emo.csv")

# 关联微weiboID和年份(post_time)
disab_year_weiboID <- disab_flt %>% 
  add_column(weiboID = 1:nrow(.), .before = 1) %>% 
  mutate(year = str_extract(disab_flt$post_time, "^\\d{4}")) %>% 
  select(weiboID, year)
# str_extract(disab_flt$post_time, "^\\d{4}") %>% is.na() %>% sum() # 有无缺失年份

disab_emo_year <- left_join(disab_emo, disab_year_weiboID, by="weiboID")

nrow(disab_emo_year)
disab_emo_year$weiboID %>% length

# 按年份统计正向和负向情感

emo_year_prob <- disab_emo_year %>% group_by(year) %>% 
  summarise(tol=n()) %>% 
  ungroup() 

emo_year <- disab_emo_year %>% group_by(year, emo1) %>% 
  summarise(freq=n()) %>% 
  ungroup() %>% 
  mutate(tol = rep(emo_year_prob$tol, each = 2),
         prob = freq/tol)  

# emo_year$freq <- emo_year$freq/sum(emo_year$freq)
emo_year$year <- as.numeric(emo_year$year)

# write_csv(disab_emo_year, "E:/Pyfiles/2014sentiment/disab_emo_year.csv")
disab_emo_year <- read_csv("E:/Pyfiles/2014sentiment/disab_emo_year.csv")
disab_emo_year %>% group_by(year) %>% summarise(freq=n())

emo_pos <- emo_year[emo_year$emo1 == 1, ]
write_csv(emo_pos, "E:/Pyfiles/2014sentiment/emo_pos.csv")
#emo_pos <- read_csv("E:/Pyfiles/2014sentiment/emo_pos.csv")

p_emo_pos <- ggplot(emo_pos, aes(x=year, y=prob)) +
  geom_point(color = "#4DBBD5FF", size = 0.8) + 
  geom_line(color = "#3C5488FF") +
  theme_bw() +
  scale_x_continuous(breaks = 2017:2024) +
  labs(x = "年份", y = "残疾人社会反馈包容程度") +
  theme_bold() 
p_emo_pos 
ggsave("emo_pos.png", width = 12,height = 6, units = "cm", dpi = 600)

emo_neg <- emo_year[emo_year$emo1 == 0, ]
write_csv(emo_neg, "E:/Pyfiles/2014sentiment/emo_neg.csv")
# emo_neg <- read_csv("E:/Pyfiles/2014sentiment/emo_neg.csv")

p_emo_neg <- ggplot(emo_neg, aes(year, prob)) +
  geom_point(color = "#4DBBD5FF", size = 0.8) + 
  geom_line(color = "#3C5488FF") +
  theme_bw() +
  scale_x_continuous(breaks = 2017:2024) +
  labs(x = "年份", y = "残疾人社会反馈排斥程度") +
  theme_bold() 
p_emo_neg 
ggsave("emo_neg.png", width = 12,height = 6, units = "cm", dpi = 600)

emo_year_dire <- tibble(year = 2017:2023,
                        diretion = emo_pos$prob/emo_neg$prob)
p_emo_posneg <- ggplot(emo_year_dire, aes(year, diretion)) +
  geom_point(color = "#4DBBD5FF", size = 0.8) + 
  geom_line(color = "#3C5488FF") +
  theme_bw() +
  scale_x_continuous(breaks = 2017:2024) +
  labs(x = "年份", y = "残疾人社会反馈状况") +
  theme_bold() 
p_emo_posneg
ggsave("emo_posneg.png", width = 12,height = 6, units = "cm", dpi = 600)


# 正负向情感主题分析（见上面# 情感分类结果主题分析）

lda_pos <- disab_emo_year[disab_emo_year$emo1 == 1, ]
write_csv(lda_pos, "E:/Pyfiles/2014sentiment/lda_pos.csv")
lda_neg <- disab_emo_year[disab_emo_year$emo1 == 0, ]
write_csv(lda_neg, "E:/Pyfiles/2014sentiment/lda_neg.csv")

lda_pos <- read_csv("E:/Pyfiles/2014sentiment/lda_pos.csv")
comb_pos <- select(lda_pos, weiboID, sentenceID, year, doc)
# length(unique(comb_pos$weiboID))
# library(reshape2)
# test <- comb_pos[1:10, ]
# dup_bool <- duplicated(test$weiboID)
# dup_index <- which(dup_bool)
# weibo <- test$doc[!dup_bool]
aggr_pos <- aggregate(x = comb_pos$doc, 
                      by = list(weiboID = comb_pos$weiboID, year = comb_pos$year), paste) %>% as_tibble()
weibo_pos <- apply(aggr_pos["x"], 1, function(x) paste(unlist(x), collapse = " "))
comb_pos_weibo <- tibble(weiboID = aggr_pos$weiboID,
                         year = aggr_pos$year,
                         weibo = weibo_pos)
write_csv(comb_pos_weibo, "E:/Pyfiles/2014sentiment/comb_pos_weibo.csv")
# 分词
# comb_pos_weibo <- read_csv("E:/Pyfiles/2014sentiment/comb_pos_weibo.csv")
library(jiebaR)
wk <- worker()
segmentcn <- comb_pos_weibo$weibo %>%
  lapply(segment, jiebar = wk) 
seg_pos <- with(comb_pos_weibo, 
                tibble(weiboID = as.factor(rep(weiboID, unlist(lapply(segmentcn, length)))),
                       year = as.factor(rep(year, unlist(lapply(segmentcn, length)))),
                       words = unlist(segmentcn)))
# wordcloud
wc_data <- seg_pos %>% 
  group_by(words) %>%
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>%
  top_n(10000) %>% 
  ungroup()
wc_data %>% DT::datatable()

library(wordcloud2)
wordcloud2(wc_data, size = 1.2, fontFamily = "Times New Roman",
           minRotation = 0, maxRotation = 0, rotateRatio = 1)  

# 负向向微博句子合并
lda_neg <- read_csv("E:/Pyfiles/2014sentiment/lda_neg.csv")
comb_neg <- select(lda_neg, weiboID, sentenceID, year, doc)
aggr_neg <- aggregate(x = comb_neg$doc, 
                      by = list(weiboID = comb_neg$weiboID, year = comb_neg$year), paste) %>% as_tibble()
weibo_neg <- apply(aggr_neg["x"], 1, function(x) paste(unlist(x), collapse = " "))
comb_neg_weibo <- tibble(weiboID = aggr_neg$weiboID,
                         year = aggr_neg$year,
                         weibo = weibo_neg)
write_csv(comb_neg_weibo, "E:/Pyfiles/2014sentiment/comb_neg_weibo.csv")
# 分词
# comb_neg_weibo <- read_csv("E:/Pyfiles/2014sentiment/comb_neg_weibo.csv")
library(jiebaR)
wk <- worker()
segmentcn <- comb_neg_weibo$weibo %>%
  lapply(segment, jiebar = wk) 
seg_neg <- with(comb_neg_weibo, 
                tibble(weiboID = as.factor(rep(weiboID, unlist(lapply(segmentcn, length)))),
                       year = as.factor(rep(year, unlist(lapply(segmentcn, length)))),
                       words = unlist(segmentcn)))

# wordcloud
wc_neg_data <- seg_neg %>% 
  group_by(words) %>%
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>%
  top_n(10000) %>% 
  ungroup()
wc_neg_data %>% DT::datatable()

library(wordcloud2)
wordcloud2(wc_neg_data, size = 0.8, fontFamily = "Times New Roman",
           minRotation = 0, maxRotation = 0, rotateRatio = 1)  





















