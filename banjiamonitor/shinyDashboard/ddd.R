library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(shiny)

rm(list = ls())

today = as.Date(with_tz(Sys.Date(), tz="Asia/Taipei"), tz="Asia/Taipei")

weibo = reactiveFileReader(1000 * 60 * 10, NULL, 
                                "https://raw.githubusercontent.com/jiyuxiongdi/miscellaneous/master/banjiamonitor/weibo.csv", fread)
#weibo.data = as.data.frame(fread("https://raw.githubusercontent.com/jiyuxiongdi/miscellaneous/master/banjiamonitor/weibo.csv",
#                                 colClasses = list(character=c("user",'crawler_time','post_time','link','child_comment_comment_id','child_comment_comment_time'))))
data_list = reactive({
  weibo.data = weibo() %>% 
    as.data.frame() %>% 
    mutate(user = as.character(user),
           crawler_time = as.character(crawler_time),
           post_time = as.character(post_time),
           link = as.character(link),
           child_comment_comment_id = as.character(child_comment_comment_id),
           child_comment_comment_time = as.character(child_comment_comment_time)) %>% 
    mutate(crawler_time = ymd_hms(crawler_time, tz="Asia/Taipei"),
           post_time = ymd_hms(post_time, tz="Asia/Taipei"),
           child_comment_comment_time = ymd_hm(child_comment_comment_time, tz="Asia/Taipei"),
           like = as.integer(like),
           comment = as.integer(comment),
           forward = as.integer(forward),
           child_comment_like = as.integer(child_comment_like),
           child_comment_reply = as.integer(child_comment_reply),
           user = gsub("[^A-Z]", "", user))
  
  daily_weibo = full_join(
    data.frame(date = seq(max(today-days(30), as.Date(ymd("2020-07-22",tz="Asia/Taipei"),tz="Asia/Taipei")), today, by='day')),
    weibo.data %>% distinct(post_time, link, user),
    by = character()
  ) %>% filter(as.Date(post_time) >= (date - days(30)) & as.Date(post_time) < date)
  
  daily_comment = full_join(
    data.frame(date = seq(max(today-days(30), as.Date(ymd("2020-07-22", tz="Asia/Taipei"), tz="Asia/Taipei")), today, by='day')),
    weibo.data %>% distinct(child_comment_comment_time, child_comment_comment_id, user) %>% filter(child_comment_comment_id!=""),
    by = character()
  ) %>% filter(as.Date(child_comment_comment_time, tz="Asia/Taipei") >= (date - days(30)) & as.Date(child_comment_comment_time, tz="Asia/Taipei") < date)
  
  # total available scores per day
  avail_score_weibo = daily_weibo %>% 
    group_by(date, user) %>%
    count(name="weibo") %>% 
    ungroup()
  
  avail_score_comment = daily_comment %>% 
    group_by(date, user) %>%
    count(name="comment") %>% 
    ungroup()
  
  avail_score = full_join(avail_score_weibo, avail_score_comment, by = c("date", "user")) %>% 
    mutate_at(c(3:4), ~replace(., is.na(.), 0)) %>% 
    rename(w = weibo, c = comment) %>% 
    mutate(s = w*3 + c * 2) %>% 
    pivot_wider(names_from = user, values_from=c(w, c, s)) %>% 
    mutate(date = as.character(date)) %>% 
    as.data.frame() %>% 
    mutate_at(c(2:7), as.integer)
  
  # data for plotting daily increase
  daily_inc_weibo = inner_join(
    daily_weibo %>% select(-post_time),
    weibo.data %>% 
      filter_at(c("like", "comment", "forward"), all_vars(!is.na(.))) %>% 
      distinct(user, crawler_time, post_time, link, like, comment, forward) %>% 
      mutate(date = as.Date(crawler_time, tz="Asia/Taipei")) %>% 
      group_by(user, link, date) %>% 
      arrange(link, crawler_time) %>% 
      top_n(1, crawler_time) %>% 
      group_by(user, link) %>%
      mutate_at(c("like", "comment", "forward"), 
                ~case_when(as.Date(post_time, tz="Asia/Taipei") == date ~ .,
                           TRUE ~ . - lag(.))) %>% 
      ungroup() %>% 
      na.omit() %>% 
      select(user, date, link, like, comment, forward),
    by = c("user", "date", "link"))
  
  daily_inc_comment = inner_join(
    daily_comment %>% select(-child_comment_comment_time),
    weibo.data %>% 
      filter_at("child_comment_like", all_vars(!is.na(.))) %>% 
      distinct(user, crawler_time, child_comment_comment_id, child_comment_comment_time, child_comment_like, child_comment_reply) %>% 
      filter(child_comment_comment_id != "") %>% 
      mutate(date = as.Date(crawler_time, tz="Asia/Taipei"),
             child_comment_reply = ifelse(is.na(child_comment_reply), 0, child_comment_reply)) %>% 
      group_by(user, child_comment_comment_id, date) %>% 
      top_n(1, crawler_time) %>% 
      group_by(user, child_comment_comment_id) %>% 
      mutate_at(c("child_comment_like", "child_comment_reply"), 
                ~case_when(as.Date(child_comment_comment_time, tz="Asia/Taipei") == date ~ .,
                           TRUE ~ . - lag(.))) %>% 
      ungroup() %>% 
      na.omit() %>% 
      select(user, date, child_comment_comment_id, child_comment_like, child_comment_reply),
    by = c("user", "date", "child_comment_comment_id"))
  
  daily_inc = full_join(
    inner_join(daily_weibo %>% select(-post_time), daily_inc_weibo, by = c("date", "user", "link")) %>% group_by(user, date) %>% summarise_at(c("like", "comment", "forward"), ~sum(.)) %>% ungroup(),
    inner_join(daily_comment %>% select(-child_comment_comment_time), daily_inc_comment, by = c("date", "user", "child_comment_comment_id")) %>% group_by(user, date) %>% summarise_at(c("child_comment_like", "child_comment_reply"), ~sum(.)) %>% ungroup(),
    by = c('date', 'user')
  ) %>% 
    mutate_at(c("like","comment","forward","child_comment_like","child_comment_reply"), ~replace(., is.na(.), 0)) %>% 
    mutate(score = like + comment + forward + child_comment_like + child_comment_reply) %>% 
    group_by(date, user) %>% 
    summarise(score = sum(score)) %>% 
    ungroup() %>% 
    spread(user, score) %>% 
    as.data.frame()
  
  
  # data for plotting hourly increase
  hourly_inc_weibo = inner_join(
    daily_weibo %>% select(-post_time),
    weibo.data %>% 
      filter_at(c("like", "comment", "forward"), all_vars(!is.na(.))) %>% 
      distinct(user, crawler_time, post_time, link, like, comment, forward) %>% 
      mutate(date = as.Date(crawler_time, tz="Asia/Taipei"),
             hour = substr(as.character(crawler_time), 1, 13)) %>% 
      filter(date >= (today - days(7))) %>%
      arrange(user, link, crawler_time) %>% 
      group_by(user, hour, link) %>% 
      top_n(1, crawler_time) %>% 
      arrange(user, link, crawler_time) %>% 
      group_by(user, link) %>% 
      mutate_at(c("like", "comment", "forward"), 
                ~ . - dplyr::lag(.)) %>% 
      ungroup() %>% 
      na.omit() %>% 
      select(user, date, hour, link, like, comment, forward),
    by = c("user", "date", "link"))
  
  hourly_inc_comment = inner_join(
    daily_comment %>% select(-child_comment_comment_time),
    weibo.data %>% 
      filter_at("child_comment_like", all_vars(!is.na(.))) %>% 
      distinct(user, crawler_time, child_comment_comment_id, child_comment_comment_time, child_comment_like, child_comment_reply) %>% 
      filter(child_comment_comment_id != "") %>% 
      mutate(date = as.Date(crawler_time, tz="Asia/Taipei"),
             hour = substr(as.character(crawler_time), 1, 13),
             child_comment_reply = ifelse(is.na(child_comment_reply), 0, child_comment_reply)) %>% 
      filter(date >= (today - days(7))) %>% 
      group_by(user, child_comment_comment_id, hour) %>% 
      top_n(1, crawler_time) %>% 
      arrange(user, child_comment_comment_id, crawler_time) %>% 
      group_by(user, child_comment_comment_id) %>%
      mutate_at(c("child_comment_like", "child_comment_reply"), 
                ~case_when(as.Date(child_comment_comment_time, tz="Asia/Taipei") == date ~ .,
                           TRUE ~ . - lag(.))) %>% 
      ungroup() %>% 
      na.omit() %>% 
      select(user, date, hour, child_comment_comment_id, child_comment_like, child_comment_reply),
    by = c("user", "date", "child_comment_comment_id"))
  
  hourly_inc = full_join(
    inner_join(daily_weibo %>% select(-post_time), hourly_inc_weibo, by = c("date", "user", "link")) %>% group_by(user, hour) %>% summarise_at(c("like", "comment", "forward"), ~sum(.)) %>% ungroup(),
    inner_join(daily_comment %>% select(-child_comment_comment_time), hourly_inc_comment, by = c("date", "user", "child_comment_comment_id")) %>% group_by(user, hour) %>% summarise_at(c("child_comment_like", "child_comment_reply"), ~sum(.)) %>% ungroup(),
    by = c('user', 'hour')
  ) %>% 
    mutate_at(c("like","comment","forward","child_comment_like","child_comment_reply"), ~replace(., is.na(.), 0)) %>% 
    mutate(score = like + comment + forward + child_comment_like + child_comment_reply) %>% 
    as.data.frame() %>% 
    arrange(user, hour) %>% 
    group_by(user, hour) %>% 
    summarise(score = sum(score)) %>% 
    ungroup() %>% 
    mutate(hour = ymd_h(hour, tz="Asia/Taipei")) %>% 
    spread(user, score)
  
  
  # data for single weibo
  avail_user = weibo.data %>% distinct(user) %>% pull()
  
  return(list(
    avail_score = avail_score,
    daily_inc = daily_inc,
    hourly_inc = hourly_inc,
    avail_user = avail_user,
    weibo.data = weibo.data))
})

weibo_zzp = function(user_input="ZKN", post_time_input="2020-07-19 13:16:00") {
  dat = data_list()$weibo.data %>% 
    filter(user == user_input) %>% 
    filter(as.character(post_time) == post_time_input) %>% 
    #filter_at(c("like", "comment", "forward"), all_vars(!is.na(.))) %>% 
    distinct(crawler_time, post_time, like, comment, forward) %>% 
    mutate(date = as.Date(crawler_time, tz="Asia/Taipei")) %>% 
    filter(date >= (today - days(7))) %>% 
    arrange(crawler_time, like, comment, forward) %>% 
    na.omit() %>% 
    select(crawler_time, like, comment, forward) 
}

weibo_zzp_inc = function(user_input="ZKN", post_time_input="2020-07-19 13:16:00") {
  dat = data_list()$weibo.data %>% 
    filter(user == user_input) %>% 
    filter(as.character(post_time) == post_time_input) %>% 
    filter_at(c("like", "comment", "forward"), all_vars(!is.na(.))) %>% 
    distinct(crawler_time, post_time, like, comment, forward) %>% 
    mutate(date = as.Date(crawler_time, tz="Asia/Taipei")) %>% 
    filter(date >= (today - days(7))) %>% 
    arrange(crawler_time, like, comment, forward) %>% 
    mutate_at(c("like", "comment", "forward"),
              ~ . - lag(.)) %>% 
    mutate(crawler_time_new = lag(crawler_time)) %>% 
    mutate(crawler_time_new = as.numeric(difftime(crawler_time, crawler_time_new, units = 'secs'))/60) %>% 
    mutate_at(c("like", "comment", "forward"),
              ~ round(. / crawler_time_new, 2)) %>% 
    na.omit() %>% 
    select(crawler_time, like, comment, forward) 
}

comment1_zp = function(user_input="ZKN", post_time_input="2020-07-19 13:16:00") {
  data_list()$weibo.data %>% 
    filter(user == user_input) %>% 
    filter(as.character(post_time) == post_time_input) %>%
    filter(child_comment_level == 1) %>% 
    filter(child_comment_comment_id != "") %>% 
    filter_at(c("child_comment_like", "child_comment_reply"), all_vars(!is.na(.))) %>% 
    distinct(child_comment_comment_id, crawler_time, child_comment_like, child_comment_reply) %>% 
    mutate(date = as.Date(crawler_time, tz="Asia/Taipei")) %>% 
    filter(date >= (today - days(7))) %>% 
    arrange(child_comment_comment_id, crawler_time, child_comment_like, child_comment_reply) %>% 
    group_by(child_comment_comment_id) %>% 
    na.omit() %>% 
    mutate(comment = cur_group_id()) %>% 
    ungroup() %>% 
    select(crawler_time, comment, child_comment_like, child_comment_reply) %>% 
    pivot_longer(cols = c("child_comment_like", "child_comment_reply"),
                 names_to = "action", values_to = "value") %>% 
    mutate(action = gsub("child_comment_","", action),
           comment = as.factor(paste("comment",comment,sep=""))) %>% 
    select(crawler_time, comment, action, value) %>% 
    as.data.frame()
}

comment1_zp_inc = function(user_input="ZKN", post_time_input="2020-07-19 13:16:00") {
  data_list()$weibo.data %>% 
    filter(user == user_input) %>% 
    filter(as.character(post_time) == post_time_input) %>%
    filter(child_comment_level == 1) %>% 
    filter(child_comment_comment_id != "") %>% 
    filter_at(c("child_comment_like", "child_comment_reply"), all_vars(!is.na(.))) %>% 
    distinct(child_comment_comment_id, crawler_time, child_comment_like, child_comment_reply) %>% 
    mutate(date = as.Date(crawler_time, tz="Asia/Taipei")) %>% 
    filter(date >= (today - days(7))) %>% 
    arrange(child_comment_comment_id, crawler_time, child_comment_like, child_comment_reply) %>% 
    group_by(child_comment_comment_id) %>% 
    mutate_at(c("child_comment_like", "child_comment_reply"),
              ~ . - lag(.)) %>% 
    mutate(crawler_time_new = lag(crawler_time)) %>% 
    mutate(crawler_time_new = as.numeric(difftime(crawler_time, crawler_time_new, units = 'secs'))/60) %>% 
    mutate_at(c("child_comment_like", "child_comment_reply"),
              ~ round(. / crawler_time_new, 2)) %>% 
    na.omit() %>% 
    mutate(comment = cur_group_id()) %>% 
    ungroup() %>% 
    select(crawler_time, comment, child_comment_like, child_comment_reply) %>% 
    pivot_longer(cols = c("child_comment_like", "child_comment_reply"),
                 names_to = "action", values_to = "increase") %>% 
    mutate(action = gsub("child_comment_","", action),
           comment = as.factor(paste("comment",comment,sep=""))) %>% 
    select(crawler_time, comment, action, increase) %>% 
    as.data.frame()
}

comment2_z = function(user_input="ZKN", post_time_input="2020-07-19 13:16:00") {
  data_list()$weibo.data %>% 
    filter(user == user_input) %>% 
    filter(as.character(post_time) == post_time_input) %>%
    filter(child_comment_level == 2) %>% 
    filter(child_comment_comment_id != "") %>% 
    filter_at(c("child_comment_like"), all_vars(!is.na(.))) %>% 
    distinct(child_comment_comment_id, crawler_time, child_comment_like) %>% 
    mutate(date = as.Date(crawler_time, tz="Asia/Taipei")) %>% 
    filter(date >= (today - days(7))) %>% 
    arrange(child_comment_comment_id, crawler_time, child_comment_like) %>% 
    group_by(child_comment_comment_id) %>% 
    na.omit() %>% 
    mutate(comment = cur_group_id()) %>% 
    ungroup() %>% 
    select(crawler_time, comment, child_comment_like) %>% 
    mutate(action = "like", value = child_comment_like) %>% 
    mutate(comment = as.factor(paste("comment",comment,sep=""))) %>% 
    select(crawler_time, comment, action, value) %>% 
    as.data.frame()
}

comment2_z_inc = function(user_input="ZKN", post_time_input="2020-07-19 13:16:00") {
  data_list()$weibo.data %>% 
    filter(user == user_input) %>% 
    filter(as.character(post_time) == post_time_input) %>%
    filter(child_comment_level == 2) %>% 
    filter(child_comment_comment_id != "") %>% 
    filter_at(c("child_comment_like"), all_vars(!is.na(.))) %>% 
    distinct(child_comment_comment_id, crawler_time, child_comment_like) %>% 
    mutate(date = as.Date(crawler_time, tz="Asia/Taipei")) %>% 
    filter(date >= (today - days(7))) %>% 
    arrange(child_comment_comment_id, crawler_time, child_comment_like) %>% 
    group_by(child_comment_comment_id) %>% 
    mutate_at(c("child_comment_like"),
              ~ . - lag(.)) %>% 
    mutate(crawler_time_new = lag(crawler_time)) %>% 
    mutate(crawler_time_new = as.numeric(difftime(crawler_time, crawler_time_new, units = 'secs'))/60) %>% 
    mutate_at(c("child_comment_like"),
              ~ round(. / crawler_time_new, 2)) %>% 
    na.omit() %>% 
    mutate(comment = cur_group_id()) %>% 
    ungroup() %>% 
    select(crawler_time, comment, child_comment_like) %>% 
    mutate(action = "like", increase = child_comment_like) %>% 
    mutate(comment = as.factor(paste("comment",comment,sep=""))) %>% 
    select(crawler_time, comment, action, increase) %>% 
    as.data.frame()
}