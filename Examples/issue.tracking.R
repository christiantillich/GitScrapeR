################################### HEADER ###################################

lib <- paste0(path.projects,"/Data_Sci/Github_Scraper")
setwd(lib)
source('show.github.path.R')
source('issues.db.R')

################################ END HEADER ##################################

require(magrittr)

qplot(
   as.Date(created_at)
  ,fill = state
  ,data = issues
) + theme_BA




issues.opened <- issues %>% 
  group_by(date = as.Date(created_at)) %>%
  summarize(opened = length(id)) %>%
  as.data.frame

issues.closed <- issues %>% 
  #filter(!is.na(closed_at)) %>% 
  group_by(date = as.Date(closed_at)) %>%
  summarize(closed = length(id)) %>% 
  as.data.frame

df <- merge(issues.opened, issues.closed, all = T) %>% 
  mutate(
     opened = ifelse(is.na(opened),0,opened)
    ,closed = ifelse(is.na(closed),0,closed)
    ,cumulative_opened = cumsum(opened)
    ,cumulative_closed = cumsum(closed)
    ,active = cumulative_opened - cumulative_closed
  ) %>% as.data.frame

qplot(
   date
  ,cumulative_closed
  ,geom="line"
  ,color=I("red")
  ,data=df
  ,main = "Issues - Open vs. Closed"
  ,size=I(1.1)
) + geom_line(aes(y=cumulative_opened),color="darkolivegreen4", size=I(1.1)) +
    theme_BA

qplot(
   date
  ,active
  ,geom="line"
  ,data=df
  ,main = "Active Issues"
  ,size = I(1.1)
) + theme_BA

