source.all <-
function (path) 
{
    path %>% paste(., list.files(.), sep = "/") %>% grep(".+\\.R", 
        ., value = T) %>% sapply(source)
}
