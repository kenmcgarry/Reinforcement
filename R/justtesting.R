
IGnames <- c("sex","sibsp","pclass","age","parch")
scum[1,1]
test <- str_extract_all(scum[3,1], regex(IGnames)) %>% unlist()

length(test)
fucked<-"absent"

if(grep(rules_kyphosis[1,1],fucked)==1){}



