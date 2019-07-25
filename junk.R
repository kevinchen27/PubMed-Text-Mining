article_type <- c() #convert authors of each paper into one string for(i in 1:20){ authorsnames[i]<-unlist(papers[[i]]$authors$name) %>% paste(collapse = " ") #unlists author names for each paper, 
  #then combines resulting elements of vectors into one string  #removes all lists with no values in the data set papers[[i]][which(names(papers[[i]]) %in% c("doccontriblist","srccontriblist","references","pubtype"))] <- NULL
            
            #reduce id types to just strings
            article_type[i] <- unlist(papers[[i]]$articleids$idtype) %>% paste(collapse = " ")  
            }
            
            #replace author list with author vector
            for(i in 1:20){
            
            papers[[i]]$authors <- authorsnames[i]
            
            papers[[i]]$articleids <- article_type[i]
            
            }

junk

papers%>% 
  