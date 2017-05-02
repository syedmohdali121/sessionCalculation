# library(jsonlite)
# library(dplyr)

# #load data
# df <- fromJSON("ggeventCopy.json")

# #cleaning up the mess
# clean_data <- data.frame( ai5 = df$headers$ai5,sdkv = df$headers$sdkv, event = df$post$event, ts = df$post$ts, timestamp = df$bottle$timestamp, game_id = df$bottle$game_id, stringsAsFactors = F)
# clean_data$timestamp <- strptime(clean_data$timestamp, format = "%Y-%m-%d %T")
# clean_data$timestamp <- as.POSIXct(clean_data$timestamp)
start_time_prog <- Sys.time()
# #find all the unique ai5
# unique_ai5 <- unique(clean_data$ai5)

total_session_record <- data.frame(ai5 = character(), valid_session = numeric(), avg_valid_session = double(), non_valid_session = numeric(), avg_non_valid_session = double(), game_id = character(), stringsAsFactors = F )

#selecting first ai5 for the demo
for(id in 1 : length(unique_ai5))
{
	# if( id %% 1000 == 0)
	# 	print(id)
	some_ai5 <- clean_data %>% filter( ai5 == unique_ai5[id])

	session_time <- data.frame(start = character(), stop = character(), stringsAsFactors = F)
	ai5_session_time <- data.frame(start = character(), stop = character(), stringsAsFactors = F)

#seperate ggstart and ggstop
	all_ggstart <- some_ai5 %>% filter(event == "ggstart")
	all_ggstop <- some_ai5 %>% filter(event == "ggstop")

	start <- all_ggstart$timestamp[1]
	stop <- all_ggstop$timestamp[1]
	i <- 1    #index for ggstart
	j <- 1    #index for ggstop
	flag <- 1
	valid_session <- vector()
	non_valid_session <- vector()
	while(i <= nrow(all_ggstart) && j <= nrow(all_ggstop))
	{
		if(all_ggstart$timestamp[i] < all_ggstop$timestamp[j] )
		{
			i<-i+1
			while(all_ggstart$timestamp[i] < all_ggstop$timestamp[j] && i<=nrow(all_ggstart))
			{

				start <- all_ggstart$timestamp[i]
				i <- i+1
			}

		}
		else if(all_ggstart$timestamp[i] > all_ggstop$timestamp[j] && i <= nrow(all_ggstart) && j <= nrow(all_ggstop))
		{
			flag <- 0
			while(all_ggstart$timestamp[i] > all_ggstop$timestamp[j] && j <= nrow(all_ggstop) )
			{
				stop <- all_ggstop$timestamp[j]
				j <- j+1

			}
			i <- i +1

			if(j >= nrow(all_ggstop))
				stop <- all_ggstop$timestamp[j-1]
			if(start < stop)
			{
			#print(paste("Valid Session from",start, stop))
				session_time <- rbind(session_time, data.frame(start = as.character(start), stop = as.character(stop), stringsAsFactors = F))
			}
			start <- all_ggstart$timestamp[i-1]
			i <- i-1

		}
		else if(all_ggstart$timestamp[i] == all_ggstop$timestamp[j] && i <= nrow(all_ggstart) && j <= nrow(all_ggstop))
		{
			if(j < nrow(all_ggstop) && (all_ggstart$timestamp[i] < all_ggstop$timestamp[j+1] ))
				j <- j+1
			else
			{
				i <- i +1
				j <- j + 1
			}
		}
	}
	if( !flag )
	{
		if(all_ggstop$timestamp[nrow(all_ggstop)] > all_ggstart$timestamp[(nrow(all_ggstart))])
		{
			stop <- all_ggstop$timestamp[nrow(all_ggstop)]
  		#print(paste("Valid Session from ", start, stop))
			session_time <- rbind(session_time, data.frame(start = as.character(start), stop = as.character(stop), stringsAsFactors = F))
		}
	}
	session_time$start <- as.POSIXct(session_time$start)
	session_time$stop <- as.POSIXct(session_time$stop)

#print(session_time)
	i <- 1
	while(nrow(session_time)>=2)
	{
		i <- 1
		j <- 2
		if(session_time$start[j] - 30 < session_time$stop[i])
		{
			session_time$stop[i] <- session_time$stop[j]
			session_time <- session_time[-j,]
		#print(nrow(session_time))
		}
		else
		{
		#print(paste("Final session : ",session_time$start[i] , session_time$stop[i]))
			ai5_session_time <- rbind(ai5_session_time, data.frame(start = as.character(session_time$start[i]), stop = as.character(session_time$stop[i]), stringsAsFactors = F))
			session_time <- session_time[-i,]
		#print(nrow(session_time))
		}
	}
	#print(paste("Final session : ",session_time$start[i] , session_time$stop[i]))
	ai5_session_time <- rbind(ai5_session_time, data.frame(start = as.character(session_time$start[i]), stop = as.character(session_time$stop[i]), stringsAsFactors = F))
	session_time[-i,]
	ai5_session_time$start <- as.POSIXct(ai5_session_time$start)
	ai5_session_time$stop <- as.POSIXct(ai5_session_time$stop)
	#print(ai5_session_time)
	i <- 1
	while(i <= nrow(ai5_session_time))
	{
		diff <- difftime(ai5_session_time$stop[i], ai5_session_time$start[i], units = "secs")
		if(is.na(diff))
		{
			nan_flag <- 1
			break
		}
		else if(as.numeric(diff) < 60)
			non_valid_session <- c(non_valid_session,diff)
		else
			valid_session <- c(valid_session,diff)
		i <- i + 1
	}
# print(paste("Total valid sessions for ai5 = ", some_ai5$ai5[1], " are ", length(valid_session)))
# print(paste("Total non valid sessions for ai5 = ", some_ai5$ai5[1], " are ", length(non_valid_session)))
# print(paste("Avg valid sessions for ai5 = ", some_ai5$ai5[1], " are ", format(round(mean(valid_session), 3),nsmall = 3)))
# print(paste("Avg non valid sessions for ai5 = ", some_ai5$ai5[1], " are ", format(round(mean(non_valid_session), 3),nsmall = 3)))
	if(nan_flag == 1)
	{
		total_session_record <- rbind(total_session_record, data.frame(ai5 = some_ai5$ai5[1] , valid_session = 0, avg_valid_session = 0, non_valid_session = 0, avg_non_valid_session = 0, game_id = some_ai5$game_id[1], stringsAsFactors = F ))
		nan_flag <- 0
	}
	else 
	{
		if(length(non_valid_session) == 0)
		{
			num <- 0
		}
		else
		{
			num <- format(round(mean(non_valid_session), 3),nsmall = 3)
		}
		if(length(valid_session) == 0)
		{
			val_num <- 0
		}
		else
		{
			val_num <- format(round(mean(valid_session), 3),nsmall = 3)
		}
		total_session_record <- rbind(total_session_record, data.frame(ai5 = some_ai5$ai5[1] , valid_session = length(valid_session), avg_valid_session = val_num, non_valid_session = length(non_valid_session), avg_non_valid_session = num, game_id = some_ai5$game_id[1], stringsAsFactors = F ))
		valid_session <- NULL
		non_valid_session <- NULL
	}
}
write.csv(total_session_record, file="total_session_record.csv")
end_time_prog <- Sys.time()

diff <- end_time_prog - start_time_prog
print(paste("Total time taken by prog ", diff))



















