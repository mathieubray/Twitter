library(taskscheduleR)

taskscheduler_create(taskname = "schedulerTestsTweets", 
                     rscript = "C:/Users/braym/Box Sync/Personal/Twitter/AutoTrackedTweets/TwitterAutoTrackTweets.R", 
                     schedule = "MINUTE",
                     startdate = format(as.Date("2018-05-24"), "%m/%d/%Y"),
                     starttime = "18:30")

taskscheduler_delete(taskname = "schedulerTestsTweets")
