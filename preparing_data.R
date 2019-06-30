library(dplyr)
library(Hmisc)
library(purrr)
jobs = read.csv("jobmag.csv")
colnames(jobs) = Cs(title, date_posted, deadline, poster, field, location, job_type, industry, minimum_qualification, experience_level, responsibilities, qualifications, benefits, application_method)
jobs[,1:ncol(jobs)] = map(jobs[,1:ncol(jobs)], as.character)
jobs = jobs[nchar(jobs$title) > 3,]
jobs = jobs[!is.na(jobs$title),]
jobs = unique.data.frame(jobs)
# 37034 jobs from 2016-09-07 to 2019-06-14
# 35865 jobs from 2016-09-07 t0 2019-04-29
jobs$date_posted = trimws(gsub("Posted on:", "", ignore.case = TRUE, jobs$date_posted), "both")
jobs$date_posted = strptime(jobs$date_posted, format = "%d %B,%Y")
jobs$deadline = ifelse(jobs$deadline == " Deadline: Not Specified ", NA, jobs$deadline)
jobs$deadline = trimws(gsub("Deadline:", "", jobs$deadline, ignore.case = TRUE))

jobs$deadline = strptime(jobs$deadline, format = "%d %B,%Y")
jobs$field = ifelse(is.na(jobs$field) & startsWith(jobs$location, "Job Field"), jobs$location, jobs$field)
jobs$field = trimws(gsub("Job Field", "", jobs$field, ignore.case = FALSE), "both")
jobs$location = ifelse(startsWith(jobs$location, "Job Field"), NA, jobs$location)
jobs$location = ifelse(is.na(jobs$location) & startsWith(jobs$experience_level, "Location"), jobs$experience_level, jobs$location)
jobs$location = trimws(gsub("Location", "", jobs$location, ignore.case = FALSE), "both")
jobs$experience_level = ifelse(startsWith(jobs$experience_level,"Location"), NA, jobs$experience_level)
jobs$poster[jobs$poster == "View Jobs posted same day"] = NA
jobs$poster = tolower(gsub("[^[:alnum:][:space:]]", "", jobs$poster))
jobs$poster = trimws(gsub("\\s+", " ", jobs$poster))
write.csv(jobs, "jobs.csv", row.names = F)

# cleaned the data in open refine(merging almost similar company names - column poster).Output - jobs_posters_cleaned.csv
jobs = read_csv("jobs_posters_cleaned.csv")
jobs$poster = gsub("public service board", " ", jobs$poster)
jobs$poster = stri_trans_general(jobs$poster, "title")
jobs$poster = gsub(" Of ", " of ", jobs$poster, ignore.case = F)
jobs$poster = gsub(" And ", " and ", jobs$poster, ignore.case = F)
jobs$poster = gsub(" For ", " for ", jobs$poster, ignore.case = F)

jobs$poster = gsub(" The ", " the ", jobs$poster, ignore.case = F)

jobs$poster = gsub(" At ", " at ", jobs$poster, ignore.case = F)

jobs$poster = gsub(" In ", " in ", jobs$poster, ignore.case = F)
jobs$poster = gsub(" On ", " on ", jobs$poster, ignore.case = F)

jobs$field = gsub("00A0>", "", jobs$field)
jobs$field = gsub("<U", "", jobs$field)



write.csv(jobs, "jobs_1.csv", row.names = F)