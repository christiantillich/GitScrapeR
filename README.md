# Git_ScrapR

###Background

This little toolkit spawned out of a couple different issues I was having. The first big one was constraints on github's main search. I could do a global search, or search within a specific repo. However, if I knew the user who created the function, but not what repo the function was in, there was no way to limit the search by author. Nor could I perform regular expression searching. So I wanted some tools where I could query specific paths and do some more complex searches. 

The second big issue was that Bhanu's looking for a way to do automated scrapes of sql queries, looker queries, etc. The idea is that we'll need to keep track of what indicators are using what data sets, for easy QA. 

The third issue was that I wanted some way to capture issues data and turn it into a data set, for analysis. Specifically, to communicate changes to stakeholders, and to monitor the progress of the repository so far. 

###Function List

1. show.github.path - This is the core API fetcher. The user needs to add two variables to his .R profile to get this to work: GITHUB_USER and GITHUB_TOKEN. I believe the token variable could theoretically be your github password, but I find it's easier to create a personal access token through Github settings. 

2. get.git - This function works like show.github.path, but it specifically hits the raw content API to pull down the raw code. 

3. code.db - This creates a table of all the files in a repository, and adds the code using get.git. 

4. issues.db - This creates a table of the issues attached to a single repository. 

5. search.repo(s) - There are two functions in this file. Singular and vector searches respectively. Scans 1/many repos for a given regular expression. 

6. issue.tracking - My core issue-monitoring report. This probably doesn't belong in the toolbox, but it's here for now. 


###Known Issues

1. search.repos is doing a recursive, brute force search. It is slow. I'm not a search optimization guy - I'm sure someone could speed this up but right now it is what it is. Turn on verbose searching if you want to make sure it's not getting stuck. 

