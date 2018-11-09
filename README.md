# TweetMoodometer

TweetMoodometer  provides the function analyse-tweets which takes a valid twurl request
and draws a graph depicting sentiments in the tweets. 

It is built on the data-science package and the Trump's tweet analysis module initially created by Nicholas Van Horn.

read-tweets-twurl reads the tweets from the twurl request provided, the tweets are under the key 'results'

tweetlist extracts the actual tweet text from each tweet hash and removes retweets.

joined-tweetlist is a tail recursion used to extract each string from what tweetlist returns (a list of lists of strings) 
and appends it into one large string.

sentiment-analysis uses the large string returned from joined-tweetlist to plot the sentiments in a histogram.

