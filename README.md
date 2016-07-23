# feedlyparser

A solo adventure on Haskell with parsers

The initial purpose of this project is to be able to successfully and completely parse stuff from Feedly's API
So initially I'll be working on a JSON file.
Next target will be to fetch JSON directly from API, maybe persist it somewhere for convenience and then process that input.

----

to test it out, 
run `stack exec fp -- "hn.json"` or in `stack ghci` and then you can play with your json output:

``` haskell
*Main Lib> Right x <- feedList "hn.json" 
*Main Lib> head $ items x
FeedItem {id = "2tM1DedEr6eqgdqJ8X4sP+CS3BVG/bkK4hS45ZnKnRQ=_1512668c2b1:11fa642:48f117c4", originId = "https://github.com/ethereon/caffe-tensorflow", fingerprint = "de117aa5", title = "Using Caffe models in Tensorflow", published = 2015-11-20 18:51:04 UTC, crawled = 2015-11-20 19:39:38.545 UTC, alternate = [FeedAlternate {href = "https://github.com/ethereon/caffe-tensorflow", _type = "text/html"}], origin = FeedOrigin {streamId = "feed/https://news.ycombinator.com/rss", foTitle = "Hacker News", htmlUrl = "https://news.ycombinator.com/"}, summary = FeedSummary {content = "<a href=\"https://news.ycombinator.com/item?id=10603037\">Comments</a>", direction = "ltr"}, visual = Just (FeedVisual {url = "https://avatars1.githubusercontent.com/u/337985?v=3&s=400", width = 400, height = 400, processor = "feedly-nikon-v3.1", contentType = "image/png"}), unread = True, categories = [FeedCategory {fcid = "user/e0bfad67-abba-4764-be3a-a44cd3ede219/category/g\252nl\252k.takip", fclabel = "g\252nl\252k.takip"}], engagement = 10, engagementRate = 0}
```
