# feedlyparser

A solo adventure on Haskell with parsers

The initial purpose of this project is to be able to successfully and completely parse stuff from Feedly's API
So initially I'll be working on a JSON file.
Next target will be to fetch JSON directly from API, maybe persist it somewhere for convenience and then process that input.

----

to test it out, run `stack ghci` and then you can play with your json output:

``` haskell
*Main Lib> Right x <- feedList "/home/korayal/Desktop/hn.json"
*Main Lib> head $ items x
FeedItem {title = "Using Caffe models in Tensorflow", engagement = 10, published = 2015-11-20 18:51:04 UTC, crawled = 2015-11-20 19:39:38.545 UTC}
```
