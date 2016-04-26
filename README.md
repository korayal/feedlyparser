# feedlyparser

A solo adventure on Haskell with parsers

to test it out, run `stack ghci` and then you can play with your json output:

```
*Main Lib> Right x <- feedList "hn.json"
*Main Lib> head $ items x
FeedItem {title = "Using Caffe models in Tensorflow", engagement = Just 10, published = 2015-11-20 18:51:04 UTC, crawled = 2015-11-20 19:39:38.545 UTC}
*Main Lib> 
```
