# feedlyparser

A solo adventure on Haskell with parsers

The initial purpose of this project is to be able to successfully and completely
parse stuff from Feedly's API So initially I'll be working on a JSON file. Next
target will be to fetch JSON directly from API, maybe persist it somewhere for
convenience and then process that input.

----

to test it out,
run `stack exec fp` or in `stack ghci` and then you can play with your json
output at one of these urls via the help of [jq](https://stedolan.github.io/jq/):

"http://localhost:8080/hn/list"
"http://localhost:8080/hn/list/titles"
"http://localhost:8080/hn/list/:minEngagement"

here is an example:

![](images/feedlyparser.gif)
