# Advent of Code

A collection of clojure solutions to advent of code problems. This is a bare bones clojure project that includes a couple dependencies that might be helpful given the problem types.

## Code  
### Input 
Lives under resources/$YEAR/$DAY

### Code Organization

#### Reading files
Code lives under `src/aoc/util.clj`

* `read-file` takes a file-name and a parse-ing function
* `simple-read-file` takes a file-name and an option split-lines? param
* `regex-split` takes a file-name, a regex, and map-keys, it takes the capture groups and maps them to the given keys and reads the string. If passed an optional parse-fn that will be used instead of the read-string.

#### Solutions
Code lives under `src/aoc/solutions/yearYYYY.clj`

Most fn's formatted to return a map of
```clojure
{:part1 "answer1"
 :part2 "answer2"}
```
