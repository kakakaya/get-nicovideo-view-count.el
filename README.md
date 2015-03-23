# get-nicovideo-view-count.el
ニコニコ動画におけるごちうさの再生数を表示するEmacs Lispです。1話だけでなく、任意の話数の再生数を表示できます。

元ネタ:http://bakedroy.hatenablog.com/entry/2015/03/22/033247
# Installation
```
(require 'get-nicovideo-view-count)
```

# Usage
```
(get-nicovideo-view-count-by-id 1397552685)
```
=> gochiusa ep01's views count
```
(get-nicovideo-view-count-by-gochiusa-ep 3)
```
=> gochiusa ep03's views count

# Todo
[ ] 以下のように、interactiveで適切に動作するように修正する
```
M-x get-nicovideo-view-count-by-id RET 1398130645 RET
```
=> gochiusa ep02's views count
```
C-u M-x get-nicovideo-view-count-by-gochiusa-ep RET
```
=> gochiusa ep04's views count
