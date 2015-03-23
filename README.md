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
=> ごちうさ1話再生数
```
(get-nicovideo-view-count-by-gochiusa-ep 2)
```
=> ごちうさ2話再生数

# Todo
- [ ] 以下のように、interactiveで適切に動作するように修正する(現状、url-retrieve-synchronouslyの保存が上手く動いていない模様)
```
M-x get-nicovideo-view-count-by-id RET 1398329907 RET
```
=> ごちうさ3話再生数
```
C-u M-x get-nicovideo-view-count-by-gochiusa-ep RET
```
=> ごちうさ4話再生数
