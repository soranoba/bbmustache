mustache
===========
Mustache template engine for Erlang/OTP.

OTP17 (or later)

## What is Mustach ?
A logic-less templates.
- [{{mustache}}](http://mustache.github.io/)

## Overview
- Binary and map base.
 - Do not use a regular expression !!

## Usage
- [Mastache Manual](http://mustache.github.io/mustache.5.html)
 - Support all of syntax !

```erlang
1> mustache:render(<<"{{name}}">>, #{"name" => "hoge"}).
<<"hoge">>

2> Template1 = mustache:parse_binary(<<"{{name}}">>).
...
3> mustache:compile(Template1, #{"name" => "hoge"}).
<<"hoge">>

4> Template2 = mustache:parse_file(<<"./hoge.mustache">>).
...
5> mustache:compile(Template2, #{"name" => "hoge"}).
<<"hoge">>
```

## Attention
- The number of new line.
 - New line in the template has left all.
- Lambda expression is included wasted processing.
 - Because it is optimized to `parse_string/1` + `compile/2`.

## Benchmark
TODO.

## Contribute
Pull request is welcome =D

## License
[MIT License](LICENSE)
