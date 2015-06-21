bbmustache
===========
[![hex.pm version](https://img.shields.io/hexpm/v/bbmustache.svg)](https://hex.pm/packages/bbmustache)

Binary pattern match Based Mustache template engine for Erlang/OTP.

## What is Mustach ?
A logic-less templates.
- [{{mustache}}](http://mustache.github.io/)

## Overview
- Binary pattern match based.
 - Do not use a regular expression !!
- Support maps and associative arrays.

## Usage
### Quick start

```bash
$ git clone git://github.com/soranoba/bbmustache.git
$ cd bbmustache
$ make start
Erlang/OTP 17 [erts-6.3] [source-f9282c6] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:true]

Eshell V6.3  (abort with ^G)
1> {ok,[bbmustache]}
1> bbmustache:render(<<"{{name}}">>, #{"name" => "hoge"}).
<<"hoge">>
2> bbmustache:render(<<"{{name}}">>, [{"name", "hoge"}]).
<<"hoge">>
```

### Use as a library
Add the following settings.

```erlang
%% rebar.config

{deps,
  [
   {bbmustache, ".*", {git, "git://github.com/soranoba/bbmustache.git", {branch, "master"}}}
  ]}.
```

If you don't use the rebar and use the OTP17 or later, this library should be compile with `-Dnamespaced_types`.

### How to use simple Mustache
- [Mastache Manual](http://mustache.github.io/mustache.5.html)
 - Support all of syntax !

Map (R17 or later)
```erlang
1> bbmustache:render(<<"{{name}}">>, #{"name" => "hoge"}).
<<"hoge">>

2> Template1 = bbmustache:parse_binary(<<"{{name}}">>).
...
3> bbmustache:compile(Template1, #{"name" => "hoge"}).
<<"hoge">>

4> Template2 = bbmustache:parse_file(<<"./hoge.mustache">>).
...
5> bbmustache:compile(Template2, #{"name" => "hoge"}).
<<"hoge">>
```

Associative array
```erlang
1> bbmustache:render(<<"{{name}}">>, [{"name", "hoge"}]).
<<"hoge">>

2> Template1 = bbmustache:parse_binary(<<"{{name}}">>).
...
3> bbmustache:compile(Template1, [{"name", "hoge"}]).
<<"hoge">>

4> Template2 = bbmustache:parse_file(<<"./hoge.mustache">>).
...
5> bbmustache:compile(Template2, [{"name", "hoge"}]).
<<"hoge">>
```

### More information
You want more information, see the [doc](doc).

## Attention
- Lambda expression is included wasted processing.
 - Because it is optimized to `parse_string/1` + `compile/2`.

## Simple Benchmark

||[moyombo/mustache.erl](https://github.com/mojombo/mustache.erl)|[soranoba/bbmustache](https://github.com/soranoba/bbmustache)|
|:--|:---|:---|
|score (time) |1016414 |33001|

- [Benchmark script](https://gist.github.com/soranoba/6c4bf489714618366a1c)

In this case, it is 30 times faster than moyombo/mustache.erl

## Contribute
Pull request is welcome =D

## License
[MIT License](LICENSE)
