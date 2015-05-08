mustache
===========
Mustache template engine for Erlang/OTP.

## What is Mustach ?
A logic-less templates.
- [{{mustache}}](http://mustache.github.io/)

## Overview
- Binary and map base.
 - Do not use a regular expression !!
- Support an associative array for the before R17.

## Usage
### Use as a library
Add the following settings.

```erlang
%% rebar.config

%% If you want to use a map is necessary
{erl_opts, [
            {platform_define, "^[0-9]+", namespaced_types}
           ]}.

{deps,
  [
   {mustache, ".*", {git, "git://github.com/soranoba/mustache.git", {branch, "master"}}}
  ]}.
```

### How to use simple Mustache
- [Mastache Manual](http://mustache.github.io/mustache.5.html)
 - Support all of syntax !

Map (R17 or later)
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

Associative array
```erlang
1> mustache:render(<<"{{name}}">>, [{"name", "hoge"}]).
<<"hoge">>

2> Template1 = mustache:parse_binary(<<"{{name}}">>).
...
3> mustache:compile(Template1, [{"name", "hoge"}]).
<<"hoge">>

4> Template2 = mustache:parse_file(<<"./hoge.mustache">>).
...
5> mustache:compile(Template2, [{"name", "hoge"}]).
<<"hoge">>
```

### More information
You want more information, see the [doc](doc).

## Attention
- The number of new line.
 - New line in the template has left all.
- Lambda expression is included wasted processing.
 - Because it is optimized to `parse_string/1` + `compile/2`.

## Simple Benchmark

||[moyombo/mustache.erl](https://github.com/mojombo/mustache.erl)|[soranoba/mustache](https://github.com/soranoba/mustache)|
|:--|:---|:---|
|score (time) |1016414 |33001|

- [Benchmark script](https://gist.github.com/soranoba/6c4bf489714618366a1c)

In this case, it is 30 times faster than moyombo/mustache.erl

## Contribute
Pull request is welcome =D

## License
[MIT License](LICENSE)
