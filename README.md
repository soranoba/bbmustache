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

%% auto http escape
3> bbmustache:render(<<"<h1>{{tag}}</h1>">>, #{"tag" => "I like Erlang & mustache"}).
<<"<h1>I like Erlang &amp; mustache</h1>">>

%% don't use http escape
4> bbmustache:render(<<"<h1>{{{tag}}}</h1>">>, #{"tag" => "I like Erlang & mustache"}).
<<"<h1>I like Erlang & mustache</h1>">>

%% change the symbol of the tag
5> bbmustache:render(<<"{{=<< >>=}} <<tag>>, <<={{ }}=>> {{tag}}">>, #{"tag" => "hi"}).
<<" hi,  hi">>
```
Please refer to [the documentation for how to use the mustache](http://mustache.github.io/mustache.5.html) as the need arises.

### Undocumented function
Although present in many of the implementation, there is a function that does not exist in the document of the mustache.<br />
The `bbmustache` corresponds as much as possible to it.

#### Render plain lists
If you specify the dot as a key, it points to this function.

```erlang
%% template.mustache
{{#mylist}}
  escape
    {{.}}
  unescape
    {{{.}}}
{{/mylist}}

%% script
1> bbmustache:compile(bbmustache:parse_file("template.mustache"), #{"mylist" => ["<b>1</b>", "<b>2</b>", "<b>3</b>"]}).
<<"  escape\n    &lt;b&gt;1&lt;&#x2F;b&gt;\n  unescape\n    <b>1</b>\n  escape\n    &lt;b&gt;2&lt;&#x2F;b&gt;\n  unescape\n   "...>>

%% result
  escape
    &lt;b&gt;1&lt;&#x2F;b&gt;
  unescape
    <b>1</b>
  escape
    &lt;b&gt;2&lt;&#x2F;b&gt;
  unescape
    <b>2</b>
  escape
    &lt;b&gt;3&lt;&#x2F;b&gt;
  unescape
    <b>3</b>
'''
However, the types of correspond is only these.<br />
The behavior when given the other types, it is undefined.

```erlang
[integer() | float() | binary() | string() | atom()]
'''

### Use as a library
Add the following settings.

```erlang
%% rebar (rebar.config)

{deps,
  [
   {bbmustache, ".*", {git, "git://github.com/soranoba/bbmustache.git", {branch, "master"}}}
  ]}.

%% rebar3 (rebar.config)

{deps, [bbmustache]}.
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
