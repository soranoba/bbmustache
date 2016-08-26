bbmustache
===========
[![Build Status](https://travis-ci.org/soranoba/bbmustache.svg?branch=master)](https://travis-ci.org/soranoba/bbmustache)
[![hex.pm version](https://img.shields.io/hexpm/v/bbmustache.svg)](https://hex.pm/packages/bbmustache)

Binary pattern match Based Mustache template engine for Erlang/OTP.

## Overview
- Binary pattern match based mustache template engine for Erlang/OTP.
 - Do not use a regular expression !!
- Support maps and associative arrays.

### What is Mustach ?
A logic-less templates.
- [{{mustache}}](http://mustache.github.io/)

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
Please refer to [the man page](http://mustache.github.io/mustache.5.html) and [the spec](https://github.com/mustache/spec) of mustache as the need arises.<br />

Please see [this](benchmarks/README.md) for a list of features that bbmustache supports.

## FAQ

### Avoid http escaping

```erlang
%% please use {{{tag}}}
1> bbmustache:render(<<"<h1>{{{tag}}}</h1>">>, #{"tag" => "I like Erlang & mustache"}).
<<"<h1>I like Erlang & mustache</h1>">>
```

### Want to use symbol of tag

```erlang
1> bbmustache:render(<<"{{=<< >>=}} <<tag>>, <<={{ }}=>> {{tag}}">>, #{"tag" => "hi"}).
<<" hi,  hi">>
```

### Want to change the type of the key

```erlang
1> bbmustache:render(<<"{{tag}}">>, #{tag => "hi"}, [{key_type, atom}]).
<<"hi">>
```

## Attention
- Lambda expression is included wasted processing.
 - Because it is optimized to `parse_string/1` + `compile/2`.

## Comparison with other libraries
[Benchmarks and check the reference implementation](benchmarks/README.md)

## Contribute
Pull request is welcome =D

## License
[MIT License](LICENSE)
