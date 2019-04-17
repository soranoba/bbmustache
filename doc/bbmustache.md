

# Module bbmustache #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Binary pattern match Based Mustach template engine for Erlang/OTP.

Copyright (c) 2015 Hinagiku Soranoba All Rights Reserved.

<a name="description"></a>

## Description ##

Please refer to [the man page](http://mustache.github.io/mustache.5.html) and [the spec](https://github.com/mustache/spec) of mustache as the need arises.<br />

Please see [this](../benchmarks/README.md) for a list of features that bbmustache supports.

<a name="types"></a>

## Data Types ##




### <a name="type-assoc_data">assoc_data()</a> ###


<pre><code>
assoc_data() = [{atom(), <a href="#type-data_value">data_value()</a>}] | [{binary(), <a href="#type-data_value">data_value()</a>}] | [{string(), <a href="#type-data_value">data_value()</a>}]
</code></pre>




### <a name="type-compile_option">compile_option()</a> ###


<pre><code>
compile_option() = {key_type, atom | binary | string} | raise_on_context_miss | {escape_fun, fun((binary()) -&gt; binary())} | {value_serializer, fun((any()) -&gt; iodata())}
</code></pre>

 - key_type: Specify the type of the key in [`data/0`](#data-0). Default value is `string`.
- raise_on_context_miss: If key exists in template does not exist in data, it will throw an exception (error).
- escape_fun: Specify your own escape function.
- value_serializer: specify how terms are converted to iodata when templating.



### <a name="type-data">data()</a> ###


<pre><code>
data() = <a href="#type-assoc_data">assoc_data()</a>
</code></pre>




### <a name="type-data_value">data_value()</a> ###


<pre><code>
data_value() = <a href="#type-data">data()</a> | iodata() | number() | atom() | fun((<a href="#type-data">data()</a>, function()) -&gt; iodata())
</code></pre>

 Function is intended to support a lambda expression.



### <a name="type-option">option()</a> ###


<pre><code>
option() = <a href="#type-compile_option">compile_option()</a>
</code></pre>

 This type has been deprecated since 1.6.0. It will remove in 2.0.0.



### <a name="type-parse_option">parse_option()</a> ###


<pre><code>
parse_option() = raise_on_partial_miss
</code></pre>

 - raise_on_partial_miss: If the template used in partials does not found, it will throw an exception (error).



### <a name="type-render_option">render_option()</a> ###


<pre><code>
render_option() = <a href="#type-compile_option">compile_option()</a> | <a href="#type-parse_option">parse_option()</a>
</code></pre>




### <a name="type-template">template()</a> ###


__abstract datatype__: `template()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#compile-2">compile/2</a></td><td>Equivalent to <a href="#compile-3"><tt>compile(Template, Data, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#compile-3">compile/3</a></td><td>Embed the data in the template.</td></tr><tr><td valign="top"><a href="#default_value_serializer-1">default_value_serializer/1</a></td><td>Default value serializer for templtated values.</td></tr><tr><td valign="top"><a href="#parse_binary-1">parse_binary/1</a></td><td>Equivalent to <a href="#parse_binary-2"><tt>parse_binary(Bin, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#parse_binary-2">parse_binary/2</a></td><td>Create a <a href="#template-0"><code>template/0</code></a> from a binary.</td></tr><tr><td valign="top"><a href="#parse_file-1">parse_file/1</a></td><td>Equivalent to <a href="#parse_file-2"><tt>parse_file(Filename, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#parse_file-2">parse_file/2</a></td><td>Create a <a href="#template-0"><code>template/0</code></a> from a file.</td></tr><tr><td valign="top"><a href="#render-2">render/2</a></td><td>Equivalent to <a href="#render-3"><tt>render(Bin, Data, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#render-3">render/3</a></td><td>Equivalent to <a href="#compile-3"><tt>compile(parse_binary(Bin), Data, Options)</tt></a>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="compile-2"></a>

### compile/2 ###

<pre><code>
compile(Template::<a href="#type-template">template()</a>, Data::<a href="#type-data">data()</a>) -&gt; binary()
</code></pre>
<br />

Equivalent to [`compile(Template, Data, [])`](#compile-3).

<a name="compile-3"></a>

### compile/3 ###

<pre><code>
compile(Bbmustache::<a href="#type-template">template()</a>, Data::<a href="#type-data">data()</a>, Options::[<a href="#type-compile_option">compile_option()</a>]) -&gt; binary()
</code></pre>
<br />

Embed the data in the template.

```
  1> Template = bbmustache:parse_binary(<<"{{name}}">>).
  2> bbmustache:compile(Template, #{"name" => "Alice"}).
  <<"Alice">>
```

Data support an associative array or a map. <br />
All keys MUST be same type.

<a name="default_value_serializer-1"></a>

### default_value_serializer/1 ###

<pre><code>
default_value_serializer(Integer::number() | binary() | string() | atom()) -&gt; iodata()
</code></pre>
<br />

Default value serializer for templtated values

<a name="parse_binary-1"></a>

### parse_binary/1 ###

<pre><code>
parse_binary(Bin::binary()) -&gt; <a href="#type-template">template()</a>
</code></pre>
<br />

Equivalent to [`parse_binary(Bin, [])`](#parse_binary-2).

<a name="parse_binary-2"></a>

### parse_binary/2 ###

<pre><code>
parse_binary(Bin::binary(), Options::[<a href="#type-parse_option">parse_option()</a>]) -&gt; <a href="#type-template">template()</a>
</code></pre>
<br />

Create a [`template/0`](#template-0) from a binary.

<a name="parse_file-1"></a>

### parse_file/1 ###

<pre><code>
parse_file(Filename::<a href="file.md#type-filename_all">file:filename_all()</a>) -&gt; <a href="#type-template">template()</a>
</code></pre>
<br />

Equivalent to [`parse_file(Filename, [])`](#parse_file-2).

<a name="parse_file-2"></a>

### parse_file/2 ###

<pre><code>
parse_file(Filename::<a href="file.md#type-filename_all">file:filename_all()</a>, Options::[<a href="#type-parse_option">parse_option()</a>]) -&gt; <a href="#type-template">template()</a>
</code></pre>
<br />

Create a [`template/0`](#template-0) from a file.

<a name="render-2"></a>

### render/2 ###

<pre><code>
render(Bin::binary(), Data::<a href="#type-data">data()</a>) -&gt; binary()
</code></pre>
<br />

Equivalent to [`render(Bin, Data, [])`](#render-3).

__See also:__ [compile/2](#compile-2), [compile_option/0](#compile_option-0), [parse_binary/1](#parse_binary-1), [parse_file/1](#parse_file-1), [parse_option/0](#parse_option-0), [render/2](#render-2).

<a name="render-3"></a>

### render/3 ###

<pre><code>
render(Bin::binary(), Data::<a href="#type-data">data()</a>, Options::[<a href="#type-render_option">render_option()</a>]) -&gt; binary()
</code></pre>
<br />

Equivalent to [`compile(parse_binary(Bin), Data, Options)`](#compile-3).

