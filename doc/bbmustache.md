

# Module bbmustache #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Binary pattern match Based Mustach template engine for Erlang/OTP.

Copyright (c) 2015 Hinagiku Soranoba All Rights Reserved.

<a name="types"></a>

## Data Types ##




### <a name="type-assoc_data">assoc_data()</a> ###


<pre><code>
assoc_data() = [{atom(), <a href="#type-data_value">data_value()</a>}] | [{binary(), <a href="#type-data_value">data_value()</a>}] | [{string(), <a href="#type-data_value">data_value()</a>}]
</code></pre>




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
option() = {key_type, atom | binary | string}
</code></pre>

 - key_type: Specify the type of the key in [`data/0`](#data-0). Default value is `string`.



### <a name="type-template">template()</a> ###


__abstract datatype__: `template()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#compile-2">compile/2</a></td><td>Equivalent to <a href="#compile-3"><tt>compile(Template, Data, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#compile-3">compile/3</a></td><td>Embed the data in the template.</td></tr><tr><td valign="top"><a href="#parse_binary-1">parse_binary/1</a></td><td>Create a <a href="#template-0"><code>template/0</code></a> from a binary.</td></tr><tr><td valign="top"><a href="#parse_file-1">parse_file/1</a></td><td>Create a <a href="#template-0"><code>template/0</code></a> from a file.</td></tr><tr><td valign="top"><a href="#render-2">render/2</a></td><td>Equivalent to <a href="#render-3"><tt>render(Bin, Data, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#render-3">render/3</a></td><td>Equivalent to <a href="#compile-3"><tt>compile(parse_binary(Bin), Data, Options)</tt></a>.</td></tr></table>


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
compile(Bbmustache::<a href="#type-template">template()</a>, Data::<a href="#type-data">data()</a>, Options::[<a href="#type-option">option()</a>]) -&gt; binary()
</code></pre>
<br />

Embed the data in the template.

```
  1> Template = bbmustache:parse_binary(<<"{{name}}">>).
  2> bbmustache:compile(Template, #{"name" => "Alice"}).
  <<"Alice">>
```

Data support assoc list or maps (OTP17 or later). <br />
All key in assoc list or maps must be same type.

<a name="parse_binary-1"></a>

### parse_binary/1 ###

<pre><code>
parse_binary(Bin::binary()) -&gt; <a href="#type-template">template()</a>
</code></pre>
<br />

Create a [`template/0`](#template-0) from a binary.

<a name="parse_file-1"></a>

### parse_file/1 ###

<pre><code>
parse_file(Filename::<a href="file.md#type-filename_all">file:filename_all()</a>) -&gt; <a href="#type-template">template()</a>
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

__See also:__ [compile/2](#compile-2), [parse_binary/1](#parse_binary-1), [parse_file/1](#parse_file-1), [render/2](#render-2).

<a name="render-3"></a>

### render/3 ###

<pre><code>
render(Bin::binary(), Data::<a href="#type-data">data()</a>, Options::[<a href="#type-option">option()</a>]) -&gt; binary()
</code></pre>
<br />

Equivalent to [`compile(parse_binary(Bin), Data, Options)`](#compile-3).

