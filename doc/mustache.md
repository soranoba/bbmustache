

# Module mustache #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Mustach template engine for Erlang/OTP.
Copyright (c) 2015 Hinagiku Soranoba All Rights Reserved.


<a name="types"></a>

## Data Types ##




### <a name="type-assoc_data">assoc_data()</a> ###



<pre><code>
assoc_data() = [{<a href="#type-data_key">data_key()</a>, <a href="#type-data_value">data_value()</a>}]
</code></pre>





### <a name="type-data">data()</a> ###



<pre><code>
data() = <a href="#type-assoc_data">assoc_data()</a>
</code></pre>





### <a name="type-data_key">data_key()</a> ###



<pre><code>
data_key() = string()
</code></pre>





### <a name="type-data_value">data_value()</a> ###



<pre><code>
data_value() = <a href="#type-data">data()</a> | iodata() | fun((<a href="#type-data">data()</a>, function()) -&gt; iodata())
</code></pre>





### <a name="type-template">template()</a> ###


__abstract datatype__: `template()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#compile-2">compile/2</a></td><td>Embed the data in the template.</td></tr><tr><td valign="top"><a href="#parse_binary-1">parse_binary/1</a></td><td>Create a <a href="#template-0"><code>template/0</code></a> from a binary.</td></tr><tr><td valign="top"><a href="#parse_file-1">parse_file/1</a></td><td>Create a <a href="#template-0"><code>template/0</code></a> from a file.</td></tr><tr><td valign="top"><a href="#render-2">render/2</a></td><td>Equivalent to <a href="#compile-2"><tt>compile(parse_binary(Bin), Data)</tt></a>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="compile-2"></a>

### compile/2 ###


<pre><code>
compile(Mustache::<a href="#type-template">template()</a>, Data::<a href="#type-data">data()</a>) -&gt; binary()
</code></pre>
<br />

Embed the data in the template.
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
parse_file(Filename::<a href="file.md#type-filename">file:filename()</a>) -&gt; <a href="#type-template">template()</a>
</code></pre>
<br />

Create a [`template/0`](#template-0) from a file.
<a name="render-2"></a>

### render/2 ###


<pre><code>
render(Bin::binary(), Data::<a href="#type-data">data()</a>) -&gt; binary()
</code></pre>
<br />

Equivalent to [`compile(parse_binary(Bin), Data)`](#compile-2).

__See also:__ [compile/2](#compile-2), [parse_binary/1](#parse_binary-1), [parse_file/1](#parse_file-1), [render/2](#render-2).
