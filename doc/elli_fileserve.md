

# Module elli_fileserve #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Elli middleware for serving static files.

__Behaviours:__ [`elli_handler`](https://github.com/elli-lib/elli/blob/master/doc/elli_handler.md).

__See also:__ [config()](#type-config).

<a name="description"></a>

## Description ##
This middleware serves static files given a URL `prefix` and a local `path`,
any request containing `"/../"` is ignored.
<a name="types"></a>

## Data Types ##




### <a name="type-config">config()</a> ###


__abstract datatype__: `config()`

Configure `elli_fileserve` with a list of tuples of the form,
<code>{<a href="#type-config_key"><code>config_key()</code></a>, binary()}</code>.



### <a name="type-config_key">config_key()</a> ###


__abstract datatype__: `config_key()`



<dt><code>charset</code></dt>




<dd>The charset to send in <code>Content-Type</code> headers.<br />
<em>Default:</em><code>undefined</code>, i.e. none.</dd>




<dt><code>default</code></dt>




<dd>The relative file name to serve if the request path ends with <code>/</code>.<br />
<em>Default:</em> <code><<"index.html">></code></dd>




<dt><code>path</code></dt>




<dd>The file path to the server root.<br />
<em>Default:</em> <code><<"/tmp">></code>.</dd>




<dt><code>prefix</code></dt>




<dd>A URL prefix to determine whether a request should be handled.<br />
<em>Default:</em> <code><<>></code>, i.e. handle every request.</dd>



<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#handle-2">handle/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="handle-2"></a>

### handle/2 ###

<pre><code>
handle(Req::<a href="http://github.com/elli-lib/elli/blob/master/doc/elli.md#type-req">elli:req()</a>, Config::<a href="#type-config">config()</a>) -&gt; <a href="http://github.com/elli-lib/elli/blob/master/doc/elli_handler.md#type-result">elli_handler:result()</a>
</code></pre>
<br />

