%% @doc: Elli middleware for serving static files.
%%
%% This middleware serves static files given a URL `prefix' and a local `path',
%% any request containing `"/../"' is ignored.
%% @see config().

-module(elli_fileserve).
-behaviour(elli_handler).

-export([handle/2, handle_event/3]).

-export_type([config/0, config_key/0]).

%% @type config(). Configure `elli_fileserve' with a list of tuples of the form,
%% `{{@link config_key()}, binary()}'.
-type config() :: [{config_key(), binary()}].

%% @type config_key().
%% <dl>
%%   <dt>`charset'</dt>
%%   <dd>The charset to send in `Content-Type' headers.<br/>
%%       <em>Default:</em>`undefined', i.e. none.</dd>
%%   <dt>`default'</dt>
%%   <dd>The relative file name to serve if the request path ends with `/'.<br/>
%%       <em>Default:</em> `<<"index.html">>'</dd>
%%   <dt>`path'</dt>
%%   <dd>The file path to the server root.<br/>
%%       <em>Default:</em> `<<"/tmp">>'.</dd>
%%   <dt>`prefix'</dt>
%%   <dd>A URL prefix to determine whether a request should be handled.<br/>
%%       <em>Default:</em> `<<>>', i.e. handle every request.</dd>
%% </dl>

-type config_key() :: charset | default | path | prefix.

-include_lib("kernel/include/file.hrl").

-import(filename, [dirname/1, extension/1, join/1]).

-ifdef(TEST).
-compile([export_all]).
-endif.


%% TODO: write docstring
-spec handle(elli:req(), config()) -> elli_handler:result().
handle(Req, Config) ->
    [Path|_] = binary:split(elli_request:raw_path(Req), [<<"?">>, <<"#">>]),
    case unprefix(Path, prefix(Config)) of
        undefined -> ignore;
        FilePath  ->
            Filename = local_path(Config, FilePath),
            case elli_util:file_size(Filename) of
                {error, _Reason} -> {404, [], <<"File Not Found">>};
                Size ->
                    {200, headers(Filename, Size, charset(Config)),
                     {file, Filename}}
            end
    end.

%% @private
-spec handle_event(elli_handler:event(), config(), [tuple()]) -> ok.
handle_event(_Event, _Config, _ElliConfig) -> ok.

%%
%% Config
%%

-spec default(config()) -> binary().
default(Config) -> proplists:get_value(default, Config, <<"index.html">>).

-spec path(config()) -> binary().
path(Config) -> proplists:get_value(path, Config, <<"/tmp">>).

-spec prefix(config()) -> binary().
prefix(Config) -> proplists:get_value(prefix, Config, <<>>).

-spec charset(config()) -> undefined | binary().
charset(Config) -> proplists:get_value(charset, Config).

%%
%% Helpers
%%

-spec unprefix(RawPath, {regex, Prefix} | Prefix) -> undefined | binary() when
      RawPath :: binary(),
      Prefix  :: binary().

unprefix(RawPath, {regex, Prefix}) ->
    case re:run(RawPath, Prefix, [{capture, all, binary}]) of
        nomatch -> undefined;
        _Result -> re:replace(RawPath, Prefix, "", [{return, binary}])
    end;

unprefix(RawPath, Prefix) ->
    PrefixSz = size(Prefix),
    case RawPath of
        <<Prefix:PrefixSz/binary, File/binary>> -> File;
        _                                       -> undefined
    end.

-spec local_path(config(), binary()) -> binary().

local_path(Config, <<"/", File/binary>>) -> local_path(Config, File);

local_path(Config, <<>>) -> join([path(Config), default(Config)]);

local_path(Config, FilePath) ->
    MappedPath = path(Config),
    case binary:match(dirname(FilePath), <<"..">>) of
        nomatch ->
            case binary:last(FilePath) of
                $/ -> join([MappedPath, FilePath, default(Config)]);
                _  -> join([MappedPath, FilePath])
            end;
        _       -> throw({403, [], <<"Not Allowed">>})
    end.

-spec headers(Filename, Size, Charset) -> elli:headers() when
      Filename :: binary(),
      Size     :: non_neg_integer(),
      Charset  :: undefined | binary().
headers(Filename, Size, Charset) ->
    MimeType = mimerl:filename(Filename),
    [{<<"Content-Length">>, integer_to_binary(Size)},
     {<<"Content-Type">>, content_type(MimeType, Charset)}].

-spec content_type(MimeType, Charset) -> binary() when
      MimeType :: binary(),
      Charset  :: undefined | binary().
content_type(MimeType, undefined) ->
    MimeType;
content_type(MimeType, Charset) ->
    <<MimeType/binary, "; charset=", Charset/binary>>.
