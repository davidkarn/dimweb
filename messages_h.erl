-module(messages_h).

-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
%    #{echo := Echo} = cowboy_req:match_qs([{echo, [], undefined}], Req0),
    Req = send_messages(Method, Req0),
    {ok, Req, Opts}.

send_messages(<<"GET">>, Req) ->
    cowboy_req:reply(200, 
		     #{
		       <<"content-type">> => <<"application/json; charset=utf-8">>
		      }, 
		     jiffy:encode(chain:get_latest_messages(100)), Req);
echo(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).) ->
