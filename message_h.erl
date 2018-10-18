-module(message_h).

-export([init/2]).

init(Req0, Opts) ->
    Method     = cowboy_req:method(Req0),
%    #{echo := Echo} = cowboy_req:match_qs([{echo, [], undefined}], Req0),
    Req        = send_messages(Method, Req0),
    {ok, Req, Opts}.

send_messages(<<"POST">>, Req) ->
    HasBody    = cowboy_req:has_body(Req0),
    Body       = cowboy_req:read_body(Req0, [{length, 100000000}]),
    JSON       = jiffy:parse(Body),

    chain:store_message(chain_json_to_message(JSON)),
    cowboy_req:reply(200, 
		     #{
		       <<"content-type">> => <<"application/json; charset=utf-8">>
		      }, 
		     jiffy:encode(ok), Req);
echo(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).) ->
