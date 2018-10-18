-module(server).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch  = cowboy_router:compile(
		  [{'_', 
		    [{"/messages",   get_latest_messages_h,   [],
		      "/message",    message_h,               []} ]} ]),

    {ok, _}   = cowboy:start_clear(
		  http, 
		  [{port, 8080}], 
		  #{env => #{dispatch => Dispatch}})
		   
    echo_get_sup:start_link().

stop(_State) ->
    
    ok.
