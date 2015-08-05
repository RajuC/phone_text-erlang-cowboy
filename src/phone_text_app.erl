-module(phone_text_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
	{'_', [
		{"/", phone_text_handler, []}
	]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8081}], [
		{env, [{dispatch, Dispatch}]}
	]),
	phone_text_sup:start_link().

stop(_State) ->
	ok.
