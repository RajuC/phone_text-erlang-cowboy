-module(phone_text_handler).

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).

%user defined functions
-export([get_html/2]).
-export([get_phone_text/2]).
-define(PRIV_DIR,"/home/raju/Documents/raju/phoneToText/priv").



init(_Transport, _Req, []) ->
%%	io:format("Req ::  ~p~n",[Req]),
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"html">>, []}, get_html}], Req, State}.

%%flow wen the request comes
%%allowed_methods
%%resource_exists
%%allowed_methods
%%resource_exists
%%content_types_accepted
%%get_phone_text


get_html(Req,index) ->
	case file:read_file(filename:join(?PRIV_DIR,"phoneToText.html")) of
		{ok,Bin} ->
			{Bin, Req, index};
		{error,Reason} ->
			{error,Reason}
	end.

resource_exists(Req, _State) ->
	case cowboy_req:binding(paste_id, Req) of
		{undefined, Req2} ->
			{true, Req2, index};
		{PasteID, Req2} ->
			case valid_path(PasteID) and file_exists(PasteID) of
				true -> {true, Req2, PasteID};
				false -> {false, Req2, PasteID}
			end
	end.

valid_path(<<>>) -> true;
valid_path(<<$., _T/binary>>) -> false;
valid_path(<<$/, _T/binary>>) -> false;
valid_path(<<_Char, T/binary>>) -> valid_path(T).

file_exists(Name) ->
	case file:read_file_info(filename:join(?PRIV_DIR, Name)) of
		{ok, _Info} -> true;
		{error, _Reason} -> false
	end.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
	{[
		{{<<"application">>, <<"x-www-form-urlencoded">>, []}, get_phone_text}
	],Req, State}.

get_phone_text(Req, State) ->
	{ok, A, Req2} = cowboy_req:body_qs(Req),
	io:format("~n Request :: ~p~n",[A]),
	[{<<"Input">>,NumOrText},{<<"data">>,Data}]  = A,
	case NumOrText of
		<<"number">> -> 
			case phone_text:get_phone_text(Data) of
				{error,Reason} ->
					cowboy_req:reply(400,[],to_bin(Reason), Req),
					{true , Req2, State};
				WordsL ->
					TextL =
					case WordsL of	
						[] ->
				 			" No Words matches your number";
				 		WordsLst ->	
							string:join(WordsLst,", ")
					end,
					Body = "<html><head><meta charset=\"utf-8\"><title>Phone Num To 
						Text</title><style>body{background-size:cover;
						background-color:#0099ff;color:#fff;}</style></head><body>
						<p><br/><br/>You have entered the number : <b>" ++ binary_to_list(Data) ++ "</b></p> 
						<p><br/><br/><h2> Words matching that number are : " ++ TextL ++" .</h2></p>
						</br></br><a href=\"http://localhost:8081/\">Enter another Combination</a></body></html>",
					case cowboy_req:method(Req2) of
						{<<"POST">>, Req4} ->
							cowboy_req:reply(200,[],to_bin(Body),Req4),
							{true , Req4, State};
						{_, Req4} ->
							{true, Req4, State}
					end
			end;
		<<"text">> ->
			case phone_text:get_text_num(Data)  of
				{error,Reason} ->
					cowboy_req:reply(400,[],to_bin(Reason), Req),
					{true , Req2, State};
				Num ->
					Body = "<html><head><meta charset=\"utf-8\"><title>Phone Text To
						Number</title><style>body{background-size:cover;
						background-color:#0099ff;color:#fff;}</style></head><body>
						<p><br/><br/>You have entered the Word : <b>" ++ binary_to_list(Data) ++ "</b></p> 
						<p><br/><br/><h2> Number matching that Word is : " ++ Num ++" .</h2>
						</br></br><a href=\"http://localhost:8081/\">Enter another Combination</a></body></html>",
					case cowboy_req:method(Req2) of
						{<<"POST">>, Req4} ->
							cowboy_req:reply(200,[],to_bin(Body),Req4),
							{true , Req4, State};
						{_, Req4} ->
							{true, Req4, State}
					end
			end
	end.

to_bin(Val) when is_atom(Val) ->
	to_bin(atom_to_list(Val));
to_bin(Val) when is_list(Val) ->
		list_to_binary(Val).