-module(phone_text).
-export([get_phone_text/1,get_text_num/1]).
-define(NUMBERS,[{"1",""},{"2","abc"},{"3","def"},{"4","ghi"},{"5","jkl"},
		{"6","mno"},{"7","pqrs"},{"8","tuv"},{"9","wxyz"},{"0",""}]).

get_phone_text(Number) ->
	case check_data(Number,"number") of
		{error,Reason} ->
			{error,Reason};
		ok->
			ValueList=get_value_from_numlist(binary_to_list(Number),[]),
			{ok,Bin} = file:read_file(filename:join(code:priv_dir(phone_text),"linuxwords.txt")),
			StringListConvLower = [string: to_lower(X) ||X<- string:tokens(erlang:binary_to_list(Bin),"\n")],
			get_words(ValueList,StringListConvLower,[])
	end.

get_text_num(Text) ->
	case check_data(Text,"text") of
		{error,Reason} ->
			{error,Reason};
		ok->	
			get_num([string: to_lower([X])||X<-binary_to_list(Text)],[])
	end.

	
%%==========================================================================
get_num([],Num) ->
	lists:append(lists:reverse(Num));
get_num([H|T],Num)->
	case get_match_num(H,?NUMBERS) of
		{error,Reason} ->
			{error,Reason};
		Numbr ->
			get_num(T,[Numbr|Num])
	end.

get_match_num(Alp,[]) ->
	{error,Alp ++ " not valid"};
get_match_num(Alp,[{Num,Str}|T]) ->
	case string:str(Str,Alp) of 
		0 ->
			get_match_num(Alp,T);
		_ ->
			Num
	end.

%%==========================================================================
get_words([],_,MnemonicsList) ->
	MnemonicsList;
get_words([H|T],StringListConvLower,Value) ->
	case lists:member(H,StringListConvLower) of
		true ->
			get_words(T,StringListConvLower,[H|Value]);
		_ ->
			get_words(T,StringListConvLower,Value)
	end.
%%==========================================================================
get_value_from_numlist([],Value) ->
	Value;
get_value_from_numlist([H|Rest],Value) ->
	{_,NumValue} = lists:keyfind([H],1,?NUMBERS),	
	{ok,ValueList}=combine_string(NumValue,Value),
	get_value_from_numlist(Rest,ValueList).

%%==========================================================================
combine_string([],[]) ->
	{ok,[]};
combine_string(String,[]) ->
	{ok,String};	
combine_string([],String) ->
	{ok,String};
combine_string(String1,String2) ->
	{ok,[lists:flatten([X,Y])|| X <- String2,Y <- String1]}.	

%%==========================================================================
check_data(Data,Type) ->
	case Type of
		"number" ->
			check_int([[X]||X<-binary_to_list(Data)]);
		"text" ->
			check_char([[X]||X<-binary_to_list(Data)])
	end.

check_char([]) ->
	ok;
check_char([H|T]) ->
	try list_to_integer(H) of
		_Num ->
			{error,"Invalid_Input: Enter a Valid Word"}
	catch 
		_:_ -> 
			check_char(T)	
	end.

check_int([]) ->
	ok;
check_int([H|T]) ->
	try list_to_integer(H) of
		_Num ->
			check_int(T)
	catch 
		_:_ -> 
			{error,"Invalid_Input: Enter a Valid Number"}
	end.