%%
%% %CopyrightBegin%
%%
%% Copyright Rob Schmersel 2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%-------------------------------------------------------------------
%% File    : easn.erl
%% Author  : Rob Schmersel <rob@schmersel.net>
%% Description : Generic asn.1 viewer & editor
%%
%% Created :  18 Aug 2012 - Initial release

%%	Still TODO: - Allow editing/re-encoding of decoded data
%%-------------------------------------------------------------------

%% Purpose: General purpose ASN.1 Decoder

-module('easn').
%-export([start/0, view/1]).
%-export([storeASN/1, retrieveASN/1, checkASN/2, updateConfig/2]).
-compile(export_all).

-include("easn.hrl").
-include("asn1_records.hrl").		%% Used for pretty printing. Copied from lib/asn, as compiler can not find:
%-include_lib("asn/src/asn1_records.hrl").
-author("Rob Schmersel <rob@schmersel.net>").

%%
%% Public API
%%

%%
%% Start gui
%%
start() ->
%	easn_gui:start().											%% Start GUI
	spawn(easn_gui, start, self()),
	loop().

%%
%% Start CLI
%%
view([FN, ASN_spec, Version, Enc, Type]) ->
	case filelib:is_regular(ASN_spec) of
	  true -> 
	  	%% Compile ASN.1 Spec, get module name and root of ASN.1 spec
		Spec = compile(#asn{file=ASN_spec,version=Version, enc=Enc}),
		parse(FN, Spec),
		show(Spec, 1, Type); 
	  false ->
		io:fwrite("~s does not exist~n",[ASN_spec])
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
show(Spec, Count, Type) when Type == xml ->
	case ets:lookup(decoded, Count) of
	[] -> 														%% Last record
		ok;
	[{error, Reason}] ->										%% Error
		io:format("~n<!-- Error: ~s -->~n",[Reason]);
	[Rec] ->													%% Decoded information
		io:format("~n<!-- Part ~B -->~n", [Count]),
		io:format("~s~n", [to_xml(Spec#asn.spec#asn_spec.root, Rec, 0, Spec#asn.spec#asn_spec.db)]),
		show(Spec, Count+1, Type)
	end;
show(Spec, Count, Type) when Type == asn ->
	case ets:lookup(decoded, Count) of
	[] -> 														%% Last record
		ok;
	[{error, Reason}] ->										%% Error
		io:format("~nError: ~s~n",[Reason]);
	[Rec] ->													%% Decoded information
		io:format("~n-- Part ~B~n", [Count]),
		io:format("~s~n", [to_asn(Spec#asn.spec#asn_spec.root, Rec, 0, Spec#asn.spec#asn_spec.db)]),
		show(Spec, Count+1, Type)
	end;
show(_, _, Type) ->
	io:format("Error: Unknown type ~s~n",[Type]).

%%
%% Handle requests
%%
loop() ->
	receive
	{parse, Pid, {FN, Asn_spec}} ->
		io:format("Start parsing ~s~n",[FN]),
		Res = parse(FN, Asn_spec),
		io:format("Parsing finished~n",[]),
		case Res of
		{ok, Count} -> Pid ! {parse_done, Count};
		{error, _} ->  Pid ! Res
		end,
		loop();
	{compile, Pid, Asn} ->
		io:format("Compile ASN.1 Specification ~s~n",[Asn#asn.file]),
		Res = compile(Asn),
		io:format("Compile finished~n",[]),
		Pid ! {compile_done, Res},
		loop();
	{retrieve_asn, Pid, Asn} ->
		io:format("Retrieving ASN ~s~n",[Asn#asn.file]),
		Res = retrieveASN(Asn),
		Pid ! {asn_spec, Res},
		loop();
	{close} ->
		ok;
	%% Handle parse and compile result for CLI version
	{status, _Reply, Msg} ->
		io:format("~s",[Msg]),
		loop();
	{compile_done, FN, Asn} ->
		self() ! {parse, self(), [], {FN, Asn#asn.spec}},
		loop();
	{parse_result, _Data, Asn, _Xml} ->
		io:format("~s",[Asn]),
		loop();
	{parse_done, _Reply, Msg} ->
		io:format("~s",[Msg]),
		ok;
	%% Unknown commands
	Msg ->
	    io:format("Got ~p ~n", [Msg]),
	    loop()
    after 5000 ->
	    loop()
    end.
	
%%
%% Parse file
%%
%% FN   - File to decode
%% Asn - ASN.1 Specification details {codec module, root tag}
%%
parse(FN, Asn_spec) ->
	ets:new(decoded, [named_table]),
	case filelib:is_regular(FN) of
	  true ->
		{ok, Bytes} = file:read_file(FN),						%% Read file
		ets:insert(decoded, {0,{size(Bytes), Bytes}}),			%% Store contents 
		parse_asn(Asn_spec, 1, Bytes);							%% Parse file
	  false ->
		{error, io_lib:format("~s does not exist",[FN])}		%% Error
	end.

%%
%% Compile asn. specification if not already done so.
%%
%% ASN_spec - asn.1 specification file name
compile(Asn) ->
	ASN_spec = Asn#asn.file,
	%% Extract specification filename without path & extension
    Base = basename(ASN_spec),									%% Get filename without extension
	Dir = filename:dirname(code:which(?MODULE)),
	io:format("~nCompiling asn.1 specification: ~s~n",[ASN_spec]),
	%% Check if specification alrady compiled
	case checkASN(Asn) of										%% Check if already compiled version exists
	true -> 
		retrieveASN(Asn);										%% asn.1 spec already compiled, copy it
	false -> 
		Enc = Asn#asn.enc,										%% Encoding rules used
		ok = asn1ct:compile(ASN_spec,[Enc,undec_rest]),			%% Compile asn.1 specification
		DBFile = filename:join([Dir, lists:concat([Base, ".asn1db"])]),
		{ok, DB} = ets:file2tab(DBFile),						%% Read DB
		Tag = get_root(DB),										%% Determine 'root' tag
		ets:delete(DB),
		DBs = checkDB(ASN_spec),								%% get ALL asn1db files produced
		io:format("  Root: ~s~n",[Tag]),
		S = #asn_spec{db=DBs, mod=list_to_atom(Base), root=Tag},
		storeASN(Asn#asn{spec=S})
	end.

%%
%% Get root tag
%%
%% DB - full path to asn1db file
%%
get_root(DB) ->											%% Determine root element, with lowest position
	case ets:first(DB) of
	  '$end_of_table' -> '';							%% Empty table
	  Key ->
		D = ets:lookup_element(DB, Key, 2),
		get_root(DB, Key, Key, element(3,D))
	end.
	
get_root(DB, Key, Root, Tag) ->
	case ets:next(DB, Key) of
	  '$end_of_table' -> Root;
	  Next ->
		D = ets:lookup_element(DB, Next, 2),
		T = element(3, D),
		if T < Tag -> 
		  get_root(DB, Next, Next, T);
		true -> 
		  get_root(DB, Next, Root, Tag) 
		end
	end.
	
%%
%% Parse ASN.1 encoded file
%%
%% Spec  - needed asn.1 data, Module, asn1db and root tag
%% Count - parts counter
%% Bytes - binary data to be decoded 
%%
parse_asn(_, Count, <<>>) -> {ok, Count};									%% Done with parsing
parse_asn(Spec, Count, <<H,T/binary>>) when H==0 -> 		%% Skip filler bytes
	parse_asn(Spec, Count, T); 
parse_asn(Spec, Count, Bytes) ->							%% Parse ASN.1 file
	Mod = Spec#asn_spec.mod,
	%Dev ! {status, io_lib:format("Decoding part ~B", [Count])},
	case Mod:decode(Spec#asn_spec.root, Bytes) of
		{ok, Dec, T} ->											%% Decoded data
			%Asn = to_asn(Spec#asn_spec.root, Dec, 0, Spec#asn_spec.db),	%% Human readable printout
			%Xml = to_xml(Spec#asn_spec.root, Dec, 0, Spec#asn_spec.db), 
			%Dev ! {parse_result, Dec, Asn, Xml},
			ets:insert(decoded, {Count, size(T), Dec}),
			parse_asn(Spec, Count + 1, T);					%% Continue parsing
		{ok, Dec} ->											%% Last piece of decoded data
			%Asn = to_asn(Spec#asn_spec.root, Dec, 0, Spec#asn_spec.db),	%% Human readable printout
			%Xml = to_xml(Spec#asn_spec.root, Dec, 0, Spec#asn_spec.db), 
			%Dev ! {parse_result, Dec, Asn, Xml},
			ets:insert(decoded, {Count, 0, Dec}),
			{ok, Count};
		Other ->												%% Error
			ets:insert(decoded, {Count, Other}),
			Other												%% Quite parsing
	end.

%%
%% Human readable printout
%% Use record definition in asn1_records.hrl, 
%% to get the structure of the decoding output
%%
%% Data   - list with decoded data
%% Indent - indentation counter
%% DB     - list of asn1db files
to_asn(Root, Data, Indent, DB) ->
	DefRec = getDefinition(DB, Root),							%% Get Root definition from DB
	Name = DefRec#typedef.name,									%% Name of element
	Def = DefRec#typedef.typespec#type.def,						%% Type of element
	%io:format("Name: ~p~nRecord: ~p~n",[Name, Data]).
	write_asn_elem(Name, Def, Data, Indent, DB).
	
write_asn_elem(Name, Def, Data, Indent, DB) when is_tuple(Data) ->
	write_asn_elem(Name, Def, tuple_to_list(Data), Indent, DB);
write_asn_elem(Name, Def, [_|T], Indent, DB) when element(1,Def)=='SEQUENCE' -> %% Handle SEQUENCE type
	Components = Def#'SEQUENCE'.components,
	N1 = [indent(Indent) | atom_to_list(Name)],
	[io_lib:format("~s ::= SEQUENCE {~n",[N1]) |
	 [write_asn_comp(T, Components, Indent+1, DB) |
	 [io_lib:format("~s}~n",[indent(Indent)])]]];
write_asn_elem(_, Def, Data, Indent, DB) when element(1,Def)=='SEQUENCE OF' -> %% Handle SEQUENCE OF type
	Type = element(2, Def),
	D1 = Type#type.def,
	case element(1, D1) of
	'Externaltypereference' ->
		N1 = D1#'Externaltypereference'.type,
		N2 = [indent(Indent) | atom_to_list(N1)],
		[io_lib:format( "~50.49s SEQUENCE OF {~n",[N2]) |
		 [[to_asn(N1, X, Indent+1, DB) || X <- Data] |
		 [io_lib:format( "~s}~n", [indent(Indent)])]]];
	_ ->
		io_lib:format("Def: ~p~n",[D1])
	end;
write_asn_elem(Name, Def, [_|T], Indent, DB) when element(1,Def)=='SET' -> %% Handle SET type
	Components = element(1,Def#'SET'.components),
	N1 = [indent(Indent) | atom_to_list(Name)],
	[io_lib:format("~s ::= SET {~n",[N1]) |
	 [write_asn_comp(T, Components, Indent+1, DB) |
	 [io_lib:format("~s}~n",[indent(Indent)])]]];
write_asn_elem(Name, Def, [H|T], Indent, DB) when element(1,Def)=='CHOICE' -> %% Handle CHOICE type
	%io:format("N: ~p~nD: ~p~nH: ~p~nT: ~p~n",[Name, Def,H,T]),
	Type = getChoice(element(2, Def), H),	%% Get spec of actual element
	D1 = Type#type.def,	%% Type of element
	[Record] = T,
	Data = to_list(Record),
	N1 = [indent(Indent) | atom_to_list(Name)],
	[io_lib:format("~s ::= CHOICE {~n",[N1]) |
	 [write_asn_elem(H, D1, Data, Indent+1, DB) |
	 [io_lib:format("~s}~n",[indent(Indent)])]]];
write_asn_elem(_, Def, Data, Indent, DB) when element(1,Def)=='Externaltypereference' -> %% New definition
	DefRec = getDefinition(DB, Def#'Externaltypereference'.type),
	N1 = DefRec#typedef.name,	%% Name of element
	D1 = DefRec#typedef.typespec#type.def,	%% Type of element
	%io_lib:format("Name: ~p~nRecord: ~p~n",[Name, Data]).
	write_asn_elem(N1, D1, Data, Indent, DB);
write_asn_elem(Name, _, Data, Indent, _) ->						%% BIG HACK, need to check!!
	N1 = [indent(Indent) | atom_to_list(Name)],
	io_lib:format( "~50.49s ~p~n",[N1, Data]).
	
write_asn_comp([], _, _,_) -> [];
write_asn_comp(Data, Comp, Indent, DB) when is_tuple(Comp) ->
	write_asn_comp(Data, element(1, Comp), Indent, DB);
write_asn_comp([DH|DT], [_|CT], Indent, DB) when DH==asn1_NOVALUE ->		%% Skip elements with no value
	write_asn_comp(DT, CT, Indent, DB);
write_asn_comp([DH|DT], [CH|CT], Indent, DB) ->								%% Handle ComponentType
	Def = CH#'ComponentType'.typespec#type.def,
	[write_asn_type(DH, CH, Def, Indent, DB) | [write_asn_comp(DT, CT, Indent, DB)]].
	
write_asn_type([],_,_,_,_) -> [];
write_asn_type(Data, _, Def, Indent, DB) when is_tuple(Def), 
										      element(1, Def)=='Externaltypereference' ->
	Name = Def#'Externaltypereference'.type,
	to_asn(Name, Data, Indent+1, DB);
%	 [write_asn_type(T, [], Def, Indent, DB)]];	
write_asn_type(Data, Comp, Def, Indent, DB) when is_tuple(Def), 
										 element(1, Def)=='SEQUENCE OF' ->
	%io:format("Comp: ~p~n", [Comp]),
	Tag = get_tag(Comp#'ComponentType'.tags),
	%% Look-ahead for child definition
	Type = element(2, Def),
	D1 = Type#type.def,
	case element(1, D1) of
	'Externaltypereference' ->
		N1 = D1#'Externaltypereference'.type,
		Name = [indent(Indent) | atom_to_list(Comp#'ComponentType'.name)],
		[io_lib:format( "~50.49s [~2.10.0B] SEQUENCE OF {~n",[Name, Tag]) |
		 [[to_asn(N1, X, Indent+1, DB) || X <- Data] |
		 [io_lib:format( "~s}~n", [indent(Indent)])]]];
	_ ->
		io_lib:format("Def: ~p~n",[D1])
	end;
write_asn_type(Data, Comp, Def, Indent, _) when Def == 'OCTET STRING' -> 
	Tag = get_tag(Comp#'ComponentType'.tags),
	D1 = [io_lib:format("~2.16.0B", [X]) || X <- Data],
	Name = [indent(Indent) | atom_to_list(Comp#'ComponentType'.name)],
	io_lib:format( "~50.49s [~2.10.0B] ~s~n",[Name, Tag, D1]);
write_asn_type(Data, Comp, _, Indent, _) when is_binary(Data) -> 	
	Tag = get_tag(Comp#'ComponentType'.tags),
	Name = [indent(Indent) | atom_to_list(Comp#'ComponentType'.name)],
	io_lib:format( "~50.49s [~2.10.0B] ~p~n",[Name, Tag, binary_to_list(Data)]);	
write_asn_type(Data, Comp, _, Indent, _)  -> 	
	Tag = get_tag(Comp#'ComponentType'.tags),
	Name = [indent(Indent) | atom_to_list(Comp#'ComponentType'.name)],
	io_lib:format( "~50.49s [~2.10.0B] ~p~n",[Name, Tag, Data]).
	 
to_xml(Root, Data, Indent, DB) -> 
	DefRec = getDefinition(DB, Root),							%% Get Root definition from DB
	Name = DefRec#typedef.name,									%% Name of element
	Def = DefRec#typedef.typespec#type.def,						%% Type of element
	%io:format("Name: ~p~nRecord: ~p~n",[Name, Data]).
	write_xml_elem(Name, Def, Data, Indent, DB).

write_xml_elem(Name, Def, Data, Indent, DB) when is_tuple(Data) ->
	write_xml_elem(Name, Def, tuple_to_list(Data), Indent, DB);
write_xml_elem(Name, Def, [_|T], Indent, DB) when element(1,Def)=='SEQUENCE' -> 	%% Handle SEQUENCE type
	Components = Def#'SEQUENCE'.components,
	[io_lib:format("~s<~s>~n",[indent(Indent),Name]) | 
	 [write_xml_comp(T, Components, Indent+1, DB) |
	 [io_lib:format("~s</~s>~n",[indent(Indent),Name])]]];
write_xml_elem(_, Def, Data, Indent, DB) when element(1,Def)=='SEQUENCE OF' -> 	%% Handle SEQUENCE OF type
	Type = element(2, Def),
	D1 = Type#type.def,
	case element(1, D1) of
	'Externaltypereference' ->
		N1 = D1#'Externaltypereference'.type,
		[io_lib:format( "~s<~s>~n",[indent(Indent), N1]) |
		 [[to_xml(N1, X, Indent+1, DB) || X <- Data] |
		 [io_lib:format( "~s</~s>~n",[indent(Indent), N1])]]];
	_ ->
		io_lib:format("Def: ~p~n",[D1])
	end;
write_xml_elem(Name, Def, [_|T], Indent, DB) when element(1,Def)=='SET' -> 	%% Handle SET type
	Components = element(1,Def#'SET'.components),
	[io_lib:format( "~s<~s>~n",[indent(Indent), Name]) |
	 [write_xml_comp(T, Components, Indent+1, DB) |
	 [io_lib:format("~s</~s>~n",[indent(Indent),Name])]]];
write_xml_elem(Name, Def, [H|T], Indent, DB) when element(1,Def)=='CHOICE' -> %% Handle CHOICE type
	%io:format("N: ~p~nD: ~p~nH: ~p~nT: ~p~n",[Name, Def,H,T]),
	Type = getChoice(element(2, Def), H),						%% Get spec of actual element
	D1 = Type#type.def,											%% Type of element
	[Record] = T,
	Data = to_list(Record),
	[io_lib:format("~s<~s>~n",[indent(Indent),Name])|
	 [write_xml_elem(H, D1, Data, Indent+1, DB) |
	 [io_lib:format("~s</~s>~n",[indent(Indent),Name])]]];
write_xml_elem(_, Def, Data, Indent, DB) when element(1,Def)=='Externaltypereference' -> %% New definition
	DefRec = getDefinition(DB, Def#'Externaltypereference'.type),
	N1 = DefRec#typedef.name,									%% Name of element
	D1 = DefRec#typedef.typespec#type.def,						%% Type of element
	write_xml_elem(N1, D1, Data, Indent, DB);
write_xml_elem(Name, _, Data, Indent, _) ->						%% BIG HACK, need to check!!
	io_lib:format( "~s<~s>~p</~s>~n",[indent(Indent), Name, Data, Name]).
	
write_xml_comp([], _, _,_) -> [];
write_xml_comp(Data, Comp, Indent, DB) when is_tuple(Comp) ->
	write_xml_comp(Data, element(1, Comp), Indent, DB);
write_xml_comp([DH|DT], [_|CT], Indent, DB) when DH==asn1_NOVALUE ->		%% Skip elements with no value
	write_xml_comp(DT, CT, Indent, DB);
write_xml_comp([DH|DT], [CH|CT], Indent, DB) ->								%% Handle ComponentType
	Def = CH#'ComponentType'.typespec#type.def,
	[write_xml_type(DH, CH, Def, Indent, DB) | [write_xml_comp(DT, CT, Indent, DB)]].
	
write_xml_type([],_,_,_,_) -> [];
write_xml_type(Data, _, Def, Indent, DB) when is_tuple(Def), 
										 element(1, Def)=='Externaltypereference' ->
	Name = Def#'Externaltypereference'.type,
	to_xml(Name, Data, Indent+1, DB);
%	 [write_xml_type(T, [], Def, Indent, DB)]];	
write_xml_type(Data, Comp, Def, Indent, DB) when is_tuple(Def), 
										 element(1, Def)=='SEQUENCE OF' ->
	%% Look-ahead for child definition
	Type = element(2, Def),
	D1 = Type#type.def,
	case element(1, D1) of
	'Externaltypereference' ->
		N1 = D1#'Externaltypereference'.type,
		Name = atom_to_list(Comp#'ComponentType'.name),
		[io_lib:format("~s<~s>~n",[indent(Indent),Name])|
		 [[to_xml(N1, X, Indent+1, DB) || X <- Data] |
		 [io_lib:format("~s</~s>~n",[indent(Indent),Name])]]];
	_ ->
		io_lib:format("Def: ~p~n",[D1])
	end;
write_xml_type(Data, Comp, Def, Indent, _) when Def == 'OCTET STRING' -> 
	D1 = [io_lib:format("~2.16.0B", [X]) || X <- Data],
	Name = Comp#'ComponentType'.name,
	io_lib:format( "~s<~s>~s</~s>~n",[indent(Indent), Name, D1, Name]);
write_xml_type(Data, Comp, _, Indent, _) when is_binary(Data) -> 	
	Name = Comp#'ComponentType'.name,
	io_lib:format( "~s<~s>~s</~s>~n",[indent(Indent), Name, binary_to_list(Data), Name]);
write_xml_type(Data, Comp, _, Indent, _)  -> 	
	Name = Comp#'ComponentType'.name,
	io_lib:format( "~s<~s>~p</~s>~n",[indent(Indent), Name, Data, Name]).
	 

to_hex(Bytes, Offset, String) ->
	case hex_line(Bytes, Offset) of
	{ok, Line, <<>>, _Offset} ->
		lists:reverse([Line|String]);
	{ok, Line, Rest, Offset1} ->
		to_hex(Rest, Offset1, [Line|String])
	end.

hex_line(<<H/binary>>, Offset) when size(H) < 16 ->
	Off = io_lib:format("~8.16.0B  ",[Offset]),
	Data = [io_lib:format("~2.16.0B ", [X]) || X <- binary_to_list(H)],
	Rest = io_lib:format("~s",[empty(16-size(H))]),
	Ascii = io_lib:format(" ~s~n",[to_ascii(H, [])]),
	{ok, [Off|[Data|[Rest|[Ascii]]]], <<>>, Offset + size(H)};
hex_line(<<H:16/binary, T/binary>>, Offset) ->
	Off = io_lib:format("~8.16.0B  ",[Offset]),
	Data = [io_lib:format("~2.16.0B ", [X]) || X <- binary_to_list(H)],
	Ascii = io_lib:format(" ~s~n",[to_ascii(H, [])]),
	{ok, [Off|[Data|[Ascii]]], T, Offset + 16};
hex_line(<<>>, Offset) ->
	{ok, [], <<>>, Offset}.

indent(0) -> "";												%% no indent
indent(1) -> "";												%% Compensate for formatting 
indent(Cnt) -> string:copies(" ", Cnt-1).
empty(0) -> "";
empty(Cnt) -> string:copies("   ", Cnt).

to_ascii(<<H:8, T/binary>>, String) when H < 32;
										 H > 126 ->
	to_ascii(T,["."|String]);									%% Don't show control codes
to_ascii(<<H:8, T/binary>>, String) ->
	to_ascii(T, [H|String]);									%% Add character
to_ascii(<<>>, String) ->
	lists:reverse(String).

to_string(Int) when Int < 10 ->
	"0" ++ integer_to_list(Int);
to_string(Int) ->
	integer_to_list(Int).

to_list(D) when is_tuple(D) -> tuple_to_list(D);
to_list(D) when is_binary(D) -> binary_to_list(D);
to_list(D) when is_list(D) -> D;
to_list(D) -> [D].

getDefinition([H|T], Comp) ->
	case ets:lookup(H, Comp) of
	[] -> 		getDefinition(T, Comp);
	[Other] -> 	element(2, Other);
	_ ->		[]
	end;
getDefinition([], _) ->
	[].
		
getChoice(Def, Name) when is_tuple(Def) ->
	getChoice(element(1, Def), Name);
getChoice([], _) -> [];
getChoice([H|_], Name) when element(1,H) == 'ComponentType',
							H#'ComponentType'.name == Name ->
	H#'ComponentType'.typespec;
getChoice([_|T], Name) -> getChoice(T, Name).

get_tag([{'CONTEXT', Tag} | _]) -> Tag;
get_tag(_Other) -> 0.

%%
%% Compiled ASN.1 files have no version information included, but it is needed
%% to handle multiple versions of the same specification.
%% In order to handle this correctly it is needed to store and retrieve
%% the files for different versions
%%

%%
%% Check if the asn.1 specification as already been compiled 
%%
checkASN(Asn) ->
	%% Extract specification filename without path & extension
	Base = basename(Asn#asn.file),
	Src = asn_dir(filename:dirname(code:which(?MODULE)), Base, 
				  Asn#asn.version, Asn#asn.enc),
	filelib:is_regular(Src ++ [Base | ".cfg"]).

%%
%% Store compiled asn.1 specification
%%
storeASN(Asn) ->
	%% Extract specification filename without path & extension
    Base = basename(Asn#asn.file),						%% Get filename without extension
	Src = filename:dirname(code:which(?MODULE)),
	Dest = asn_dir(Src, Base, Asn#asn.version, Asn#asn.enc),
	%% Copy compiled asn.1 files (.beam & .asn1db) and actual asn.1 file for later use
	file:make_dir(Dest),	
	Files = Asn#asn.spec#asn_spec.db,
	%% Copy relevant files
	B = Base++".beam",
	copy_files([B|Files], Src, Dest),
	New = filename:join([Src, Base++".asn"]),
	%% Copy ASN.1 spcification
	file:copy(Asn#asn.file, New),
	%% Replace file reference with new location
	A1 = Asn#asn{file=New},
	%% Create config file
	C = filename:join([Dest, Base++".cfg"]),
	file:write_file(C, io_lib:format("~p.~n",[A1])),
	%% Replace DB filenames with ETS references
	DB = get_db_ref(Files, Src, []),
	A1#asn{spec=Asn#asn.spec#asn_spec{db=DB}}.

%%
%% Retrieve asn.1 specification
%%
retrieveASN(Asn) ->
	%% Extract specification filename without path & extension
    Base = basename(Asn#asn.file),
	Dest = filename:dirname(code:which(?MODULE)),
	Src = asn_dir(Dest, Base, Asn#asn.version, Asn#asn.enc),
	%% Copy compiled asn.1 files
	B = Base++".beam",
	file:copy(filename:join([Src, B]), filename:join([Dest, B])),
	%% Get config
	io:format("~p~n",[filename:join([Src, Base++".cfg"])]),
	{ok, [Asn|_]} = file:consult(filename:join([Src, Base++".cfg"])),
	%% Replace DB filenames with ETS references
	DB = get_db_ref(Asn#asn.spec#asn_spec.db, Src, []),
	Asn#asn{spec=Asn#asn.spec#asn_spec{db=DB}}.

asn_dir(Dir, Base, Version, Enc) ->
	[Dir | ["/../asn/" |[Base | [$. |[remove(Version,$.) | [$. |[atom_to_list(Enc)]]]]]]].

basename(Data) ->
	basename(lists:reverse(Data), []).
basename([], Res) ->
	Res;
basename([H|T],_Res) when H == $. ->
	basename(T, []);
basename([H|_], Res) when H == $/ ->
	Res;
basename([H|T], Res) ->
	basename(T, [H|Res]).

remove(Data, Token) ->
	remove(Data, Token, []).
remove([], _, Res) ->
	lists:reverse(Res);
remove([H|T],Token, Res) when H == Token ->
	remove(T, Token, Res);
remove([H|T], Token, Res) ->
	remove(T, Token, [H|Res]).

get_db_ref([H|T], Src, DB) ->
	{ok, F} = ets:file2tab(filename:join([Src,H])),
	get_db_ref(T, Src, [F|DB]);
get_db_ref([], _, DB) ->
	io:fwrite("  DB references: ~p~n",[DB]),
	DB.
	
copy_files([], _, _) ->
	ok;
copy_files([H|T], Src, Dest) ->
	file:copy(filename:join([Src, H]), filename:join([Dest, H])),
	copy_files(T, Src, Dest).
	
checkDB(File) ->
	case list_to_binary(lists:reverse(File)) of
	<<"nsa.set.", _T/binary>> ->							%% Multiple ASN.1 specifications
		{ok, Data} = file:read_file(File),
		get_db_files(Data, <<>>, []);
	_ -> 
		[File]
	end.
	
get_db_files(<<>>, <<>>, Files) ->
	Files;
get_db_files(<<>>, Line, Files) ->
	[binary_to_list(Line)|Files];
get_db_files(<<H:1/binary, T>>, <<>>, Files) when H == 10;
												  H == 13 ->
	get_db_files(T, <<>>, Files);
get_db_files(<<H:1/binary, T>>, Line, Files) when H == 10;
												  H == 13 ->
	get_db_files(T, <<>>, [binary_to_list(Line)|Files]).
								
updateConfig(File, Asn) ->
	Dir = filename:dirname(code:which(?MODULE)),
	FN = filename:join([Dir, "..", lists:concat([?MODULE_STRING, ".cfg"])]),
	{ok, [Config]} = file:consult(FN),
	C = newConfig(File, Asn, Config),
	file:write_file(FN, io_lib:format("~p.~n",[C])).

newConfig({}, Asn, Config) ->
	Config#config{asn=[Asn|Config#config.asn]};
newConfig(File, {}, Config) ->
	Config#config{files=lists:sublist([File|Config#config.files],9)};
newConfig(File, Asn, Config) ->
	Config#config{files=lists:sublist([File|Config#config.files],9),
				  asn=[Asn|Config#config.asn]}.
