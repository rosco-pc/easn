%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2012. All Rights Reserved.
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
%% File    : view_asn.erl
%% Author  : Rob Schmersel <robert.schmersel@ericsson.com>
%% Description : Generic asn.1 viewer
%%
%% Created :  18 Aug 2012 - Initial release
%% Updated :  15 Sep 2012 - Add support for ASN.1 specification with
%%							import statements (multiple asn1db files
%%							are used in this case)
%%						  - Add support for multiple versions of the 
%%							same ASN.1 specification
%%						  - Add tag in printout
%% Updated :  30 Sep 2012 - Add wxErlang GUI
%%						  - Add hex View/Editor
%%						  - Add XML View
%%	Still TODO: - Allow editing/re-encoding of decoded data
%%-------------------------------------------------------------------

%% Purpose: General purpose ASN.1 Decoder

-module('easn').
%-export([start/0, view/1]).
%-export([storeASN/1, retrieveASN/1, checkASN/2, updateConfig/2]).
-compile(export_all).

-include("easn.hrl").
-include_lib("asn1_records.hrl").


%%
%% Public API
%%

%%
%% Start gui
%%
start() ->
	spawn(easn_gui,start,[self()]),								%% Start GUI
	loop().														%% Wait for parsing request from GUI

%%
%% Start CLI
%%
view([FN, ASN_spec, Version]) ->
	case filelib:is_regular(ASN_spec) of
	  true -> 
	  	%% Compile ASN.1 Spec, get module name and root of ASN.1 spec
		self() ! {compile, self, FN, #asn{file=ASN_spec,version=Version}},
		loop();
	  false ->
		io:fwrite("~s does not exist~n",[ASN_spec])
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Handle requests
%%
loop() ->
	receive
	{parse, From, ReplyAs, Args} ->
		{FN, Asn_spec} = Args,
		Reply = parse(FN, Asn_spec, From, ReplyAs),
		From ! {parse_done, ReplyAs, Reply},
		loop();
	{compile, From, ReplyAs, Asn} ->
		Reply = compile(Asn, From, ReplyAs),
		From ! {compile_done, ReplyAs, Reply},
		loop();
	{close} ->
		ok;
	%% Handle parse adn compile result for CLI version
	{status, _Reply, Msg} ->
		io:format("~s",[Msg]),
		loop();
	{compile_done, FN, Spec} ->
		self() ! {parse, self(), [], {FN, Spec}},
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
%% Spec - ASN.1 Specification details {codec module, root tag}
%%
parse(FN, Spec, From, State) ->
	case filelib:is_regular(FN) of
	  true ->
		%% Parse File
		{ok, Bytes} = file:read_file(FN),
		From ! {hex, State, to_hex(Bytes,0,[])},
		parse_asn(Spec, 1, Bytes, From, State);								
	  false ->
		From ! {error, State, io_lib:format("~s does not exist",[FN])}
	end.

%%
%% Compile asn. specification if not already done so.
%%
%% ASN_spec - asn.1 specification file name
compile(Asn, Dev, ReplyAs) ->
	ASN_spec = Asn#asn.file,
	%% Extract specification filename without path & extension
    Ext = filename:extension(ASN_spec),							%% Get Extension
    Base = filename:basename(ASN_spec,Ext),						%% Get filename without extension
	Dir = filename:dirname(code:which(?MODULE)),
	Dev ! {status, ReplyAs, io_lib:format("~nCompiling asn.1 specification: ~s~n",[ASN_spec])},
	%% Check if specification alrady compiled
	case checkASN(Asn#asn.file, Asn#asn.version) of				%% Check if already compiled version exists
	true -> 
		retrieveASN(Asn);										%% asn.1 spec already compiled, copy it
	false -> 
		ok = asn1ct:compile(ASN_spec,[ber,undec_rest]),			%% Compile asn.1 specification
		DBFile = filename:join([Dir, lists:concat([Base, ".asn1db"])]),
		{ok, DB} = ets:file2tab(DBFile),						%% Read DB
		Tag = get_root(DB),										%% Determine 'root' tag
		Dev ! {status, ReplyAs, io_lib:format("Root: ~s~n",[Tag])},
		S = #asn_spec{db=DB, mod=list_to_atom(Base), root=Tag},
		storeASN(Asn#asn{spec=S}),
		S
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
%% Count - piece counter
%% Bytes - binary data to be decoded 
%%
parse_asn(_, _, <<>>, _, _) -> ok;									%% Done with parsing
parse_asn(Spec, Count, <<H,T/binary>>, Dev, ReplyAs) when H==0 -> 		%% Skip filler bytes
	parse_asn(Spec, Count, T, Dev, ReplyAs); 
parse_asn(Spec, Count, Bytes, Dev, ReplyAs) when is_binary(Bytes) ->		%% Parse ASN.1 file
	Mod = Spec#asn_spec.mod,
	case Mod:decode(Spec#asn_spec.root, Bytes) of
		{ok, Dec, T} ->											%% Decoded data
			Dev ! {status, ReplyAs, io_lib:format("~n~n<!-- CDR #~p -->~n",[Count])},
			%Data = tuple_to_list(Dec),		%% Get data
			Asn = to_asn(Spec#asn_spec.root, Dec, 0, Spec#asn_spec.db),	%% Human readable printout
			Xml = to_xml(Spec#asn_spec.root, Dec, 0, Spec#asn_spec.db), 
			{From, State} = Dev,
			From ! {parse_result, State, Dec, Asn, Xml},
			parse_asn(Spec, Count + 1, T, Dev, ReplyAs);					%% Continue parsing
		{ok, Dec} ->											%% Last piece of decoded data
			Dev ! {status, ReplyAs, io_lib:format("~n~n<!-- CDR #~p -->~n",[Count])},
			%Data = tuple_to_list(Dec),		%% Get data
			%io:fwrite("~p~n",[Data]),
			Asn = to_asn(Spec#asn_spec.root, Dec, 0, Spec#asn_spec.db),	%% Human readable printout
			Xml = to_xml(Spec#asn_spec.root, Dec, 0, Spec#asn_spec.db), 
			{From, State} = Dev,
			From ! {parse_result, State, Dec, Asn, Xml},
			ok;
		Other ->												%% Error
			%%io:format("~p~n",[Other]),							%% Report
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
	%io_lib:format("Name: ~p~nRecord: ~p~n",[Name, Data]).
	write_asn_elem(Name, Def, Data, Indent, DB).

write_asn_elem(Name, Def, Data, Indent, DB) when is_tuple(Data) ->
	write_asn_elem(Name, Def, tuple_to_list(Data), Indent, DB);
write_asn_elem(Name, Def, [_|T], Indent, DB) when element(1,Def)=='SEQUENCE' -> 	%% Handle SEQUENCE type
	Components = Def#'SEQUENCE'.components,
	N1 = [indent(Indent) | atom_to_list(Name)],
	[io_lib:format("~s ::= SEQUENCE {~n",[N1]) | 
	 [write_asn_comp(T, Components, Indent+1, DB) |
	 [io_lib:format("~s}~n",[indent(Indent)])]]];
write_asn_elem(_, Def, Data, Indent, DB) when element(1,Def)=='SEQUENCE OF' -> 	%% Handle SEQUENCE OF type
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
write_asn_elem(Name, Def, [_|T], Indent, DB) when element(1,Def)=='SET' -> 	%% Handle SET type
	Components = element(1,Def#'SET'.components),
	N1 = [indent(Indent) | atom_to_list(Name)],
	[io_lib:format("~s ::= SET {~n",[N1]) | 
	 [write_asn_comp(T, Components, Indent+1, DB) |
	 [io_lib:format("~s}~n",[indent(Indent)])]]];
write_asn_elem(Name, Def, [H|T], Indent, DB) when element(1,Def)=='CHOICE' -> %% Handle CHOICE type
	%io_lib:format("N: ~p~nD: ~p~nH: ~p~nT: ~p~n",[Name, Def,H,T]),
	Type = getChoice(element(1, element(2, Def)), H),			%% Get spec of actual element
	D1 = Type#type.def,											%% Type of element
	[Record] = T,
	Data = to_list(Record),
	N1 = [indent(Indent) | atom_to_list(Name)],
	[io_lib:format("~s ::= CHOICE {~n",[N1]) |
	 [write_asn_elem(H, D1, Data, Indent+1, DB) |
	 [io_lib:format("~s}~n",[indent(Indent)])]]];
write_asn_elem(_, Def, Data, Indent, DB) when element(1,Def)=='Externaltypereference' -> %% New definition
	DefRec = getDefinition(DB, Def#'Externaltypereference'.type),
	N1 = DefRec#typedef.name,									%% Name of element
	D1 = DefRec#typedef.typespec#type.def,						%% Type of element
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
	[{'CONTEXT', Tag} | _] = Comp#'ComponentType'.tags,
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
	[{'CONTEXT', Tag} | _] = Comp#'ComponentType'.tags,
	D1 = [io_lib:format("~2.16.0B", [X]) || X <- Data],
	Name = [indent(Indent) | atom_to_list(Comp#'ComponentType'.name)],
	io_lib:format( "~50.49s [~2.10.0B] ~s~n",[Name, Tag, D1]);
write_asn_type(Data, Comp, _, Indent, _) when is_binary(Data) -> 	
	[{'CONTEXT', Tag} | _] = Comp#'ComponentType'.tags,
	Name = [indent(Indent) | atom_to_list(Comp#'ComponentType'.name)],
	io_lib:format( "~50.49s [~2.10.0B] ~p~n",[Name, Tag, binary_to_list(Data)]);	
write_asn_type(Data, Comp, _, Indent, _)  -> 	
	[{'CONTEXT', Tag} | _] = Comp#'ComponentType'.tags,
	Name = [indent(Indent) | atom_to_list(Comp#'ComponentType'.name)],
	io_lib:format( "~50.49s [~2.10.0B] ~p~n",[Name, Tag, Data]).
	 
to_xml(Root, Data, Indent, DB) -> 
	DefRec = getDefinition(DB, Root),							%% Get Root definition from DB
	Name = DefRec#typedef.name,									%% Name of element
	Def = DefRec#typedef.typespec#type.def,						%% Type of element
	%io_lib:format("Name: ~p~nRecord: ~p~n",[Name, Data]).
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
	%io_lib:format("N: ~p~nD: ~p~nH: ~p~nT: ~p~n",[Name, Def,H,T]),
	Type = getChoice(element(1, element(2, Def)), H),			%% Get spec of actual element
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
		
getChoice([], _) -> [];
getChoice([H|_], Name) when element(1,H) == 'ComponentType',
							H#'ComponentType'.name == Name ->
	H#'ComponentType'.typespec;
getChoice([_|T], Name) -> getChoice(T, Name).

%%
%% Compiled ASN.1 files have no version information included, but it is needed
%% to handle multiple versions of the same specification.
%% In order to handle this correctly it is needed to store and retrieve
%% the files for different versions
%%

%%
%% Check if the asn.1 specification as already been compiled 
%%
checkASN(ASN_spec, Version) ->
	%% Extract specification filename without path & extension
    Ext = filename:extension(ASN_spec),							%% Get Extension
    Base = filename:basename(ASN_spec,Ext),						%% Get filename without extension
	V = string:join(string:tokens(Version,"."),""),				%% remove any '.' from the version number
	Src = filename:dirname(code:which(?MODULE)),
	Dest = filename:join([Src, "..", "asn", lists:concat([Base, ".", V])]),
	filelib:is_dir(Dest).

%%
%% Store compiled asn.1 specification
%%
storeASN(Asn) ->
	ASN_spec = Asn#asn.file,
	%% Extract specification filename without path & extension
    Ext = filename:extension(ASN_spec),							%% Get Extension
    Base = filename:basename(ASN_spec,Ext),						%% Get filename without extension
	V = string:join(string:tokens(Asn#asn.version,"."),""),		%% remove any '.' from the version number
	Src = filename:dirname(code:which(?MODULE)),
	Dest = filename:join([Src, "..", "asn", lists:concat([Base, ".", V])]),
	%% Copy compiled asn.1 files (.beam & .asn1db) and actual asn.1 file for later use
	file:make_dir(Dest),	
	B = lists:concat([Base, ".beam"]),
	file:copy(filename:join([Src, B]), filename:join([Dest, B])),
	D = lists:concat([Base, ".asn1db"]),
	file:copy(filename:join([Src, D]), filename:join([Dest, D])),
	New = filename:join([Src, lists:concat([Base, ".asn"])]),
	file:copy(ASN_spec, New),
	%% Store asn.1 spec (DB, Module, Root Tag) for later use
	C = filename:join([Dest, lists:concat([Base, ".cfg"])]),
	%% Replace ETS reference with DB filename
	file:write_file(C, io_lib:format("~p.~n",[Asn])),
	Asn#asn{file=New}.
	
%%
%% Retrieve asn.1 specification
%%
retrieveASN(Asn) ->
	ASN_spec = Asn#asn.file,
	%% Extract specification filename without path & extension
    Ext = filename:extension(ASN_spec),							%% Get Extension
    Base = filename:basename(ASN_spec,Ext),						%% Get filename without extension
	Dest = filename:dirname(code:which(?MODULE)),				%% Destination Directory
	V = string:join(string:tokens(Asn#asn.version,"."),""),		%% remove any '.' from the version number
	Src = filename:join([Dest, "..", "asn", lists:concat([Base, ".", V])]), %% Source Directory
	io:fwrite("  Source: ~s~nDest  : ~s~nBase  :~s~n",[Src, Dest, Base]),
	%% Copy compiled asn.1 files
	B = lists:concat([Base, ".beam"]),
	file:copy(filename:join([Src, B]), filename:join([Dest, B])),
	%% Copy and open DBs (there can be more then one)
	{ok, [Spec]} = file:consult(filename:join([Src, lists:concat([Base, ".cfg"])])),
	%% Open DB file and replace filename with ETS reference
	DB = getDB(Spec#asn_spec.db, Src, []),							%% Read DB
	Spec#asn_spec{db=DB}.

getDB([H|T], Src, DB) ->
	%file:copy(filename:join([Src, D]), filename:join([Dest, D])),	%% There is no need to copy these
	{ok, F} = ets:file2tab(filename:join([Src,H])),
	getDB(T, Src, [F|DB]);
getDB([], _, DB) ->
	io:fwrite("  DB references: ~p~n",[DB]),
	DB.
	
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
