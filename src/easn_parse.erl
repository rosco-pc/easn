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

%%  Still TODO: - Allow editing/re-encoding of decoded data
%%-------------------------------------------------------------------

%% Purpose: General purpose ASN.1 Decoder

-module('easn_parse').
%-export([start/0, view/1]).
%-export([storeASN/1, retrieveASN/1, checkASN/2, updateConfig/2]).
-compile(export_all).

-include("easn.hrl").
-include("asn1_records.hrl").    %% Used for pretty printing. Copied from lib/asn, as compiler can not find:
%-include_lib("asn/src/asn1_records.hrl").
-author("Rob Schmersel <rob@schmersel.net>").

%%
%% Start server
%%
%start(Pid) ->
%  try ets:new(decoded, [named_table])
%  catch
%  end,
%  loop(Pid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
show(Spec, Count, Type) when Type == xml ->
  case ets:lookup(decoded, Count) of
  [] ->                             %% Last record
    ok;
  [{error, Reason}] ->                    %% Error
    io:format("~n<!-- Error: ~s -->~n",[Reason]);
  [Rec] ->                          %% Decoded information
    io:format("~n<!-- Part ~B -->~n", [Count]),
    io:format("~s~n", [to_xml(Spec#asn.spec#asn_spec.root, Rec, 0, Spec#asn.spec#asn_spec.db)]),
    show(Spec, Count+1, Type)
  end;
show(Spec, Count, Type) when Type == asn ->
  case ets:lookup(decoded, Count) of
  [] ->                             %% Last record
    ok;
  [{error, Reason}] ->                    %% Error
    io:format("~nError: ~s~n",[Reason]);
  [Rec] ->                          %% Decoded information
    io:format("~n-- Part ~B~n", [Count]),
    io:format("~s~n", [to_asn(Spec#asn.spec#asn_spec.root, Rec, 0, Spec#asn.spec#asn_spec.db)]),
    show(Spec, Count+1, Type)
  end;
show(_, _, Type) ->
  io:format("Error: Unknown type ~s~n",[Type]).

%%
%% Handle requests
%%
loop(Pid) ->
  receive
  {parse, {FN, Asn_spec}} ->
    io:format("  Start parsing ~s~n",[FN]),
    case parse(FN, Asn_spec) of
    {ok, Count} ->  Pid ! {parse_done, Count};
    Res ->      Pid ! Res
    end,
    io:format("  Parsing finished~n",[]),
    loop(Pid);
  {compile, Asn} ->
    io:format("  Compile ASN.1 Specification ~s~n",[Asn#asn.file]),
    Res = compile(Asn),
    Pid ! {compile_done, Res},
    io:format("  Compile finished~n",[]),
    loop(Pid);
  {retrieve_asn, Asn} ->
    io:format("  Retrieving ASN ~s~n",[Asn#asn.file]),
    Res = retrieveASN(Asn),
    Pid ! {asn_spec, Res},
    loop(Pid);
  {close} ->
    ets:delete(decoded),
    ok;
  %% Unknown commands
  Msg ->
      io:format("  easn_util: Got ~p ~n", [Msg]),
      loop(Pid)
    after 5000 ->
      loop(Pid)
    end.
  
%%
%% Parse file
%%
%% FN   - File to decode
%% Asn - ASN.1 Specification details {codec module, root tag}
%%
parse(FN, Asn_spec) ->
  try ets:new(decoded, [named_table])
  catch
  error:_ -> ets:delete_all_objects(decoded)
  end,
  io:format("test decoded: ~p~n", [ets:first(decoded)]),
  case filelib:is_regular(FN) of
  true ->
    {ok, Bytes} = file:read_file(FN),            %% Read file
    ets:insert(decoded, {0,{size(Bytes), Bytes}}),      %% Store contents 
    parse_asn(Asn_spec, 1, Bytes, 0, size(Bytes));      %% Parse file
  false ->
    {error, io_lib:format("~s does not exist",[FN])}    %% Error
  end.

%%
%% Compile asn. specification if not already done so.
%%
%% ASN_spec - asn.1 specification file name
compile(Asn) ->
  ASN_spec = Asn#asn.file,
  %% Extract specification filename without path & extension
  Base = basename(ASN_spec),                                  %% Get filename without extension
  Dir = filename:dirname(code:which(?MODULE)),
  io:format("~nCompiling asn.1 specification: ~s~n",[ASN_spec]),
  %% Check if specification alrady compiled
  case checkASN(Asn) of                                       %% Check if already compiled version exists
  true -> 
    retrieveASN(Asn);                                         %% asn.1 spec already compiled, copy it
  false -> 
    Enc = Asn#asn.enc,                                        %% Encoding rules used
    try
      ok = asn1ct:compile(ASN_spec,[Enc,undec_rest]),         %% Compile asn.1 specification
      DBFile = filename:join([Dir, Base++".asn1db"]),
      {ok, DB} = ets:file2tab(DBFile),                        %% Open DB
      Tag = get_root(DB),                                     %% Determine 'root' tag
      ets:delete(DB),                                         %% Close DB
      DBs = get_db(ASN_spec),                                 %% get ALL asn1db files for this specification
      io:format("  Root: ~s~n",[Tag]),
      S = #asn_spec{db=DBs, mod=list_to_atom(Base), root=Tag},
      storeASN(Asn#asn{spec=S})
    catch 
      throw:Error -> Error
    end
  end.

%%
%% Get root tag
%%
%% DB - full path to asn1db file
%%
get_root(DB) ->                                               %% Determine root element, with lowest position
  case ets:first(DB) of
    '$end_of_table' -> '';                                    %% Empty table
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
%% Spec   - needed asn.1 data, Module, asn1db and root tag
%% Count  - parts counter
%% Bytes  - binary data to be decoded 
%% Offset - Offset into binary file
%% Size   - Size of binary file
%%
parse_asn(_, Count, <<>>, _, _) -> {ok, Count};               %% Done with parsing
parse_asn(Spec, Count, <<H,T/binary>>, Offset, Size) when H==0 -> %% Skip filler bytes
  parse_asn(Spec, Count, T, Offset+1, Size); 
parse_asn(Spec, Count, Bytes, Start, Size) ->                 %% Parse ASN.1 file
  Mod = Spec#asn_spec.mod,
  case Mod:decode(Spec#asn_spec.root, Bytes) of
    {ok, Dec, T} ->                                           %% Decoded data
      End = Start + size(T),
      ets:insert(decoded, {Count, {Start, End} , Dec}),
      parse_asn(Spec, Count + 1, T, End, Size);               %% Continue parsing
    {ok, Dec} ->                                              %% Last piece of decoded data
      ets:insert(decoded, {Count, {Start, Size} , Dec}),
      {ok, Count};
    Other ->                                                  %% Error
      io:format("Parse error ~p~n",[Other]),
      ets:insert(decoded, {Count, Other}),
      Other                                                   %% Quite parsing
  end.
  
%%-----------------------------------------------------------------------
%% Decode asn1 coded binary into parse tree
%% Handles indefinite length if re-assembly has already been
%% done - should be relatively easy to allow for segmented though
%% as we keep a count of unrequited indefinite length
%% constructor tags.
%%-----------------------------------------------------------------------
asn1_decode(Bin) ->
asn1_decode(Bin, 0).

asn1_decode(<<>>, 0) ->
  [];
asn1_decode(<<0:8, T/binary>>, N) ->
  asn1_decode(T, N);
asn1_decode(Bin, N0) ->
  {Class, Form, Tag, Rest, N} = get_tag(Bin, N0),
  case tag_type(Class, Form, Tag) of
  indefinite_end ->
    asn1_decode(Rest, N);
  tag ->
    {Len, Rest1} = get_length(Rest),
    {Data, Rest2} = get_content(Len, Rest1),
    [{{tag, fmt_class(Class), Tag}, Data}|asn1_decode(Rest2, N)];
  Constructor ->
    case get_length(Rest) of
    {indefinite, Rest1} ->
      [{{Constructor, indef, Class, Tag}, asn1_decode(Rest1,N+1)}];
    {Len, Rest1} ->
      {Data, Rest2} = get_content(Len, Rest1),
      [{{Constructor, Class, Tag}, asn1_decode(Data, 0)}|
       asn1_decode(Rest2, N)]
    end
  end.


%% Get tag data. 0:1, 0:15 gets around compiler
%% bug as I haven't updated my PC yet..
get_tag(<<0:16, Rest/binary>>, 0) ->
  exit(unexpected_end_of_indefinite_length);
get_tag(<<0:16, Rest/binary>>, N) ->
  {indefinite_end, 0, 0, Rest, N-1};
get_tag(<<Class:2, Form:1, Tag:5, Rest/binary>>, N) ->
  {Tag1, Rest1} = get_tag_ext(Tag, Rest),
  {Class, Form, Tag1, Rest1, N}.

%% Handle extension parts of the tag field
get_tag_ext(31, <<0:1, Tag:7, Rest/binary>>) ->
  {Tag, Rest};
get_tag_ext(31, <<1:1, Msb:7, _:1, Lsb:7, Rest/binary>>) ->
  {Msb*128+Lsb, Rest};
get_tag_ext(Tag, Rest) ->
  {Tag, Rest}.

% Do short and long definite length forms
% And *now*... indefinite length!
get_length(<<0:1, Len:7, Rest/binary>>) ->
  {Len, Rest};
get_length(<<1:1, 0:7, Rest/binary>>) ->
  {indefinite, Rest};
get_length(<<1:1, Len_len:7, Rest/binary>>) ->
  <<Len:Len_len/unit:8, Rest1/binary>> = Rest,
  {Len, Rest1}.

% Get actual content of field
get_content(Len, Rest) ->
  <<Data:Len/binary, Rest1/binary>> = Rest,
  {Data, Rest1}.

% tag_type(Class, Form, Tag) -> tag|seq|set|constructor
tag_type(indefinite_end, _, _) -> indefinite_end;
tag_type(Class, 0, Tag) -> tag;
tag_type(0, 1, 16) -> seq;
tag_type(0, 1, 17) -> set;
tag_type(Class, 1, Els) -> constructor.

fmt_class(0) -> univ;
fmt_class(1) -> app;
fmt_class(2) -> context;
fmt_class(3) -> priv. 

%%
%% Human readable printout
%% Use record definition in asn1_records.hrl, 
%% to get the structure of the decoding output
%%
%% Data   - list with decoded data
%% Indent - indentation counter
%% DB     - list of asn1db files
to_asn(Root, Data, Indent, DB) ->
  DefRec = getDefinition(DB, Root),                           %% Get Root definition from DB
  Name = DefRec#typedef.name,                                 %% Name of element
  Def = DefRec#typedef.typespec#type.def,                     %% Type of element
  % io:format("Name: ~p~nRecord: ~p~n",[Name, Data]),
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
  Type = getChoice(element(2, Def), H),                       %% Get spec of actual element
  D1 = Type#type.def,                                         %% Type of element
  [Record] = T,
  Data = to_list(Record),
  N1 = [indent(Indent) | atom_to_list(Name)],
  [io_lib:format("~s ::= CHOICE {~n",[N1]) |
   [write_asn_elem(H, D1, Data, Indent+1, DB) |
   [io_lib:format("~s}~n",[indent(Indent)])]]];
write_asn_elem(_, Def, Data, Indent, DB) when element(1,Def)=='Externaltypereference' -> %% New definition
  DefRec = getDefinition(DB, Def#'Externaltypereference'.type),
  N1 = DefRec#typedef.name,                                   %% Name of element
  D1 = DefRec#typedef.typespec#type.def,                      %% Type of element
  %io_lib:format("Name: ~p~nRecord: ~p~n",[Name, Data]).
  write_asn_elem(N1, D1, Data, Indent, DB);
write_asn_elem(Name, _, Data, Indent, _) ->                   %% BIG HACK, need to check!!
  N1 = [indent(Indent) | atom_to_list(Name)],
  io_lib:format( "~50.49s ~p~n",[N1, Data]).
  
write_asn_comp([], _, _,_) -> [];
write_asn_comp(Data, Comp, Indent, DB) when is_tuple(Comp) ->
  write_asn_comp(Data, element(1, Comp), Indent, DB);
write_asn_comp([DH|DT], [_|CT], Indent, DB) when DH==asn1_NOVALUE ->    %% Skip elements with no value
  write_asn_comp(DT, CT, Indent, DB);
write_asn_comp([DH|DT], [CH|CT], Indent, DB) ->               %% Handle ComponentType
  Def = CH#'ComponentType'.typespec#type.def,
  [write_asn_type(DH, CH, Def, Indent, DB) | [write_asn_comp(DT, CT, Indent, DB)]].
  
write_asn_type([],_,_,_,_) -> [];
write_asn_type(Data, _, Def, Indent, DB) when is_tuple(Def), 
                          element(1, Def)=='Externaltypereference' ->
  Name = Def#'Externaltypereference'.type,
  to_asn(Name, Data, Indent+1, DB);
%   [write_asn_type(T, [], Def, Indent, DB)]];  
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
  DefRec = getDefinition(DB, Root),                           %% Get Root definition from DB
  Name = DefRec#typedef.name,                                 %% Name of element
  Def = DefRec#typedef.typespec#type.def,                     %% Type of element
  %io:format("Name: ~p~nRecord: ~p~n",[Name, Data]).
  write_xml_elem(Name, Def, Data, Indent, DB).

write_xml_elem(Name, Def, Data, Indent, DB) when is_tuple(Data) ->
  write_xml_elem(Name, Def, tuple_to_list(Data), Indent, DB);
write_xml_elem(Name, Def, [_|T], Indent, DB) when element(1,Def)=='SEQUENCE' ->   %% Handle SEQUENCE type
  Components = Def#'SEQUENCE'.components,
  [io_lib:format("~s<~s>~n",[indent(Indent),Name]) | 
   [write_xml_comp(T, Components, Indent+1, DB) |
   [io_lib:format("~s</~s>~n",[indent(Indent),Name])]]];
write_xml_elem(_, Def, Data, Indent, DB) when element(1,Def)=='SEQUENCE OF' ->   %% Handle SEQUENCE OF type
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
write_xml_elem(Name, Def, [_|T], Indent, DB) when element(1,Def)=='SET' ->   %% Handle SET type
  Components = element(1,Def#'SET'.components),
  [io_lib:format( "~s<~s>~n",[indent(Indent), Name]) |
   [write_xml_comp(T, Components, Indent+1, DB) |
   [io_lib:format("~s</~s>~n",[indent(Indent),Name])]]];
write_xml_elem(Name, Def, [H|T], Indent, DB) when element(1,Def)=='CHOICE' -> %% Handle CHOICE type
  %io:format("N: ~p~nD: ~p~nH: ~p~nT: ~p~n",[Name, Def,H,T]),
  Type = getChoice(element(2, Def), H),                       %% Get spec of actual element
  D1 = Type#type.def,                                         %% Type of element
  [Record] = T,
  Data = to_list(Record),
  [io_lib:format("~s<~s>~n",[indent(Indent),Name])|
   [write_xml_elem(H, D1, Data, Indent+1, DB) |
   [io_lib:format("~s</~s>~n",[indent(Indent),Name])]]];
write_xml_elem(_, Def, Data, Indent, DB) when element(1,Def)=='Externaltypereference' -> %% New definition
  DefRec = getDefinition(DB, Def#'Externaltypereference'.type),
  N1 = DefRec#typedef.name,                                   %% Name of element
  D1 = DefRec#typedef.typespec#type.def,                      %% Type of element
  write_xml_elem(N1, D1, Data, Indent, DB);
write_xml_elem(Name, _, Data, Indent, _) ->                   %% BIG HACK, need to check!!
  io_lib:format( "~s<~s>~p</~s>~n",[indent(Indent), Name, Data, Name]).
  
write_xml_comp([], _, _,_) -> [];
write_xml_comp(Data, Comp, Indent, DB) when is_tuple(Comp) ->
  write_xml_comp(Data, element(1, Comp), Indent, DB);
write_xml_comp([DH|DT], [_|CT], Indent, DB) when DH==asn1_NOVALUE ->    %% Skip elements with no value
  write_xml_comp(DT, CT, Indent, DB);
write_xml_comp([DH|DT], [CH|CT], Indent, DB) ->                %% Handle ComponentType
  Def = CH#'ComponentType'.typespec#type.def,
  [write_xml_type(DH, CH, Def, Indent, DB) | [write_xml_comp(DT, CT, Indent, DB)]].
  
write_xml_type([],_,_,_,_) -> [];
write_xml_type(Data, _, Def, Indent, DB) when is_tuple(Def), 
                     element(1, Def)=='Externaltypereference' ->
  Name = Def#'Externaltypereference'.type,
  to_xml(Name, Data, Indent+1, DB);
%   [write_xml_type(T, [], Def, Indent, DB)]];  
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
   
%%
%% Provide a hexadecimal view of the original file
%% fixed format: 
%%  Offset  Data  ASCII 
%%
%% Offset is a 8 byte field
%% Data   show 16 bytes of separated with a space
%% ASCII  Ascii representation of the data
%%
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

indent(0) -> "";                        %% no indent
indent(1) -> "";                        %% Compensate for formatting 
indent(Cnt) -> string:copies(" ", Cnt-1).
empty(0) -> "";
empty(Cnt) -> string:copies("   ", Cnt).

to_ascii(<<H:8, T/binary>>, String) when H < 32;
                     H > 126 ->
  to_ascii(T,["."|String]);                  %% Don't show control codes
to_ascii(<<H:8, T/binary>>, String) ->
  to_ascii(T, [H|String]);                  %% Add character
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
  [] ->     getDefinition(T, Comp);
  [Other] ->   element(2, Other);
  _ ->    []
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
  Base = basename(Asn#asn.file),                              %% Get filename without extension
  Src = filename:dirname(code:which(?MODULE)),
  Dest = asn_dir(Src, Base, Asn#asn.version, Asn#asn.enc),
  %% Delete *.erl and *.hrl files created as part of the compile
  file:delete(filename:join([Src, Base++".erl"])),
  file:delete(filename:join([Src, Base++".hrl"])),
  %% Copy compiled asn.1 files (.beam & .asn1db) and actual asn.1 file for later use
  file:make_dir(Dest),                                        %% Create directory
  A = Asn#asn.spec#asn_spec.db,                               %% .asn1db files
  B = Base++".beam",                                          %% .beam file
  copy_files([B|A], Src, Dest),              
  %% Copy ASN.1 spcification
  New = filename:join([Dest, Base++".asn"]),
  file:copy(Asn#asn.file, New),
  A1 = Asn#asn{file=New},
  %% Create config file
  C = filename:join([Dest, Base++".cfg"]),
  file:write_file(C, io_lib:format("~p.~n",[A1])),
  %% Replace DB filenames with ETS references
  DB = get_db_ref(A, Src, []),
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
%%
%% basename of filename, similar to filename:basename,
%% but remove ALL extension
%%
basename(Data) ->
  basename(lists:reverse(Data), []).
basename([], Res) ->
  Res;
basename([H|T],_Res) when H == $. ->
  basename(T, []);
basename([H|_], Res) when H == $/;
                          H == $\\ ->
  Res;
basename([H|T], Res) ->
  basename(T, [H|Res]).
  
%%
%% extension of filename, similar to filename:extension,
%% but get all extensions, not just the last
%%
extension(Data) ->
  extension(lists:reverse(Data), [], []).
extension([], Res, Ext) ->
  lists:flatten(Ext);
extension([H|T],Res, Ext) when H == $. ->
  extension(T, [], [H | [Res|[Ext]]]);
extension([H|_], _Res, Ext) when H == $/;
                                 H == $\\ ->
  lists:flatten(Ext);
extension([H|T], Res, Ext) ->
  extension(T, [H|Res], Ext).
  
%%
%% Remove char from string
%%
remove(Data, Token) ->
  remove(Data, Token, []).
remove([], _, Res) ->
  lists:reverse(Res);
remove([H|T],Token, Res) when H == Token ->
  remove(T, Token, Res);
remove([H|T], Token, Res) ->
  remove(T, Token, [H|Res]).

%%
%% Replace filename with an ets table reference
%%
get_db_ref([H|T], Src, DB) ->
  {ok, F} = ets:file2tab(filename:join([Src,H])),
  get_db_ref(T, Src, [F|DB]);
get_db_ref([], _, DB) ->
  io:fwrite("  DB references: ~p~n",[DB]),
  DB.

%%
%% Copy multiple files in list
%%
copy_files([], _, _) ->
  ok;
copy_files([H|T], Src, Dest) ->
  file:copy(filename:join([Src, H]), filename:join([Dest, H])),
  copy_files(T, Src, Dest).
  
get_db(File) ->
  case extension(File) of
  ".set.asn" ->              %% Multiple ASN.1 specifications
    {ok, Dev} = file:open(File, [read]),
    get_lines(Dev);
  _ -> 
    [basename(File) ++ ".asn1db"]
  end.

get_lines(Dev) ->
    case io:get_line(Dev, "") of
    eof  -> 
    file:close(Dev), 
    [];
    Line -> 
    [basename(Line)++".asn1db" | get_lines(Dev)]
    end.  
                
