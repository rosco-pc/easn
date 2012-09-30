%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
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
%%
%%

-ifdef(debug).
-define(dbg(Fmt, Args), ok=io:format("~p: " ++ Fmt, [?LINE|Args])).
-else.
-define(dbg(Fmt, Args), no_debug).
-endif.

-define('RT_BER_BIN',"asn1rt_ber_bin").
-define('RT_PER_BIN',"asn1rt_per_bin").

%% Some encoding are common for BER and PER. Shared code are in RT_COMMON
-define('RT_COMMON',asn1rt_ber_bin).

-define('COMPLETE_ENCODE',1).
-define('TLV_DECODE',2).


-record(module,{pos,name,defid,tagdefault='EXPLICIT',exports={exports,[]},imports={imports,[]}, extensiondefault=empty,typeorval}).

-record('ExtensionAdditionGroup',{number}).
-record('SEQUENCE',{pname=false,tablecinf=false,extaddgroup,components=[]}).
-record('SET',{pname=false,sorted=false,tablecinf=false,components=[]}).
-record('ComponentType',{pos,name,typespec,prop,tags,textual_order}).
-record('ObjectClassFieldType',{classname,class,fieldname,type}).

-record(typedef,{checked=false,pos,name,typespec}).
-record(classdef,{checked=false,pos,name,typespec}).
-record(valuedef,{checked=false,pos,name,type,value,module}).
-record(ptypedef,{checked=false,pos,name,args,typespec}).
-record(pvaluedef,{checked=false,pos,name,args,type,value}).
-record(pvaluesetdef,{checked=false,pos,name,args,type,valueset}).
-record(pobjectdef,{checked=false,pos,name,args,class,def}).
-record(pobjectsetdef,{checked=false,pos,name,args,class,def}).

-record(typereference,{pos,val}).
-record(identifier,{pos,val}).
-record(constraint,{c,e}).
-record('Constraint',{'SingleValue'=no,'SizeConstraint'=no,'ValueRange'=no,'PermittedAlphabet'=no,
		      'ContainedSubtype'=no, 'TypeConstraint'=no,'InnerSubtyping'=no,e=no,'Other'=no}).
-record(simpletableattributes,{objectsetname,c_name,c_index,usedclassfield,
			       uniqueclassfield,valueindex}).
-record(type,{tag=[],def,constraint=[],tablecinf=[],inlined=no}).

-record(objectclass,{fields=[],syntax}).
-record('Object',{classname,gen=true,def}).
-record('ObjectSet',{class,gen=true,uniquefname,set}).

-record(tag,{class,number,type,form=32}). % form = ?CONSTRUCTED
% This record holds information about allowed constraint types per type
-record(cmap,{single_value=no,contained_subtype=no,value_range=no,
		size=no,permitted_alphabet=no,type_constraint=no,
		inner_subtyping=no}).


-record('EXTENSIONMARK',{pos,val}).

% each IMPORT contains a list of 'SymbolsFromModule'
-record('SymbolsFromModule',{symbols,module,objid}).

% Externaltypereference -> modulename '.' typename
-record('Externaltypereference',{pos,module,type}).
% Externalvaluereference -> modulename '.' typename
-record('Externalvaluereference',{pos,module,value}).

