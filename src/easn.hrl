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

%% Record to handle application state
-record(state, {wx,				% Frame reference
				parse,			% Reference to module which can parse ASN.1
				file, 			% Last used file
				asn,			% Last used ASN.1 specification
				len}).			% Size of parsed file

%% Record for window references
-record(win, {frame,				% Frame reference
			  asn,				% ASN.1 view wxTxtCtrl reference
			  xml,				% XML view wxTxtCtrl reference
			  hex,				% Hex view wxTxtCtrl reference
			  info,				% Information view wxTxtCtrl reference
			  comp,				% Components wxTreeCtrl reference
			  choice}).			% ASN.1 wxComboBox reference
				 
%% Parse result
-record(result, {data,			% Raw output from parsing
				 hex,			% Offset in orignal file as {start, stop}
				 asn,			% Offset in ASN.1 window as {start, stop}
				 xml}).			% Offset in XML window as {start, stop}

%% Records to store application config
-record(config, {files, 		% List of {files, asn} with:
								%	file - full path to file, 
								%	asn  - title as used in wxComboBox
				 asn}). 		% List of compiled asn.1 specifications

-record(asn, {file, 			% Full path to asn.1 specification
			  spec, 			% compiled asn.1 info 
			  version, 			% version of asn.1 specification
			  title,			% String to show in wxComboBox
			  enc}).			% Encoding rules to use

-record(asn_spec, {db, 			% reference to .asn1db files for pretty print & XML output
				   mod, 		% reference to compiled asn1 module
				   root}).		% Root/Main tag in asn.1 specification
