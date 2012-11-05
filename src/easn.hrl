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
-record(state, {wx,				              % Frame reference
                find,             		  % Search dialogue 
                parse,			      	    % Reference to module which can parse ASN.1
                file, 			      	    % Last used file
                asn,			              % Last used ASN.1 specification
                config}).	        	    % easn config 

%% Record for window references
-record(win, {frame,			              % Frame reference
              asn,				              % ASN.1 view wxStyledTextCtrl reference
              xml,				              % XML view wxStyledTextCtrl reference
              hex,				              % Hex view wxTextCtrl reference
              info,				              % Information view wxTextCtrl reference
              comp,				              % Components wxTreeCtrl reference
              nb,                 		  % Notebook reference
              choice}).			      	    % ASN.1 wxComboBox reference

%% REcord for search dilaog
-record(search, {dlg,             		  % Reference to dialogue window
                 find,            		  % Text to find
                 start,           		  % Start position
                 current,         		  % Last positon used
                 found,           		  % List with parts containing found text
                 flags}).         		  % Search flags as defined by wxStyledTextCtrl
%% Offsets into different text controls
-record(offset, {count,			     	      % Decoded Part identifier
                 hex,			       	      % Offset in orignal file as {start, stop}
                 asn,			              % Offset in ASN.1 window as {start, stop}
                 xml}).			      	    % Offset in XML window as {start, stop}

%% Records to store application config
-record(config, {files, 		      	% List of {files, asn} with:
										%	file - full path to file, 
										%	asn  - title as used in wxComboBox
                 font}). 		      	% Font used for displaying text

-record(asn, {file, 			        % Full path to asn.1 specification
              spec, 			        % compiled asn.1 info 
              version, 			      	% version of asn.1 specification
              title,			        % String to show in wxComboBox
              enc}).			        % Encoding rules to use

-record(asn_spec, {db, 			      	% reference to .asn1db files for pretty print & XML output
                   mod, 		      	% reference to compiled asn1 module
                   root}).		    	% Root/Main tag in asn.1 specification
