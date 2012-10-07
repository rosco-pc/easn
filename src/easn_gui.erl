%%
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
%% File    : easn_gui.erl
%% Author  : Rob Schmersel <rob@schmersel.net>
%% Description : GUI for generic asn.1 viewer & editor
%%
%% Created :  30 Aug 2012 - Initial release
%% Updated :  30 Sep 2012 - use xrc for GUI

%%	Still TODO: - Allow editing/re-encoding of decoded data
%%-------------------------------------------------------------------


-module(easn_gui).
%-export([start/1]).
-compile(export_all).

-include("easn.hrl").
-include_lib("wx/include/wx.hrl").


start(Pid) ->
    Wx = wx:new(),											    %% Starts wxwidgets
    Xrc = wxXmlResource:get(),									%% use XRC to define appplication layout
    wxXmlResource:initAllHandlers(Xrc),							%% Initialize ALL handlers (quick and dirty)
    true = wxXmlResource:load(Xrc, rc_dir("main.xrc")),			%% Load main window
    true = wxXmlResource:load(Xrc, rc_dir("asn_dlg.xrc")),		%% Load ASN.1 dialog
    true = wxXmlResource:load(Xrc, rc_dir("menu.xrc")),			%% Load Menu bar
    Frame = wxFrame:new(),										%% Create new WIndow
    myframe(Wx,Frame),											%% Build-up window
    
	%% Get references for later use
	Chooser = wxXmlResource:xrcctrl(Frame, "choose", wxComboBox),
	Out = wxXmlResource:xrcctrl(Frame, "out_info", wxTextCtrl),
	Asn = wxXmlResource:xrcctrl(Frame, "out_asn", wxTextCtrl),
	Xml = wxXmlResource:xrcctrl(Frame, "out_xml", wxTextCtrl),
	Hex = wxXmlResource:xrcctrl(Frame, "out_hex", wxTextCtrl),
	Comp = wxXmlResource:xrcctrl(Frame, "components", wxTreeCtrl),
	Recent = wxXmlResource:xrcctrl(Frame, "recent", wxMenu),
	W = #window{frame=Frame, asn=Asn, xml=Xml, info=Out,
				hex=Hex,comp=Comp,choice=Chooser},
				
	%% Add already known ASN specifications to the ComboBox
	List = scan_asn(),
	[wxComboBox:append(Chooser, C#asn.title, C) || C <- List],
	
	%% Select last used ASN.1 spec
	Config = get_config(),
	Index = case Config#config.files of
			[] 		   -> 0;
			[{_F,A}|_] -> wxComboBox:findString(Chooser, A#asn.title)
			end, 
	wxComboBox:setSelection(Chooser, Index),
	Asn1 = easn:retrieveASN(wxComboBox:getClientData(Chooser, Index)),	%% Make sure that all files are in place
																	%% and get the correct DB references
	%% Add recent file to menu
	add_recent(Recent, Config#config.files),
	
	wxFrame:show(Frame),										%% Show frame
	loop(#state{window=W,parse=Pid,asn=Asn1}),					%% Handle GUI events
	io:format("Close wx~n",[]),
    wx:destroy(),												%% Exit.
	Pid ! {close}.

rc_dir(File) ->
	SelfDir = filename:dirname(code:which(?MODULE)),
	filename:join([SelfDir,"rc",File]).

%% Main event loop
loop(State) ->
	io:format("waiting for event~n"),
    receive 
    %% Handle window acions
	#wx{id=Id, event=#wxCommand{}} ->
		io:format("  Got ~p (~s)~n",[Id, get(Id)]),
	    loop(handle_cmd(get(Id), Id, State));
	%% Close Application
	#wx{event=#wxClose{}} ->
		io:format("Close Application~n",[]),
	    wxWindow:destroy(State#state.window#window.frame),		 %% Close window
	    ok;
	%% Handle response to view_asn request
	%% Parsing Encoded file done
	{parse_result, Res} ->
		io:format("  Got partial result",[]),
		Asn = State#state.window#window.asn,
		A1 = wxTextCtrl:getInsertionPoint(Asn),					%% Start of ASN section
		wxTextCtrl:appendText(Asn, ASN),
		A2 = wxTextCtrl:getInsertionPoint(Asn),					%% End fo ASN section
		Xml = State#state.window#window.xml,
		X1 = wxTextCtrl:getInsertionPoint(Xml),					%% Start of XML section
		wxTextCtrl:appendText(Xml, XML),
		X2 = wxTextCtrl:getInsertionPoint(Xml),					%% Start of XML section
		%addComponent(State#state.window#window.comp, #result{asn={A1, A2},xml={X1, X2}}),
		loop(State);
	{parse_done, {ok, Count} ->
		Out = State#state.window#window.info,
		wxTextCtrl:appendText(Out, Msg),
		loop(State);
	%% Compiling ASN.1 specification done
	{compile_done, _State, Asn} ->
		Msg = "ASN.1 specification successfuly compiled",
		Out = State#state.window#window.info,
		wxTextCtrl:appendText(Out, Msg),
		%% Add new ASN.1 spec to dropdown box and select it
		Title = Asn#asn.title,
		Chooser = State#state.window#window.choice,
		wxComboBox:setSelection(Chooser, wxComboBox:append(Chooser, Title, Asn)),
		%% Update State and continue
		loop(State#state{asn=Asn});
	{hex, Data} ->
		io:format("  Got hexview~n",[]),
		Out = State#state.window#window.hex,
		wxTextCtrl:appendText(Out, Data),
		loop(State);
	%% Retrieved ASN data (and files put in place)
	{asn_spec, _State, Asn} ->
		io:format("  Update ASN: ~p~n",[Asn]),
		loop(State#state{asn=Asn});
	%% Status message
	{status, _State, Msg} ->
		Out = State#state.window#window.info,
		wxTextCtrl:appendText(Out, Msg),
		loop(State);
	%% Error Message
	{error, _State, Msg} ->
		Out = State#state.window#window.info,
		wxTextCtrl:appendText(Out, Msg),
		loop(State);
	Ev = #wx{} ->
	    io:format("  Got wxEvent: ~p ~n", [Ev]),
	    loop(State);
	Ev ->
		io:format("  Got ~p~n", [Ev]),
		loop(State)
    end.

%% asn_dialog event loop
loop_asn(State={Dlg, File, Version, Title, Enc}) ->
	receive
  	#wx{event=#wxClose{}} ->
  	    io:format("Closing ASN.1 dialog ~n",[]),
		{error, "Closed"}; 
	#wx{event=#wxCommand{type=command_button_clicked}, id=?wxID_CANCEL} ->
  	    io:format("Cancelling ASN.1 dialog ~n",[]),
		{error, "Cancel"}; 
	#wx{event = #wxFileDirPicker{type = command_filepicker_changed, path = Path}} ->
		E = filename:extension(Path),
		T = case filename:extension(filename:basename(Path, E)) of
			[] -> filename:basename(Path, E);
			E1 -> filename:basename(Path, E1++E)				%% Deal with .set.asn
			end,
		%% Suggest title
		wxTextCtrl:clear(Title),
		wxTextCtrl:appendText(Title, T),
		loop_asn(State);
	#wx{event=#wxCommand{type=command_button_clicked}, id=?wxID_OK} ->
		Res = #asn{file=wxFilePickerCtrl:getPath(File),
				   version=wxTextCtrl:getValue(Version),
				   title=wxTextCtrl:getValue(Title),
				   enc=wxRadioBox:getSelection(Enc)},
  	    io:format("ASN.1 Data ~p~n",[Res]),
		{ok, Res};
	#wx{event=#wxCommand{type=command_button_clicked}, id=Id} ->
		handle_cmd(get(Id), Id, Dlg),
		loop_asn(State);
	Msg ->
	    io:format("ASN.1 dialog Got ~p ~n", [Msg]),
	    loop_asn(State)
    after 5000 ->
	    loop_asn(State)
    end.

myframe(Parent, Frame) ->
    Xrc = wxXmlResource:get(),
    wxXmlResource:loadFrame(Xrc, Frame, Parent, "main"),
    wxTopLevelWindow:setIcon(Frame, wxXmlResource:loadIcon(Xrc,"appicon")),
    %% Load and setup menubar
    wxFrame:setMenuBar(Frame, wxXmlResource:loadMenuBar(Xrc, "menu")),
    %wxFrame:setToolBar(Frame, wxXmlResource:loadToolBar(Xrc, Frame, "main_toolbar")),
    wxFrame:createStatusBar(Frame, [{number,1}]),
    ok = wxFrame:connect(Frame, close_window), 
    connect(Frame).
  
connect(Frame) ->    
	%% Connect 'standard' menus, make sure that NAME in xrc = wxID_XXXXX
    wxFrame:connect(Frame, command_menu_selected, [{id, ?wxID_OPEN}]),
    wxFrame:connect(Frame, command_menu_selected, [{id, ?wxID_SAVE}]),
    wxFrame:connect(Frame, command_menu_selected, [{id, ?wxID_SAVEAS}]),
    wxFrame:connect(Frame, command_menu_selected, [{id, ?wxID_COPY}]),
    wxFrame:connect(Frame, command_menu_selected, [{id, ?wxID_HELP}]),
    wxFrame:connect(Frame, command_menu_selected, [{id, ?wxID_EXIT}]),
    wxFrame:connect(Frame, command_menu_selected, [{id, ?wxID_ABOUT}]),
	%% Connect own menus
    Menus = [importASN, options],
    [connect_xrcid(Str,Frame) || Str <- Menus],
    ok.

connect_xrcid(Name,Frame) ->
    ID = wxXmlResource:getXRCID(atom_to_list(Name)),
    put(ID, Name),
    wxFrame:connect(Frame, command_menu_selected, [{id,ID}]).

%%
%% Update Recently used files menu
%%
add_recent(_, []) -> ok;
add_recent(Recent, Files) ->
	%% Add last used files
	Add = fun(C, Cnt) -> 
			wxMenu:append(Recent, wxMenuItem:new([
            {id,	?wxID_FILE+Cnt},
            {text,	element(1,C)}])), 
			Cnt + 1 
		  end,
	lists:foldl(Add, 0, Files).

%% Handle commands
    
handle_cmd(_, ?wxID_OPEN, State) ->
	io:format("Select file: ",[State]),
	Frame = State#state.window#window.frame,
    FD = wxFileDialog:new(Frame,[{style, ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST}]),
    S1 = case wxDialog:showModal(FD) of
	?wxID_OK ->
		FN = wxFileDialog:getPath(FD),
		io:format("~s~n",[FN]),
		State#state.parse ! {parse, {FN, State#state.asn}},
		State#state{file=FN};
	_ ->
		io:format("-~n"),
	    State
    end,
    wxFileDialog:destroy(FD),
    S1;

handle_cmd(_, ?wxID_SAVE, State) ->
	File = State#state.file,
	Dir = filename:dirname(File),							
    Ext = filename:extension(File),	
    Base = filename:basename(File,Ext),
	%% Save ASN.1
	Path = lists:concat([Dir, "/", Base, ".txt"]), 
	Out = State#state.window#window.asn,
	wxTextCtrl:saveFile(Out, [{file, Path}]),
	%% Save XML
	Path = lists:concat([Dir, "/", Base, ".xml"]), 
	Out = State#state.window#window.xml,
	wxTextCtrl:saveFile(Out, [{file, Path}]),
    State;

handle_cmd(_, ?wxID_SAVEAS, State) ->
	Frame = State#state.window#window.frame,
    FD = wxFileDialog:new(Frame, [{style, ?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT}]),
    case wxFileDialog:showModal(FD) of
	?wxID_OK ->
	    Path = wxFileDialog:getPath(FD),
		Out = case filename:extension(Path) of
			  ".xml" -> State#state.window#window.xml;
			  _      -> State#state.window#window.asn
			  end,
		wxTextCtrl:saveFile(Out, [{file, Path}]);
	_ ->
	    ignore
    end,
    wxDialog:destroy(FD),
    State;

handle_cmd(_, ?wxID_EXIT, State) ->
	io:format("Exit application~n",[]),
    wxFrame:close(State#state.window#window.frame),				 %% Close window
	State;

handle_cmd(_, ?wxID_ABOUT, State) ->
	Frame = State#state.window#window.frame,
    Msg = lists:flatten(["ASN.1 viewer/editor version 1.0",
						 "\n\n",169," 2012 Robert Schmersel"]),
    MD = wxMessageDialog:new(Frame,Msg,
			     [{style, ?wxOK bor ?wxICON_INFORMATION}, 
			      {caption, "About"}]),
    wxDialog:showModal(MD),
    wxDialog:destroy(MD),
	State;

handle_cmd(importASN, _, State) ->
	Frame = State#state.window#window.frame,
	%% Create Dialog
    Dlg = wxDialog:new(),
    Xrc = wxXmlResource:get(),
    true = wxXmlResource:loadDialog(Xrc, Dlg, Frame, "asn_dlg"),
	
	%% Get references
	File = wxXmlResource:xrcctrl(Dlg, "file", wxFilePickerCtrl),
	Version = wxXmlResource:xrcctrl(Dlg, "version", wxTextCtrl),
	Title = wxXmlResource:xrcctrl(Dlg, "title", wxTextCtrl),
	Enc = wxXmlResource:xrcctrl(Dlg, "enc", wxRadioBox),
	
	%% Connect actions
	wxDialog:connect(Dlg, close_window),
	wxFilePickerCtrl:connect(File, command_filepicker_changed, []),
	wxDialog:connect(Dlg, command_button_clicked, [{id, ?wxID_OK}]),
	wxDialog:connect(Dlg, command_button_clicked, [{id, ?wxID_CANCEL}]),
	
	%% Show Dialog
	wxDialog:show(Dlg),
	
	%% Handle dialog loop
	case loop_asn({Dlg, File, Version, Title, Enc}) of
	{ok, Data} ->
	    State#state.parse ! {compile, self(), Data};
	{error, Reason} ->
	    io:format("~s dialog~n",[Reason]),
		Data = State#state.asn
    end,    
	
    %% Delete the dialog
    wxDialog:destroy(Dlg),
	State#state{asn=Data};

handle_cmd(Dialog, Id, State) ->
    io:format("Not implemented yet ~p (~p) ~n",[Dialog, Id]),
	State.
   
scan_asn() ->
	Dir = filename:dirname(code:which(?MODULE)),				%% Directory
	Src = filename:join([Dir, "../asn"]), 						%% ASN.1 Directory
	case file:list_dir(Src) of
	{ok, Files} -> scan_asn(Files, Src, []);					%% Read cfg files
	{error, Reason}  -> {error, Reason}
	end.

scan_asn([], _, Res) -> Res;
scan_asn([H|T], Dir, Res) ->
	FN = Dir ++  [$/|H],
	case filelib:is_dir(FN) of
	true ->														%% Get spec
		Base = easn:basename(H),
		case file:consult(filename:join(FN, Base++".cfg")) of
		{error, _} ->		scan_asn(T, Dir, Res);
		{ok, [Asn|_]} ->	scan_asn(T, Dir, [Asn|Res])
		end;
	false ->												
		scan_asn(T, Dir, Res)									%% Continue
	end.
	
get_config() ->	
	Dir = filename:dirname(code:which(?MODULE)),				%% Directory
	case file:consult(filename:join([Dir, "..", "easn.cfg"])) of
	{error, _} ->	#config{files=[]};
	{ok, [C|_]} ->	C
	end.
