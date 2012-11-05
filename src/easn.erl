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
%% File         : easn.erl
%% Author       : Rob Schmersel <rob@schmersel.net>
%% Description  : GUI for generic asn.1 viewer & editor
%% Created      : 3 See 2012 - Initial Release
%%-------------------------------------------------------------------


-module(easn).
%-export([start/1]).
-compile(export_all).

-include("easn.hrl").
-include_lib("wx/include/wx.hrl").


%%
%% Start CLI
%%
view([FN, ASN_spec, Version, Enc, Type]) ->
  case filelib:is_regular(ASN_spec) of
    true ->
      %% Compile ASN.1 Spec, get module name and root of ASN.1 spec
    Spec = easn_parse:compile(#asn{file=ASN_spec,version=Version, enc=Enc}),
    easn_parse:parse(FN, Spec),
    easn_parse:show(Spec, 1, Type);
    false ->
    io:fwrite("~s does not exist~n",[ASN_spec])
  end.

%%
%% Start GUI
%%
start() ->
  Wx = wx:new(),                                              %% Starts wxwidgets
  Xrc = wxXmlResource:get(),                                  %% use XRC to define appplication layout
  wxXmlResource:initAllHandlers(Xrc),                         %% Initialize ALL handlers (quick and dirty)
  true = wxXmlResource:load(Xrc, rc_dir("main.xrc")),         %% Load main window
  true = wxXmlResource:load(Xrc, rc_dir("asn_dlg.xrc")),      %% Load ASN.1 dialog
  true = wxXmlResource:load(Xrc, rc_dir("search_dlg.xrc")),   %% Load search dialog
  true = wxXmlResource:load(Xrc, rc_dir("menu.xrc")),         %% Load Menu bar
  Frame = wxFrame:new(),                                      %% Create new Window
  myframe(Wx,Frame),                                          %% Build-up window

  %% Get references for later use
  Out = wxXmlResource:xrcctrl(Frame, "out_info", wxTextCtrl),
  Asn = wxXmlResource:xrcctrl(Frame, "out_asn", wxStyledTextCtrl),
  Xml = wxXmlResource:xrcctrl(Frame, "out_xml", wxStyledTextCtrl),
  Hex = wxXmlResource:xrcctrl(Frame, "out_hex", wxTextCtrl),
  Comp = wxXmlResource:xrcctrl(Frame, "components", wxTreeCtrl),
  Book = wxXmlResource:xrcctrl(Frame, "notebook", wxNotebook),
  Recent = wxXmlResource:xrcctrl(Frame, "wxID_FILE", wxMenu),
  Chooser = wxXmlResource:xrcctrl(Frame, "choose", wxComboBox),
  W = #win{frame=Frame, asn=Asn, xml=Xml, info=Out,
           hex=Hex,comp=Comp,choice=Chooser, nb=Book},

  %% Add already known ASN specifications to the ComboBox
  List = scan_asn(),
  [wxComboBox:append(Chooser, C#asn.title, C) || C <- List],

  %% Select last used ASN.1 spec
  Config = get_config(),
  Files = Config#config.files,
  Index = case Files of
      []        -> 0;
      [{_F,A}|_] -> 
        %% Add recent file to menu
        S = size(list_to_tuple(Files)),
        io:format("Recent ~p~n",[Recent]),
        % [add_recent(Recent, Frame, F, C) || {F, _} <- Files, C <- lists:seq(1,S)],
        %% Get index for ASN spec
        wxComboBox:findString(Chooser, A#asn.title)
      end,
  wxComboBox:setSelection(Chooser, Index),
  Asn1 = easn_parse:retrieveASN(wxComboBox:getClientData(Chooser, Index)),  %% Make sure that all files are in place
                                                                            %% and get the correct DB references
 
  wxFrame:show(Frame),                                        %% Show frame
  %Pid = spawn(easn_parse, start, [self()]),                  %% Start the parser
  %loop(#state{wx=W,parse=Pid,asn=Asn1}),                     %% Handle GUI events
  loop(#state{wx=W,asn=Asn1, find=#search{found=[]}}),			%% Handle GUI events
  io:format("Close wx~n",[]),
    wx:destroy().                                             %% Stop wx server.
  %exit(Pid, kill).                                            %% Stop parser

rc_dir(File) ->
  SelfDir = filename:dirname(code:which(?MODULE)),
  filename:join([SelfDir,"rc",File]).

myframe(Parent, Frame) ->
  Xrc = wxXmlResource:get(),
  wxXmlResource:loadFrame(Xrc, Frame, Parent, "main"),
  %% Add wxStyledTextCtrl
  [attachSTC(Xrc, Frame, X) || X <- ["out_asn", "out_xml"]],
  %% Add application icon
  wxTopLevelWindow:setIcon(Frame, wxXmlResource:loadIcon(Xrc,"appicon")),
  %% Load and setup menubar
  wxFrame:setMenuBar(Frame, wxXmlResource:loadMenuBar(Xrc, "menu")),
  %wxFrame:setToolBar(Frame, wxXmlResource:loadToolBar(Xrc, Frame, "toolbar")),
  %% Add Status bar
  wxFrame:createStatusBar(Frame, [{number,1}]),
  connect(Frame).

attachSTC(Xrc, Parent, Name) ->
  Ref = wxStyledTextCtrl:new(Parent, [{size,{800,500}},
                                      {style, ?wxTE_READONLY}]),
  wxStyledTextCtrl:styleSetFont(Ref, ?wxSTC_STYLE_DEFAULT,
                                wxFont:new(9, ?wxMODERN, ?wxNORMAL, ?wxNORMAL)),
  wxXmlResource:attachUnknownControl(Xrc, Name, Ref),
  Ref.

connect(Frame) ->
  %% Handle Close window event
  ok = wxFrame:connect(Frame, close_window),
  %% Handle menu events, make sure that NAME in xrc = wxID_XXXXX
  Def = [?wxID_OPEN, ?wxID_SAVE, ?wxID_SAVEAS, ?wxID_COPY, ?wxID_FIND,
         ?wxID_EXIT, ?wxID_ABOUT, ?wxID_HELP],
  [connect_menu(Id, Frame) || Id <- Def],
  %% Handle non-standard/own menus
  Menus = [importASN, savePart, findNext, findPrev, options],
  [connect_xrcid(Str,Frame) || Str <- Menus],
  %% Handle combobox events
  put(wxXmlResource:getXRCID("choose"), asn_spec),
  Chooser = wxXmlResource:xrcctrl(Frame, "choose", wxComboBox),
  wxComboBox:connect(Chooser, command_combobox_selected),
  %% Handle treelist events
  put(wxXmlResource:getXRCID("components"), comp),
  Comp = wxXmlResource:xrcctrl(Frame, "components", wxTreeCtrl),
  %wxTreeCtrl:connect(Comp, command_tree_item_collapsed),
  %wxTreeCtrl:connect(Comp, command_tree_item_expanded),
  wxTreeCtrl:connect(Comp, command_tree_sel_changed),
  ok.

connect_menu(Id, Frame) ->
  wxFrame:connect(Frame, command_menu_selected, [{id, Id}]).

connect_xrcid(Name,Frame) ->
  ID = wxXmlResource:getXRCID(atom_to_list(Name)),
  put(ID, Name),
  wxFrame:connect(Frame, command_menu_selected, [{id,ID}]).

%%
%% Update Recently used files menu
%%
add_recent(Recent, Frame, File, Cnt) ->
  M = wxMenuItem:new([{id, ?wxID_FILE+Cnt}, {text, File}]),
  wxMenu:append(Recent, M),
  wxFrame:connect(Frame, command_menu_selected,[{id, ?wxID_FILE+Cnt}]).


%% Main event loop
loop(State) ->
  receive
  %% Handle window acions
  #wx{id=Id, event=#wxCommand{}} ->
    loop(handle_cmd(get(Id), Id, State));
  %% Close Application
  #wx{obj=Obj, event=#wxClose{}} when Obj == State#state.wx#win.frame->
    io:format("Close Application~n",[]),
    wxWindow:destroy(State#state.wx#win.frame),               %% Close window
    ok;
  #wx{obj=Obj, event=#wxClose{}} when Obj == State#state.find#search.dlg->
    io:format("Close dialog ~p~n",[Obj]),
    wxDialog:destroy(State#state.find#search.dlg),            %% Close search dialog
    F = State#state.find,
    loop(State#state{find=F#search{dlg=undefined}});
  #wx{id=Id, obj=Ref, event=#wxTree{type=command_tree_sel_changed}=Ev} ->
    loop(handle_tree(get(Id), Ref, Ev, State));
  {show, Count, Offset, Root} ->
    loop(show(State, Count, Offset, Root));
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
    E1 -> filename:basename(Path, E1++E)        %% Deal with .set.asn
    end,
    %% Suggest title
    wxTextCtrl:clear(Title),
    wxTextCtrl:appendText(Title, T),
    loop_asn(State);
  #wx{event=#wxCommand{type=command_button_clicked}, id=?wxID_OK} ->
    E1 = wxRadioBox:getSelection(Enc),
    E2 = wxRadioBox:getString(Enc, E1),
    Res = #asn{file=wxFilePickerCtrl:getPath(File),
               version=wxTextCtrl:getValue(Version),
               title=wxTextCtrl:getValue(Title),
               enc=list_to_atom(string:to_lower(E2))},
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


%% Handle commands
%% Open File
handle_cmd(_, ?wxID_OPEN, State) ->
  io:format("Select file: ",[]),
  Frame = State#state.wx#win.frame,
  FD = wxFileDialog:new(Frame,[{style, ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST},
                               {wildCard, "All Files (*.*)|*|ASN files (*.asn;*.asn1)|*.asn;*.asn1"}]),
  S1 = case wxDialog:showModal(FD) of
  ?wxID_OK ->
    FN = wxFileDialog:getPath(FD),
    io:format("~s~n",[FN]),
    status(State#state.wx#win.info, io_lib:format("Parsing ~s~n",[FN])),
    easn_parse:parse(FN, State#state.asn#asn.spec),
    status(State#state.wx#win.info, io_lib:format("Loading data ...~n",[])),
    %% Get ASN information, with DB filnames
    Asn = State#state.asn,
    Base = easn_parse:basename(Asn#asn.file),
    Dir = filename:dirname(code:which(?MODULE)),
    Src = easn_parse:asn_dir(Dir, Base, Asn#asn.version, Asn#asn.enc),
    {ok, [Cfg|_]} = file:consult(filename:join([Src, Base++".cfg"])),
    CF = filename:join([Dir, "..", ?MODULE_STRING++".cfg"]),
    updateConfig(CF, FN, Cfg),                                %% Update Config file
    show(State#state{file=FN});                               %% Show decoded data
  _ ->
    io:format("-~n"),
    State
  end,
  wxFileDialog:destroy(FD),
  S1;

%% Save selected page in notebook, with name = file.txt
handle_cmd(_, ?wxID_SAVE, State) ->
  File = State#state.file,
  Dir = filename:dirname(File),
  Base = easn_parse:basename(File),
  case wxNotebook:getSelection(State#state.wx#win.nb) of
  1 -> 
    %% Save XML
    wxStyledTextCtrl:saveFile(State#state.wx#win.xml, 
                              filename:join([Dir, Base++".xml"]));
  _ ->
    %% Save ASN.1
    wxStyledTextCtrl:saveFile(State#state.wx#win.asn, 
                              filename:join([Dir, Base++".txt"]))
  end,
  State;

%% Save selected page in Notebook with user specified name
handle_cmd(_, ?wxID_SAVEAS, State) ->
  Frame = State#state.wx#win.frame,
  FD = wxFileDialog:new(Frame, [{style, ?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT}]),
  case wxFileDialog:showModal(FD) of
  ?wxID_OK ->
    Path = wxFileDialog:getPath(FD),
    Out = case filename:extension(Path) of
          ".xml" -> State#state.wx#win.xml;
          _      -> State#state.wx#win.asn
    end,
    wxStyledTextCtrl:saveFile(Out, Path);
  _ ->
     ignore
  end,
  wxDialog:destroy(FD),
  State;

%% Save selected part in selected page, user can specify name
handle_cmd(savePart, _, State) ->
  File = State#state.file,
  Dir = filename:dirname(File),
  Base = easn_parse:basename(File),
  Item = wxTreeCtrl:getSelection(State#state.wx#win.comp),
  Data = wxTreeCtrl:getItemData(State#state.wx#win.comp, Item),
  Count = Data#offset.count,
  {Out, {Start, End}, FN} = case wxNotebook:getSelection(State#state.wx#win.nb) of
                            1 ->  {State#state.wx#win.xml, Data#offset.xml,
                                   io_lib:format("~s_Part-~B.xml",[Base, Count])}; %% Save XML
                            _ ->  {State#state.wx#win.asn, Data#offset.asn,
                                   io_lib:format("~s_Part-~B.txt",[Base, Count])}  %% Save ASN.1
                            end,
  %Line = wxStyledTextCtrl:getFirstVisibleLine(Out),
  Text = wxStyledTextCtrl:getTextRange(Out, Start, End),
  FD = wxFileDialog:new(State#state.wx#win.frame, 
                        [{style, ?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT},
                         {defaultDir, Dir}, {defaultFile, FN}]),
  case wxFileDialog:showModal(FD) of
  ?wxID_OK ->
    file:write_file(wxFileDialog:getPath(FD), Text);
  _ ->
    ignore
  end,
  wxDialog:destroy(FD),
  State;
  
handle_cmd(_, ?wxID_EXIT, State) ->
  io:format("Exit application~n",[]),
  wxFrame:close(State#state.wx#win.frame),         %% Close window
  State;

handle_cmd(_, ?wxID_ABOUT, State) ->
  Frame = State#state.wx#win.frame,
  Msg = lists:flatten(["ASN.1 viewer/editor version 1.0",
             "\n\n",169," 2012 Robert Schmersel"]),
  MD = wxMessageDialog:new(Frame,Msg,
           [{style, ?wxOK bor ?wxICON_INFORMATION},
            {caption, "About"}]),
  wxDialog:showModal(MD),
  wxDialog:destroy(MD),
  State;

handle_cmd(_, ?wxID_COPY, State) ->
  Out = case wxNotebook:getSelection(State#state.wx#win.nb) of
        1 -> State#state.wx#win.xml;
        _ -> State#state.wx#win.asn
        end,
  wxStyledTextCtrl:copy(Out),
  State;

handle_cmd(importASN, _, State) ->
  Frame = State#state.wx#win.frame,
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
  Res = loop_asn({Dlg, File, Version, Title, Enc}), 
  %% Delete the dialog
  wxDialog:destroy(Dlg),
  %% Evaluate dialog loop 
  case Res of
  {ok, Asn_spec} ->
    M1 = io_lib:format("Compiling ASN.1 specification ~s version ~s~n",
                       [Asn_spec#asn.file, Asn_spec#asn.version]),
    status(State#state.wx#win.info, M1),
    case easn_parse:compile(Asn_spec) of
    {error, Reason} ->
      M2 = io_lib:format("Error while compiling ASN.1 specification:~n~p~n",
                         [Reason]),
      status(State#state.wx#win.info, M2),
      State;
    Asn ->
      %% Add new ASN.1 spec to dropdown box and select it
      T = Res#asn.title,
      Chooser = State#state.wx#win.choice,
      wxComboBox:setSelection(Chooser, wxComboBox:append(Chooser, T, Res)),
      status(State#state.wx#win.info, "ASN.1 specification successfuly compiled\n"),
      State#state{asn=Asn}
    end;
  {error, Reason} ->
    io:format("~s dialog~n",[Reason]),
    State
  end;

%% Handle wxComboBox selection change
handle_cmd(asn_spec, _, State) when State#state.file /= undefined ->
  S1 = getASN(State),
  Msg = io_lib:format("Parsing file: ~s~n",[State#state.file]),
  status(S1#state.wx#win.info, Msg),
  easn_parse:parse(S1#state.file, S1#state.asn#asn.spec),
  status(S1#state.wx#win.info, "Loading parts...\n"),
  show(S1);
handle_cmd(asn_spec, _, State) ->
  getASN(State);

%%
%% Handle search dialog commands
%%
handle_cmd(_, ?wxID_FIND, State) ->
  Frame = State#state.wx#win.frame,
  %% Create Dialog
  Dlg = wxDialog:new(),
  Xrc = wxXmlResource:get(),
  true = wxXmlResource:loadDialog(Xrc, Dlg, Frame, "search_dlg"),

  %% Connect buttons
  wxDialog:connect(Dlg, close_window),
  wxDialog:connect(Dlg, command_button_clicked, [{id, ?wxID_OK}]),
  ID = wxXmlResource:getXRCID(atom_to_list(findAll)),
  put(ID, findAll),
  wxDialog:connect(Dlg, command_button_clicked, [{id, ID}]),
  wxDialog:connect(Dlg, command_button_clicked, [{id, ?wxID_CANCEL}]),

  %% Show Dialog
  wxDialog:show(Dlg),
  State#state{find=#search{dlg=Dlg}};

handle_cmd(_, ?wxID_OK, State) ->                             %% Find part containing text
  io:format("  Find text: ",[]),
  F = findText(false, State),
  State#state{find=F};
handle_cmd(findAll, _, State) ->                              %% Find all parts containing text
  io:format("  Find All text: ",[]),
  F = findText(true, State),
  State#state{find=F};
handle_cmd(_, ?wxID_CANCEL, State) ->                         %% Close dialog, do nothing
  wxDialog:close(State#state.find#search.dlg),
  State;
  
%% Handle unknown commands 
handle_cmd(Dialog, Id, State) ->
  io:format("  Not implemented yet ~p (~p) ~n",[Dialog, Id]),
  Dlg = wxMessageDialog:new(State#state.wx#win.frame, "Not implemented yet"),
  wxDialog:showModal(Dlg),
  wxDialog:destroy(Dlg),
  State.

%handle_tree(comp, _, State) when State#state.load == true ->
%  State;
handle_tree(comp, Ref, #wxTree{item=Item, itemOld=Old}, State) when Item == 0 ->
  io:format("  Old Selection: ~B~n",[Old]), 
  State;
%% New selection
handle_tree(comp, Ref, #wxTree{item=Item, itemOld=Old}, State) ->
	io:format("  Selection: ~B, ~p~n",[Item, wxTreeCtrl:getSelections(Ref)]), 
	Offset = case wxTreeCtrl:getRootItem(Ref) of
			 Item -> #offset{asn={0,0}, xml={0,0}};
			 _ 	  -> wxTreeCtrl:getItemData(Ref, Item)
			 end,
	%io:format("  Show ~B: ~p~n",[Item, Offset]),
	case [X || X <- State#state.find#search.found, X==Item] of
	[] ->
		showLine(State#state.wx#win.asn, element(1, Offset#offset.asn)),
		showLine(State#state.wx#win.xml, element(1, Offset#offset.xml)),
		State;
	_ ->
		State#state{find=State#state.find#search{found=[]}}		%% Reset found list
	end;
handle_tree(Id, Ref, Ev, State) ->
  io:format("  Not implemented yet Id : ~p~n  Ev : ~p~n  Ref: ~p~n",[Id, Ev, Ref]),
  State.

getASN(State) ->
  [ets:delete(T) || T <- State#state.asn#asn.spec#asn_spec.db], %% Delete existing tabs
  Item = wxComboBox:getSelection(State#state.wx#win.choice),    %% get new ASN.1 specification
  Data = wxComboBox:getClientData(State#state.wx#win.choice, Item),
  Asn = easn_parse:retrieveASN(Data),							%% retrieve the ASN spec
  Msg = io_lib:format("Change ASN.1 specification to ~s~n",[Asn#asn.title]),
  status(State#state.wx#win.info, Msg),
  State#state{asn=Asn}.

findText(All, State) ->
	Dlg = State#state.find#search.dlg,
	T = wxXmlResource:xrcctrl(Dlg, "findText", wxComboBox),
	Text = wxComboBox:getValue(T),
	Flags = getCheck(Dlg, ["findRE", "findCase", "findWord", "findWrap"], 
						  [?wxSTC_FIND_REGEXP, ?wxSTC_FIND_MATCHCASE, 
						   ?wxSTC_FIND_WHOLEWORD, 8], 0),
	io:format("~s~n  Flags : ~B~n",[Text, Flags]),
	{Out, Id}  = case wxNotebook:getSelection(State#state.wx#win.nb) of
				 1 ->  {State#state.wx#win.xml, "<!-- Part "};
				 _ ->  {State#state.wx#win.asn, "-- Part "}
				 end,
	wxStyledTextCtrl:searchAnchor(Out),
	case wxStyledTextCtrl:searchNext(Out, Flags, Text) of
	-1  -> 
		State;
	Pos -> 
		showLine(Out, Pos),										%% Show found selection
		Part = findPart(Out, Id),								%% get part including found text
		[{_, _, _, _, Item}] = ets:lookup(decoded, Part),		%% Get item 
		wxTreeCtrl:selectItem(State#state.wx#win.comp, Item),	%% Select item
		io:format(" Found ~s in Part ~B~n", [Text, Part]),
		State#state.find#search{found=[item], find=Text, flags=Flags}
	end.
  
findPart(Ref, Id) ->
  findPart(Ref, Id, wxStyledTextCtrl:getCurrentLine(Ref)-3).
findPart(Ref, Id, Line) ->
  Text = wxStyledTextCtrl:getLine(Ref, Line),
  case lists:prefix(Id, Text) of
  true -> extractPart(Text, Id);
  _ ->    findPart(Ref, Id, Line-1)
  end.
extractPart(Text, Id) ->
  Start = length(Id)+1,
  Len = length(Text)-Start,
  io:format("  ~s : ~B, ~B~n",[Text, Start, Len]),
  case lists:suffix(" -->", Text) of
  true ->  list_to_integer(lists:sublist(Text, Start, Len-4));
  false -> list_to_integer(lists:sublist(Text, Start, Len))
  end.

getCheck(Dlg, [CH|CT], [FH|FT], Flags) ->
  CB = wxXmlResource:xrcctrl(Dlg, CH, wxCheckBox),
  F = Flags bor case wxCheckBox:getValue(CB) of
                true -> FH;
                false -> 0
                end,
  getCheck(Dlg, CT, FT, F);
getCheck(_, [], _, Flags) ->
  Flags.

getRadio(Dlg, [RH|RT], [FH|FT], Flags) ->
  RB = wxXmlResource:xrcctrl(Dlg, RH, wxRadioBox),
  F = Flags bor element(wxRadioBox:getSelection(RB)+1, FH),
  getRadio(Dlg, RT, FT, F);
getRadio(_, [], _, Flags) ->
  Flags.
  
scan_asn() ->
  Dir = filename:dirname(code:which(?MODULE)),                  %% Directory
  Src = filename:join([Dir, "../asn"]),                         %% ASN.1 Directory
  case file:list_dir(Src) of
  {ok, Files} -> scan_asn(Files, Src, []);                      %% Read cfg files
  {error, Reason}  -> {error, Reason}
  end.

scan_asn([], _, Res) -> Res;
scan_asn([H|T], Dir, Res) ->
  FN = Dir ++  [$/|H],
  case filelib:is_dir(FN) of
  true ->                                                       %% Get spec
    Base = easn_parse:basename(H),
    case file:consult(filename:join(FN, Base++".cfg")) of
    {error, _} ->    scan_asn(T, Dir, Res);
    {ok, [Asn|_]} ->  scan_asn(T, Dir, [Asn|Res])
    end;
  false ->
    scan_asn(T, Dir, Res)                                       %% Continue
  end.

get_config() ->
  Dir = filename:dirname(code:which(?MODULE)),                  %% get directory
  case file:consult(filename:join([Dir, "..", "easn.cfg"])) of  %% Read config file
  {error, _} ->                                                 %% Nay, an error
    io:format("Error reading config file ~s~n",[filename:join([Dir, "..", "easn.cfg"])]),
    #config{files=[]};
  {ok, [C|_]} ->  C                                             %% Yeah, it was correct
  end.

show(State) ->
  [{_,{Size, Data}}] = ets:lookup(decoded, 0),
  %% Clear text fields
  wxStyledTextCtrl:clearAll(State#state.wx#win.asn),
  wxStyledTextCtrl:clearAll(State#state.wx#win.xml),
  wxTextCtrl:clear(State#state.wx#win.hex),
  % Clear component list
  wxTreeCtrl:deleteAllItems(State#state.wx#win.comp),
  %% Add filename as root
  %io:format("~n~s~n",[filename:basename(State#state.file)]),
  Root = wxTreeCtrl:addRoot(State#state.wx#win.comp,
                            filename:basename(State#state.file), []),
  wxTreeCtrl:expand(State#state.wx#win.comp, Root),             %% Seems to be needed!
  showHex(Data, Size, 0, State),                                %% Show Hex data
  self() ! {show, 1, 32768, Root},
  State.
show(State, Count, Hex, Root) ->
  case ets:lookup(decoded, Count) of
  [] ->                                                         %% Last record
    Msg = io_lib:format("Decoded ~B parts~n",[Count-1]),
    status(State#state.wx#win.info, Msg),
    State;
  [{_, {error, Reason}}] ->                                     %% Error
    Msg = io_lib:format("Decoded ~B parts~nError: ~p~n",[Count-1,Reason]),
    status(State#state.wx#win.info, Msg),
    State;
  [{_, Offset, Tag, Rec, _}] ->                                 %% Decoded information
    %io:format("  Offset: ~B, ~p~n",[Hex, Offset]),
    Hex1 = if element(1, Offset) >= Hex ->                      %% Display more Hex data?
             [{_,{Size, Data}}] = ets:lookup(decoded, 0),
             showHex(Data, 
Size, Hex, State),
             Hex + 32768;
           true ->
             Hex
           end,
    Spec = State#state.asn#asn.spec,
    %% ASN.1 output
    T1 = easn_parse:to_asn(Tag, Rec, 0, Spec#asn_spec.db),
    M1 = io_lib:format("-- Part ~B~n~n~s~n", [Count, T1]),
    Off_asn = insert(State#state.wx#win.asn, M1),
    %% XML output
    T2 = easn_parse:to_xml(Tag, Rec, 0, Spec#asn_spec.db),
    M2 = io_lib:format("<!-- Part ~B -->~n~n~s~n", [Count, T2]),
    Off_xml = insert(State#state.wx#win.xml, M2),
    %% Add item to tree list
    Name = io_lib:format("Part ~B (~s)",[Count, atom_to_list(element(1, element(2, Rec)))]),
    %Root = wxTreeCtrl:getRootItem(State#state.wx#win.comp),
    Item = wxTreeCtrl:appendItem(State#state.wx#win.comp, Root, Name,
                                 [{data, #offset{count=Count,
                                                 hex=Offset,
                                                 asn=Off_asn,
                                                 xml=Off_xml}}]),
    ets:insert(decoded, {Count, Offset, Tag, Rec, Item}),
    case Count of
    1 -> wxTreeCtrl:selectItem(State#state.wx#win.comp, Item);
    _ -> ignore
    end,
    self() ! {show, Count + 1, Hex1, Root},
    State
  end.

insert(Out, Msg) ->
  Start = wxStyledTextCtrl:getLength(Out),
  wxStyledTextCtrl:appendText(Out, Msg),
  Stop = wxStyledTextCtrl:getLength(Out),
  {Start, Stop}.

showLine(Ref, Pos) ->
  L = wxStyledTextCtrl:lineFromPosition(Ref, Pos),
  wxStyledTextCtrl:scrollToLine(Ref, L).
  %$case wxStyledTextCtrl:getFirstVisibleLine(Ref) of
  %L -> ignore;
  %V -> wxStyledTextCtrl:scrollToLine(Ref, L+(L-V))
  %end.

showHex(Data, Size, Start, State) when Start < Size ->
  O = Size - Start,
  End = if
        O > 32768 -> 32768;
        true      -> O
        end,
  <<_H:Start/binary, P:End/binary, _T/binary>> = Data,
  wxTextCtrl:appendText(State#state.wx#win.hex,
                        easn_parse:to_hex(P, Start, []));

showHex(_,_,_,_) ->
  ok.
  
status(Out, Msg) ->
  wxTextCtrl:appendText(Out, Msg).

updateConfig(FN, File, Asn) ->
  {ok, [Config]} = file:consult(FN),
  C = newConfig({File, Asn}, Config),
  file:write_file(FN, io_lib:format("~p.~n",[C])).

newConfig(File, Config) ->
  Config#config{files=lists:sublist([File|Config#config.files],9)}.
