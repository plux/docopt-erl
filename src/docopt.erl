-module(docopt).

-compile([export_all]).

%%%_* Includes ================================================================

-include_lib("eunit/include/eunit.hrl").

%-define(DEBUG, true).

%%%_* Records =================================================================

-record(state, { options = [] :: options()
               , tokens  = [] :: [string()]
               , mode         :: parse_mode()
               }).

%% Parent patterns
-record(one_or_more , {children :: patterns()}).
-record(required    , {children :: patterns()}).
-record(optional    , {children :: patterns()}).
-record(either      , {children :: patterns()}).

%% Child patterns
-record(command  , {name :: string(), value = false     :: any()}).
-record(argument , {name :: string(), value = undefined :: any()}).
-record(option   , { short            :: string()
                   , long             :: string()
                   , argcount = 0     :: non_neg_integer()
                   , value    = false :: any()
                   }).

%%%_* Types ===================================================================
-type state()  :: #state{}.

-type options() :: [#option{}].

-type patterns() :: [pattern()].

-type pattern() :: child_pattern() | parent_pattern().

-type child_pattern()  :: #command{}
                        | #argument{}
                        | #option{}.

-type parent_pattern() :: #one_or_more{}
                        | #required{}
                        | #optional{}
                        | #either{}.

-type parse_mode() :: parse_args | parse_pattern.

%%%_* Code ====================================================================

-spec docopt(string(), string()) -> orddict:orddict().
docopt(Doc, Args) ->
  Usage                = printable_usage(Doc),
  Opts0                = parse_doc_options(Doc),
  {Pattern, Opts1}     = parse_pattern(formal_usage(Usage), Opts0),
  ParsedArgs           = parse_args(Args, Opts1),
  {FixedPattern, Opts} = fix_list_arguments(Pattern, Opts1),
  case match(FixedPattern, ParsedArgs) of
    {true, [], Collected} ->
      debug("\n"
             "args:       ~p\n"
             "usage:      ~p\n"
             "options:    ~p\n"
             "pattern:    ~p\n"
             "fixd patns: ~p\n"
             "parsedargs: ~p\n"
             "collected:  ~p\n"
             "flat patns: ~p\n",
         [Args,Usage,Opts,Pattern,FixedPattern,ParsedArgs,Collected,
          flatten(FixedPattern)]),
      lists:foldl(fun (Pat, Acc) ->
                      orddict:store(name(Pat), value(Pat), Acc)
                  end,
                  orddict:new(),
                  flatten(FixedPattern) ++ Opts ++ Collected);
    _Res -> throw(parse_failure)
  end.

-spec fix_list_arguments(pattern(), options()) -> {pattern(), options()}.
fix_list_arguments(Pat, Opts) ->
  Either    = [children(C) || C <- children(fix_either(Pat))],
  FixThese  = [E || Case <- Either, E <- Case, count(E, Case) > 1],
  FixedPat  = do_fix_list_arguments(Pat, FixThese),
  FixedOpts = [do_fix_list_arguments(Opt, FixThese) || Opt <- Opts],
  {FixedPat, FixedOpts}.

-spec count(any(), list()) -> non_neg_integer().
count(X, Patterns) ->
  length([P || P <- Patterns, X == P]).

-spec do_fix_list_arguments(pattern(), patterns()) -> pattern().
do_fix_list_arguments(Pat, FixThese) ->
  case children(Pat) of
    undefined ->
      case lists:member(Pat, FixThese) of
        false -> Pat;
        true  -> set_default_value(Pat)
      end;
    Children ->
      set_children(Pat, [do_fix_list_arguments(C, FixThese) || C <- Children])
  end.

-spec fix_either(pattern() | patterns()) -> pattern() | patterns().
fix_either(Pat) when not is_list(Pat) ->
  either(lists:map(fun ([[_|_]=P]) -> req(P);
                       (P)         -> req(P)
                   end, fix_either([[Pat]])));
fix_either([]) -> [];
fix_either([Children0|Groups0]) ->
  Groups = lists:foldl(fun(Type, false) ->
                           %% Eeek, matching on record tuple structure..
                           case lists:keyfind(Type, 1, Children0) of
                             false -> false;
                             Pat   ->
                               Children = lists:delete(Pat, Children0),
                               Groups0 ++ fix_either(Pat, Children)
                           end;
                          (_Type, Acc) -> Acc
                       end, false, [either, required, optional, one_or_more]),
  case Groups of
    false  -> [Children0|fix_either(Groups0)];
    Groups -> fix_either(Groups)
  end.

-spec fix_either(parent_pattern(), [child_pattern()]) -> [child_pattern()].
fix_either(#either{}=Pat, Children)      ->
  [[C|Children] || C <- Pat#either.children];
fix_either(#required{}=Pat, Children)    -> [Pat#required.children ++ Children];
fix_either(#optional{}=Pat, Children)    -> [Pat#optional.children ++ Children];
fix_either(#one_or_more{}=Pat, Children) ->
  [Pat#one_or_more.children ++ Pat#one_or_more.children ++ Children].

-spec flatten(pattern()) -> patterns().
flatten(#required{children=Children})    -> flatten_children(Children);
flatten(#either{children=Children})      -> flatten_children(Children);
flatten(#one_or_more{children=Children}) -> flatten_children(Children);
flatten(#optional{children=Children})    -> flatten_children(Children);
flatten(#command{}=Cmd)                  -> [Cmd];
flatten(#option{}=Opt)                   -> [Opt];
flatten(#argument{}=Arg)                 -> [Arg].

-spec flatten_children(patterns()) -> patterns().
flatten_children(Children) ->
  lists:flatten(lists:map(fun flatten/1, Children)).

-spec children(parent_pattern()) -> patterns();
              (child_pattern())  -> undefined.
children(#required{children=Children})    -> Children;
children(#either{children=Children})      -> Children;
children(#one_or_more{children=Children}) -> Children;
children(#optional{children=Children})    -> Children;
children(_)                               -> undefined.

-spec set_children(parent_pattern(), patterns()) -> parent_pattern().
set_children(#required{}    = P, Children) -> P#required{children=Children};
set_children(#either{}      = P, Children) -> P#either{children=Children};
set_children(#one_or_more{} = P, Children) -> P#one_or_more{children=Children};
set_children(#optional{}    = P, Children) -> P#optional{children=Children}.

-spec name(child_pattern()) -> string().
name(#command{name=Name})                 -> Name;
name(#argument{name=Name})                -> Name;
name(#option{long=undefined, short=Name}) -> Name;
name(#option{long=Name})                  -> Name.

-spec value(child_pattern()) -> any().
value(#command{value=Value})  -> Value;
value(#argument{value=Value}) -> Value;
value(#option{value=Value})   -> Value.

-spec set_value(child_pattern(), any()) -> child_pattern().
set_value(#command{}  = P, Value) -> P#command{value=Value};
set_value(#argument{} = P, Value) -> P#argument{value=Value};
set_value(#option{}   = P, Value) -> P#option{value=Value}.

-spec set_default_value(child_pattern()) -> child_pattern().
set_default_value(#argument{}         = P) -> P#argument{value=[]};
set_default_value(#command{}          = P) -> P#command{value=0};
set_default_value(#option{argcount=0} = P) -> P#option{value=0};
set_default_value(#option{}           = P) -> P#option{value=[]}.

-spec parse_doc_options(string()) -> options().
parse_doc_options(Doc) ->
  [_|OptStrings] = re:split(Doc, "^ *-|\\n *-", [{return, list}]),
  [option_parse("-" ++ S) || S <- OptStrings].

-spec strip(string()) -> string().
strip(Str) ->
  StripLeft = fun (S) ->
                  lists:dropwhile(fun(C) -> lists:member(C, [$ , $\n]) end, S)
              end,
  lists:reverse(StripLeft(lists:reverse(StripLeft(Str)))).

-spec option_parse(string()) -> #option{}.
option_parse(Str) ->
  {Options, Desc} = partition(strip(Str), "  "),
  lists:foldl(fun([$-,$-|_] = S, Opt) -> Opt#option{long  = S};
                 ([$-|_]    = S, Opt) -> Opt#option{short = S};
                 (_            , Opt) ->
                  Opt#option{argcount = 1, value = default_value(Desc)}
              end, #option{}, string:tokens(Options, ",= ")).

-spec default_value(string()) -> string() | undefined.
default_value(Desc) ->
  case re:run(Desc,"\\[default: (.*)\\]", [{capture, [1], list}, caseless]) of
    {match, [DefaultValue]} -> DefaultValue;
    nomatch                 -> undefined
  end.

-spec parse_args(string(), options()) -> [child_pattern()].
parse_args(Args, Options) ->
  State = #state{ tokens  = string:tokens(Args, " ")
                , options = Options
                , mode    = parse_args
                },
  parse_args_tokens(State).

-spec parse_args_tokens(state()) -> [child_pattern()].
parse_args_tokens(#state{tokens=[]}) -> [];
parse_args_tokens(State0) ->
  case current(State0) of
    "--"      -> [#argument{value=Arg} || Arg <- tokens(State0)];
    "-"       -> [#argument{value=Arg} || Arg <- tokens(State0)];
    [$-,$-|_] ->
      {Opts, State} = parse_long(State0),
      Opts ++ parse_args_tokens(State);
    [$-|_]    ->
      {Opts, State} = parse_shorts(State0),
      Opts ++ parse_args_tokens(State);
    _         ->
      [#argument{value=current(State0)}|parse_args_tokens(move(State0))]
  end.

current(#state{tokens=[Current|_]}) -> Current;
current(#state{tokens=[]})          -> undefined.
move(#state{tokens=[_|Rest]}=St)    -> St#state{tokens=Rest}.
tokens(#state{tokens=Tokens})       -> Tokens.
rest(#state{tokens=[_|Rest]})       -> Rest.
options(#state{options=Options})    -> Options.
mode(#state{mode=Mode})             -> Mode.

long(#option{long=Long}) -> Long.

-spec parse_long(state()) -> {[#option{}], state()}.
parse_long(State0) ->
  {Raw, Value} = partition(current(State0), "="),
  Opt0 = [O || O <- options(State0), long(O) == Raw],
  Opt1 = case mode(State0) == parse_args andalso Opt0 == [] of
           true  -> [O || O <- options(State0), starts_with(long(O), Raw)];
           false -> Opt0
         end,
  {Opt, State} = case {mode(State0), Opt1} of
                   {parse_pattern, []} ->
                     O = case Value == [] of
                           true  -> #option{long=Raw, argcount=0, value=false};
                           false -> #option{long=Raw, argcount=1, value=undefined}
                         end,
                     {O, move(State0#state{options=[O|options(State0)]})};
                   {parse_args, []} -> throw({Raw, "not recognized"});
                   {_, [O]} -> {O, move(State0)};
                   {_, _}   -> throw({Raw, "is not a unique prefix"})
                 end,
  Rest = tokens(State),
  case Opt#option.argcount of
    1 when Value == [],
           Rest  == [] -> throw({Raw, "requires an argument"});
    1 when Value == [] -> {[Opt#option{value = current(State)}], move(State)};
    1 when Value /= [] -> {[Opt#option{value = Value}], State};
    0 when Value /= [] -> throw({Raw, "must not have an argument"});
    0 when Value == [] -> {[Opt#option{value = true}], State}
  end.

-spec starts_with(string(), string()) -> boolean().
starts_with(Str, SubStr) when is_list(Str), is_list(SubStr) ->
  string:str(Str, SubStr) == 1;
starts_with(_, _) -> false.

-spec parse_shorts(state()) -> {options(), state()}.
parse_shorts(State) ->
  [$-|Str] = current(State),
  parse_shorts(Str, move(State), []).

-spec parse_shorts([string()], state(), options()) -> {options(), state()}.
parse_shorts([], State, Acc) -> {lists:reverse(Acc), State};
parse_shorts([H|T], State, Acc) ->
 case [O || O <- options(State), tl(O#option.short) == [H]] of
   [] when State#state.mode == parse_args ->
     throw({[$-, H], "not recognized", State});
   [] when State#state.mode == parse_pattern ->
     Opt = option([$-, H]),
     parse_shorts(T, add_option(State, Opt), [Opt|Acc]);
   Opt when length(Opt) > 1 -> throw({[$-, H], "specified ambiguously"});
   [Opt] when Opt#option.argcount == 0 ->
     Value = mode(State) == parse_args,
     parse_shorts(T, State, [Opt#option{value = Value}|Acc]);
   [Opt] when Opt#option.argcount == 1 ->
     {Value, Rest} = get_value_shorts(H, T, State),
     {[Opt#option{value = Value}|Acc], Rest}
 end.

-spec add_option(state(), #option{}) -> state().
add_option(State, Option) ->
  State#state{options=[Option|options(State)]}.

-spec option(string()) -> #option{}.
option(ShortName) ->
  #option{short = ShortName, argcount = 0, long = undefined, value = false}.

-spec get_value_shorts(char(), string(), state()) -> {string(), state()}.
get_value_shorts(H, [], State) ->
  case current(State) of
    []      -> throw({H, "requires an argument"});
    Current -> {Current, move(State)}
  end;
get_value_shorts(_, Arg, State) -> {Arg, State}.

-spec printable_usage(string()) -> string().
printable_usage(Doc) ->
  case re:split(Doc, "([Uu][Ss][Aa][Gg][Ee]:)", [{return, list}]) of
    UsageSplit when length(UsageSplit) < 3 ->
      throw("\"usage:\" (case-insensitive) not found.");
    UsageSplit when length(UsageSplit) > 3 ->
      throw("More than one \"usage:\" (case-insensitive)");
    [_|UsageSplit] ->
      L = re:split(lists:flatten(UsageSplit), "\\n\\s*\\n", [{return, list}]),
      strip(hd(L))
  end.

-spec formal_usage(string()) -> string().
formal_usage(PrintableUsage) ->
  %% Split and drop "usage:"
  [_Usage, ProgName|Args] = string:tokens(PrintableUsage, " \n"),
  F = fun (S) when S == ProgName -> ") | (";
          (S)                    -> S
      end,
  "( " ++ string:join(lists:map(F, Args), " ") ++ " )".

-spec parse_pattern(string(), options()) -> {#required{}, options()}.
parse_pattern(Source0, Options) ->
  %% Add spaces around []()| and ...
  Source = re:replace(Source0, "([\\[\\]\\(\\)\\|]|\\.\\.\\.)", " \\1 ",
                      [{return, list}, global]),
  State0 = #state{ tokens  = string:tokens(Source, " ")
                 , options = Options
                 , mode    = parse_pattern
                 },
  {Result, State} = parse_expr(State0),
  debug("state; ~p", [State]),
  {#required{children=Result}, options(State)}.

% expr ::= seq ( '|' seq )* ;
-spec parse_expr(state()) -> {patterns(), state()}.
parse_expr(State0) ->
  {Seq, State} = parse_seq(State0),
  case current(State) of
    "|" -> parse_expr(move(State), [maybe_required_seq(Seq)]);
    _   -> {Seq, State}
  end.

-spec parse_expr(state(), patterns()) -> {patterns(), state()}.
parse_expr(State0, Acc)                 ->
  debug("in parse_expr: ~p, ~p", [tokens(State0), Acc]),
  {Seq, State} = parse_seq(State0),
  debug("in parse_expr after parse_seq: ~p, ~p", [tokens(State), Seq]),
  case current(State) of
    "|" -> parse_expr(move(State), [maybe_required_seq(Seq)|Acc]);
    _   ->
      case lists:reverse([maybe_required_seq(Seq)|Acc]) of
        Result when length(Result) > 1 -> {[#either{children=Result}], State};
        Result                         -> {Result, State} % Needed?
      end
   end.

-spec maybe_required_seq(patterns()) -> pattern().
maybe_required_seq([Seq]) -> Seq;
maybe_required_seq(Seq)   -> #required{children=Seq}.

%% seq ::= ( atom [ '...' ] )* ;
-spec parse_seq(state()) -> {patterns(), state()}.
parse_seq(State) -> parse_seq(State, []).

-spec parse_seq(state(), patterns()) -> {patterns(), state()}.
parse_seq(#state{tokens=[]}      = State, Acc) -> {lists:reverse(Acc), State};
parse_seq(#state{tokens=["]"|_]} = State, Acc) -> {lists:reverse(Acc), State};
parse_seq(#state{tokens=[")"|_]} = State, Acc) -> {lists:reverse(Acc), State};
parse_seq(#state{tokens=["|"|_]} = State, Acc) -> {lists:reverse(Acc), State};
parse_seq(State0, Acc) ->
  debug("in parse seq: ~p, ~p", [tokens(State0), Acc]),
  {Atom, State} = parse_atom(State0),
  debug("in parse seq after parse_atom: ~p, ~p, ~p", [Atom, tokens(State), Acc]),
  case current(State) of
    "..." -> parse_seq(move(State), [#one_or_more{children=Atom}|Acc]);
    _     -> parse_seq(State, Atom ++ Acc)
  end.

%% atom ::= '(' expr ')' | '[' expr ']' | 'options'
%%       | long | shorts | argument | command ;
-spec parse_atom(state()) -> {patterns(), state()}.
parse_atom(State) ->
  debug("in parse atom: ~p", [tokens(State)]),
  case current(State) of
    "["       -> parse_optional(move(State));
    "("       -> parse_required(move(State));
    "options" -> {options(State), move(State)};
    "--"  = C -> {[#command{name=C}], move(State)};
    "-"   = C -> {[#command{name=C}], move(State)};
    [$-,$-|_] -> parse_long(State);
    [$-|_]    -> parse_shorts(State);
    Current   ->
      case is_arg(Current) of
        true  -> {[#argument{name=Current}], move(State)};
        false -> {[#command{name=Current}] , move(State)}
      end
  end.

-spec parse_optional(state()) -> {[#optional{}], state()}.
parse_optional(State0) ->
  debug("parse optional ~p", [State0]),
  {Expr, State} = parse_expr(State0),
  debug("parse optional after parse_expr ~p\n~p", [Expr, State]),
  case current(State) of
    "]" -> {[#optional{children=Expr}], move(State)};
    _   -> throw("Unmatched '['")
  end.

-spec parse_required(state()) -> {[#required{}], state()}.
parse_required(State0) ->
  debug("parse required ~p", [tokens(State0)]),
  {Expr, State} = parse_expr(State0),
  debug("parse required after parse_expr ~p, ~p", [tokens(State), Expr]),
  case current(State) of
    ")" -> {[#required{children=Expr}], move(State)};
    _   -> throw("Unmatched '('")
  end.

-spec is_arg(string()) -> boolean().
is_arg(S) ->
  (hd(S) == $< andalso lists:last(S) == $>) orelse string:to_upper(S) == S.

-spec match(pattern(), patterns()) -> {boolean(), patterns(), patterns()}.
match(Pat, Rest) ->
  {Bool, R, A} = match(Pat, Rest, []),
  {Bool, R, lists:reverse(A)}.

-spec match(pattern(), patterns(), patterns()) ->
               {boolean(), patterns(), patterns()}.
match(#optional{}    = Pat, Rest, Acc) -> match_optional(Pat, Rest, Acc);
match(#required{}    = Pat, Rest, Acc) -> match_required(Pat, Rest, Acc);
match(#either{}      = Pat, Rest, Acc) -> match_either(Pat, Rest, Acc);
match(#one_or_more{} = Pat, Rest, Acc) -> match_one_or_more(Pat, Rest, Acc);
match(                 Pat, Rest, Acc) -> match_child_pattern(Pat, Rest, Acc).

-spec match_optional(#optional{}, patterns(), patterns()) ->
                        {boolean(), patterns(), patterns()}.
match_optional(#optional{children=Children}, Rest0, Acc0) ->
  lists:foldl(fun(Pat, {true, R, A}) ->
                  {_, Rest, Acc} = match(Pat, R, A),
                  {true, Rest, Acc}
              end, {true, Rest0, Acc0}, Children).

-spec match_required(#required{}, patterns(), patterns()) ->
                        {boolean(), patterns(), patterns()}.
match_required(#required{children=Children}, Rest0, Acc0) ->
  lists:foldl(fun(Pat, {true, R, A}) ->
                  case match(Pat, R, A) of
                    {true , _, _} = Res -> Res;
                    {false, _, _}       -> {false, Rest0, Acc0}
                  end;
                 (_, {false, _, _}) -> {false, Rest0, Acc0}
              end, {true, Rest0, Acc0}, Children).

-spec match_either(#either{}, patterns(), patterns()) ->
                      {boolean(), patterns(), patterns()}.
match_either(#either{children=Children}, Rest0, Acc0) ->
  Outcomes = lists:foldl(fun(Pat, Acc) ->
                             case match(Pat, Rest0, Acc0) of
                               {true , _, _} = Res -> [Res|Acc];
                               {false, _, _}       -> Acc
                             end
                         end, [], Children),
  case lists:reverse(Outcomes) of
    []    -> {false, Rest0, Acc0};
    [H|T] -> lists:foldl(fun most_consumed/2, H, T)
  end.

most_consumed({_, R, _}=Res, {_, Min, _}) when length(R) < length(Min) -> Res;
most_consumed({_, _, _}    , Acc)                                      -> Acc.

-spec match_one_or_more(#one_or_more{}, patterns(), patterns()) ->
                           {boolean(), patterns(), patterns()}.
match_one_or_more(#one_or_more{children=[Child]}, Rest0, Acc0) ->
  %% If the child is optional we do not need to consume anything
  ConsumptionNotRequired = is_record(Child, optional),
  case consume_one_or_more(Child, Rest0, Acc0) of
    {Rest0 , Acc0} -> {ConsumptionNotRequired , Rest0 , Acc0};
    {Rest  , Acc}  -> {true                   , Rest  , Acc }
  end.

-spec consume_one_or_more(pattern(), patterns(), patterns()) ->
                             {patterns(), patterns()}.
consume_one_or_more(Pat, Rest0, Acc0) ->
  case match(Pat, Rest0, Acc0) of
    {true, Rest0, Acc0} ->
      {Rest0, Acc0};
    {true, Rest, Acc}  ->
      consume_one_or_more(Pat, Rest, Acc);
    {false, Rest, Acc} ->
      {Rest, Acc}
  end.

-spec match_child_pattern(pattern(), patterns(), patterns()) ->
                             {boolean(), patterns(), patterns()}.
match_child_pattern(Pat, Rest0, Acc0) ->
  case single_match(Pat, Rest0) of
    nomatch              -> {false, Rest0, Acc0};
    {match, Match, Rest} ->
      SameName = [P || P <- Acc0, name(P) == name(Pat)],
      Acc =
        case {value(Pat), SameName} of
          {0 , []}       -> [set_value(Match, 1)|Acc0];
          {0 , [Same|_]} -> replace(Same, set_value(Same, value(Same)+1), Acc0);
          {[], []}       -> [set_value(Match, [value(Match)])|Acc0];
          {[], [Same|_]} ->
            replace(Same, set_value(Same, value(Same) ++ [value(Match)]), Acc0);
          _              -> [Match|Acc0]
        end,
      {true, Rest, Acc}
  end.

-spec replace(any(), any(), list()) -> list().
replace(_Old, New, [])     -> [New];
replace(Old, New, [Old|T]) -> [New|T];
replace(Old, New, [H|T])   -> [H|replace(Old, New, T)].

-spec single_match(pattern(), patterns()) ->
                      nomatch | {match, child_pattern(), patterns()}.
single_match(Pat, Rest) ->
  case lists:filter(match_fun(Pat), Rest) of
    []        -> nomatch;
    [Match|_] -> {match, match_result(Pat, Match), lists:delete(Match, Rest)}
  end.

-spec match_result(child_pattern(), string()) -> child_pattern().
match_result(#option{}   = Opt, O) -> Opt#option{value=O#option.value};
match_result(#argument{} = Arg, A) -> Arg#argument{value=A#argument.value};
match_result(#command{}  = Cmd, _) -> Cmd#command{value=true}.

-spec match_fun(child_pattern()) -> fun((child_pattern()) -> boolean()).
match_fun(#option{}=Opt) ->
  fun(#option{}=O) -> name(O) == name(Opt);
     (_)           -> false
  end;
match_fun(#argument{}) ->
  fun(#argument{}) -> true;
     (_)           -> false
  end;
match_fun(#command{name=Name}) ->
  fun(#argument{value=Arg}) -> Arg == Name;
     (_)                    -> false
  end.

-spec partition(string(), string()) -> {string(), string()}.
partition(Str, Delim) ->
  case string:str(Str, Delim) of
    0 -> {Str, ""};
    I -> {string:substr(Str, 1, I - 1), string:substr(Str, I + length(Delim))}
  end.

-ifdef(DEBUG).
debug(Fmt, Args) -> ct:pal(Fmt, Args).
-else.
debug(_Fmt, _Args) -> ok.
-endif.

%%%_* Tests ===================================================================

replace_test_() ->
  [ ?_assertEqual([a,2,3], replace(1, a, [1,2,3]))
  , ?_assertEqual([a]    , replace(1, a, []))
  , ?_assertEqual([2,3,a], replace(1, a, [2,3]))
  , ?_assertEqual([1,b,3], replace(2, b, [1,2,3]))
  ].

docopt_any_options_test_() ->
  Doc = "Usage: prog [options] A

    -q  Be quiet
    -v  Be verbose.",
  [ ?_assertEqual(([{"-q", false}, {"-v", false}, {"A", "arg"}]),
                  docopt(Doc, "arg"))
  , ?_assertEqual(([{"-q", false}, {"-v", true}, {"A", "arg"}]),
                  docopt(Doc, "-v arg"))
  , ?_assertEqual(([{"-q", true}, {"-v", false}, {"A", "arg"}]),
                  docopt(Doc, "-q arg"))
  ].

docopt_commands_test_() ->
  [ ?_assertEqual(([{"add", true}]) , docopt("Usage: prog add", "add"))
  , ?_assertEqual(([{"add", false}]), docopt("Usage: prog [add]", ""))
  , ?_assertEqual(([{"add", true}]) , docopt("Usage: prog [add]", "add"))
  , ?_assertEqual(([{"add", true}, {"rm", false}]),
                  docopt("Usage: prog (add|rm)", "add"))
  , ?_assertEqual(([{"add", false}, {"rm", true}]),
                  docopt("Usage: prog (add|rm)", "rm"))
  , ?_assertEqual(([{"a", true}, {"b", true}]),
                  docopt("Usage: prog a b", "a b"))
  %% TODO:
  %% , ?_assertThrow(_, docopt("Usage: prog a b", "b a"))
  ].

parse_doc_options_test() ->
  Doc = "-h, --help  Print help message.
         -o FILE     Output file.
         --verbose   Verbose mode.",
  ?assertEqual([ opt("-h", "--help")
               , opt("-o", undefined, 1)
               , opt(undefined, "--verbose")
               ], parse_doc_options(Doc)).

basic_pattern_matching_test_() ->
  %% ( -a N [ -x Z ] )
  P = req([opt("-a"), arg("N"), optional([opt("-x"), arg("Z")])]),
  [ { "-a N"
    , ?_assertEqual({true, [], [opt("-a"), arg("N", 9)]},
                    match(P, [opt("-a"), arg(9)]))}
  , { "-a -x N Z"
    , ?_assertEqual({true, [], [opt("-a"), arg("N",9), opt("-x"), arg("Z", 5)]},
                    match(P, [opt("-a"), opt("-x"), arg(9), arg(5)]))}
  , { "-x N Z # BZZ!"
    , ?_assertEqual({false, [opt("-x"), arg(9), arg(5)], []},
                    match(P, [opt("-x"), arg(9), arg(5)]))}
  ].

allow_double_dash_test_() ->
  Doc = "Usage: prog [-o] [--] <arg>

         -o",
  D = fun(L) -> orddict:from_list(L) end,
  [ ?_assertEqual(D([{"-o", false}, {"<arg>", "-o"}, {"--", true}]),
                  docopt(Doc, "-- -o"))
  , ?_assertEqual(D([{"-o", true}, {"<arg>", "1"}, {"--", false}]),
                  docopt(Doc, "-o 1"))
  , ?_assertThrow(parse_failure,
                  docopt("Usage: prog [-o] <arg>\n\n-o", "-- -o"))
  ].

allow_single_dash_test_() ->
  [ ?_assertEqual([{"-", true}] , docopt("usage: prog [-]", "-"))
  , ?_assertEqual([{"-", false}], docopt("usage: prog [-]", ""))
  ].

allow_empty_pattern_test() ->
  ?assertEqual([], docopt("usage: prog", "")).

docopt_test_() ->
  Doc = "Usage: prog [-vqr] [FILE]
                prog INPUT OUTPUT
                prog --help

  Options:
    -v  print status messages
    -q  report only file names
    -r  show all occurrences of the same error
    --help

  ",
  D = fun(L) -> orddict:from_list(L) end,
  [ ?_assertEqual(D([ {"-v", true}, {"-q", false}, {"-r", false}
                    , {"--help", false}, {"FILE", "file.py"}
                    , {"INPUT", undefined}, {"OUTPUT", undefined}]),
                  docopt(Doc, "-v file.py"))
  , ?_assertEqual(D([ {"-v", true}, {"-q", false}, {"-r", false}
                    , {"--help", false}, {"FILE", undefined}
                    , {"INPUT", undefined}, {"OUTPUT", undefined}]),
                  docopt(Doc, "-v"))
  , ?_assertThrow(parse_failure, docopt(Doc, "-v input.py output.py"))
  , ?_assertThrow({"--fake", "not recognized"}, docopt(Doc, "--fake"))
  %% , ?_assertThrow(parse_failure, docopt(Doc, "--hel"))
  %% TODO: Assert exceptions
  ].

docopt_options_without_description_test_() ->
  [ ?_assertEqual([{"--hello", true}], docopt("usage: prog --hello", "--hello"))
  , ?_assertEqual([{"--hello", undefined}],
                  docopt("usage: prog [--hello=<world>]", ""))
  , ?_assertEqual([{"--hello", "wrld"}],
                  docopt("usage: prog [--hello=<world>]", "--hello wrld"))
  , ?_assertEqual([{"-o", false}], docopt("usage: prog [-o]", ""))
  , ?_assertEqual([{"-o", true}], docopt("usage: prog [-o]", "-o"))
  , ?_assertEqual([{"-o", true}, {"-p", true}, {"-r", false}],
                  docopt("usage: prog [-opr]", "-op"))
  , ?_assertEqual([{"--verbose", false}, {"-v", true}],
                  docopt("usage: git [-v | --verbose]", "-v"))
  , ?_assertEqual([{"--verbose", false}, {"-v", true}, {"remote", true}],
                  docopt("usage: git remote [-v | --verbose]", "remote -v"))

  ].

match_option_test_() ->
  A  = opt("-a"),
  AT = A#option{value = true},
  X  = opt("-x"),
  N  = arg("N"),
  [ ?_assertEqual({true , []    , [AT]}, match(A, [AT]))
  , ?_assertEqual({false, [X]   , []}  , match(A, [X]))
  , ?_assertEqual({false, [N]   , []}  , match(A, [N]))
  , ?_assertEqual({true , [X, N], [A]} , match(A, [X, A, N]))
  , ?_assertEqual({true , [A]   , [AT]}, match(A, [AT, A]))
  ].

match_argument_test_() ->
  A  = arg("V"),
  AV = arg("V", 9),
  V  = arg(9),
  V0 = arg(0),
  OX = opt("-x"),
  OA = opt("-a"),
  [ ?_assertEqual({true , []      , [AV]}, match(A, [AV]))
  , ?_assertEqual({false, [OX]    , []}  , match(A, [OX]))
  , ?_assertEqual({true , [OX, OA], [AV]}, match(A, [OX, OA, V]))
  , ?_assertEqual({true , [V0]    , [AV]}, match(A, [V, V0]))
  ].

match_command_test_() ->
  C  = cmd("c"),
  AC = arg(undefined, "c"),
  CT = cmd("c", true),
  OX = opt("-x"),
  OA = opt("-a"),
  [ ?_assertEqual({true , []      , [CT]}, match(C, [AC]))
  , ?_assertEqual({false, [OX]    , []}  , match(C, [OX]))
  , ?_assertEqual({true , [OX, OA], [CT]}, match(C, [OX, OA, AC]))
  , ?_assertEqual({true , []      , [cmd("rm", true)]},
                  match(either([cmd("add"), cmd("rm")]),
                        [arg(undefined, "rm")]))
  ].

match_optional_test_() ->
  OA = opt("-a"),
  OB = opt("-b"),
  OX = opt("-x"),
  A  = arg("A"),
  AV = arg("A", 9),
  V  = arg(9),
  [ ?_assertEqual({true, []  , [OA]}, match(optional([OA])    , [OA]))
  , ?_assertEqual({true, []  , []}  , match(optional([OA])    , []))
  , ?_assertEqual({true, [OX], []}  , match(optional([OA])    , [OX]))
  , ?_assertEqual({true, []  , [OA]}, match(optional([OA, OB]), [OA]))
  , ?_assertEqual({true, []  , [OB]}, match(optional([OA, OB]), [OB]))
  , ?_assertEqual({true, [OX], []}  , match(optional([OA, OB]), [OX]))
  , ?_assertEqual({true, []  , [AV]}, match(optional([A])     , [V]))
  , ?_assertEqual({true, [OX], [OA, OB]},
                  match(optional([OA, OB]), [OB, OX, OA]))
  ].

match_required_test_() ->
  A = opt("-a"),
  X = opt("-x"),
  [ ?_assertEqual({true , [] , [A]}, match(req([A])   , [A]))
  , ?_assertEqual({false, [] , []} , match(req([A])   , []))
  , ?_assertEqual({false, [X], []} , match(req([A])   , [X]))
  , ?_assertEqual({false, [A], []} , match(req([A, X]), [A]))
  ].

match_either_test_() ->
  OA  = opt("-a"),
  OB  = opt("-b"),
  OC  = opt("-c"),
  OX  = opt("-x"),
  AN  = arg("N"),
  AM  = arg("M"),
  A1  = arg(1),
  A2  = arg(2),
  AN1 = arg("N", 1),
  AM2 = arg("M", 2),
  [ ?_assertEqual({true , []  , [OA]}, match(either([OA, OB])    , [OA]))
  , ?_assertEqual({true , [OB], [OA]}, match(either([OA, OB])    , [OA, OB]))
  , ?_assertEqual({false, [OX], []}  , match(either([OA, OB])    , [OX]))
  , ?_assertEqual({true , [OX], [OB]}, match(either([OA, OB, OC]), [OX, OB]))
  , ?_assertEqual({true , []  , [AN1, AM2]},
                  match(either([AM, req([AN, AM])]), [A1, A2]))
  ].

match_one_or_more_test_() ->
  A  = arg("A"),
  A8 = arg("A", 8),
  A9 = arg("A", 9),
  V8 = arg(undefined, 8),
  V9 = arg(undefined, 9),
  OA = opt("-a"),
  OX = opt("-x"),
  [ ?_assertEqual({true , []  , [A9]}, match(one_or_more([A]), [V9]))
  , ?_assertEqual({false, []  , []}  , match(one_or_more([A]), []))
  , ?_assertEqual({false, [OX], []}  , match(one_or_more([A]), [OX]))
  , ?_assertEqual({true, [], [A9, A8]}, match(one_or_more([A]), [V9, V8]))
  , ?_assertEqual({true, [OX], [A9, A8]}, match(one_or_more([A]), [V9, OX, V8]))
  , ?_assertEqual({true, [V8], [OA, OA]},
                  match(one_or_more([OA]), [OA, V8, OA]))
  , ?_assertEqual({false, [V8, OX], []}, match(one_or_more([OA]), [V8, OX]))
  , ?_assertEqual({true, [OX], [OA, A8, OA, A9]},
                  match(one_or_more([req([OA, A])]), [OA, V8, OX, OA, V9]))
  , ?_assertEqual({true, [], [A9]}, match(one_or_more([optional([A])]), [V9]))
  , ?_assertEqual({true, [], []}, match(one_or_more([optional([A])]), []))
  ].

parse_atom_test_() ->
  O = [ #option{short="-h"}
      , #option{short="-v", long="--verbose"}
      , #option{short="-f", long="--file", argcount=1}
      ],
  St = fun(Tokens) ->
           #state{ options = O
                 , tokens  = Tokens
                 , mode    = parse_pattern}
       end,
  [ ?_assertEqual({[#argument{name="FOO"}], St([])},
                  parse_atom(St(["FOO"])))
  , ?_assertEqual({[#argument{name="<foo>"}], St([])},
                  parse_atom(St(["<foo>"])))
  , ?_assertEqual({[#command{name="foo"}], St([])},
                  parse_atom(St(["foo"])))
  , ?_assertEqual({O, St([])},
                  parse_atom(St(["options"])))
  , ?_assertEqual({[#option{short="-v", long="--verbose", value=true}], St([])},
                  parse_atom(St(["--verbose"])))
  , ?_assertEqual({[#option{short="-h"}], St([])},
                  parse_atom(St(["-h"])))
  , ?_assertEqual({[#required{children=[#argument{name="FOO"}]}], St([])},
                  parse_atom(St(["(", "FOO", ")"])))
  ].

printable_and_formal_usage_test_() ->
  Doc =
    "Usage: prog [-hv] ARG
            prog N M

     prog is a program.",
  [ ?_assertEqual("Usage: prog [-hv] ARG\n            prog N M",
                  printable_usage(Doc))
  , ?_assertEqual("( [-hv] ARG ) | ( N M )",
                  formal_usage(printable_usage(Doc)))
  , ?_assertEqual("uSaGe: prog ARG",
                  printable_usage("uSaGe: prog ARG\n\t \t\n bla"))
  ].

parse_args_test_() ->
  HelpOpt    = #option{short="-h", value=true},
  FileOpt    = #option{short="-f", long="--file", argcount=1, value="f.txt"},
  VerboseOpt = #option{short="-v", long="--verbose", value=true},
  Arg        = fun(V) -> #argument{value=V} end,
  O = [ #option{short="-h"}
      , #option{short="-v", long="--verbose"}
      , #option{short="-f", long="--file", argcount=1}
      ],
  [ ?_assertEqual([]                    , parse_args(""                , O))
  , ?_assertEqual([HelpOpt]             , parse_args("-h"              , O))
  , ?_assertEqual([HelpOpt, VerboseOpt] , parse_args("-h -v"           , O))
  , ?_assertEqual([HelpOpt, VerboseOpt] , parse_args("-hv"             , O))
  , ?_assertEqual([HelpOpt, FileOpt]    , parse_args("-h -f f.txt"     , O))
  , ?_assertEqual([HelpOpt, FileOpt]    , parse_args("-h -ff.txt"      , O))
  , ?_assertEqual([HelpOpt, FileOpt]    , parse_args("-h --file f.txt" , O))
  , ?_assertEqual([HelpOpt, FileOpt]    , parse_args("-h --file=f.txt" , O))
  , ?_assertEqual([HelpOpt, VerboseOpt] , parse_args("-h --verbose"    , O))
  , ?_assertEqual([HelpOpt, VerboseOpt] , parse_args("-h --ver"        , O))
  , ?_assertEqual([Arg("arg")]          , parse_args("arg"             , O))
  , ?_assertEqual([HelpOpt, FileOpt, Arg("arg"), Arg("arg2")],
                  parse_args("-h --file f.txt arg arg2", O))
  , ?_assertEqual([HelpOpt, Arg("arg"), Arg("--"), Arg("-v")],
                  parse_args("-h arg -- -v", O))
  ].

parse_pattern_test_() ->
  HelpOpt    = #option{short="-h"},
  FileOpt    = #option{short="-f", long="--file", argcount=1, value="<f>"},
  VerboseOpt = #option{short="-v", long="--verbose"},
  O = [opt("-h"), opt("-v", "--verbose"), opt("-f", "--file", 1)],
  ParsePattern = fun (Str, Opts) ->
                     {Pattern, _} = parse_pattern(Str, Opts),
                     Pattern
                 end,
  [ ?_assertEqual(
       req([optional([HelpOpt])]),
       ParsePattern("[ -h ]", O))
  , ?_assertEqual(
       req([optional([#option{short="-h"}])]),
       ParsePattern("[ -h ]", []))
  , ?_assertEqual(
       req([optional([#option{long="--verbose", value=true}])]),
       ParsePattern("[ --verbose ]", []))
  , ?_assertEqual(
       req([optional([one_or_more([arg("ARG")])])]),
       ParsePattern("[ ARG ... ]", O))
  , ?_assertEqual(
       req([optional([either([HelpOpt, VerboseOpt])])]),
       ParsePattern("[ -h | -v ]", O))
  , ?_assertEqual(
       req([VerboseOpt, optional([FileOpt])]),
       ParsePattern("-v [ --file <f> ]", O))
  , ?_assertEqual(
       req([optional([either([arg("M"), req([either([arg("K"),
                                                     arg("L")])])])])]),
       ParsePattern("[M | (K | L)]", O))
  , ?_assertEqual(
       req([arg("N"), arg("M")]),
       ParsePattern("N M", O))
  , ?_assertEqual(
       req([arg("N"), optional([arg("M")])]),
       ParsePattern("N [M]", O))
  , ?_assertEqual(
       req([arg("N"), optional([either([arg("M"), arg("K"), arg("L")])])]),
       ParsePattern("N [M | K | L]", O))
  , ?_assertEqual(
       req([optional([HelpOpt]), optional([arg("N")])]),
       ParsePattern("[ -h ] [N]", O))
  , ?_assertEqual(
       req([optional(lists:reverse(O))]),
       ParsePattern("[options]", O))
  , ?_assertEqual(
       req([arg("ADD")]),
       ParsePattern("ADD", O))
  , ?_assertEqual(
       req([arg("<add>")]),
       ParsePattern("<add>", O))
  , ?_assertEqual(
       req([cmd("add")]),
       ParsePattern("add", O))
  , ?_assertEqual(
       req([req([either([HelpOpt, req([VerboseOpt, optional([arg("A")])])])])]),
       ParsePattern("( -h | -v [ A ] )", O))
  , ?_assertEqual(
       req([req([either([req([arg("N"),
                              optional([either([arg("M"),
                                                req([either([arg("K"),
                                                             arg("L")
                                                            ])])])])]),
                         req([arg("O"), arg("P")])])])]),
       ParsePattern("(N [M | (K | L)] | O P)", O))
  ].

arg(A) when is_list(A) -> #argument{name=A};
arg(V)                 -> #argument{value=V}.
arg(Arg, Value)       -> #argument{name=Arg, value=Value}.
cmd(Cmd)              -> #command{name=Cmd}.
cmd(Cmd, Value)       -> #command{name=Cmd, value=Value}.
req(Children)         -> #required{children=Children}.
either(Children)      -> #either{children=Children}.
optional(Children)    -> #optional{children=Children}.
one_or_more(Children) -> #one_or_more{children=Children}.

opt(Short)            -> #option{short=Short}.
opt(Short, Long)      -> #option{short=Short, long=Long}.
opt(Short, Long, 1)   -> #option{short=Short, long=Long, argcount=1, value=undefined}.

partition_test_() ->
  [ ?_assertEqual({"foobar", ""}     , partition("foobar"      , "abc"))
  , ?_assertEqual({"foo", "bar"}     , partition("foo bar"     , " "))
  , ?_assertEqual({"foo", "bar baz"} , partition("foo bar baz" , " "))
  , ?_assertEqual({"foo", "bar"}     , partition("foo  bar"    , "  "))
  , ?_assertEqual({"foo", "bar"}     , partition("fooabcbar"   , "abc"))
  ].

option_parse_test_() ->
  [ ?_assertEqual(opt("-h"), option_parse("-h"))
  , ?_assertEqual(opt(undefined, "--help"), option_parse("--help"))
  , ?_assertEqual(opt("-h", "--help"), option_parse("-h --help"))
  , ?_assertEqual(opt("-h", "--help"), option_parse("--help -h"))
  , ?_assertEqual(opt("-h", "--help"), option_parse("-h,--help"))

  , ?_assertEqual(#option{short="-h", argcount=1, value=undefined},
                  option_parse("-h TOPIC"))
  , ?_assertEqual(#option{long="--help", argcount=1, value=undefined},
                  option_parse("--help TOPIC"))
  , ?_assertEqual(#option{short="-h", long="--help", argcount=1, value=undefined},
                  option_parse("-h TOPIC --help TOPIC"))
  , ?_assertEqual(#option{short="-h", long="--help", argcount=1, value=undefined},
                  option_parse("-h TOPIC, --help TOPIC"))
  , ?_assertEqual(#option{short="-h", long="--help", argcount=1, value=undefined},
                  option_parse("-h TOPIC, --help=TOPIC"))

  , ?_assertEqual(#option{short="-h"}, option_parse("-h  Description..."))
  , ?_assertEqual(#option{short="-h", long="--help"},
                  option_parse("-h --help  Description..."))
  , ?_assertEqual(#option{short="-h", argcount=1, value=undefined},
                  option_parse("-h TOPIC  Description..."))
  , ?_assertEqual(#option{short="-h"}, option_parse("    -h"))
  , ?_assertEqual(#option{short="-h", argcount=1, value="2"},
                  option_parse("-h TOPIC  Descripton... [default: 2]"))
  , ?_assertEqual(#option{short="-h", argcount=1, value="topic-1"},
                  option_parse("-h TOPIC  Descripton... [default: topic-1]"))
  , ?_assertEqual(#option{long="--help", argcount=1, value="3.14"},
                  option_parse("--help=TOPIC  ... [default: 3.14]"))
  , ?_assertEqual(#option{short="-h", long="--help", argcount=1, value="./"},
                  option_parse("-h, --help=DIR  ... [default: ./]"))
  , ?_assertEqual(#option{short="-h", argcount=1, value="2"},
                  option_parse("-h TOPIC  Descripton... [dEfAuLt: 2]"))
  ].

fix_either_test_() ->
  OA = opt("-a"),
  OB = opt("-b"),
  OC = opt("-c"),
  AN = arg("N"),
  AM = arg("M"),
  [ ?_assertEqual(either([req([OA])]), fix_either(OA))
  , ?_assertEqual(either([req([AN])]), fix_either(AN))
  , ?_assertEqual(either([req([AN, AM, AN, AM])]),
                  fix_either(one_or_more([AN, AM])))
  , ?_assertEqual(either([req([OA, OC]), req([OB, OC])]),
                  fix_either(req([either([OA, OB]), OC])))
  , ?_assertEqual(either([req([OB, OA]), req([OC, OA])]),
                  fix_either(optional([OA, either([OB, OC])])))
  , ?_assertEqual(either([req([OA]), req([OB]), req([OC])]),
                  fix_either(either([OA, either([OB, OC])])))

  ].

name_test_() ->
  [ ?_assertEqual("-h"    , name(opt("-h")))
  , ?_assertEqual("--help", name(opt("-h", "--help")))
  , ?_assertEqual("--help", name(opt(undefined, "--help")))
  , ?_assertEqual("foo"   , name(arg("foo")))
  , ?_assertEqual("foo"   , name(cmd("foo")))
  ].

list_argument_match_test_() ->
  M = fun (Pat, Args) ->
          {Fixed, _} = fix_list_arguments(Pat, []),
          match(Fixed, Args)
      end,
  [ ?_assertEqual({true, [], [arg("N", ["1", "2"])]},
                  M(req([arg("N"), arg("N")]),
                    [arg(undefined, "1"), arg(undefined, "2")]))
  , ?_assertEqual({true, [], [arg("N", ["1", "2", "3"])]},
                 M(one_or_more([arg("N")]),
                   [ arg(undefined, "1")
                   , arg(undefined, "2")
                   , arg(undefined, "3")
                   ]))
  , ?_assertEqual({true, [], [arg("N", ["1", "2", "3"])]},
                  M(req([arg("N"), one_or_more([arg("N")])]),
                    [ arg(undefined, "1")
                    , arg(undefined, "2")
                    , arg(undefined, "3")
                    ]))
  , ?_assertEqual({true, [], [arg("N", ["1", "2"])]},
                  M(req([arg("N"), req([arg("N")])]),
                    [arg(undefined, "1"), arg(undefined, "2")]))
  ].

fix_list_arguments_test_() ->
  Fix = fun(Pat) ->
            {Fixed, _} = fix_list_arguments(Pat, []),
            Fixed
        end,
  [ ?_assertEqual(opt("-a"), Fix(opt("-a")))
  , ?_assertEqual(arg("N", undefined), Fix(arg("N", undefined)))
  , ?_assertEqual(req([arg("N", []), arg("N", [])]),
                  Fix(req([arg("N"), arg("N")])))
  , ?_assertEqual(either([arg("N", []), one_or_more([arg("N", [])])]),
                  Fix(either([arg("N"), one_or_more([arg("N")])])))
  ].

strip_test_() ->
  [ ?_assertEqual(""       , strip(""))
  , ?_assertEqual("foo"    , strip("foo"))
  , ?_assertEqual("foo"    , strip("\n    \n  \nfoo"))
  , ?_assertEqual("foo"    , strip("    \n\n  \nfoo   \n \n  "))
  , ?_assertEqual("foo bar", strip("  \n  \n  \nfoo bar \n \n"))
  ].

matching_paren_test_() ->
  [ ?_assertThrow("Unmatched '['", docopt("Usage: prog [a [b]", []))
  , ?_assertThrow("Unmatched '('", docopt("Usage: [a [b] ] c)", []))
  ].

bug_not_list_argument_if_nothing_matched_test_() ->
  D = "Usage: prog [NAME [NAME ...]]",
  [ ?_assertEqual([{"NAME", ["a", "b"]}], docopt(D, "a b"))
  , ?_assertEqual([{"NAME", []}]        , docopt(D, ""))
  ].

pattern_flat_test() ->
  ?assertEqual([arg("N"), opt("-a"), arg("M")],
               flatten(req([one_or_more([arg("N")]), opt("-a"), arg("M")]))).

bug_test_() ->
  [ ?_assertEqual([], docopt("usage: prog", ""))
  , ?_assertEqual([{"<a>", "1"}, {"<b>", "2"}],
                  docopt("usage: prog \n prog <a> <b>", "1 2"))
  , ?_assertEqual([{"<a>", undefined}, {"<b>", undefined}],
                  docopt("usage: prog \n prog <a> <b>", ""))
  , ?_assertEqual([{"<a>", undefined}, {"<b>", undefined}],
                  docopt("usage: prog <a> <b> \n prog", ""))
  ].

issue40_test_() ->
  [ ?_assertEqual(orddict:from_list([{"--aabb", false}, {"--aa", true}]),
                  docopt("usage: prog --aabb | --aa", "--aa"))
%% TODO: ?
%%, ?_assertThrow(_, docopt("usage: prog --help-commands | --help", "--help"))
  ].

count_multiple_flags_test_() ->
  [ ?_assertEqual([{"-v", true}] , docopt("usage: prog [-v]", "-v"))
  , ?_assertEqual([{"-v", false}], docopt("usage: prog [-v]", ""))
  , ?_assertEqual([{"-v", 0}]    , docopt("usage: prog [-vv]", ""))
  , ?_assertEqual([{"-v", 1}]    , docopt("usage: prog [-vv]", "-v"))
  , ?_assertEqual([{"-v", 2}]    , docopt("usage: prog [-vv]", "-vv"))
  , ?_assertThrow(_              , docopt("usage: prog [-vv]", "-vvv"))
  , ?_assertEqual([{"-v", 3}]    , docopt("usage: prog [-v | -vv | -vvv]", "-vvv"))
  , ?_assertEqual([{"-v", 6}]    , docopt("usage: prog -v...", "-vvvvvv"))
  , ?_assertEqual([{"-v", 0}]    , docopt("usage: prog [-v...]", ""))
  , ?_assertEqual([{"--ver", 2}] , docopt("usage: prog [--ver --ver]", "--ver --ver"))
  ].

count_multiple_commands_test_() ->
  [ ?_assertEqual([{"go", true}], docopt("usage: prog [go]", "go"))
  , ?_assertEqual([{"go", 0}], docopt("usage: prog [go go]", ""))
  , ?_assertEqual([{"go", 1}], docopt("usage: prog [go go]", "go"))
  , ?_assertEqual([{"go", 2}], docopt("usage: prog [go go]", "go go"))
  , ?_assertThrow(_, docopt("usage: prog [go go]", "go go go"))
  , ?_assertEqual([{"go", 5}], docopt("usage: prog go...", "go go go go go"))
  ].

accumulate_multiple_options_test_() ->
  [ ?_assertEqual([{"--long", ["one"]}],
                  docopt("usage: prog --long=<arg> ...", "--long one"))
  , ?_assertEqual([{"--long", ["one", "two"]}],
                  docopt("usage: prog --long=<arg> ...", "--long one --long two"))
  ].

multiple_different_elements_test_() ->
  [ ?_assertEqual(
       [{"--speed", ["5", "9"]}, {"<direction>", ["left", "right"]}, {"go", 2}],
       docopt("usage: prog (go <direction> --speed=<km/h>)...",
              "go left --speed=5  go right --speed=9"))
  ].

bug_option_argument_should_not_capture_default_value_from_pattern_test_() ->
  Doc = "Usage: tau [-a <host:port>]

    -a, --address <host:port>  TCP address [default: localhost:6283].

    ",
  [ ?_assertEqual([{"--file", undefined}],
                  docopt("usage: prog [--file=<f>]", ""))
  , ?_assertEqual([{"--file", undefined}],
                  docopt("usage: prog [--file=<f>]\n\n--file <a>", ""))
  , ?_assertEqual([{"--address", "localhost:6283"}], docopt(Doc, ""))
  ].

language_errors_test_() ->
  [ ?_assertThrow(_, docopt("no usage with colon here", ""))
  , ?_assertThrow(_, docopt("usage: here \n\n and again usage: here", ""))
  ].

option_arguments_default_to_undefined_test() ->
  D = "usage: prog [options]

    -a        Add
    -m <msg>  Message

    ",
  ?assertEqual([{"-a", true}, {"-m", undefined}], docopt(D, "-a")).

options_with_trailing_argument_test() ->
  D = "usage: prog [options]

    -b       B
    -a       A
    -z <zs>  Z

    ",
  ?assertEqual([{"-a", true}, {"-b", true}, {"-z", "ar"}], docopt(D, "-bazar")).

one_or_more_optional_arguments_test() ->
  ?assertEqual([{"NAME", ""}], docopt("usage: prog [NAME]...", "")).

%%%_* Emacs ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
