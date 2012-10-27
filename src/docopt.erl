-module(docopt).

-compile([export_all]).

%%%_* Includes ================================================================

-include_lib("eunit/include/eunit.hrl").

%%%_* Records =================================================================

-record(state, { options = [] :: [child_pattern()]
               , tokens  = [] :: [string()]
               , mode         :: parse_mode()
               }).

%% Parent patterns
-record(one_or_more , {children :: [pattern()]}).
-record(required    , {children :: [pattern()]}).
-record(optional    , {children :: [pattern()]}).
-record(either      , {children :: [pattern()]}).

%% Child patterns
-record(command  , {name :: string(), value :: any()}).
-record(argument , {name :: string(), value :: any()}).
-record(option   , { short            :: string()
                   , long             :: string()
                   , argcount = 0     :: non_neg_integer()
                   , value    = false :: any()
                   }).

%%%_* Types ===================================================================

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

docopt(Doc, Args) ->
  Options = parse_doc_options(Doc),
  Args    = parse_args(Args, Options),
  [{option_name(Opt), Opt#option.value} || Opt <- Options].

parse_doc_options(Doc) ->
  [_|OptStrings] = re:split(Doc, "^ *-|\\n *-", [{return, list}]),
  [option_parse("-" ++ S) || S <- OptStrings].

parse_args(Args, Options) ->
  State = #state{ tokens  = string:tokens(Args, " ")
                , options = Options
                , mode    = parse_args
                },
  parse_args_tokens(State).

parse_args_tokens(#state{tokens=[]}) -> [];
parse_args_tokens(State0) ->
  case current(State0) of
    "--"      -> [#argument{value=Arg} || Arg <- tokens(State0)];
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

parse_long(State0) ->
  {Raw, Value} = partition(current(State0), "="),
  Opt =
    case [O || O <- options(State0), O#option.long == Raw] of
      [O] -> O;
      []  ->
        case [O || O <- options(State0), starts_with(O#option.long, Raw)] of
          []                         -> throw({Raw, "not recognized"});
          Opts when length(Opts) > 1 -> throw({Raw, "is not a unique prefix"});
          [O]                        -> O
        end
    end,
  Rest  = rest(State0),
  State = move(State0),
  case Opt#option.argcount of
    1 when Value == [],
           Rest  == [] -> throw({Raw, "requires an argument"});
    1 when Value == [] -> {[Opt#option{value = current(State)}], move(State)};
    1 when Value /= [] -> {[Opt#option{value = Value}], State};
    0 when Value /= [] -> throw({Raw, "must not have an argument"});
    0 when Value == [] -> {[Opt#option{value = true}], State}
  end.

starts_with(Str, SubStr) when is_list(Str), is_list(SubStr) ->
  string:str(Str, SubStr) == 1;
starts_with(_, _) -> false.

parse_shorts(State) ->
  [$-|Tokens] = current(State),
  parse_shorts(Tokens, move(State), []).

parse_shorts([], State, Acc) -> {lists:reverse(Acc), State};
parse_shorts([H|T], State, Acc) ->
 case [O || O <- options(State), tl(O#option.short) == [H]] of
   []                       -> throw({[$-, H], "not recognized", State});
   Opt when length(Opt) > 1 -> throw({[$-, H], "specified ambiguously"});
   [Opt] when Opt#option.argcount == 0 ->
     parse_shorts(T, State, [Opt#option{value = true}|Acc]);
   [Opt] ->
     {Value, Rest} = get_value_shorts(H, T, State),
     {[Opt#option{value = Value}], Rest}
 end.

get_value_shorts(H, [], [])     -> throw({H, "requires an argument"});
get_value_shorts(_, [], State)  -> {current(State), move(State)};
get_value_shorts(_, Arg, State) -> {Arg, State}.

printable_usage(Doc) ->
  case re:split(Doc, "([Uu][Ss][Aa][Gg][Ee]:)", [{return, list}]) of
    UsageSplit when length(UsageSplit) < 3 ->
      throw("\"usage:\" (case-insensitive) not found.");
    UsageSplit when length(UsageSplit) > 3 ->
      throw("More than one \"usage:\" (case-insensitive)");
    [_|UsageSplit] ->
      L = re:split(lists:flatten(UsageSplit), "\\n\\s*\\n", [{return, list}]),
      string:strip(hd(L))
  end.

formal_usage(PrintableUsage) ->
  %% Split and drop "usage:"
  [_Usage, ProgName|Args] = string:tokens(PrintableUsage, " \n"),
  F = fun (S) when S == ProgName -> ") | (";
          (S)                    -> S
      end,
  "( " ++ string:join(lists:map(F, Args), " ") ++ " )".

parse_pattern(Source0, Options) ->
  %% Add spaces around []()| and ...
  Source = re:replace(Source0, "([\\[\\]\\(\\)\\|]|\\.\\.\\.)", " \\1 ",
                      [{return, list}, global]),
  State = #state{ tokens  = string:tokens(Source, " ")
                , options = Options
                , mode    = parse_pattern
                },
  {Result, _} = parse_expr(State),
  #required{children=Result}.

% expr ::= seq ( '|' seq )* ;
parse_expr(State0) ->
  {Seq, State} = parse_seq(State0),
  case current(State) of
    "|" -> parse_expr(move(State), [maybe_required_seq(Seq)]);
    _   -> {Seq, State}
  end.

parse_expr(State0, Acc)                 ->
  ct:pal("in parse_expr: ~p, ~p", [tokens(State0), Acc]),
  {Seq, State} = parse_seq(State0),
  ct:pal("in parse_expr after parse_seq: ~p, ~p", [tokens(State), Seq]),
  case current(State) of
    "|" -> parse_expr(move(State), [maybe_required_seq(Seq)|Acc]);
    _   ->
      case lists:reverse([maybe_required_seq(Seq)|Acc]) of
        Result when length(Result) > 1 -> {[#either{children=Result}], State};
        Result                         -> {Result, State} % Needed?
      end
   end.

maybe_required_seq([Seq]) -> Seq;
maybe_required_seq(Seq)   -> #required{children=Seq}.

%% seq ::= ( atom [ '...' ] )* ;
parse_seq(State) -> parse_seq(State, []).

parse_seq(#state{tokens=[]}      = State, Acc) -> {lists:reverse(Acc), State};
parse_seq(#state{tokens=["]"|_]} = State, Acc) -> {lists:reverse(Acc), State};
parse_seq(#state{tokens=[")"|_]} = State, Acc) -> {lists:reverse(Acc), State};
parse_seq(#state{tokens=["|"|_]} = State, Acc) -> {lists:reverse(Acc), State};
parse_seq(State0, Acc) ->
  ct:pal("in parse seq: ~p, ~p", [tokens(State0), Acc]),
  {Atom, State} = parse_atom(State0),
  ct:pal("in parse seq after parse_atom: ~p, ~p, ~p", [Atom, tokens(State), Acc]),
  case current(State) of
    "..." -> parse_seq(move(State), [#one_or_more{children=Atom}|Acc]);
    _     -> parse_seq(State, Atom ++ Acc)
  end.

%% atom ::= '(' expr ')' | '[' expr ']' | 'options'
%%       | long | shorts | argument | command ;
parse_atom(State) ->
  ct:pal("in parse atom: ~p", [tokens(State)]),
  case current(State) of
    "["       -> parse_optional(move(State));
    "("       -> parse_required(move(State));
    "options" -> {options(State), move(State)};
    [$-,$-|_] -> parse_long(State);
    [$-|_]    -> parse_shorts(State);
    Current   ->
      case is_arg(Current) of
        true  -> {[#argument{name=Current}], move(State)};
        false -> {[#command{name=Current}] , move(State)}
      end
  end.

parse_optional(State0) ->
  ct:pal("parse optional ~p", [State0]),
  {Expr, State} = parse_expr(State0),
  ct:pal("parse optional after parse_expr ~p\n~p", [Expr, State]),
  case current(State) of
    "]" -> {[#optional{children=Expr}], move(State)};
    _   -> throw("Unmatched '['")
  end.

parse_required(State0) ->
  ct:pal("parse required ~p", [tokens(State0)]),
  {Expr, State} = parse_expr(State0),
  ct:pal("parse required after parse_expr ~p, ~p", [tokens(State), Expr]),
  case current(State) of
    ")" -> {[#required{children=Expr}], move(State)};
    Res -> throw({"Unmatched '(':", Res})
  end.

is_arg(S) ->
  (hd(S) == $< andalso lists:last(S) == $>) orelse string:to_upper(S) == S.

%% docopt_any_options_test_() ->
%%   Doc = "Usage: prog [options] A

%%     -q  Be quiet
%%     -v  Be verbose.",
%%   [ ?_assertEqual([{"A", "arg"}, {"-v", false}, {"-q", false}],
%%                   docopt(Doc, "arg"))
%%   , ?_assertEqual([{"A", "arg"}, {"-v", true}, {"-q", false}],
%%                   docopt(Doc, "-v arg"))
%%   , ?_assertEqual([{"A", "arg"}, {"-v", false}, {"-q", true}],
%%                   docopt(Doc, "-q arg"))
%%   ].

option_parse(Str) ->
  {Options, Desc} = partition(string:strip(Str), "  "),
  lists:foldl(fun([$-,$-|_] = S, Opt) -> Opt#option{long  = S};
                 ([$-|_]    = S, Opt) -> Opt#option{short = S};
                 (_            , Opt) ->
                  Opt#option{argcount = 1, value = default_value(Desc)}
              end, #option{}, string:tokens(Options, ",= ")).

default_value(Desc) ->
  case re:run(Desc,"\\[default: (.*)\\]", [{capture, [1], list}, caseless]) of
    {match, [DefaultValue]} -> DefaultValue;
    nomatch                 -> false
  end.

option_name(#option{long=undefined, short=Short}) -> Short;
option_name(#option{long=Long})                   -> Long.

partition(Str, Delim) ->
  case string:str(Str, Delim) of
    0 -> {Str, ""};
    I -> {string:substr(Str, 1, I - 1), string:substr(Str, I + length(Delim))}
  end.

%%%_* Tests ===================================================================

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
  , ?_assertEqual({[#option{short="-h", value=true}], St([])},
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
  HelpOpt    = #option{short="-h", value=true},
  FileOpt    = #option{short="-f", long="--file", argcount=1, value="<f>"},
  VerboseOpt = #option{short="-v", long="--verbose", value=true},
  O = [option("-h"), option("-v", "--verbose"), option("-f", "--file", 1)],
  [ ?_assertEqual(
       required([optional([HelpOpt])]),
       parse_pattern("[ -h ]", O))
  , ?_assertEqual(
       required([optional([one_or_more([argument("ARG")])])]),
       parse_pattern("[ ARG ... ]", O))
  , ?_assertEqual(
       required([optional([either([HelpOpt, VerboseOpt])])]),
       parse_pattern("[ -h | -v ]", O))
  , ?_assertEqual(
       required([VerboseOpt, optional([FileOpt])]),
       parse_pattern("-v [ --file <f> ]", O))
  , ?_assertEqual(
       required([optional([either([argument("M"),
                                   required([either([argument("K"),
                                                     argument("L")])])])])]),
       parse_pattern("[M | (K | L)]", O))
  , ?_assertEqual(
       required([argument("N"), argument("M")]),
       parse_pattern("N M", O))
  , ?_assertEqual(
       required([argument("N"), optional([argument("M")])]),
       parse_pattern("N [M]", O))
  , ?_assertEqual(
       required([argument("N"), optional([either([argument("M"),
                                                  argument("K"),
                                                  argument("L")])])]),
      parse_pattern("N [M | K | L]", O))
  , ?_assertEqual(
       required([optional([HelpOpt]), optional([argument("N")])]),
       parse_pattern("[ -h ] [N]", O))
  , ?_assertEqual(
       required([optional(lists:reverse(O))]),
       parse_pattern("[options]", O))
  , ?_assertEqual(
       required([argument("ADD")]),
       parse_pattern("ADD", O))
  , ?_assertEqual(
       required([argument("<add>")]),
       parse_pattern("<add>", O))
  , ?_assertEqual(
       required([command("add")]),
       parse_pattern("add", O))
  , ?_assertEqual(
       required([required([either([HelpOpt,
                                   required([VerboseOpt,
                                             optional([argument("A")])])])])]),
       parse_pattern("( -h | -v [ A ] )", O))
  , ?_assertEqual(
       required(
         [required(
            [either(
               [required([argument("N"),
                          optional([either([argument("M"),
                                            required([either([argument("K"),
                                                              argument("L")
                                                             ])])])])]),
                required([argument("O"), argument("P")])])])]),
       parse_pattern("(N [M | (K | L)] | O P)", O))
  ].

argument(Arg)         -> #argument{name=Arg}.
command(Cmd)          -> #command{name=Cmd}.
required(Children)    -> #required{children=Children}.
either(Children)      -> #either{children=Children}.
optional(Children)    -> #optional{children=Children}.
one_or_more(Children) -> #one_or_more{children=Children}.

option(Short)           -> #option{short=Short}.
option(Short, Long)     -> #option{short=Short, long=Long}.
option(Short, Long, Ac) -> #option{short=Short, long=Long, argcount=Ac}.

partition_test_() ->
  [ ?_assertEqual({"foobar", ""}     , partition("foobar"      , "abc"))
  , ?_assertEqual({"foo", "bar"}     , partition("foo bar"     , " "))
  , ?_assertEqual({"foo", "bar baz"} , partition("foo bar baz" , " "))
  , ?_assertEqual({"foo", "bar"}     , partition("foo  bar"    , "  "))
  , ?_assertEqual({"foo", "bar"}     , partition("fooabcbar"   , "abc"))
  ].

option_parse_test_() ->
  [ ?_assertEqual(#option{short="-h"}, option_parse("-h"))
  , ?_assertEqual(#option{long="--help"}, option_parse("--help"))
  , ?_assertEqual(#option{short="-h", long="--help"}, option_parse("-h --help"))
  , ?_assertEqual(#option{short="-h", long="--help"}, option_parse("--help -h"))
  , ?_assertEqual(#option{short="-h", long="--help"}, option_parse("-h,--help"))

  , ?_assertEqual(#option{short="-h", argcount=1}, option_parse("-h TOPIC"))
  , ?_assertEqual(#option{long="--help", argcount=1},
                  option_parse("--help TOPIC"))
  , ?_assertEqual(#option{short="-h", long="--help", argcount=1},
                  option_parse("-h TOPIC --help TOPIC"))
  , ?_assertEqual(#option{short="-h", long="--help", argcount=1},
                  option_parse("-h TOPIC, --help TOPIC"))
  , ?_assertEqual(#option{short="-h", long="--help", argcount=1},
                  option_parse("-h TOPIC, --help=TOPIC"))

  , ?_assertEqual(#option{short="-h"}, option_parse("-h  Description..."))
  , ?_assertEqual(#option{short="-h", long="--help"},
                  option_parse("-h --help  Description..."))
  , ?_assertEqual(#option{short="-h", argcount=1},
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
  , ?_assertEqual(#option{short="-h",argcount=1, value="2"},
                  option_parse("-h TOPIC  Descripton... [dEfAuLt: 2]"))
  ].

option_name_test_() ->
  [ ?_assertEqual("-h"    , option_name(#option{short="-h"}))
  , ?_assertEqual("--help", option_name(#option{short="-h", long="--help"}))
  , ?_assertEqual("--help", option_name(#option{long="--help"}))
  ].

%%%_* Emacs ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
