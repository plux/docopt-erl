-module(docopt).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-record(option, { short
                , long
                , argcount = 0
                , value    = false
                }).

-record(argument, { name
                  , value
                  }).

docopt(Doc, Args) ->
  Options = parse_doc_options(Doc),
  Args    = parse_args(Args, Options),
  [{option_name(Opt), Opt#option.value} || Opt <- Options].

parse_doc_options(Doc) ->
  [_|OptStrings] = re:split(Doc, "^ *-|\\n *-", [{return, list}]),
  [option_parse("-" ++ S) || S <- OptStrings].

parse_args(Args, Options) ->
  Tokens = string:tokens(Args, " "),
  parse_args_tokens(Tokens, Options).

parse_args_tokens([], _Options)            -> [];
parse_args_tokens([Current|Rest]=Tokens, Options) ->
  case Current of
    "--"           -> [#argument{value=Arg} || Arg <- Tokens];
    [$-,$-|_]      ->
      {Opt, NewRest} = parse_long(Current, Rest, Options),
      [Opt|parse_args_tokens(NewRest, Options)];
    [$-|[_|_]=Raw] ->
      {Opts, NewRest} = parse_shorts(Raw, Rest, Options, []),
      Opts ++ parse_args_tokens(NewRest, Options);
    _              ->
      [#argument{value=Current}|parse_args_tokens(Rest, Options)]
  end.

parse_long(Current, Rest, Options) ->
  {Raw, Value} = partition(Current, "="),
  Opt =
    case [O || O <- Options, O#option.long == Raw] of
      [O] -> O;
      []  ->
        case [O || O <- Options, starts_with(O#option.long, Raw)] of
          []                         -> throw({Raw, "not recognized"});
          Opts when length(Opts) > 1 -> throw({Raw, "is not a unique prefix"});
          [O]                        -> O
        end
    end,
  case Opt#option.argcount of
    1 when Value == [], Rest == [] -> throw({Raw, "requires an argument"});
    1 when Value == []             -> {Opt#option{value = hd(Rest)}, tl(Rest)};
    1 when Value /= []             -> {Opt#option{value = Value}, Rest};
    0 when Value /= []             -> throw({Raw, "must not have an argument"});
    0 when Value == []             -> {Opt#option{value = true}, Rest}
  end.

starts_with(Str, SubStr) when is_list(Str), is_list(SubStr) ->
  string:str(Str, SubStr) == 1;
starts_with(_, _) -> false.

parse_shorts([], Rest, _Options, Acc) -> {lists:reverse(Acc), Rest};
parse_shorts([H|T], Rest, Options, Acc) ->
 case [O || O <- Options, tl(O#option.short) == [H]] of
   []                       -> throw({[$-, H], "not recognized"});
   Opt when length(Opt) > 1 -> throw({[$-, H], "specified ambiguously"});
   [Opt] when Opt#option.argcount == 0 ->
     parse_shorts(T, Rest, Options, [Opt#option{value = true}|Acc]);
   [Opt] ->
     {Value, NewRest} = case Opt#option.argcount of
                          0 -> {true, Rest};
                          1 -> get_value_shorts(H, T, Rest)
                        end,
     {[Opt#option{value = Value}], NewRest}
 end.

get_value_shorts(H, [], [])         -> throw({H, "requires an argument"});
get_value_shorts(_, [], [Arg|Rest]) -> {Arg, Rest};
get_value_shorts(_, Arg, Rest)      -> {Arg, Rest}.

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
  F = fun (S) when (S) == ProgName -> ") | (";
          (S)                      -> S
      end,
  "( " ++ string:join(lists:map(F, Args), " ") ++ " )".

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


parse_pattern(Source, Options) ->
  ok.

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
  ParsedOpt = lists:foldl(fun([$-,$-|_] = S, Opt) -> Opt#option{long     = S};
                             ([$-|_]    = S, Opt) -> Opt#option{short    = S};
                             (_            , Opt) -> Opt#option{argcount = 1}
                          end, #option{}, string:tokens(Options, ",= ")),
  case ParsedOpt of
    #option{argcount = 1} -> ParsedOpt#option{value = default_value(Desc)};
    ParsedOpt             -> ParsedOpt
  end.

default_value(Desc) ->
  {ok, Re} = re:compile("\\[default: (.*)\\]", [caseless]),
  case re:run(Desc, Re, [{capture, [1], list}]) of
    {match, [DefaultValue]} -> DefaultValue;
    nomatch                 -> false
  end.

option_name(#option{long=undefined, short=Short}) -> Short;
option_name(#option{long=Long})                   -> Long.

partition(Str, Delim) ->
  case string:str(Str, Delim) of
    0 -> {Str, ""};
    I ->
      Left  = string:substr(Str, 1, I - 1),
      Right = string:substr(Str, I + length(Delim)),
      {Left, Right}
  end.

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
  , ?_assertEqual(#option{short="-h", long="--help"},
                  option_parse("-h, --help"))

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
  [ ?_assertEqual("-h", option_name(#option{short="-h"}))
  , ?_assertEqual("--help", option_name(#option{short="-h", long="--help"}))
  , ?_assertEqual("--help", option_name(#option{long="--help"}))
  ].

%%%_* Emacs ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
