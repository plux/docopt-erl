#!/usr/bin/env escript
main(Args) ->
  true = code:add_pathz(filename:dirname(escript:script_name()) ++ "/../ebin"),
  try docopt:docopt(read_stdin([]), string:join(Args, " ")) of
      Out -> io:format("~s~n", [to_json(Out)])
  catch
    _:_ -> io:format("\"user-error\"~n")
  end.

read_stdin(Acc) ->
  case io:get_chars('', 8192) of
    eof  -> lists:flatten(Acc);
    Text -> read_stdin([Acc, Text])
  end.

to_json(KVs) ->
  StrKVs = lists:map(fun({Key, Value}) ->
                             io_lib:format("~p: ~s", [Key, format_value(Value)])
                     end, KVs),
  io_lib:format("{~s}", [string:join(StrKVs,", ")]).

format_value(undefined) -> null;
format_value(V)         -> io_lib:format("~p", [V]).
