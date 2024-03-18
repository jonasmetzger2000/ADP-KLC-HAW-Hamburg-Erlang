-module(hbq).
-export([initHBQ/2, hbqLoop/6, pollSearch/2, checkOldMessagesInHBQ/7, listHBQ/1]).

hbqLoop(HBQ,DLQ,Datei,DDatei,DLQSize, Num) ->
  receive
    % Schnittstelle {request,pushHBQ,[NNr,Msg,TSclientout]}:
    {From, {request, pushHBQ, Msg}} when is_pid(From) ->
      {NewHBQ, NewDLQ, NewNum} = checkOldMessagesInHBQ(HBQ, DLQ, Datei, DDatei, DLQSize, Num, From),
      pushHBQ(NewHBQ, NewDLQ, Datei, DDatei, DLQSize, NewNum, From, Msg);
    % Schnittstelle {From, {request,deliverMSG, NNr, ToClient}}
    {From, {request,deliverMSG, NNr, ToClient}} when is_pid(From) and is_pid(ToClient) ->
      SendNNr = dlq:deliverMSG(NNr, ToClient, DLQ, DDatei),
      From ! {reply, SendNNr},
      hbqLoop(HBQ,DLQ,Datei,DDatei,DLQSize, Num);
    % Schnittstelle {From, {request,listHBQ}}
    {From, {request,listHBQ}} ->
      Inhalt = lists:concat(["HBQ>>> Content(",length(HBQ),"): ",util:to_String(listHBQ(HBQ)),"\n"]),
      util:logging(Datei,Inhalt),
      From ! {reply, ok},
      hbqLoop(HBQ,DLQ,Datei,DDatei,DLQSize, Num);
    % Schnittstelle {From, {request,listDLQ}}
    {From, {request,listDLQ}} ->
      Inhalt = lists:concat(["DLQ>>> Content(",length(HBQ),"): ",util:to_String(dlq:listDLQ(DLQ)),"\n"]),
      util:logging(Datei,Inhalt),
      From ! {reply, ok},
      hbqLoop(HBQ,DLQ,Datei,DDatei,DLQSize, Num);
    % Schnittstelle {From, {request,dellHBQ}}
    {From, {request,dellHBQ}} ->
      dlq:delDLQ(DLQ),
      From ! {reply, ok},
      ok;
    % Fehlerfall, ungültiger Request!
    Any ->
      Inhalt = lists:concat(["HBQ>>> in hbqLoop unerwartete Nachricht erhalten: ",util:to_String(Any),".\n"]),
      util:logging(Datei,Inhalt),
      hbqLoop(HBQ,DLQ,Datei,DDatei,DLQSize, Num)
  end.

pushHBQ(HBQ, DLQ, Datei, DDatei, DLQSize, NextNumToBeDelivered, From, [NNr, Msg, TSclientout]) ->
  ExpectedNr = dlq:expectedNr(DLQ),
  case NNr < ExpectedNr of
    false ->
      TShbqin = erlang:timestamp(),
      NMsg = lists:concat([Msg," HBQ In: ",vsutil:now2string(TShbqin)," "]),
      case NextNumToBeDelivered == NNr of
        true ->
          NewDLQ = dlq:push2DLQ([NNr, NMsg, TSclientout, TShbqin], DLQ, DDatei),
          From ! {reply, ok},
          Inhalt = lists:concat(["HBQ>>> Nachricht in DLQ verschoben: ",util:to_String(NMsg),".\n"]),
          util:logging(Datei,Inhalt),
          hbqLoop(HBQ, NewDLQ, Datei, DDatei, DLQSize, NNr+1);
        false ->
          Inhalt = lists:concat(["HBQ>>> Nachricht in die HBQ verschoben: ",util:to_String(NMsg),".\n"]),
          util:logging(Datei,Inhalt),
          From ! {reply, ok},
          hbqLoop([{NNr, NMsg, TSclientout, TShbqin}] ++ HBQ, DLQ, Datei, DDatei, DLQSize, NextNumToBeDelivered)
      end;
    true ->
      From ! {reply, ok},
      Inhalt = lists:concat(["HBQ>>> Nachricht verworfen: ",util:to_String(NNr),".\n"]),
      util:logging(Datei,Inhalt),
      hbqLoop(HBQ, DLQ, Datei, DDatei, DLQSize, NextNumToBeDelivered)
  end.

checkOldMessagesInHBQ(HBQ, DLQ, Datei, DDatei, DLQSize, NextNumToBeDelivered, From) ->
  {NextMsg, NewHBQ} = pollSearch(NextNumToBeDelivered, HBQ),
  Size = length(NewHBQ),
  case Size > DLQSize*0.66 of
    true ->
      Min = minNr(HBQ),
      NewDLQ = dlq:push2DLQ(errormessage({NextNumToBeDelivered, Min - 1}, Datei), DLQ, DDatei),
      checkOldMessagesInHBQ(NewHBQ, NewDLQ, Datei, DDatei, DLQSize, Min, From);
    false ->
      util:logging(Datei, lists:concat(["HBQ>>> Suche nach Nachricht mit Nummer in HBQ: ",util:to_String(NextNumToBeDelivered),".\n"])),
      case NextMsg == null of
        true ->
          util:logging(Datei,"HBQ>>> Keine zur DLQ sendende Nachricht gefunden.\n"),
          {NewHBQ, DLQ, NextNumToBeDelivered};
        false ->
          {NNr, _NMsg, _TSclientout, _TShbqin} = NextMsg,
          NewDLQ = dlq:push2DLQ([NNr, _NMsg, _TSclientout, _TShbqin], DLQ, DDatei),
          util:logging(Datei, lists:concat(["HBQ>>> Nachricht aus HBQ wird gesendet: ",util:to_String(NextMsg),".\n"])),
          checkOldMessagesInHBQ(NewHBQ, NewDLQ, Datei, DDatei, DLQSize, NextNumToBeDelivered+1, From)
      end
  end.

listHBQ([]) -> [];
listHBQ([{Nr, _NMsg, _TSclientout, _TShbqin}|Tail]) -> [Nr|listHBQ(Tail)].

pollSearch(_NNr, []) -> {null, []};
pollSearch(NNr, List) -> pollSearch(NNr, List, []).
pollSearch(_NNr, [], Acc) -> {null, Acc};
pollSearch(NNr, [{NNr, _NMsg, _TSclientout, _TShbqin}|Tail], Acc) -> {{NNr, _NMsg, _TSclientout, _TShbqin}, Acc ++ Tail};
pollSearch(NNr, [Head|Tail], Acc) -> pollSearch(NNr, Tail, [Head] ++ Acc).

minNr([Head|Tail]) -> minNr(Head, Tail).
minNr(Min, []) -> Min;
minNr(Min, [{Nr, _NMsg, _TSclientout, _TShbqin}|Tail]) when Nr < Min -> minNr(Nr, Tail);
minNr(Min, [_Head|Tail]) -> minNr(Min, Tail).

% Schnittstelle initHBQ(DLQ-Limit,HBQ-Name):
initHBQ(DLQSize,HBQName) ->
  {ok, HostName} = inet:gethostname(),
  Datei = lists:concat(["HBQ@",HostName,".log"]),
  DDatei = lists:concat(["DLQ@",HostName,".log"]),

  EmptyHBQ = [],
  EmptyDLQ = dlq:initDLQ(DLQSize,DDatei),
  HBQPid = spawn(fun() -> hbqLoop(EmptyHBQ, EmptyDLQ, Datei, DDatei, DLQSize, 1) end),
  register(HBQName,HBQPid),
  HBQPid.

errormessage({First,Last}, Datei) ->
  Gap = Last - First,
  case Gap of
    0 -> Fehlernachricht = lists:concat(["***Fehlernachricht fuer Nachricht ",integer_to_list(First),
      " um ",util:timeMilliSecond(),"."]);
    X when X > 0 -> Fehlernachricht = lists:concat(["***Fehlernachricht fuer Nachrichten ",integer_to_list(First),
      " bis ",integer_to_list(Last)," um ",util:timeMilliSecond(),"."])
  end,
  Inhalt = lists:concat(["HBQ>>> Fehlernachricht fuer Nachrichten ",integer_to_list(First)," bis ",integer_to_list(Last)," generiert.\n"]),
  util:logging(Datei,Inhalt),
  [Last,Fehlernachricht, erlang:timestamp(), erlang:timestamp()].