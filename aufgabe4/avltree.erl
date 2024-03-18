-module(avltree).
-compile(export_all).
-import(io_lib, [format/2]).
-compile(nowarn_export_all).
-compile(nowarn_nomatch).
-compile({no_auto_import,[max/2]}).
-define(DEBUGMSG, false).

print(Msg) when ?DEBUGMSG -> io:format("Debug: ~p\n", [Msg]);
print(_) -> ok.

% ------------------------------- %
% ------ initBT: ∅ → btree ------ %
% ------------------------------- %
initBT() -> {}.

% -------------------------------- %
% ------ isBT: btree → bool ------ %
% -------------------------------- %
isBT({}) -> true;
isBT(BT) -> not(isBT(BT, 0, inf)).
isBT({E, _H, {}, {}} = BT, Min, Max) -> checkElemType(E), checkHeight(BT) or checkSort(Min, E, Max);
isBT({E, _H, {}, R} = BT, Min, Max) -> checkElemType(E), checkHeight(BT) or checkSort(Min, E, Max) or isBTcheckBalance(BT) or isBT(R, E, Max);
isBT({E, _H, L, {}} = BT, Min, Max) -> checkElemType(E), checkHeight(BT) or checkSort(Min, E, Max) or isBTcheckBalance(BT) or isBT(L, Min, E);
isBT({E, _H, L, R} = BT, Min, Max) -> checkElemType(E), checkHeight(BT) or checkSort(Min, E, Max) or isBTcheckBalance(BT) or isBT(L, Min, E) or isBT(R, E, Max).

checkElemType(E) when not(is_integer(E)) -> io:format("\t>>>isBT Fehler: Typfehler ~p ist kein Integer\n", [E]), true;
checkElemType(_) -> false.

checkHeight({E, H, {}, {}}) when not(H == 1) -> io:format("\t>>>isBT Fehler: falsche Hoehe ~p, (soll: 1) bei Knoten ~p\n", [H, E]), true;
checkHeight({E, H, {_, HL, _, _}, {}}) when not(H == HL+1) -> io:format("\t>>>isBT Fehler: falsche Hoehe ~p, (soll: ~p) bei Knoten ~p\n", [H, HL+1, E]), true;
checkHeight({E, H, {}, {_, HR, _, _}}) when not(H == HR+1) -> io:format("\t>>>isBT Fehler: falsche Hoehe ~p, (soll: ~p) bei Knoten ~p\n", [H, HR+1, E]), true;
checkHeight({E, H, {_, HL, _, _}, {_, HR, _, _}}) when (HL =< HR) and not(H == HR+1) -> io:format("\t>>>isBT Fehler: falsche Hoehe ~p, (soll: ~p) bei Knoten ~p\n", [H, HR+1, E]), true;
checkHeight({E, H, {_, HL, _, _}, {_, HR, _, _}}) when (HL > HR) and not(H == HL+1) -> io:format("\t>>>isBT Fehler: falsche Hoehe ~p, (soll: ~p) bei Knoten ~p\n", [H, HL+1, E]), true;
checkHeight(_) -> false.

checkSort(Min, E, Max) when not((Min =< E) and (E =< Max)) -> io:format("\t>>>isBT Fehler: falsche Sortierung: ¬(~p ≤ ~p ≤ ~p)\n", [Min, E, Max]), true;
checkSort(_Min, _E, _Max) -> false.

isBTcheckBalance({E, _, {_, L, _, _}, {}}) when not((-1 =< L) and (L =< 1)) -> io:format("\t>>>isBT Fehler: AVL-Bedingung verletzt_ Balance ~p bei Knoten ~p\n", [L, E]), true;
isBTcheckBalance({E, _, {}, {_, R, _, _}}) when not((-1 =< R) and (R =< 1)) -> io:format("\t>>>isBT Fehler: AVL-Bedingung verletzt_ Balance ~p bei Knoten ~p\n", [R, E]), true;
isBTcheckBalance({E, _, {_, L, _, _}, {_, R, _, _}}) when not((-1 =< R-L) and (R-L =< 1)) -> io:format("\t>>>isBT Fehler: AVL-Bedingung verletzt_ Balance ~p bei Knoten ~p\n", [R-L, E]), true;
isBTcheckBalance(_BT) -> false.
% ------------------------------------- %
% ------ isEmptyBT: btree → bool ------ %
% ------------------------------------- %
isEmptyBT({}) -> true;
isEmptyBT(_BT) -> false.

% -------------------------------------------- %
% ------ insertST: btree × elem → btree ------ %
% -------------------------------------------- %
insertAT({}, EA) -> {EA, 1, {}, {}};
insertAT({E, H, {}, {}}, EA) when EA =< E -> {E, H+1, {EA, 1, {}, {}}, {}};
insertAT({E, H, {}, {}}, EA) when EA >  E -> {E, H+1, {}, {EA, 1, {}, {}}};

% Abstieg
insertAT({E, _H, L, R}, EA) when EA =< E ->
  NewL = insertAT(L, EA),
  balance({E, maxHeight(NewL, R)+1, NewL, R}, r);
insertAT({E, _H, L, R}, EA) when EA > E ->
  NewR = insertAT(R, EA),
  balance({E, maxHeight(L, NewR)+1, L, NewR}, l).

% --------------------------------------------- %
% ------ deleteAT: btree × elem → btree  ------ %
% --------------------------------------------- %
% Nicht gefunden
deleteAT({}, _ETD) -> {};
% Gefunden als Blatt
deleteAT({ETD, _H, {}, {}}, ETD) -> {};
% Gefunden als Halbblatt
deleteAT({ETD, _H, L, {}}, ETD) -> print("Gefunden als Halbblatt"), L;
deleteAT({ETD, _H, {}, R}, ETD) -> print("Gefunden als Halbblatt"), R;
% Gefunden als Knoten
deleteAT({ETD, _H, L, R}, ETD) ->
  print("Gefunden als Knoten"),
  B = blatt(R),
  CR = deleteAT(R, B),
  balance({B, maxHeight(L, CR)+1, L, CR}, r);

% Abstieg
deleteAT({E, _H, L, R}, ETD) when ETD < E ->
  CL = deleteAT(L, ETD),
  balance({E, maxHeight(CL, R)+1, CL, R}, l);
deleteAT({E, _H, L, R}, ETD) when ETD > E ->
  CR = deleteAT(R, ETD),
  balance({E, maxHeight(L, CR)+1, L, CR}, r).

% Erstes Blatt
blatt({Blatt, _H, {}, _R}) -> Blatt;
blatt({_E, _H, L, _R}) -> blatt(L).

% --------------------------------------------- %
% ------ findST: btree × elem → integer  ------ %
% --------------------------------------------- %
% Gefunden
findBT({ETS, H, _L, _R}, ETS) -> H;

% Nicht gefunden
findBT({}, _ETS) -> -1;

% Abstieg
findBT({E, _H, L, _R}, ETS) when ETS =< E  -> findBT(L, ETS);
findBT({E, _H, _L, R}, ETS) when ETS > E  -> findBT(R, ETS).

% -------------------------------------- %
% ------ inOrderBT: btree → list  ------ %
% -------------------------------------- %
inOrderBT({E, _, {}, {}}) -> [E];
inOrderBT({E, _, {}, R}) -> [E|inOrderBT(R)];
inOrderBT({E, _, L, {}}) -> inOrderBT(L) ++ [E];
inOrderBT({E, _, L, R}) -> inOrderBT(L) ++ [E|inOrderBT(R)].

% ---------------------------------------------- %
% ------ printBT: btree × filename → dot  ------ %
% ---------------------------------------------- %
printBT(BT, F) -> {ok, Fd} = file:open(F, [write]), file:write(Fd, buildHeader() ++ printBT(BT) ++ buildFooter()).
printBT({E, H, {}, {}}) -> buildNode(E, H);
printBT({E, H, {}, R}) -> buildNode(E, H) ++ buildEdge(E, R) ++ printBT(R);
printBT({E, H, L, {}}) -> buildNode(E, H) ++ buildEdge(E, L) ++ printBT(L);
printBT({E, H, L, R}) -> buildNode(E, H) ++ buildEdge(E, L) ++ buildEdge(E, R) ++ printBT(L) ++ printBT(R).

buildHeader() -> "digraph G {\n".
buildNode(E, H) -> io_lib:format("\t~p[label=<~p<br/><font color=\"purple\" point-size=\"10\">~p</font>>];\n", [E, E, H]).
buildEdge(F, {T, _, _, _}) -> io_lib:format("\t~p -> ~p;\n", [F, T]).
buildFooter() -> "}".

% -------------------- %
% ------ Utils  ------ %
% -------------------- %
% Max von zwei werten
maxHeight({}, {}) -> 0;
maxHeight({_, H, _, _}, {}) -> H;
maxHeight({}, {_, H, _, _}) -> H;
maxHeight({_, L, _, _}, {_, R, _, _}) when L >= R -> L;
maxHeight({_, L, _, _}, {_, R, _, _}) when L < R -> R.
max(L, R) when L >= R -> L;
max(L, R) when L < R -> R.

% Balance
checkBalance(L, R) -> BAL = abs(height(L)-height(R)), 1 < BAL.

balance({E, H, L, R} = BT, l) ->
  case checkBalance(L, R) of
    true -> print("ETR < E"),
            {_ER, _HR, RL, RR} = R,
            case height(RR) >= height(RL) of
              true -> print("LL"), leftRotate(BT); % Links Links Balancierung (Einfache Rotation)
              false -> print("LR"), leftRotate({E, H, L, rightRotate(R)})  % Links Rechts Balancierung (Doppelrotation)
            end;
    false -> BT
  end;

balance({E, H, L, R} = BT, r) ->
  case checkBalance(L, R) of
    true -> print("ETR > E"),
            {_EL, _HL, LL, LR} = L,
            case height(LL) >= height(LR) of
              true -> print("RR"), rightRotate(BT); % Rechts Rechts Balancierung (Einfache Rotation)
              false -> print("RL"), rightRotate({E, H, leftRotate(L), R}) % Rechts Links Balancierung (Doppelrotation)
            end;
    false -> BT
  end.

% Höhe
height({}) -> 0;
height({_E, H, _L, _R}) -> H.

% Rotation Links
leftRotate({E, _H, L, {RE, _RH, RL, RR}}) ->
  HL = maxHeight(L, RL)+1,
  {RE, max(HL, height(RR))+1, {E, HL, L, RL}, RR}.
% Rotation Rechts
rightRotate({E, _H, {EL, _LH, LL, LR}, R}) ->
  HL = maxHeight(LR, R)+1,
  {EL, max(height(LL), HL)+1, LL, {E, HL, LR, R}}.

% -------------------------- %
% ------ TEST SECTION ------ %
% -------------------------- %

randomTree(Elem) when is_integer(Elem) ->
    BT = initBT(),
    Elems = util:randomliste(Elem),
    NewBT = insertInto(util:randomliste(Elem), BT),
    printBT(NewBT, "bevor.dot"),
    SmallerBT = deleteTill(trunc(Elem*0.88), util:shuffle(Elems), NewBT),
    printBT(SmallerBT, "nachher.dot").

insertInto([], BT) -> BT;
insertInto([Head|Tail], BT) -> insertInto(Tail, insertAT(BT, Head)).

deleteTill(0, _, BT) -> BT;
deleteTill(I, [Head|Tail], BT) -> deleteTill(I-1, util:shuffle(Tail), deleteAT(BT, Head)).