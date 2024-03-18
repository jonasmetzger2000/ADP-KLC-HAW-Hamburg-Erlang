-module(splaytree).
-export([initBT/0, isBT/1, isEmptyBT/1, insertST/2, insertAll/2, deleteST/2, deleteAll/2, findST/2, inOrderBT/1, printBT/2, test/1, zeitmessung/6]).
-define(DEBUG, 0).

% ------------------------------- %
% ------ initBT: ∅ → btree ------ %
% ------------------------------- %
initBT() -> avltree:initBT().


% -------------------------------- %
% ------ isBT: btree → bool ------ %
% -------------------------------- %
isBT({}) -> true;
isBT({E, H, L, R}) -> isBT_aux({E, H, L, R}, -1, -1);
isBT(_) -> false.

isBT_aux({}, _, _) -> true;
isBT_aux({Val, H, LT, RT}, Min, Max) when (is_integer(Val) and is_integer(H) and is_tuple(LT) and is_tuple(RT) and (Val > 0))->
    MaxH = max(getH(LT), getH(RT)),
    case H == (MaxH + 1) of
        false -> Out = false;
        true ->
            LVal = getV(LT),
            RVal = getV(RT),
            case checkSorted(Min, Max, Val, LVal, RVal) of
                true -> Out = isBT_aux(LT, Min, Val) and isBT_aux(RT, Val, Max);
                false -> Out = false
            end
    end,
    Out;
isBT_aux(_, _, _) -> false.


checkSorted(Min, Max, Val, LVal, RVal) -> 
    Erg = 
    ((LVal == -1) or
        ((LVal < Val) and 
        ((LVal < Max) or (Max == -1)) and 
        ((LVal > Min) or (Min == -1))))
    and
    ((RVal == -1) or
        ((RVal > Val) and
        ((RVal < Max) or (Max == -1)) and
        ((RVal > Min) or (Min == -1)))),
    %io:format("~p~n", [Erg]),
    Erg.


% ------------------------------------- %
% ------ isEmptyBT: btree → bool ------ %
% ------------------------------------- %
isEmptyBT(Tree) -> avltree:isEmptyBT(Tree).


% -------------------------------------------- %
% ------ insertST: btree × elem → btree ------ %
% -------------------------------------------- %
insertST(Tree, Elem) when (is_integer(Elem) and (Elem > 0))-> 
    {Temp, MaxT, Dirs, Fehler} = insertST_aux(Tree, Elem, 0, []),
    case Fehler of
        true -> Out = Tree;
        false ->
            case MaxT rem 2 of
                0 -> Out = Temp;

                %Move To Root über zig
                1 -> case Dirs of
                    [l] -> Out = rotateRight(Temp);
                    [r] -> Out = rotateLeft(Temp)
                end
            end
    end,
    Out;

insertST(Tree, _) -> Tree.

insertST_aux(Tree, Elem, S, Dirs) ->
    case isEmptyBT(Tree) of
        false -> 
            Val = getV(Tree),
            case Elem == Val of
                false ->
                    case Elem < Val of
                        true -> 
                            RT = getR(Tree),
                            {LT, MaxT, NextDirs, Fehler} = insertST_aux(getL(Tree), Elem, S+1, Dirs ++ [l]);
                        false ->
                            LT = getL(Tree),
                            {RT, MaxT, NextDirs, Fehler} = insertST_aux(getR(Tree), Elem, S+1, Dirs ++ [r])
                    end,
                    MaxH = max(getH(LT), getH(RT)) + 1,
                    NewT = {Val, MaxH, LT, RT},

                    %Move To Root über zigZag/zigZig
                    case (MaxT rem 2 == S rem 2) of
                        true -> 
                            {NewDirs, Last2} = seperateLast(NextDirs),
                            Out = {zigZ(NewT, Last2), MaxT, NewDirs, Fehler};
                        false ->
                            Out = {NewT, MaxT, NextDirs, Fehler}
                    end;
                true ->
                    Out = {{}, -1, [], true}
            end;
        true -> Out = {{Elem, 1, {}, {}}, S, Dirs, false}
    end,
    Out.

insertAll(Tree, []) -> Tree;
insertAll(Tree, [H|T]) -> 
    Z = insertST(Tree, H),
    insertAll(Z, T).


% --------------------------------------------- %
% ------ deleteST: btree × elem → btree  ------ %
% --------------------------------------------- %
deleteST(Tree, Elem) -> 
    {H, Temp} = findST(Tree, Elem),
    case H > 0 of
        false -> Out = Tree;        %Element nicht in Baum enthalten
        true -> 

            %Überprüfen, welcher Fall des Löschens eintritt
            {_, _, LT, RT} = Temp,
            L = not isEmptyBT(LT),
            R = not isEmptyBT(RT),
            case (L or R) of
                false -> Out = {};      %Fall 1
                true -> 
                    case (L and R) of
                        false ->        %Fall 2
                            case L of
                                true -> Out = LT;
                                false -> Out = RT
                            end;
                        true ->         %Fall 3
                            %Move To Root über zig
                            {0, TempL, MaxT, _, true, _} = findST_aux(LT, Elem, 0, []),
                            case MaxT rem 2 of
                                0 -> NewL = TempL;
                                1 -> NewL = rotateLeft(TempL)
                            end,
                            MaxH = max(getH(NewL), getH(RT) + 1),
                            Val = getV(NewL),
                            LLT = getL(NewL),
                            Out = {Val, MaxH, LLT, RT}
                    end
            end
    end,
    Out.

deleteAll(Tree, []) -> Tree;
deleteAll(Tree, [H|T]) -> 
    Z = deleteST(Tree, H),
    deleteAll(Z, T).
    


% --------------------------------------------- %
% ------ findST: btree × elem → integer  ------ %
% --------------------------------------------- %
findST(Tree, Elem) -> 
    {H, Temp, MaxT, Dirs, Fehler, _} = findST_aux(Tree, Elem, 0, []),
    case Fehler of
        true -> Out = Tree;         %Element nicht im Baum enthalten
        false ->
            %Move to Root über zig
            case MaxT rem 2 of
                0 -> Out = Temp;
                1 -> case Dirs of
                    [l] -> Out = rotateRight(Temp);
                    [r] -> Out = rotateLeft(Temp)
                end
            end
    end,
    {H, Out}.

findST_aux(Tree, Elem, S, Dirs) ->
    case isEmptyBT(Tree) of
        true -> Out = {0, {}, S-1, Dirs, true, true};           %Element nicht gefunden, zeige einen Fehler an und gehe einen Schritt zurück
        false ->
            Val = getV(Tree),
            case Elem == Val of
                true -> Out = {getH(Tree), Tree, S, Dirs, false, false};        %gefunden
                false ->
                    case Elem < Val of
                        true ->
                            RT = getR(Tree),
                            {H, LT, MaxS, TempDirs, Fehler, Letztes} = findST_aux(getL(Tree), Elem, S+1, Dirs ++ [l]);
                        false ->
                            LT = getL(Tree),
                            {H, RT, MaxS, TempDirs, Fehler, Letztes} = findST_aux(getR(Tree), Elem, S+1, Dirs ++ [r])
                    end,
                    %Move To Root über zig-zig und zig-zag
                    case (Letztes) of               % Abweichung vom Entwurf, da Fehler und Letztes beim letzten Schritt nur zusammen auftreten können
                        true -> NextDirs = Dirs;
                        false -> NextDirs = TempDirs
                    end,
                    MaxH = max(getH(LT), getH(RT)) + 1,
                    NewT = {Val, MaxH, LT, RT},
                    case ((MaxS rem 2 == S rem 2) and (not Letztes)) of
                        true ->
                            {NewDirs, Last2} = seperateLast(NextDirs),
                            Out = {H, zigZ(NewT, Last2), MaxS, NewDirs, Fehler, false};
                        false ->
                            Out = {H, NewT, MaxS, NextDirs, Fehler, false}
                    end
            end
    end,
    Out.


findAll(Tree, []) -> Tree;
findAll(Tree, [H|T]) -> 
    {_, Z} = findST(Tree, H),
    findAll(Z, T).

% -------------------------------------- %
% ------ inOrderBT: btree → list  ------ %
% -------------------------------------- %
inOrderBT({}) -> [];
inOrderBT({E, _H, L, R}) -> inOrderBT(L) ++ [E| inOrderBT(R)].


% ---------------------------------------------- %
% ------ printBT: btree × filename → dot  ------ %
% ---------------------------------------------- %
printBT(Tree, File) -> avltree:printBT(Tree, File).






% ------------------------------------------- %
% ------ zigZig: btree × Elem → btree  ------ %
% ------ zigZag: btree × Elem → btree  ------ %
% ------------------------------------------- %
% zigZ = Zusammenfassung von zigZig und zigZag
zigZ({V, H, L, R}, Dirs) -> 
    [Dir1, Dir2] = Dirs,
    case Dir1 of
        l -> case Dir2 of
            l -> Out = rotateRight(rotateRight({V, H, L, R}));      %Im Entwurf dargestellt: zweite Variante des zig-zig, Abweichung, da im Test die erste Variante verlangt wird.
            r -> Out = rotateRight({V, H, rotateLeft(L), R})
            end;
        r -> case Dir2 of
            l -> Out = rotateLeft({V, H, L, rotateRight(R)});
            r -> Out = rotateLeft(rotateLeft({V, H, L, R}))         %Im Entwurf dargestellt: zweite Variante des zig-zig, Abweichung, da im Test die erste Variante verlangt wird.
            end
    end,
    Out.



% ---------------------------------------- %
% ------ rotateLeft: btree → btree  ------ %
% ---------------------------------------- %
rotateLeft(Tree) -> 
    {ElemT, _H, L1, P} = Tree,
    {ElemP, Hp, L2, R} = P,
    NewHT = max(getH(L1), getH(L2)) + 1,
    NewH = max(NewHT + 1, Hp),
    NewL = {ElemT, NewHT, L1, L2},
    Out = {ElemP, NewH, NewL, R},
    Out.


% ----------------------------------------- %
% ------ rotateRight: btree → btree  ------ %
% ----------------------------------------- %
rotateRight(Tree) -> 
    {ElemT, _H, P, R1} = Tree,
    {ElemP, Hp, L, R2} = P,
    NewHT = max(getH(R1), getH(R2)) + 1,
    NewH = max(NewHT + 1, Hp),
    NewR = {ElemT, NewHT, R2, R1},
    Out = {ElemP, NewH, L, NewR},
    Out.





getV({}) -> -1;
getV({Elem, _H, _L, _R}) -> Elem.

getH({}) -> 0;
getH({_Elem, H, _L, _R}) -> H.

getL({_, _, L, _}) -> L.

getR({_, _, _, R}) -> R.

seperateLast([F,S|[]]) -> {[], [F,S]};
seperateLast([H|T]) -> 
    {Lst, Last} = seperateLast(T),
    {[H|Lst], Last}.















% -------------------------- %
% ------ TEST SECTION ------ %
% -------------------------- %

%Fügt die Elemente einer Liste in einen Splaytree ein und prüft anschließend, ob der entstehende Baum noch ein BT ist
insertAllTest(Tree, [], Bool) -> {Tree, Bool};
insertAllTest(Tree, [H|T], Bool) when Bool == true -> 
    Z = insertST(Tree, H),
    BoolN = isBT(Z),
    insertAllTest(Z, T, BoolN);
insertAllTest(Tree, _, _) -> {Tree, false}.


%Entfernt die Elemente einer Liste aus einem Splaytree und prüft anschließend, ob der entstehende Baum noch ein BT ist
deleteAllTest(Tree, [], Bool) -> {Tree, Bool};
deleteAllTest(Tree, [H|T], Bool) when Bool == true -> 
    Z = deleteST(Tree, H),
    BoolN = isBT(Z),
    deleteAllTest(Z, T, BoolN);
deleteAllTest(Tree, _, _) -> {Tree, false}.


%Testet, ob das Hinzufügen korrekt funktioniert
testInsert(Elems) -> 
    {Tree, Bool} = insertAllTest({}, util:randomliste(Elems), true),
    isBT(Tree) and Bool.

%Testet, ob das Entfernen korrekt funktioniert, wenn die gleichen Elemente entfernt werden, die hinzugefügt werden
testDelete(Elems) ->
    {Tree1, Bool1} = insertAllTest({}, util:randomliste(Elems), true),
    {Tree2, Bool2} = deleteAllTest(Tree1, util:randomliste(Elems), Bool1),
    (isEmptyBT(Tree2) and Bool1 and Bool2).

%Testet, ob das Entfernen korrekt funktioniert, wenn mehr Elemente entfernt werden, als im Baum enthalten sind
testDeleteMore(Elems) ->
    {Tree1, Bool1} = insertAllTest({}, util:randomliste(Elems), true),
    {Tree2, Bool2} = deleteAllTest(Tree1, util:randomliste(Elems * 2), Bool1),
    (isEmptyBT(Tree2) and Bool1 and Bool2).

%Testet, ob das Entfernen korrekt funktioniert, wenn weniger Elemente entfernt werden, als im Baum enthalten sind
testDeleteLess(Elems) ->
    {Tree1, Bool1} = insertAllTest({}, util:randomliste(Elems), true),
    {Tree2, Bool2} = deleteAllTest(Tree1, util:randomliste(Elems div 2), Bool1),
    (isBT(Tree2) and Bool1 and Bool2).

%Prüft, ob das Hinzufügen und Entfernen von Elementen korrekt funktioniert. Da das löschen das Suchen beinhaltet, muss das Suchen nicht explizit getestet werden
test(Elems) -> 
    Bool1 = testInsert(Elems),
    Bool2 = testDelete(Elems),
    Bool3 = testDeleteMore(Elems),
    Bool4 = testDeleteLess(Elems),
    io:format("~p; ~p; ~p; ~p~n", [Bool1, Bool2, Bool3, Bool4]),
    Bool1 and Bool2 and Bool3 and Bool4.
% 13.01.24 12:00 - 





% ----------------------- %
% ------ Messungen ------ %
% ----------------------- %

createCompletelyRandom(Elems, _, _) when Elems =< 0 -> [];
createCompletelyRandom(Elems, Min, Max) -> [rand:uniform(Max - Min) + Max|createCompletelyRandom(Elems-1, Min, Max)].

createNonRepeatable(Elems) -> util:randomliste(Elems).

createRepeated(Elems, _Min, _Max, _MaxSectorLen, _MaxSpan) when Elems =< 0 -> [];
createRepeated(Elems, Min, Max, MaxSectorLen, MaxSpan) ->
    Low = rand:uniform(Max - Min) + Min,
    SectorLen = rand:uniform(MaxSectorLen),
    Sector = createCompletelyRandom(SectorLen, Low, Low + MaxSpan),
    Sector ++ createRepeated(Elems - SectorLen, Min, Max, MaxSectorLen, MaxSpan).

insertAllAVL(Tree, []) -> Tree;
insertAllAVL(Tree, [H|T]) ->
    Z = avltree:insertAT(Tree, H),
    insertAllAVL(Z, T).

deleteAllAVL(Tree, []) -> Tree;
deleteAllAVL(Tree, [H|T]) ->
    Z = avltree:deleteAT(Tree, H),
    deleteAllAVL(Z, T).

findAllAVL(Tree, []) -> Tree;
findAllAVL(Tree, [H|T]) ->
    avltree:findBT(Tree, H),
    findAllAVL(Tree, T).

zeitmessungFor(Elems, Min, Max, Dateiname) ->

    %Kompletter Zufall
    TreeList1 = createCompletelyRandom(Elems, Min, Max),
    Then1 = erlang:timestamp(),
    Tree1 = insertAll({}, TreeList1),
    Now1 = erlang:timestamp(),
    ThenAVL1 = erlang:timestamp(),
    AVL1 = insertAllAVL({}, TreeList1),
    NowAVL1 = erlang:timestamp(),
    SearchList1 = createCompletelyRandom(Elems, Min, Max),
    Then2 = erlang:timestamp(),
    findAll(Tree1, SearchList1),
    Now2 = erlang:timestamp(),
    ThenAVL2 = erlang:timestamp(),
    findAllAVL(AVL1, SearchList1),
    NowAVL2 = erlang:timestamp(),
    DeleteList1 = createCompletelyRandom(Elems, Min, Max),
    Then3 = erlang:timestamp(),
    deleteAll(Tree1, DeleteList1),
    Now3 = erlang:timestamp(),
    ThenAVL3 = erlang:timestamp(),
    deleteAllAVL(AVL1, DeleteList1),
    NowAVL3 = erlang:timestamp(),

    %Nicht Wiederholt
    TreeList2 = createNonRepeatable(Elems),
    Then4 = erlang:timestamp(),
    Tree2 = insertAll({}, TreeList2),
    Now4 = erlang:timestamp(),
    ThenAVL4 = erlang:timestamp(),
    AVL2 = insertAllAVL({}, TreeList2),
    NowAVL4 = erlang:timestamp(),
    SearchList2 = createNonRepeatable(Elems),
    Then5 = erlang:timestamp(),
    findAll(Tree2, SearchList2),
    Now5 = erlang:timestamp(),
    ThenAVL5 = erlang:timestamp(),
    findAllAVL(AVL2, SearchList2),
    NowAVL5 = erlang:timestamp(),
    DeleteList2 = createNonRepeatable(Elems),
    Then6 = erlang:timestamp(),
    deleteAll(Tree2, DeleteList2),
    Now6 = erlang:timestamp(),
    ThenAVL6 = erlang:timestamp(),
    deleteAllAVL(AVL2, DeleteList2),
    NowAVL6 = erlang:timestamp(),

    %Wiederholt
    TreeList3 = createRepeated(Elems, Min, Max, Elems div 50, (Max - Min) div 100),
    Then7 = erlang:timestamp(),
    Tree3 = insertAll({}, TreeList3),
    Now7 = erlang:timestamp(),
    ThenAVL7 = erlang:timestamp(),
    AVL3 = insertAllAVL({}, TreeList3),
    NowAVL7 = erlang:timestamp(),
    SearchList3 = createRepeated(Elems, Min, Max, Elems div 50, (Max - Min) div 100),
    Then8 = erlang:timestamp(),
    findAll(Tree3, SearchList3),
    Now8 = erlang:timestamp(),
    ThenAVL8 = erlang:timestamp(),
    findAllAVL(AVL3, SearchList3),
    NowAVL8 = erlang:timestamp(),
    DeleteList3 = createRepeated(Elems, Min, Max, Elems div 50, (Max - Min) div 100),
    Then9 = erlang:timestamp(),
    deleteAll(Tree2, DeleteList3),
    Now9 = erlang:timestamp(),
    ThenAVL9 = erlang:timestamp(),
    deleteAllAVL(AVL3, DeleteList3),
    NowAVL9 = erlang:timestamp(),

    Dif1 = timestampDif(Then1, Now1),
    Dif2 = timestampDif(Then2, Now2),
    Dif3 = timestampDif(Then3, Now3),
    Dif4 = timestampDif(Then4, Now4),
    Dif5 = timestampDif(Then5, Now5),
    Dif6 = timestampDif(Then6, Now6),
    Dif7 = timestampDif(Then7, Now7),
    Dif8 = timestampDif(Then8, Now8),
    Dif9 = timestampDif(Then9, Now9),

    AVLDif1 = timestampDif(ThenAVL1, NowAVL1),
    AVLDif2 = timestampDif(ThenAVL2, NowAVL2),
    AVLDif3 = timestampDif(ThenAVL3, NowAVL3),
    AVLDif4 = timestampDif(ThenAVL4, NowAVL4),
    AVLDif5 = timestampDif(ThenAVL5, NowAVL5),
    AVLDif6 = timestampDif(ThenAVL6, NowAVL6),
    AVLDif7 = timestampDif(ThenAVL7, NowAVL7),
    AVLDif8 = timestampDif(ThenAVL8, NowAVL8),
    AVLDif9 = timestampDif(ThenAVL9, NowAVL9),

    ThenBT1 = erlang:timestamp(),
    true = isBT(Tree1),
    NowBT1 = erlang:timestamp(),
    ThenBT2 = erlang:timestamp(),
    true = avltree:isBT(AVL1),
    NowBT2 = erlang:timestamp(),
    ThenBT3 = erlang:timestamp(),
    true = isBT(Tree2),
    NowBT3 = erlang:timestamp(),
    ThenBT4 = erlang:timestamp(),
    true = avltree:isBT(AVL2),
    NowBT4 = erlang:timestamp(),
    ThenBT5 = erlang:timestamp(),
    true = isBT(Tree3),
    NowBT5 = erlang:timestamp(),
    ThenBT6 = erlang:timestamp(),
    true = avltree:isBT(AVL3),
    NowBT6 = erlang:timestamp(),

    IsBTDif1 = timestampDif(ThenBT1, NowBT1),
    IsBTDif2 = timestampDif(ThenBT2, NowBT2),
    IsBTDif3 = timestampDif(ThenBT3, NowBT3),
    IsBTDif4 = timestampDif(ThenBT4, NowBT4),
    IsBTDif5 = timestampDif(ThenBT5, NowBT5),
    IsBTDif6 = timestampDif(ThenBT6, NowBT6),

    io:format("Splaytree für ~p Elemente~n~n", [Elems]),

    io:format("insert:\t~pms; ~pms; ~pms~n", [Dif1, Dif4, Dif7]),
    io:format("delete:\t~pms; ~pms; ~pms~n", [Dif2, Dif5, Dif8]),
    io:format("  find:\t~pms; ~pms; ~pms~n", [Dif3, Dif6, Dif9]),
    io:format("  isBT:\t~pms; ~pms; ~pms~n", [IsBTDif1, IsBTDif3, IsBTDif5]),



    io:format("~n~n~nAVLtree für ~p Elemente~n~n", [Elems]),

    io:format("insert:\t~pms; ~pms; ~pms~n", [AVLDif1, AVLDif4, AVLDif7]),
    io:format("delete:\t~pms; ~pms; ~pms~n", [AVLDif2, AVLDif5, AVLDif8]),
    io:format("  find:\t~pms; ~pms; ~pms~n", [AVLDif3, AVLDif6, AVLDif9]),
    io:format("  isBT:\t~pms; ~pms; ~pms~n", [IsBTDif2, IsBTDif4, IsBTDif6]),

    {ok, Datei} = file:open(Dateiname, [append]),
    file:write(Datei, getLine([Elems, Dif1, Dif2, Dif3, Dif4, Dif5, Dif6, Dif7, Dif8, Dif9, IsBTDif1, IsBTDif3, IsBTDif5, AVLDif1, AVLDif2, AVLDif3, AVLDif4, AVLDif5, AVLDif6, AVLDif7, AVLDif8, AVLDif9, IsBTDif2, IsBTDif4, IsBTDif6])).

zeitmessung(Elems, Min, Max, Iterationen, Inkrement, Dateiname) ->
    {ok, Datei} = file:open(Dateiname, [write]),
    file:write(Datei, "Elemente; insertSplay Random; insertSplay Nicht Wiederholend; insertSplay Wiederholend; findSplay Random; findSplay Nicht Wiederholend; findSplay Wiederholend; deleteSplay Random; deleteSplay Nicht Wiederholend; deleteSplay Wiederholend; isBTSplay Random; isBTSplay Nicht Wiederholend; isBTSplay Wiederholend; insertAVL Random; insertAVL Nicht Wiederholend; insertAVL Wiederholend; findAVL Random; findAVL Nicht Wiederholend; findAVL Wiederholend; deleteAVL Random; deleteAVL Nicht Wiederholend; deleteAVL Wiederholend; isBTAVL Random; isBTAVL Nicht Wiederholend; isBTAVL Wiederholend; \n"),
    iteriere(Elems, Min, Max, Iterationen, 0, Inkrement, Dateiname).

iteriere(_, _, _, Its, Its, _, _) -> ok;
iteriere(Elems, Min, Max, Its, Step, Inkr, Name) ->
    zeitmessungFor(Elems, Min, Max, Name),
    iteriere(Elems + Inkr, Min, Max, Its, Step + 1, Inkr, Name).



timestampDif(TS1, TS2) -> timer:now_diff(TS2, TS1) div 1000.

getLine(Lst) -> getLine(Lst, "").
getLine([], Line) -> Line ++ "\n";
getLine([H|T], Line) -> getLine(T, Line ++ integer_to_list(H) ++ ";").