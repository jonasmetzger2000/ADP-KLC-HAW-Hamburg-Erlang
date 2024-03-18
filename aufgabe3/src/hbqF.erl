-module(hbqF).
-export([initHBQ/2,pushHBQ/2,deliverMSG/3,listHBQ/1,listDLQ/1,dellHBQ/1,anyM/2,genMes/2]).

% HBQName, so wie er in der server.cfg unter {hbqname, eingetragen ist
% DLQSize, so wie sie in der server.cfg unter {dlqlimit, eingetragen ist
initHBQ(DLQSize,HBQName) -> 
	hbq:initHBQ(DLQSize,HBQName).	

% HBQ, die PID der HBQ oder das Tupel {HBQName,HBQnode}, wie sie in der server.cfg eingetragen sind
% NNr, eine legale Nachrichtennummer
% Msg, ein string
% TSclientout, ein Zeitstempel mittel erlang:timestamp() erstellt
pushHBQ(HBQ,[NNr,Msg,TSclientout]) ->
	HBQ ! {self(), {request,pushHBQ,[NNr,Msg,TSclientout]}},
	receive
		{reply, _Answer} -> done
	end,
	HBQ.

% HBQ, die PID der HBQ oder das Tupel {HBQName,HBQnode}, wie sie in der server.cfg eingetragen sind
% NNr, eine legale Nachrichtennummer
% ToClient, eine PID eines Clients
deliverMSG(HBQ,NNr,ToClient) ->
	HBQ ! {self(), {request,deliverMSG,NNr,ToClient}},
	receive
		{reply, Answer} -> done
	end,
	{HBQ,Answer}.

% HBQ, die PID der HBQ oder das Tupel {HBQName,HBQnode}, wie sie in der server.cfg eingetragen sind
listHBQ(HBQ) ->
	HBQ ! {self(), {request,listHBQ}},
	receive
		{reply, _Answer} -> done
	end,
	HBQ.

% HBQ, die PID der HBQ oder das Tupel {HBQName,HBQnode}, wie sie in der server.cfg eingetragen sind
listDLQ(HBQ) ->
	HBQ ! {self(), {request,listDLQ}},
	receive
		{reply, _Answer} -> done
	end,
	HBQ.

% HBQ, die PID der HBQ oder das Tupel {HBQName,HBQnode}, wie sie in der server.cfg eingetragen sind
dellHBQ(HBQ) ->
	HBQ ! {self(), {request,dellHBQ}},
	receive
		{reply, Answer} -> done
	end,
	Answer.

% HBQ, die PID der HBQ oder das Tupel {HBQName,HBQnode}, wie sie in der server.cfg eingetragen sind
% Message, unterliegt keiner Bedingung
anyM(HBQ,Message) ->
	HBQ ! {self(), Message}.
	
% generiert eine Nachricht, wie sie ein Redakteur senden würde
genMes(NNumber,Message) when is_integer(NNumber) and is_list(Message) ->
	[NNumber,Message,erlang:timestamp()];
genMes(_NNumber,_Message) ->
	[42,"genMes nicht mit Zahl und Zeichenkette aufgerufen!",erlang:timestamp()].		