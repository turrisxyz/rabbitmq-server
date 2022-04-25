%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2022 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_router).
-include_lib("stdlib/include/qlc.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").

-export([match_bindings/2, match_routing_key/2, route/2]).

%%----------------------------------------------------------------------------

-export_type([routing_key/0, match_result/0]).

-type routing_key() :: binary().
-type match_result() :: [rabbit_types:binding_destination()].

%%----------------------------------------------------------------------------

-spec match_bindings(rabbit_types:binding_source(),
                     fun ((rabbit_types:binding()) -> boolean())) ->
    match_result().

match_bindings(SrcName, Match) ->
    MatchHead = #route{binding = #binding{source      = SrcName,
                                          _           = '_'}},
    Routes = ets:select(rabbit_route, [{MatchHead, [], [['$_']]}]),
    [Dest || [#route{binding = Binding = #binding{destination = Dest}}] <-
             Routes, Match(Binding)].

-spec match_routing_key(rabbit_types:binding_source(),
                        [routing_key(), ...] | ['_']) ->
    match_result().

match_routing_key(SrcName, [RoutingKey]) ->
    find_routes(#route{binding = #binding{source      = SrcName,
                                          destination = '$1',
                                          key         = RoutingKey,
                                          _           = '_'}},
                []);
match_routing_key(SrcName, [_|_] = RoutingKeys) ->
    find_routes(#route{binding = #binding{source      = SrcName,
                                          destination = '$1',
                                          key         = '$2',
                                          _           = '_'}},
                [list_to_tuple(['orelse' | [{'=:=', '$2', RKey} ||
                                            RKey <- RoutingKeys]])]).

%% Normally we'd call mnesia:dirty_select/2 here, but that is quite
%% expensive for the same reasons as above, and, additionally, due to
%% mnesia 'fixing' the table with ets:safe_fixtable/2, which is wholly
%% unnecessary. According to the ets docs (and the code in erl_db.c),
%% 'select' is safe anyway ("Functions that internally traverse over a
%% table, like select and match, will give the same guarantee as
%% safe_fixtable.") and, furthermore, even the lower level iterators
%% ('first' and 'next') are safe on ordered_set tables ("Note that for
%% tables of the ordered_set type, safe_fixtable/2 is not necessary as
%% calls to first/1 and next/2 will always succeed."), which
%% rabbit_route is.
find_routes(MatchHead, Conditions) ->
    ets:select(rabbit_route, [{MatchHead, Conditions, ['$1']}]).

%% rabbit_router:match_bindings/2 and rabbit_router:match_routing_key/2 use
%% ets:select/2 to get destinations. ets:select/2 is expensive because it needs
%% to compile the match spec every time and lookup does not happen by a hash key.
%%
%% In contrast, rabbit_router:route/2 increases end-to-end message
%% sending throughput (i.e. from RabbitMQ client to the queue process)
%% by up to 35% by using ets:lookup_element/3.
%% This function is currently only used by the direct exchange type because
%% only the direct exchange type uses the rabbit_index_route table to store its
%% bindings by key {source_exchange, routing_key}.
-spec route(rabbit_types:binding_source(), [routing_key(), ...]) ->
    match_result().
route(SrcName, [RoutingKey]) ->
    %% optimization
    destinations(SrcName, RoutingKey);
route(SrcName, [_|_] = RoutingKeys) ->
    lists:flatmap(fun(Key) ->
                          destinations(SrcName, Key)
                  end, RoutingKeys).

destinations(SrcName, RoutingKey) ->
    %% Prefer try-catch block over checking Key existence with ets:member/2.
    %% The latter reduces throughput by a few thousand messages per second because
    %% of function db_member_hash in file erl_db_hash.c.
    %% We optimise for the happy path, that is the binding / table key is present.
    try
        ets:lookup_element(rabbit_index_route, {SrcName, RoutingKey}, 3)
    catch
        error:badarg ->
            []
    end.
