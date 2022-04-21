%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2022 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_router).
-include_lib("rabbit_common/include/rabbit.hrl").

-export([match_bindings/2, match_routing_key/2]).

%%----------------------------------------------------------------------------

-export_type([routing_key/0, match_result/0]).

-type routing_key() :: binary().
-type match_result() :: [rabbit_types:binding_destination()].

%%----------------------------------------------------------------------------

-spec match_bindings(rabbit_types:binding_source(),
                     fun ((rabbit_types:binding()) -> boolean())) ->
    match_result().

match_bindings(SrcName, Match) ->
    Bindings = ets:lookup(rabbit_route_index_2, SrcName),
    lists:filtermap(
      fun(#binding{destination = Destination} = Binding) ->
              case Match(Binding) of
                  true -> {true, Destination};
                  false -> false
              end
      end, Bindings).

-spec match_routing_key(rabbit_types:binding_source(),
                        [routing_key(), ...] | ['_']) ->
    match_result().

match_routing_key(SrcName, ['_']) ->
    %% The '_' wildcard got used in the match specification of ets:select/2
    %% in a previous version of this function.
    destinations(SrcName);
match_routing_key(SrcName, [RoutingKey]) ->
    %% optimization
    destinations(SrcName, RoutingKey);
match_routing_key(SrcName, [_|_] = RoutingKeys) ->
    %%TODO
    %% Q1: filter out duplicate destinations?
    %% A1: Seems to happen two levels up in
    %% https://github.com/rabbitmq/rabbitmq-server/blob/08ec75e208880890222571a3d700404d31c11632/deps/rabbit/src/rabbit_exchange.erl#L420
    %% Q2: What about other exchanges calling rabbit_router directly? Compare result sets from previous ets:select call
    lists:flatmap(fun(RKey) ->
                          destinations(SrcName, RKey)
                  end, RoutingKeys).

destinations(SrcName) ->
    lookup_element(rabbit_route_index_2, SrcName, 4).

destinations(SrcName, RoutingKey) ->
    lookup_element(rabbit_route_index_1, {SrcName, RoutingKey}, 3).

%% Prefer try-catch block over checking Key existence with ets:member/2.
%% The latter reduces throughput by a few thousand messages per second because
%% of function db_member_hash in file erl_db_hash.c.
%% We optimise for the happy path, that is the binding (table Key) is present.
lookup_element(Tab, Key, Pos) ->
    try
        ets:lookup_element(Tab, Key, Pos)
    catch
        error:badarg ->
            []
    end.
