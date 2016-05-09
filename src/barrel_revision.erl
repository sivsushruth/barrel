%% Copyright 2016 Benoit Chesneau
%%
%% Licensed under the EUPL, Version 1.1 only (the "Licence");
%% You may not use this work except in compliance with the Licence.
%% You may obtain a copy of the Licence at:
%%
%% https://joinup.ec.europa.eu/software/page/eupl
%%
%% Unless required by applicable law or agreed to in  writing, software
%% distributed under the Licence is distributed on an "AS IS" basis, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the Licence for the specific language governing permissions and
%% limitations under the Licence.

-module(barrel_revision).
-export([revid/3, parse/1, compare/2]).


%% @doc create a new revision id using a new position, its parent id and the related document.
-spec revid(Pos :: integer(), Parent :: binary(), Doc :: map()) -> binary().
revid(Pos, Parent, Body) ->
    Ctx0 = crypto:hash_init(md5),
	%% filter the document remove all private properties
    Body1 = maps:filter(fun
                            (<< "" >>, _) -> false;
                            (<< "_", _/binary >>, _) -> false;
                            (_, _) -> true
                        end, Body),
    BinPos = list_to_binary(integer_to_list(Pos)),

	Ctx2 = lists:foldl(fun(V, Ctx1) ->
							   crypto:hash_update(Ctx1, V)
					   end, Ctx0, [BinPos, Parent, term_to_binary(Body1)]),

    Digest = crypto:hash_final(Ctx2),
	<< BinPos/binary, "-", (barrel_lib:to_hex(Digest))/binary >>.


%% @doc parse a revision binary
-spec parse(Revision :: binary()) -> {Pos :: integer(), Hash :: binary()}.
parse(<<"">>) -> {0, <<"">>};
parse(BinRev) when is_binary(BinRev) ->
	[BinPos, Hash] = binary:split(BinRev, <<"-">>),
    Pos = list_to_integer(binary_to_list(BinPos)),
    {Pos, Hash};
parse(StrRev) ->
    parse(list_to_binary(StrRev)).

%% @doc compare 2 revisions. return -1 if RevA < RevB, 1, if RevA > RevB or 0 if they are equal.
-spec compare(RevA :: binary(), RevB :: binary()) -> 1 | -1 | 0.
compare(RevA, RevB) ->
    compare1(parse(RevA), parse(RevB)).


compare1({PA, _}, {PB, _}) when PA > PB -> 1;
compare1({PA, _}, {PB, _}) when PA < PB -> -1;
compare1({_, HA}, {_, HB}) when HA > HB -> 1;
compare1({_, HA}, {_, HB}) when HA < HB -> -1;
compare1(_, _) -> 0.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

revid_test() ->
	Rev = barrel_revision:revid(10, <<"9-test">>, #{<<"_id">> => <<"test">>,
													<<"hello">> => <<"yo">>}),
	?assertEqual(<<"10-2f25ea96da3fed514795b0ced028d58a">>, Rev).


parse_test() ->
	Parsed = barrel_revision:parse(<<"10-2f25ea96da3fed514795b0ced028d58a">>),
	?assertEqual({10, <<"2f25ea96da3fed514795b0ced028d58a">>}, Parsed).



-endif.
