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

-module(barrel_doc).

-export([get_idrev/1,
         get_id/1,
         get_rev/1,
         is_deleted/1]).

-export([add_meta/3]).

-type doc() :: #{}.
-type docid() :: binary().
-type rev() :: binary().
-export_types([doc/0]).


%% @doc extract the docid and revision from the doc
-spec get_idrev(doc()) -> { docid() | undefined, rev() | undefined}.
get_idrev(#{ <<"_id">> := Id, <<"_rev">> := Rev }) -> {Id, Rev};
get_idrev(#{ <<"_id">> := Id }) -> {Id, undefined};
get_idrev(_) -> {undefined, undefined}.


%% @doc extract the docid from the doc
-spec get_id(doc()) -> docid() | undefined.
get_id(#{ <<"_id">> := Id }) -> Id;
get_id(_) -> undefined.

%% @doc extract the revision from the doc
-spec get_rev(doc()) -> rev() | undefined.
get_rev(#{ <<"_rev">> := Rev}) -> Rev;
get_rev(_) -> undefined.

%% @doc return if a doc is deleted or not
-spec is_deleted(doc()) -> true | false.
is_deleted(#{<< "_deleted" >> := Del}) -> Del;
is_deleted(_) -> false.



add_meta([{revs, true} | Rest], Doc, Tree) ->
    Revs = maps:keys(Tree),
    add_meta(Rest, Doc#{ <<"_revs">> => Revs }, Tree);
add_meta([revs | Rest], Doc, Tree) ->
    Revs = maps:keys(Tree),
    add_meta(Rest, Doc#{ <<"_revs">> => Revs }, Tree);
add_meta([{revs_info, true} | Rest], Doc, Tree) ->
    RevsInfo = maps:values(Tree),
    add_meta(Rest, Doc#{ <<"_revs_info">> => RevsInfo }, Tree);
add_meta([revs_info | Rest], Doc, Tree) ->
    RevsInfo = maps:values(Tree),
    add_meta(Rest, Doc#{ <<"_revs_info">> => RevsInfo }, Tree);
add_meta([{open_revs, true} | Rest], Doc, Tree) ->
    Leaves = barrel_revtree:leaves(Tree),
    add_meta(Rest, Doc#{ <<"_open_revs">> => Leaves }, Tree);
add_meta([open_revs | Rest], Doc, Tree) ->
    Leaves = barrel_revtree:leaves(Tree),
    add_meta(Rest, Doc#{ <<"_open_revs">> => Leaves }, Tree);
add_meta([_ | Rest], Doc, Meta) ->
    add_meta(Rest, Doc, Meta);
add_meta([], Doc, _Meta) ->
    Doc.
