.. highlight:: erlang

.. 7 Tables and databases

========================
7 テーブルとデータベース
========================

.. 7.1 Ets, Dets and Mnesia

.. index::
  single: Ets
  single: Dets
  single: Mnesia

7.1 Ets, Dets, Mnesia
=====================

.. Every example using Ets has a corresponding example in Mnesia. In general all Ets examples also apply to Dets tables.

Etsの使用方法に関するサンプルというのは、全てMnesiaのサンプルに関連するものです。一般的にはEtsに関するサンプルは全て、Detsテーブルに対しても適用することができます。

.. 7.1.1 Select/Match operations

.. index::
  single: テーブル; セレクト
  single: テーブル; マッチ操作
  single: etsモジュール; match_object/2
  single: etsモジュール; select/2
  single: etsモジュール; match/2
  single: mnesiaモジュール; match_object/3
  single: mnesiaモジュール; select/3
  pair: テーブル; bag
  pair: テーブル; ordered_set
  single: _; テーブルのマッチ

7.1.1 セレクト、マッチ操作
---------------------------

.. Select/Match operations on Ets and Mnesia tables can become very expensive operations. They usually need to scan the complete table. You should try to structure your data so that you minimize the need for select/match operations. However, if you really need a select/match operation, it will still be more efficient than using tab2list. Examples of this and also of ways to avoid select/match will be provided in some of the following sections. The functions ets:select/2 and mnesia:select/3 should be preferred over ets:match/2,ets:match_object/2, and mnesia:match_object/3.

EtsテーブルとMnesiaテーブルに関するセレクト、マッチの操作は非常に高価な操作になる可能性があります。これらの操作を行うときはテーブルを全てスキャンする必要があります。そのため、セレクト、マッチの操作を行う必要性を最小限するにようなデータ構造を目指さなければなりません。しかし、もし本当にセレクト、マッチの操作が必要な場合には、これは ``tab2list`` を使用する場合と比べれば、まだ効率的です。これから続くセクションでは、このことや、セレクト、マッチを避ける方法に関するサンプルを提供していきます。 ``ets:select/2`` と、 ``mnesia:select/3`` という関数は ``ets:match/2``, ``ets:match_object/2``, ``mnesia:match_object/3`` と比べればまだ好ましいと言えるでしょう。

.. Note

..  There are exceptions when the complete table is not scanned, for instance if part of the key is bound when searching an ordered_set table, or if it is a Mnesia table and there is a secondary index on the field that is selected/matched. If the key is fully bound there will, of course, be no point in doing a select/match, unless you have a bag table and you are only interested in a sub-set of the elements with the specific key.

.. Note::

  テーブル全体がスキャンされない場合には、いくつか例外的なケースがあります。例えば、 ``ordered_set`` テーブルを検索する時に、キーの一部が制限されている場合、もしくは、セカンダリーインデックスにより、効率的にMnesiaテーブル上のフィールドを検索している場合などです。もちろん、これは選択やマッチのときに、 ``bag`` 形式のテーブルを効率的に使用してインデックスを利用して、特定のサブセットのアクセスだけに限定できた場合に限ります。

.. When creating a record to be used in a select/match operation you want most of the fields to have the value '_'. The easiest and fastest way to do that is as follows

選択、マッチ操作の中で使用される、一部以外のフィールドの値が ``'_'`` というレコードを作成する場合(訳注: この例では年齢だけがリテラルで、他のフィールドはすべて ``'_'`` )は、以下のようなコードがもっとも簡単で最速の実装になります::

  #person{age = 42, _ = '_'}. 

.. 7.1.2 Deleting an element

.. index::
  single: テーブル; 要素削除
  single: etsモジュール; delete/2
  single: etsモジュール; lookup/2

7.1.2 要素の削除
----------------

.. The delete operation is considered successful if the element was not present in the table. Hence all attempts to check that the element is present in the Ets/Mnesia table before deletion are unnecessary. Here follows an example for Ets tables.

削除操作はテーブルの中に要素がなかったとしても成功とみなされます。そのため、すべての人は削除する前にEts/Mnesiaテーブルに要素が存在するかどうかチェックしたがりますが、それは不要です。Etsテーブルに関するサンプルを以下に示します。

.. DO

推奨::
 
  ...
  ets:delete(Tab, Key),
  ...

.. DO NOT

非推奨::

  ...
  case ets:lookup(Tab, Key) of
      [] ->
          ok;
      [_|_] ->
          ets:delete(Tab, Key)
  end,
  ...

.. 7.1.3 Data fetching

.. index::
  single: etsモジュール; lookup/2
  single: テーブル; データの取得

7.1.3 データの取得
------------------

.. Do not fetch data that you already have! Consider that you have a module that handles the abstract data type Person. You export the interface function print_person/1 that uses the internal functions print_name/1, print_age/1, print_occupation/1.

すでに持っているデータを取得することは避けましょう。抽象データ型の ``Person`` を操作するモジュールがあったとします。このモジュールはインタフェースとなる関数, ``print_person/1`` をエクスポートしています。また、この関数は内部関数として ``print_name/1``, ``print_age/1``, ``print_occupation/1`` の3つの関数を利用しています。

.. Note::

  ``print_name/1`` などの関数をインタフェース関数を定義すると、問題を明るみにだすことができます。ユーザ向けのインタフェースとして、データの内部表現を公開して知らせたくない場合に効果を発揮します。

..   If the functions print_name/1 and so on, had been interface functions the matter comes in to a whole new light, as you do not want the user of the interface to know about the internal data representation.

.. DO
.. %%% Interface function  
..      %% Look up the person in the named table person,
.. %%% Internal functions  

推奨::

  %%% インタフェース関数
  print_person(PersonId) ->
      %% 名前付きテーブルのpersionから人を検索
      case ets:lookup(person, PersonId) of
          [Person] ->
              print_name(Person),
              print_age(Person),
              print_occupation(Person);
          [] ->
              io:format("No person with ID = ~p~n", [PersonID])
      end.

  %%% 内部関数
  print_name(Person) -> 
      io:format("No person ~p~n", [Person#person.name]).
                      
  print_age(Person) -> 
      io:format("No person ~p~n", [Person#person.age]).

  print_occupation(Person) -> 
      io:format("No person ~p~n", [Person#person.occupation]).

.. DO NOT

非推奨::

  %%% インタフェース関数
  print_person(PersonId) ->
      %% 名前付きテーブルのpersonから人を検索
      case ets:lookup(person, PersonId) of
          [Person] ->
              print_name(PersonID),
              print_age(PersonID),
              print_occupation(PersonID);
          [] ->
              io:format("No person with ID = ~p~n", [PersonID])
      end.

  %%% 内部関数
  print_name(PersonID) -> 
      [Person] = ets:lookup(person, PersonId),
      io:format("No person ~p~n", [Person#person.name]).

  print_age(PersonID) -> 
      [Person] = ets:lookup(person, PersonId),
      io:format("No person ~p~n", [Person#person.age]).

  print_occupation(PersonID) -> 
      [Person] = ets:lookup(person, PersonId),
      io:format("No person ~p~n", [Person#person.occupation]).

..  %%% Interface function
..      %% Look up the person in the named table person,
..  %%% Internal functions          

.. 7.1.4 Non-persistent data storage

.. index::
  single: テーブル; 永続化しない

7.1.4 永続化しないデータストレージ
----------------------------------

.. For non-persistent database storage, prefer Ets tables over Mnesia local_content tables. Even the Mnesia dirty_write operations carry a fixed overhead compared to Ets writes. Mnesia must check if the table is replicated or has indices, this involves at least one Ets lookup for each dirty_write. Thus, Ets writes will always be faster than Mnesia writes.

永続化しないデータストレージとしては、Mnesiaの ``local_content`` テーブルよりもEtsテーブルの方が良いです。例えMnesiaの ``dirty_write`` 操作が、Ets書き込みと比較して、決まった大きさのオーバーヘッドしかかからないとしても同様です。Mnesiaはもしテーブルが複製されたり、インデックス付けをしたりする場合にはチェックが必要になり、毎回の ``dirty_write`` の操作ごとに少なくとも一回のEtsの探索が行われることになります。そのため、Etsの書き込みは、つねにMnesiaの書き込みよりも高速です。

.. index::
  single: etsモジュール; tab2list/1
  single: etsモジュール; select/2
  single: テーブル; データ取得

7.1.5 tab2list
--------------

.. Assume we have an Ets-table, which uses idno as key, and contains

idnoをキーとして、以下のデータを含むEtsテーブルがあったとします::

  [#person{idno = 1, name = "Adam",  age = 31, occupation = "mailman"},
   #person{idno = 2, name = "Bryan", age = 31, occupation = "cashier"},
   #person{idno = 3, name = "Bryan", age = 35, occupation = "banker"},
   #person{idno = 4, name = "Carl",  age = 25, occupation = "mailman"}]

.. If we must return all data stored in the Ets-table we can use ets:tab2list/1. However, usually we are only interested in a subset of the information in which case ets:tab2list/1 is expensive. If we only want to extract one field from each record, e.g., the age of every person, we should use:

もしEtsテーブルに保存されているすべてのデータを取り出さなければならない場合には、 ``ets:tab2list/1`` を使用することができます。しかし、通常の使い方では、データの全部ではなく、そのサブセットだけが必要なことが多いため、 ``ets:tab2list/1`` ではコストがかかりすぎてしまいます。例えば、それぞれのレコードの中の一つのフィールド、ここでは全員の年齢だけが必要になったとします。その場合は以下のように書きます::

.. DO

推奨::

  ...
  ets:select(Tab,[{ #person{idno='_', 
                        name='_', 
                        age='$1', 
                        occupation = '_'},
                  [],
                  ['$1']}]),
  ...

.. DO NOT

非推奨::

  ...
  TabList = ets:tab2list(Tab),
  lists:map(fun(X) -> X#person.age end, TabList),
  ...

.. If we are only interested in the age of all persons named Bryan, we should:

もし、Bryanという名前のすべての人の年齢が必要になったとしたら、以下のように書きます::

.. DO

推奨::

  ...
  ets:select(Tab,[{ #person{idno='_', 
                        name="Bryan", 
                        age='$1', 
                        occupation = '_'},
                  [],
                  ['$1']}]),
  ...

.. DO NOT

非推奨::

  ...
  TabList = ets:tab2list(Tab),
  lists:foldl(fun(X, Acc) -> case X#person.name of
                                  "Bryan" ->
                                      [X#person.age|Acc];
                                   _ ->
                                       Acc
                             end
               end, [], TabList),
  ...


.. REALLY DO NOT

もっとも非推奨::

  ...
  TabList = ets:tab2list(Tab),
  BryanList = lists:filter(fun(X) -> X#person.name == "Bryan" end,
                           TabList),
  lists:map(fun(X) -> X#person.age end, BryanList),
  ...

.. If we need all information stored in the Ets table about persons named Bryan we should

もし、Bryanという名前の人に関して、Etsテーブルに保存されているすべての属性が必要になった場合には以下のようにします。

.. DO

推奨::

  ...
  ets:select(Tab, [{#person{idno='_', 
                          name="Bryan", 
                          age='_', 
                          occupation = '_'}, [], ['$_']}]),
  ...

.. DO NOT

非推奨::

  ...
  TabList = ets:tab2list(Tab),
  lists:filter(fun(X) -> X#person.name == "Bryan" end, TabList),
  ...

.. 7.1.6 Ordered_set tables

.. index::
  single: テーブル; ordered_set
  single: テーブル; 順序付き

7.1.6 ordered_setテーブル
-------------------------

.. If the data in the table should be accessed so that the order of the keys in the table is significant, the table type ordered_set could be used instead of the more usual set table type. An ordered_set is always traversed in Erlang term order with regard to the key field so that return values from functions such as select, match_object, and foldl are ordered by the key values. Traversing an ordered_set with the first and next operations also returns the keys ordered.

もし、テーブル上のデータに対して、キーの順番でアクセスするという使い方をするのであれば、通常の ``set`` テーブル型の代わりに、 ``ordered_set`` テーブル型を使用することができます。 ``ordered_set`` はキーフィールドに関連づけられたErlangの項の順番でトラバースされます。そのため、 ``select``, ``match_object``, ``foldl`` といった関数の返値はkeyの値の順序で返されるようになります。 ``ordered_set`` に対する ``first``, ``next`` といった操作をしても、キーの順序で値が返されます。

.. Note::

  ``ordered_set`` はオブジェクトがキーの順番に処理されるということしか保証していません。 ``ets:select/2`` のような関数の返値は、もしキーが結果に含まれていないとしても(訳注：意味?)、キーの順序に現れることになります。

..  An ordered_set only guarantees that objects are processed in key order. Results from functions as ets:select/2 appear in the key order even if the key is not included in the result.

.. 7.2 Ets specific

7.2 Etsの仕様
=============

.. 7.2.1 Utilizing the keys of the Ets table

.. index::
  pair: Etsテーブル; bag
  pair: Etsテーブル; ordered_set
  single: Etsテーブル; 自家製インデックステーブル
  single: Etsテーブル; 単一キーテーブル
  module: ets

7.2.1 Etsテーブルのキーの使用
-----------------------------

.. An Ets table is a single key table (either a hash table or a tree ordered by the key) and should be used as one. In other words, use the key to look up things whenever possible. A lookup by a known key in a set Ets table is constant and for a ordered_set Ets table it is O(logN). A key lookup is always preferable to a call where the whole table has to be scanned. In the examples above, the field idno is the key of the table and all lookups where only the name is known will result in a complete scan of the (possibly large) table for a matching result.

Etsテーブルは単一キーテーブルです。内部実装はハッシュテーブルもしくは、キーの順序で格納されるツリーの2種類あり、どちらかを使用することができます。言い換えると、可能な場合はいつでも、キーを利用して値を検索する、ということです。登録済みのキーを利用して検索する場合、 ``set`` を利用したEtsテーブルの場合は定数時間で、 ``ordered_set`` を利用したEtsテーブルの場合はO(logN)で検索することができます。キーによる検索は、テーブル全体を検索するよりも望ましいと言えます。上記の例で言うと、 ``idno`` フィールドはテーブルのキーになっているため、名前だけを指定した検索の場合にはテーブル全体を完全にスキャンしてマッチする結果を見つけ出します。

.. A simple solution would be to use the name field as the key instead of the idno field, but that would cause problems if the names were not unique. A more general solution would be create a second table with name as key and idno as data, i.e. to index (invert) the table with regards to the name field. The second table would of course have to be kept consistent with the master table. Mnesia could do this for you, but a home brew index table could be very efficient compared to the overhead involved in using Mnesia.

簡単な解決策としては名前(``name``)のフィールドを ``idno`` の代わりにキーにするというものがありますが、もし名前がユニークでない場合に問題になります。より汎用性の高い解決策としては、もう一つテーブルを作成し、 ``name`` をキーに、 ``idno`` をデータに格納することです。こうすると、名前欄に対して、インデックスを作成するようなことが可能になります。2番目のテーブルは当然、マスターのテーブルと不一致があってはいけません。Mnesiaを使うことでこのようなことができますが、自家製のインデックステーブルを使うと、Mnesiaを使用するオーバーヘッドに比べて、かなり効率的に処理できるはずです。

.. An index table for the table in the previous examples would have to be a bag (as keys would appear more than once) and could have the following contents

前のサンプルのテーブルに対するインデックステーブルは ``bag`` である必要があります。 ``bag`` を使うと、キーの値は1つ以上格納することができるようになります。以下のような構成になります::
 
  [#index_entry{name="Adam", idno=1},
   #index_entry{name="Bryan", idno=2},
   #index_entry{name="Bryan", idno=3},
   #index_entry{name="Carl", idno=4}]

.. Given this index table a lookup of the age fields for all persons named "Bryan" could be done like this

このインデックステーブルを利用して、"Bryan"という名前の全ての人の年齢のフィールドを探索する場合には、以下のようにします::

  ...
  MatchingIDs = ets:lookup(IndexTable,"Bryan"),
  lists:map(fun(#index_entry{idno = ID}) ->
                   [#person{age = Age}] = ets:lookup(PersonTable, ID),
                   Age
            end,
          MatchingIDs),
  ...

.. Note that the code above never uses ets:match/2 but instead utilizes the ets:lookup/2 call. The lists:map/2 call is only used to traverse the idnos matching the name "Bryan" in the table; therefore the number of lookups in the master table is minimized.

注意すべきポイントとしては、上記のコードが、 ``ets:match/2`` を使用しないで、代わりに ``ets:lookup/2`` を呼んでいる点です。 ``lists:map/2`` の呼び出しは、 ``name`` が"Bryan"にマッチした ``idno`` に対してのみ行われます。そのため、マスターテーブルの検索の回数は最低限の回数に抑えられます。

.. Keeping an index table introduces some overhead when inserting records in the table, therefore the number of operations gained from the table has to be weighted against the number of operations inserting objects in the table. However, note that the gain when the key can be used to lookup elements is significant.

インデックステーブルを維持するには、テーブルにレコードを挿入するたびにある程度のオーバーヘッドが生じます。そのため、テーブルに挿入操作をする回数よりも、テーブルから検索する操作の回数の方が多くなければ、コストに見合わないということになります。しかし、要素の検索にキーが使用できる場合には効果はかなり大きいということは覚えておくといいでしょう。

.. 7.3 Mnesia specific

.. index::
  module: mnesia

7.3 Mnesiaの仕様
=================

.. 7.3.1 Secondary index

.. index::
  single: Mnesiaテーブル; セカンダリーインデックス
  single: mnesiaモジュール; create_table/2
  single: mnesiaモジュール; add_table_index/2
  single: mnesiaモジュール; dairy_index_read/3

7.3.1 セカンダリーインデックス
------------------------------

.. If you frequently do a lookup on a field that is not the key of the table, you will lose performance using "mnesia:select/match_object" as this function will traverse the whole table. You may create a secondary index instead and use "mnesia:index_read" to get faster access, however this will require more memory. Example:

もしも、テーブルのキーになっている以外のフィールドを検索する機会が多い場合には、 ``mnesia:select/match_object`` を使用すると、テーブル全体を探索するため、パフォーマンス上の劣化が大きくなります。このような場合には、セカンダリーインデックスを作成し、 ``mnesia:index_read`` を使用して、高速なアクセスをすべきです。しかし、これを行うと、メモリの必要量は増加します。以下にサンプルを示します::

  -record(person, {idno, name, age, occupation}).
          ...
  {atomic, ok} = 
  mnesia:create_table(person, [{index,[#person.age]},
                                {attributes,
                                 record_info(fields, person)}]),
  {atomic, ok} = mnesia:add_table_index(person, age), 
  ...

  PersonsAge42 = mnesia:dirty_index_read(person, 42, #person.age),
  ...

.. 7.3.2 Transactions

.. index::
  pair: Mnesiaテーブル; トランザクション
  single: mnesiaモジュール; read/1
  single: mnesiaモジュール; transaction/1
  single: mnesiaモジュール; dirty_read/1

7.3.2 トランザクション
----------------------

.. Transactions is a way to guarantee that the distributed Mnesia database remains consistent, even when many different processes update it in parallel. However if you have real time requirements it is recommended to use dirty operations instead of transactions. When using the dirty operations you lose the consistency guarantee, this is usually solved by only letting one process update the table. Other processes have to send update requests to that process.

トランザクションは、多くの異なるプロセスが、平行で内容を更新するような、分散Mnesiaデータベースの一致性を保証するために使用します。しかし、リアルタイムのリクエストが必要になった場合には、トランザクションを使用する代わりに、「汚い」操作を行う必要があるでしょう。汚い操作を行うことで、一致性の保証は失われます。これを解決するには、通常、一つのプロセスだけがテーブルをアップデートするようにすることで解決します。他のプロセスはこのプロセスに対して、アップデート要求を送信するという実装になります::

  ...
  % トランザクションを使用する
        
  Fun = fun() ->
            [mnesia:read({Table, Key}),
             mnesia:read({Table2, Key2})]
            end, 

  {atomic, [Result1, Result2]}  = mnesia:transaction(Fun),
  ...
        
  % 汚い操作を行う
  ...
        
  Result1 = mnesia:dirty_read({Table, Key}),
  Result2 = mnesia:dirty_read({Table2, Key2}),
  ...

.. % Using transaction
.. % Same thing using dirty operations

Copyright © 1991-2009 Ericsson AB
