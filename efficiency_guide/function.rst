.. highlight:: erlang

.. 6 Functions

======
6 関数
======

.. 6.1 Pattern matching

.. index::
  pair: 関数; パターンマッチ
  statement: case
  statement: receive
  statement: when
  builtin: is_integer/1

6.1 パターンマッチ
==================

.. Pattern matching in function head and in case and receive clauses are optimized by the compiler. With a few exceptions, there is nothing to gain by rearranging clauses.

関数の先頭、もしくは ``case`` 節、 ``receive`` 節にあるパターンマッチはコンパイラによって最適化されます。いくつかのケースでは、節を整理し直しても何も改善されないというものもいくつかあります。

.. One exception is pattern matching of binaries. The compiler will not rearrange clauses that match binaries. Placing the clause that matches against the empty binary last will usually be slightly faster than placing it first.

例外の一つはバイナリのパターンマッチです。コンパイラはバイナリマッチの節を整列し直すことはしません。空のバイナリにマッチするものは、 **先頭** に配置するよりも **最後** に記述する方がわずかではありますが早くなります。

.. Here is a rather contrived example to show another exception

多少不自然ではありますが、他の例外について示すサンプルを紹介します。

.. DO NOT

非推奨::

  atom_map1(one) -> 1;
  atom_map1(two) -> 2;
  atom_map1(three) -> 3;
  atom_map1(Int) when is_integer(Int) -> Int;
  atom_map1(four) -> 4;
  atom_map1(five) -> 5;
  atom_map1(six) -> 6.

.. The problem is the clause with the variable Int. Since a variable can match anything, including the atoms four, five, and six that the following clauses also will match, the compiler must generate sub-optimal code that will execute as follows:

問題となるのは、 ``Int`` という変数を伴った節です。この変数はどのような物に対してもマッチしてしまいます。それに加え、アトムの ``four`` , ``five`` , ``six`` も他の節でマッチします。コンパイラはこれから説明するように動作する、部分最適なコードを生成することになります。

.. First the input value is compared to one, two, and three (using a single instruction that does a binary search; thus, quite efficient even if there are many values) to select which one of the first three clauses to execute (if any).

まず最初に、入力値を ``one`` , ``two`` , ``three`` というアトムと比較します。比較には、バイナリサーチを行う命令が使用されます。これは値が多くなればなるほど効率的に動作します。もし、マッチすれば、この3つの節の中から1つ選択して実行します。

.. If none of the first three clauses matched, the fourth clause will match since a variable always matches. If the guard test is_integer(Int) succeeds, the fourth clause will be executed.

もし最初の3つの節にマッチするものがなければ、変数を使用しているためにどんなものともマッチする4つ目の節とのマッチを試みます。もしガードテストの ``is_integer(Int)`` が成功すれば、4番目の節が実行されます。

.. If the guard test failed, the input value is compared to four, five, and six, and the appropriate clause is selected. (There will be a function_clause exception if none of the values matched.)

もしガードテストにも失敗したとすると、入力値は ``four`` , ``five`` , ``six`` と比較されて、適切な節が選択されます。もし入力値がこれらのうちのどれともマッチしないと、 ``function_clause`` 例外が発生します。

.. Rewriting to either

2通りの方法で以下のように書き換えてみます。

.. DO

推奨::

  atom_map2(one) -> 1;
  atom_map2(two) -> 2;
  atom_map2(three) -> 3;
  atom_map2(four) -> 4;
  atom_map2(five) -> 5;
  atom_map2(six) -> 6;
  atom_map2(Int) when is_integer(Int) -> Int.

.. or

あるいは、

.. DO

推奨::

  atom_map3(Int) when is_integer(Int) -> Int;
  atom_map3(one) -> 1;
  atom_map3(two) -> 2;
  atom_map3(three) -> 3;
  atom_map3(four) -> 4;
  atom_map3(five) -> 5;
  atom_map3(six) -> 6.

.. will give slightly more efficient matching code.

これらは最初の例よりは多少効率的にマッチを行うことができます。

.. Here is a less contrived example

前のサンプルよりは多少不自然ではないサンプルを紹介します。

.. DO NOT

非推奨::

  map_pairs1(_Map, [], Ys) ->
      Ys;
  map_pairs1(_Map, Xs, [] ) ->
      Xs;
  map_pairs1(Map, [X|Xs], [Y|Ys]) ->
      [Map(X, Y)|map_pairs1(Map, Xs, Ys)].

.. The first argument is not a problem. It is variable, but it is a variable in all clauses. The problem is the variable in the second argument, Xs, in the middle clause. Because the variable can match anything, the compiler is not allowed to rearrange the clauses, but must generate code that matches them in the order written.

最初の引数に関しては問題ありません。これは変数ですが、すべての節がどれも変数になっているからです。問題となるのは、真ん中の節の ``Xs`` という2番目の変数です。変数はすべてのものにマッチするため、コンパイラは節の順番を再配置することができず、書かれた順序でマッチを行うコードを生成します。

.. If the function is rewritten like this

もし下記のように関数を書き換えたとします。

.. DO

推奨::

  map_pairs2(_Map, [], Ys) ->
      Ys;
  map_pairs2(_Map, [_|_]=Xs, [] ) ->
      Xs;
  map_pairs2(Map, [X|Xs], [Y|Ys]) ->
      [Map(X, Y)|map_pairs2(Map, Xs, Ys)].

.. the compiler is free rearrange the clauses. It will generate code similar to this

コンパイラは自由に節の順序を再配置して、下記のようなコードを生成するでしょう。

.. DO NOT (already done by the compiler)

非推奨(既にコンパイラが内部でこのように最適化済み)::

  explicit_map_pairs(Map, Xs0, Ys0) ->
      case Xs0 of
          [X|Xs] ->
              case Ys0 of
                  [Y|Ys] ->
                      [Map(X, Y)|explicit_map_pairs(Map, Xs, Ys)];
                  [] ->
                      Xs0
              end;
          [] ->
              Ys0
      end.

.. which should be slightly faster for presumably the most common case that the input lists are not empty or very short. (Another advantage is that Dialyzer is able to deduce a better type for the variable Xs.)

この関数は、与えられたリストが空ではないがとても短いという、良くあると思われるケースにおいては、わずかに速くなっているはずです。 ``Xs`` という変数の型について、Dialyzerを使用したときに、より適切な型の推測ができるようになるという他のメリットもあります。

.. 6.2 Function Calls

.. index::
  single: 関数; 呼び出し
  builtin: apply/3
  object: fun

6.2 関数呼び出し
================ 

.. Here is an intentionally rough guide to the relative costs of different kinds of calls. It is based on benchmark figures run on Solaris/Sparc:

ここでは、大ざっぱな(意図的です)、呼び出しの種類ごとの相対的な呼び出しコストに関する説明をおこなっていきます。ここの説明はSolaris/Sparc上で実行したベンチマークを元にしています。

.. Calls to local or external functions (foo(), m:foo()) are the fastest kind of calls. Calling or applying a fun (Fun(), apply(Fun, [])) is about three times as expensive as calling a local function. Applying an exported function (Mod:Name(), apply(Mod, Name, [])) is about twice as expensive as calling a fun, or about six times as expensive as calling a local function.

ローカルの関数や、外部の関数(``foo()``, ``m:foo()``)の呼び出しは、関数呼び出しの中ではもっとも速い呼び出し形式になります。fun、もしくは、funに対してapplyを適用するケース(``Fun()``, ``apply(Fun, [])``)は、ローカル関数呼び出しと比較すると **3倍の呼び出しコスト** がかかります。Exportされた関数に対する呼び出し(``Mod:Name()``, ``apply(Mod, Name, [])``)はfunと比べて2倍、ローカル関数呼び出しと比較すると、 **6倍のコスト** がかかります。

.. 6.2.1 Notes and implementation details

.. index::
  pair 実装; リスト
  builtin: apply/3
  object: fun
  single: タプルfun

6.2.1 注意点と、実装の詳細
--------------------------

.. Calling and applying a fun does not involve any hash-table lookup. A fun contains an (indirect) pointer to the function that implements the fun.

funに対する呼び出しやapplyでは、ハッシュテーブルの検索は行われません。funには、実装の実体の関数を指す、間接的なポインタが含まれています。

.. Warning
.. Tuples are not fun(s). A "tuple fun", {Module,Function}, is not a fun. The cost for calling a "tuple fun" is similar to that of apply/3 or worse. Using "tuple funs" is strongly discouraged, as they may not be supported in a future release.

.. warning::

  **タプルは ``fun(s)`` ではありません** 。 ``{Module, Function}`` という形式の"タプルfun"はfunではありません。タプルfunに対する呼び出しコストは ``apply/3`` と同じか遅いぐらいです。将来のリリースでは"タプルfun"はサポートされなくなることもありえるため、 **使用すると失望することになるでしょう。**
  
.. apply/3 must look up the code for the function to execute in a hash table. Therefore, it will always be slower than a direct call or a fun call.

``apply/3`` は実行する関数のためのコードをハッシュテーブルの中から検索しなければなりません。そのため、これは直接の関数呼び出しや、funを使った呼び出しよりも確実に遅くなります。

.. It no longer matters (from a performance point of view) whether you write

性能の観点でみると、以下のように書くのと、::

  Module:Function(Arg1, Arg2)

.. or

以下のようなスタイルで書くのは、もはや問題になる差はありません。コンパイラが内部的に後者のコードを前者の形式に書き直すからです。::

  apply(Module, Function, [Arg1,Arg2])

.. (The compiler internally rewrites the latter code into the former.)

.. The following code

以下のようなコードは、コンパイル時には引数の型が分からず、動的に引数のリストを作成するコストがかかるため、多少遅くなります。::

  apply(Module, Function, Arguments)

.. is slightly slower because the shape of the list of arguments is not known at compile time.

.. 6.3 Memory usage in recursion

.. index::
  single: 末尾再帰; メモリ使用量

6.3 再帰呼び出し時のメモリ使用量
================================

.. When writing recursive functions it is preferable to make them tail-recursive so that they can execute in constant memory space.

再帰関数を書く場合には、末尾再帰にすると必要なメモリスペースを定数にすることができるため、なるべく末尾再帰を使う方がお勧めです。

.. Do

推奨::

  list_length(List) ->
      list_length(List, 0).
  list_length([], AccLen) -> 
      AccLen; % 基本ケース
  list_length([_|Tail], AccLen) ->
      list_length(Tail, AccLen + 1). % 末尾再帰

..      AccLen; % Base case
..       list_length(Tail, AccLen + 1). % Tail-recursive

.. DO NOT

非推奨::

  list_length([]) ->
      0. % 基本ケース
  list_length([_ | Tail]) ->
      list_length(Tail) + 1. % 末尾再帰ではない

..      list_length(Tail) + 1. % Not tail-recursive
..      0. % Base case


Copyright © 1991-2009 Ericsson AB

