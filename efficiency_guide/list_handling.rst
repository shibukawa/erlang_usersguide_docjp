.. highlight:: erlang

.. 5 List handling

5 リストの操作
==============

.. 5.1 Creating a list

.. index::
  single: リスト; 作成
  operator: ++
  single: listsモジュール; append/1
  single: listsモジュール; reverse/1

5.1 リストの作成
----------------

.. Lists can only be built starting from the end and attaching list elements at the beginning. If you use the ++ operator like this

リストを組み立てるのは、リストの末尾から要素を足していくことでのみ行うことができます。++演算子を使いたい場合には以下のようにします::

  List1 ++ List2

.. you will create a new list which is copy of the elements in List1, followed by List2. Looking at how lists:append/1 or ++ would be implemented in plain Erlang, it can be seen clearly that the first list is copied:

この演算の結果、List1の要素のコピーにList2がつながった新しいリストを作ることができます。lists:append/1もしくは++がどのようにプレーンなErlangで実装されるのかを見ると、最初のリストがコピーされる理由というものがはっきりと理解できるでしょう。::

  append([H|T], Tail) ->
      [H|append(T, Tail)];
  append([], Tail) ->
      Tail.

.. So the important thing when recursing and building a list is to make sure that you attach the new elements to the beginning of the list, so that you build a list, and not hundreds or thousands of copies of the growing result list.

重要なことは、再帰をしてリストを作成する場合は、リストの先方に新しい要素を足していきます。そうすると、リストの組み立てを行うのに、結果のリストに要素を足していくたびに何百回も何千回ものコピーをすることがなくなります。

.. Let us first look at how it should not be done:

まずはすべきでない実装方法について見てみましょう。

.. DO NOT

非推奨::

  bad_fib(N) ->
      bad_fib(N, 0, 1, []).

  bad_fib(0, _Current, _Next, Fibs) ->
      Fibs;
  bad_fib(N, Current, Next, Fibs) -> 
      bad_fib(N - 1, Next, Current + Next, Fibs ++ [Current]).

.. Here we are not a building a list; in each iteration step we create a new list that is one element longer than the new previous list.

ここではリストを組み立てているのではなく、繰り返しのステップの中で、一つ前のリストよりも長さが１つだけ長い、新しいリストを新規で作成することになります。

.. To avoid copying the result in each iteration, we must build the list in reverse order and reverse the list when we are done:

繰り返しの中でコピーをするのを避けるには、逆順でリストを組み立て、組み立て終わったらリストを反転させる必要があります。

.. DO

推奨::

  tail_recursive_fib(N) ->
      tail_recursive_fib(N, 0, 1, []).

  tail_recursive_fib(0, _Current, _Next, Fibs) ->
      lists:reverse(Fibs);
  tail_recursive_fib(N, Current, Next, Fibs) -> 
      tail_recursive_fib(N - 1, Next, Current + Next, [Current|Fibs]).


.. 5.2 List comprehensions

.. index::
  single: リスト; 内包表記

5.2 リスト内包表記
------------------

.. Lists comprehensions still have a reputation for being slow. They used to be implemented using funs, which used to be slow.

リスト内包表記には、使用すると遅くなるという噂が未だに残っています。以前のリスト内包表記の実装ではfunが使用されていました。以前の実装ではfun自体の動作が遅いという問題がありました。

.. In recent Erlang/OTP releases (including R12B), a list comprehension

現在のErlang/OTPのリリース(R12B以降)では以下のようなリスト内包表記は、基本的に、ローカルの関数として変換されます。::

  [Expr(E) || E <- List]

.. is basically translated to a local function

変換後は以下のようなコードになります。::

  'lc^0'([E|Tail], Expr) ->
      [Expr(E)|'lc^0'(Tail, Expr)];
  'lc^0'([], _Expr) -> [].

.. In R12B, if the result of the list comprehension will obviously not be used, a list will not be constructed. For instance, in this code

R12Bでは、リスト内包表記の結果のリストが明らかに使用されないという状況では、リストは構築されなくなります。例えば以下のような場合には作成されません::

  [io:put_chars(E) || E <- List],
  ok.

.. or in this code

もしくはこのような場合にも作成されません::

  .
  .
  .
  case Var of
      ... ->
          [io:put_chars(E) || E <- List];
      ... ->
  end,
  some_function(...),
  .
  .
  .

.. the value is neither assigned to a variable, nor passed to another function, nor returned, so there is no need to construct a list and the compiler will simplify the code for the list comprehension to

この場足、値は変数に格納されることもありませんし、他の関数に渡されたり、返り値として返されることもありません。ここではコンパイラが、リストを作成する必要がないということを知ることができるため、リスト内包表記のコードもシンプルに生成します。::

  'Lc^0'([E|Tail], Expr) ->
      Expr(E),
      'lc^0'(Tail, Expr);
  'lc^0'([], _Expr) -> [].

.. 5.3 Deep and flat lists

.. index::
  single: listsモジュール; flatten/1
  single: listsモジュール; append/1
  builtin: list_to_binary/1
  builtin: iolist_to_binary/1
  single: リスト; フラット

5.3 深いリスト/フラットなリスト
-------------------------------

.. lists:flatten/1 builds an entirely new list. Therefore, it is expensive, and even more expensive than the ++ (which copies its left argument, but not its right argument).

`lists:flatten/1`_ を実行すると、完全に新しいリストを構築します。そのため、この関数の呼び出しは++演算よりも処理時間が多くかかります。ちなみに、++演算子は左側の引数のコピーは行われますが、右側の引数のコピーは行われません。

.. _`lists:flatten/1`: http://erlang.org/doc/man/lists.html#flatten-1

.. In the following situations, you can easily avoid calling lists:flatten/1:

以下のような状況であれば、lists:flatten/1の呼び出しを避けることができます。

.. When sending data to a port. Ports understand deep lists so there is no reason to flatten the list before sending it to the port.
.. When calling BIFs that accept deep lists, such as list_to_binary/1 or iolist_to_binary/1.
.. When you know that your list is only one level deep, you can can use lists:append/1.

* ポートにデータを送信するとき。ポートはリストの深さを理解できるため、ポートにリストを送信する前にはフラットにする必要はありません。

* `list_to_binary/1`_ , `iolist_to_binary/1`_ などの深いリストを受け取る組み込み関数を呼び出す時。

* `lists:append/1`_ を使用するが、リストが一つの深さしかないと分かっているとき。

.. _`list_to_binary/1`: http://erlang.org/doc/man/erlang.html#list_to_binary-1
.. _`iolist_to_binary/1`: http://erlang.org/doc/man/erlang.html#iolist_to_binary-1
.. _`lists:append/1`: http://erlang.org/doc/man/lists.html#append-1

.. Port example

ポートの例
~~~~~~~~~~

.. DO

推奨::

      ...
      port_command(Port, DeepList)
      ...

.. DO NOT

非推奨::

      ...
      port_command(Port, lists:flatten(DeepList))
      ...

.. A common way to send a zero-terminated string to a port is the following:

ゼロ終端された文字列をポートに送信する際は一般的には以下のように行われます。

.. DO NOT

非推奨::

      ...
      TerminatedStr = String ++ [0], % String="foo" => [$f, $o, $o, 0]
      port_command(Port, TerminatedStr)
      ...

.. Instead do like this:

代わりに以下のようにしてください。

.. DO

推奨::

      ...
      TerminatedStr = [String, 0], % String="foo" => [[$f, $o, $o], 0]
      port_command(Port, TerminatedStr) 
      ...

.. Append example

appendの例
~~~~~~~~~~

.. DO

推奨::

      > lists:append([[1], [2], [3]]).
      [1,2,3]
      >

.. DO NOT

非推奨::

      > lists:flatten([[1], [2], [3]]).
      [1,2,3]
      >

.. 5.4 Why you should not worry about recursive lists functions

.. index::
  pair: リスト; 末尾再帰

5.4 なぜリストを再帰する関数に対して心配する必要がないのか？
-------------------------------------------------------------

.. In the performance myth chapter, the following myth was exposed: Tail-recursive functions are MUCH faster than recursive functions.

パフォーマンスの都市伝説に関する章では、"末尾再帰関数は、再帰関数と比べて「べらぼう」に高速である"というものが説明されています。

.. To summarize, in R12B there is usually not much difference between a body-recursive list function and tail-recursive function that reverses the list at the end. Therefore, concentrate on writing beautiful code and forget about the performance of your list functions. In the time-critical parts of your code (and only there), measure before rewriting your code.

簡単にまとめると、R12B以降の実装では、通常の場合は、リストに対して、本体で再帰する関数と、末尾で再帰させて、最後にリストを反転させる関数ではそれほどパフォーマンスの差はない、というものでした。そのため、どちらの実装の方が美しく書けるか、という点のみを気にしていればよく、リストの関数のパフォーマンスについては忘れてしまうことが可能です。時間の制約の厳しいコードを書く際は、コードを書き直す前に、測定するようにしてください。

.. Important note: This section talks about lists functions that construct lists. A tail-recursive function that does not construct a list runs in constant space, while the corresponding body-recursive function uses stack space proportional to the length of the list. For instance, a function that sums a list of integers, should not be written like this

重要な情報: このセクションでは、リストを作成する関数について説明してきました。リストを作成しない末尾再帰関数は一定のメモリ消費で呼び出すことができます。本体再帰の関数はリストの長さに応じたスタック領域を消費します。リストに格納されている数値を足す関数は以下のように書きます。

.. DO NOT

非推奨::

  recursive_sum([H|T]) -> H+recursive_sum(T);
  recursive_sum([])    -> 0.
  but like this

.. DO

推奨::

  sum(L) -> sum(L, 0).

  sum([H|T], Sum) -> sum(T, Sum + H);
  sum([], Sum)    -> Sum.