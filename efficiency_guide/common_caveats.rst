.. highlight:: erlang

.. 3 Common Caveats

警戒すべきモジュールと組み込み関数
==================================

.. Here we list a few modules and BIFs to watch out for, and not
   only from a performance point of view.

ここでは、いくつか注意すべきモジュールと組み込み関数について説明していきます。ここで説明するのは、パフォーマンスの観点だけではなく、もっと広い視点で見ていきます。

.. 3.1 The regexp module

.. index::
  module: regexpモジュール
  module: reモジュール

regexpモジュール
----------------

.. The regular expression functions in the regexp module are written
   in Erlang, not in C, and were meant for occasional use on small
   amounts of data, for instance for validation of configuration
   files when starting an application.

regexpモジュールの中の正規表現関数はC言語ではなく、Erlangで書かれています。もともとは、アプリケーションの起動時に、コンフィグファイルの文法をチェックするなど、少ないデータに対して使用するのを意図して書かれました。

.. Use the re module (introduced in R13A) instead, especially 
   in time-critical code.

今後は、R13Aから導入された、reモジュールを代わりに使用してください。特に、大量のデータの処理など、時間に厳しい処理に関してはこちらを使用してください。

.. 3.2 The timer module

.. index::
  module: timerモジュール
  single: erlangモジュール; send_after/3
  single: erlangモジュール; start_timer/3
  single: timerモジュール; tc/3
  single: timerモジュール; sleep/1

timerモジュール
---------------

.. Creating timers using erlang:send_after/3 and erlang:start_timer/3 is
   much more efficient than using the timers provided by the timer module.
   The timer module uses a separate process to manage the timers, and that
   process can easily become overloaded if many processes create and cancel
   timers frequently (especially when using the SMP emulator).

timerモジュールで提供されているタイマーを使用するよりも、erlang:send_after/3および、erlang:start_timer/3を使用したほうが、効果的に書けるでしょう。timerモジュールは、タイマーを管理するのに、別のプロセスを使用しますが、多くのプロセスが頻繁にタイマーを作成したりキャンセルしたりしているうちに、間違って上書きしやすい、という問題があります。特に、SMPエミュレータを使用していると、この危険性が高まります。

.. The functions in the timer module that do not manage timers
.. (such as timer:tc/3 or timer:sleep/1), do not call the
.. timer-server process and are therefore harmless.

timer:tc/3, timer:sleep/1などといった、timerモジュールの中のタイマーの管理をしない関数は、タイマーサーバプロセスを呼ぶことはないため、実害はありません。

.. index::
  single: アトム; list_to_existing_atom/1
  single: アトム; list_to_atom/1
  single: list_to_atom/1
  single: list_to_existing_atom/1
  pair: アトム; ガーベジコレクタ

list_to_atom/1
--------------

.. Atoms are not garbage-collected. Once an atom is created, it
.. will never be removed. The emulator will terminate if the
.. limit for the number of atoms (1048576) is reached.

アトムはガーベジコレクタで回収されません。一度アトムが作成されると、削除されることはありません。もし、アトムが生成できる限界数(1048576)に達すると、エミュレータは終了します。

.. Therefore, converting arbitrary input strings to atoms could be
.. dangerous in a system that will run continuously. If only certain
.. well-defined atoms are allowed as input, you can use
.. list_to_existing_atom/1
.. to guard against a denial-of-service attack. (All atoms that are
.. allowed must have been created earlier, for instance by simply
.. using all of them in a module and loading that module.)

そのため、サーバのように継続して実行されるようなシステムにおいて、入力されたアルファベットで構成された文字列をアトムに変換していくようなコードは危険であるといえます。もしアトムとして使用可能なもののみが入力して入ってくる場合にのみlist_to_existing_atom/1を使用することで、サービス拒否攻撃から身を守ることができます。使用したいすべてのアトムはこの関数を呼ぶ前にすべて作成されていなければ、許容されません。例えば、必要なアトムがモジュール内で既に使用してあったり、もしくは使用したいアトムが定義されているモジュールをロードする必要があります。

.. Using list_to_atom/1 to construct an atom that is passed to apply/3 
.. like this

``apply/3`` 関数に渡すことで、 ``list_to_atom/1`` を使用してアトムを構築することができます。::

 apply(list_to_atom("some_prefix"++Var), foo, Args)

.. is quite expensive and is not recommended in time-critical code.

このコードは呼び出しのコストが高いため、時間の制約の厳しいコードでは使用しないほうがいいでしょう。

.. index::
  builtin: length/1
  builtin: tuple_size/1
  builtin: byte_size/1
  builtin: bit_size/1
  single: リスト; 不適切なリスト
  single: リスト; 長さ取得

length/1
--------

.. The time for calculating the length of a list is proportional
.. to the length of the list, as opposed to tuple_size/1, byte_size/1,
..  and bit_size/1, which all execute in constant time.

リストの長さを計算する処理時間は、リストの長さに比例します。一方、tuple_size/1, byte_size/1, bit_size/1は定数時間で実行することができます。

.. Normally you don't have to worry about the speed of length/1,
   because it is efficiently implemented in C. In time critical-code,
   though, you might want to avoid it if the input list could
   potentially be very long.

length/1はC言語を用いて効率的に実装されていますので、通常の場合には、length/1のスピードに関して心配する必要はありません。しかし、時間の制約の厳しいコードでは、入力されるリストが非常に長いということもありえるため、length/1を使用するのは避けたいと思うでしょう。

.. Some uses of length/1 can be replaced by matching. For instance, 
   this code

length/1を用いているケースのうち、パターンマッチに置き換えられるものもあります。例えば、以下のようなコードは置き換えることが可能です::

   foo(L) when length(L) >= 3 ->
       ...

.. can be rewritten to

このコードは以下のようになります::

   foo([_,_,_|_]=L) ->
       ...

.. (One slight difference is that length(L) will fail if the L
   is an improper list, will the pattern in the second code
   fragment will accept an improper list.)

Lが不適切なリストの時に失敗するという点だけが、length(L)とのわずかな違いになります。２めのコードが不適切なリスト [1]_ にも適用されてしまうからです。

.. [1] (訳注) Erlang（を含む関数型言語）のリストは連結リスト（linked list）です。連結リストはセルという要素から構成されます。セルはデータを二つ持つことができ、それぞれhead/tailと呼ばれます。headに任意の値を入れ、tailに他のセルのheadへのポインタを入れていくと、一連のリストができます。擬似的には以下のような構造になっています::

         セル = (head, tail)
         空のセル = (nil, nil) # tailが次のセルへのポインタを持たない
         リスト = (h1, (h2, (h3, ())))

       リストの終端は、tailが次のセルへのポインタを持たない空のセルになります。Erlangでは表向きセルは出てきませんが、[H|T]というパターンマッチは、先頭のセルのheadとtailを取り出すことになります。上の例で言えば、::

         H = h1
         T = (h2, (h3, ()))
 
       になります。

       このように連結リストはtailで次のセルを指しているので、tailがセルへのポインタでなければ、連結リストとして使えません。例えば (h1, (h2, (h3, "string"))) のような構造になっていたら、連結リストとして処理しようとすると失敗します。最後のセルのtailがセルを指してないからです。このような連結リストが「不適切なリスト(improper list)」と呼ばれます。

.. index::
  builtin: setelement/1
  pair: タプル; 変更

setelement/3
------------

.. setelement/3 copies the tuple it modifies. Therefore,
   updating a tuple in a loop using setelement/3 will create
   a new copy of the tuple every time.

setelement/3はタプルをコピーしてから変更します。そのため、ループの中でタプルをアップデートするのにsetelement/3を使用すると、毎回タプルの新しいコピーを作成してしまいます。

.. There is one exception to the rule that the tuple is copied.
   If the compiler clearly can see that destructively updating
   the tuple would give exactly the same result as if the tuple
   was copied, the call to setelement/3 will be replaced with
   a special destructive setelement instruction. In the
   following code sequence

タプルがコピーされるというルールにはひとつだけ例外があります。コンパイラから見て、破壊的なアップデートを行っても、タプルのコピーを行っても、明らかに同じ結果になるということが分かれば、setelement/3の呼び出しは、特別な破壊的なsetelement命令へと置き換えられます。以下のようなコードがあったとします。::

   multiple_setelement(T0) ->
       T1 = setelement(9, T0, bar),
       T2 = setelement(7, T1, foobar),
       setelement(5, T2, new_value).

.. the first setelement/3 call will copy the tuple and
   modify the ninth element. The two following setelement/3
   calls will modify the tuple in place.

最初のsetelement/3呼び出しはタプルをコピーして、9番目の要素を書き換えます。それに続く2つのsetelement/3の呼び出しではタプルをその場で変更します。

.. For the optimization to be applied, all of the followings
   conditions must be true:

最適化が適用されるためには、以下の条件を満たす必要があります。

..    * The indices must be integer literals, not variables or expressions.
      * The indices must be given in descending order.
      * There must be no calls to other function in between the calls 
        to setelement/3.
      * The tuple returned from one setelement/3 call must only be 
        used in the subsequent call to setelement/3.

 * インデックスは数値リテラルである必要があります。変数や式ではいけません。
 * インデックスは降順に指定される必要があります。
 * setelement/3を何回か呼び出しの間には、他の関数呼び出しを挟んではいけません。
 * ひとつのsetelement/3呼び出しから返されたタプルは、その次に続くsetelement/3の呼び出しの中でのみ使用されます。

.. If it is not possible to structure the code as in the
   multiple_setelement/1 example, the best way to modify
   multiple elements in a large tuple is to convert the tuple
   to a list, modify the list, and convert the list back to a tuple.

もし、長いタプルの複数の要素を変更したいが、上記のサンプルのmultiple_setelement/1のようにコードの構造を変更することができない場合には、一度リストに変換する方法が最適な方法になります。タプルをリストに変換して、リスト上で要素の変更を行い、最後にまたタプルに再変換します。

.. index::
  single: タプル; サイズ
  single: バイナリ; サイズ
  builtin: size/1
  builtin: tuple_size/1
  builtin: byte_size/1
  builtin: Dialyzer

size/1
------

.. size/1 returns the size for both tuples and binary.

size/1はタプルとバイナリの両方のサイズを返すのに使用することができます。

.. Using the new BIFs tuple_size/1 and byte_size/1 introduced in
   R12B gives the compiler and run-time system more opportunities
   for optimization. A further advantage is that the new BIFs
   could help Dialyzer find more bugs in your program.

R12Bから導入された、新しい組み込み関数のtuple_size/1, byte_size/1を使用すると、コンパイラやランタイムのシステムが最適化を行いやすくなります。もうひとつの利点としては、新しい組み込み関数を使用した方が、Dialyzerと連携することで、プログラムのバグを見つけやすくなるでしょう。

.. index::
  single: バイナリ; 分割
  single: バイナリ; split_binary/2
  builtin: split_binary/2

split_binary/2
--------------

.. It is usually more efficient to split a binary using matching instead
   of calling the split_binary/2 function. Furthermore, mixing bit syntax
   matching and split_binary/2 may prevent some optimizations of bit syntax
   matching.

通常の場合、split_binary/2関数を呼ぶ代わりに、マッチングを利用すると、効率よくバイナリデータを分割することができます。また、これに加えて、ビット文法のマッチングと、split_binary/2を混ぜて使用すると、ビット文法のマッチングの最適化が邪魔されます。

.. DO

推奨::

       <<Bin1:Num/binary,Bin2/binary>> = Bin,

.. DO NOT

非推奨::

       {Bin1,Bin2} = split_binary(Bin, Num)


.. 3.8 The '--' operator

.. index::
  operator: --
  operator: =:=
  operator: ==
  module: ordsetsモジュール
  module: gb_setsモジュール
  single: ordsetsモジュール; from_list/1
  single: ordsetsモジュール; subtract/2
  single: gb_setsモジュール; is_element/2
  single: setsモジュール; from_list/1
  single: gb_setsモジュール; from_list/1

'--'演算子
----------

.. Note that the '--' operator has a complexity proportional to
   the product of the length of its operands, meaning that it
   will be very slow if both of its operands are long lists:

'--'演算子は、オペランドに来る要素の長さの積に比例して遅くなるということに注意してください。これはつまり、両方のリスト長ければ、非常に遅くなるということを意味しています。

.. DO NOT

非推奨::

       HugeList1 -- HugeList2

.. Instead use the ordsets module:

これの代わりに、ordsetsモジュールを利用してください。

.. DO

推奨::

       HugeSet1 = ordsets:from_list(HugeList1),
       HugeSet2 = ordsets:from_list(HugeList2),
       ordsets:subtract(HugeSet1, HugeSet2)


.. Obviously, that code will not work if the original order of
   the list is important. If the order of the list must be
   preserved, do like this:

これのコードは明らかに、入力時のリストの順序を保存したい場合には使用することができません。もし、リストの順番を保存したい場合には、以下のようにしてください。

.. DO

推奨::

       Set = gb_sets:from_list(HugeList2),
       [E || E <- HugeList1, not gb_sets:is_element(E, Set)]

.. Subtle note 1: This code behaves differently from '--' if the lists
   contain duplicate elements. (One occurrence of an element in HugeList2
   will remove all occurrences in HugeList1.)

微妙な問題 1: このコードは、もしリスト内に重複した要素が格納されていた場合には、'--'を使ったコードとは異なった動作をします。HugeList2に一度でも登場すると、HugeList1に含まれるすべての要素が削除されることになります。

.. Subtle note 2: This code compares lists elements using the '=='
   operator,
   while '--' uses the '=:='. If that difference is important, sets can be
   used instead of gb_sets, but note that sets:from_list/1 is much slower
   than gb_sets:from_list/1 for long lists.

微妙な問題 2: このコードでは、 '==' 演算子を用いてリストの要素の比較をしていますが、 '--' 演算子は裏では '=:=' を使用します。もしこの違いが重要であれば、gb_setsの代わりにsetsを用いることができます。ただし、長いリストに対して使用する場合、sets:from_list/1はgb_sets:from_list/1よりもはるかに遅いということに注意してください。

.. Using the '--' operator to delete an element from a list is not a
   performance problem:

要素をひとつだけリストから削除する場合には、パフォーマンス上の問題はありません。

.. DO

推奨::

       HugeList1 -- [Element]


Copyright (c) 1991-2009 Ericsson AB
