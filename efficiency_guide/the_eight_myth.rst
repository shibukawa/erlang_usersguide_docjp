.. highlight:: erlang

.. 2 The Eight Myths of Erlang Performance

.. index:: 都市伝説

2. Erlangのパフォーマンスに関する8つの都市伝説
==============================================

.. Some truths seem to live on well beyond their best-before date,
   perhaps because "information" spreads more rapidly from
   person-to-person faster than a single release note that
   notes, for instance, that funs have become faster.

「真実」の中には、賞味期限を越えて生き残るものがあります。これは、リリースノートに何か情報が載っていたとしても、それよりも人から人へと「情報」が広がっていく速度の方が早いからです。実際には、かつて古いといわれていたfunも、現在では高速になっています。

.. Here we try to kill the old truths (or semi-truths) that
.. have become myths.

このドキュメントを通じて私たちが行おうとしていることは、かつて真実(あるいは多少誤解があるもの)だったもののうち、もはや古くなって、すでに都市伝説となっているものを、正しく葬り去ることです。

.. 2.1 Myth: Funs are slow

.. index:: 
  object: fun
  single: 関数; ローカル
  builtin: apply/3

2.1 都市伝説: funは遅い
-----------------------

.. Yes, funs used to be slow. Very slow. Slower than apply/3. Originally,
   funs were implemented using nothing more than compiler trickery,
   ordinary tuples, apply/3, and a great deal of ingenuity.

そうです。かつてfunは遅かったです。とても遅かったです。apply/3よりも遅かったです。最初は、funはコンパイラ上のトリックと通常のタプル、apply/3と、大量のアイディアを使って実装されていました。

.. But that is ancient history. Funs was given its own data type
   in the R6B release and was further optimized in the R7B release.
   Now the cost for a fun call falls roughly between the cost for a
   call to local function and apply/3.

しかし、これは過去の歴史でしかありません。R6Bリリースからはfunに対して専用のデータ型が与えられ、R7Bリリースではかなりの最適化が行われました。現在の実装においては、funを呼び出すコストは、大雑把に言うと、ローカルの関数を呼び出すコストと、apply/3を使用するコストの中間ぐらいになっています。

.. 2.2 Myth: List comprehensions are slow

.. index:: 
  single: リスト; 内包表記

2.2 都市伝説: リスト内包表記は遅い
----------------------------------

.. List comprehensions used to be implemented using funs, and in
   the bad old days funs were really slow.

リスト内包表記は、かつてはfunを利用して実装されていました。そして、暗黒時代のfunは本当に遅かったということがありました。

.. Nowadays the compiler rewrites list comprehensions into an
   ordinary recursive function. Of course, using a tail-recursive
   function with a reverse at the end would be still faster.
   Or would it? That leads us to the next myth.

現在のコンパイラの実装では、リスト内包表記は通常の再帰関数へとリライトされます。当然、末尾再帰関数を使って最後にreverseをする方がとっても早いのです。本当？それでは次の都市伝説を見ていきましょう。

.. 2.3 Myth: Tail-recursive functions are MUCH faster than recursive functions

.. index:: 末尾再帰

2.3 都市伝説: 末尾再帰関数は、再帰関数と比べて「べらぼう」に高速である
-----------------------------------------------------------------------

.. According to the myth, recursive functions leave references
   to dead terms on the stack and the garbage collector will
   have to copy all those dead terms, while tail-recursive
   functions immediately discard those terms.

都市伝説によると、再帰関数はスタック上の不要になったtermを参照し続けてしまうため、ガーベジコレクタはこれらの不要なtermをすべてコピーしなければなりません。一方、末尾再帰関数の場合は、不要になったらすぐに捨てることができる、ということです。

.. That used to be true before R7B. In R7B, the compiler started
   to generate code that overwrites references to terms that
   will never be used with an empty list, so that the garbage
   collector would not keep dead values any longer than necessary.

これはR7B以前は本当でした。R7B以降のコンパイラは、もう使用しないtermへの参照を、空のリストで上書きをするようなコードを生成するようになりました。このため、必要以上に長い期間、ガーベジコレクタが不要な値を保持し続けることもなくなりました。

.. Even after that optimization, a tail-recursive function would
   still most of the time be faster than a body-recursive function. Why?

このような最適化の後も、ほとんどの場合で、末尾再帰関数は本体再帰(訳語正しい？)関数よりも高速なままでした。それはなぜでしょうか？

.. It has to do with how many words of stack that are used in each
   recursive call. In most cases, a recursive function would use
   more words on the stack for each recursion than the number of
   words a tail-recursive would allocate on the heap. Since more
   memory is used, the garbage collector will be invoked more
   frequently, and it will have more work traversing the stack.

それは、再帰のコール一回ごとに、どれだけのメモリ量のスタックが使用されるのか、ということと関係があります。ほとんどの場合では、末尾再帰呼び出し時ヒープ上に割り当てられるメモリ量よりも、再帰関数呼び出してスタック上に確保されるメモリ量の方が多くなります。より多くのメモリが使用されると、ガーベジコレクタの起動回数は増えますし、スタックをたどる動作が大変になります。

.. In R12B and later releases, there is an optimization that
   will in many cases reduces the number of words used on the
   stack in body-recursive calls, so that a body-recursive
   list function and tail-recursive function that calls
   lists:reverse/1 at the end will use exactly the same amount
   of memory. lists:map/2, lists:filter/2, list comprehensions,
   and many other recursive functions now use the same amount
   of space as their tail-recursive equivalents.

R12B以降のリリースでは、本体再帰呼び出しのほとんどの場合において、スタック上で使用するメモリ量を減らすように最適化するようになりました。そのため、リストを本体再帰して処理する関数を使用する場合と、末尾再帰関数を実行して最後にlists:reverse/1を実行する場合では、同じ量のメモリを使用するようになりました。現在では、lists:map/2, lists:filter/2, リスト内包表記、その他の多くの再帰関数は、末尾再帰を使用した場合と、同じメモリ量で動作するようになっています。

.. So which is faster?

それでは、最終的にどちらが速いと言えるのでしょうか？

.. It depends. On Solaris/Sparc, the body-recursive function
   seems to be slightly faster, even for lists with very
   many elements. On the x86 architecture, tail-recursion
   was up to about 30 percent faster.

それは状況に依存します。Sparc上で動作しているSolarisで、リスト中の要素が極めて多い場合には、本体再帰関数の方が速くなります。x86アーキテクチャでは、末尾再帰のほうが30%ほど高速です。

.. So the choice is now mostly a matter of taste. If you really
   do need the utmost speed, you must measure. You can no longer
   be absolutely sure that the tail-recursive list function will
   be the fastest in all circumstances.

そのような状況なので、現在ではどちらを選択するかは好みの問題でしかありません。もし最大限の速度が必要ということであれば、どちらの方がすぐれているか測定してみるしかありません。どんな状況においても、末尾再帰のリスト処理の関数の方が確実に最速であるということは、もはや言うことはできません。

.. Note: A tail-recursive function that does not need to reverse
   the list at the end is, of course, faster than a body-recursive
   function, as are tail-recursive functions that do not
   construct any terms at all (for instance, a function that sums all
   integers in a list).

.. Note::
   もしも末尾再帰関数がtermをまったく作成しないという条件で、かつ、処理の後でリストを反転させる必要がなければ、もちろん末尾再帰関数の方が本体再帰関数よりも高速になります。実例で言えば、リスト中の数値を足していくような関数がこれにあたります。

.. 2.4 Myth: '++' is always bad

.. index:: 
  operator: ++

2.4 都市伝説: '++' は使用しないほうがいい
-----------------------------------------

.. The ++ operator has, somewhat undeservedly, got a very bad
   reputation. It probably has something to do with code like

++演算子には、大げさに言うと、とても悪いうわさが付いて回っています。これはおそらく、以下のようなコードに関するものです。

.. DO NOT

非推奨::

   naive_reverse([H|T]) ->
       naive_reverse(T)++[H];
   naive_reverse([]) ->
       [].

.. which is the most inefficient way there is to reverse a list.
   Since the ++ operator copies its left operand, the
   result will be copied again and again and again...
   leading to quadratic complexity.

これは、リストを反転させる方法としては、もっとも非効率的な方法です。++演算子は左側のオペランドのコピーを作成し、結果もまたコピーされ、そしてそれもコピーされて・・・と、N^2のオーダーでの非効率が発生するというのがその理由です。

.. On the other hand, using ++ like this

一方以下のようなケースで ++ 演算子を利用するのは問題ありません。

OK::

   naive_but_ok_reverse([H|T], Acc) ->
       naive_but_ok_reverse(T, [H]++Acc);
   naive_but_ok_reverse([], Acc) ->
       Acc.

.. is not bad. Each list element will only be copied once.
   The growing result Acc is the right operand for the ++
   operator, and it will not be copied.

この場合は、リストの要素ごとに1度だけコピーされます。結果を結合するときに、徐々に結果が大きくなっていく側の Acc は ++ 演算子の右側のオペランドであるため、これに含まれたものが何度もコピーされることはありません。

.. Of course, experienced Erlang programmers would actually write

当然、経験のあるErlangプログラマは、実際には以下のように書くでしょう。

.. DO

推奨::

   vanilla_reverse([H|T], Acc) ->
       vanilla_reverse(T, [H|Acc]);
   vanilla_reverse([], Acc) ->
       Acc.

.. which is slightly more efficient because you don't build a
   list element only to directly copy it. (Or it would be more
   efficient if the the compiler did not automatically rewrite
   [H]++Acc to [H|Acc].)

この方法は、上記であげた良い例よりもさらに多少効率的です。というのは、コピーをしないでリストの要素を組み立てていっているからです。(もしくは、コンパイラが [H]++Acc を [H|Acc] へと自動で書き換えるようなことをしなかったため、より効率的でした。)

.. 2.5 Myth: Strings are slow

.. index:: 文字列

2.5 都市伝説: 文字列は遅い
--------------------------

.. Actually, string handling could be slow if done improperly. In Erlang,
   you'll have to think a little more about how the strings are used and
   choose an appropriate representation and use the re instead of the
   obsolete regexp module if you are going to use regualr expressions.

実際に適切でないやりかたで文字列を操作すると遅くなる可能性はあります。Erlangでは、文字列がどのように使用されているかについて考える必要があります。また、正規表現を使用しようと思ったときに、古くなったregexpモジュールではなくて、新しいreモジュールを使用するなどの配慮をする必要があります。

.. 2.6 Myth: Repairing a Dets file is very slow

.. index::
  pair: Dets; 修復

2.6 都市伝説: Detsファイルの修復はとても時間がかかる
--------------------------------------------------------

.. The repair time is still proportional to the number of records
   in the file, but Dets repairs used to be much, much slower in the
   past. Dets has been massively rewritten and improved.

Detsファイルの修復時間は、ファイルに含まれるレコード数に比例しますが、以前の実装ではとてつもなく時間がかかっていました。その後、大幅に実装が書き換えられたため、性能は改善しています。

.. 2.7 Myth: BEAM is a stack-based byte-code virtual machine (and therefore slow)

.. index::
  pair: BEAM; 仮想マシン
  pair: スタックベース; 仮想マシン
  pair: レジスタベース; 仮想マシン
  single: 仮想レジスタ
  single: ダイレクトスレデッドコード

2.7 都市伝説: BEAMはスタックベースのバイトコードの仮想マシンである(ために遅い)
---------------------------------------------------------------------------------

.. BEAM is a register-based virtual machine. It has 1024
.. virtual registers that are used for holding temporary
.. values and for passing arguments when calling functions.
.. Variables that need to survive a function call are saved
.. to the stack.

BEAMはレジスタベースの仮想マシンです。1024個の仮想レジスタを持ち、関数呼び出しがあった際に、引数を渡すために一時的に値を格納するために使用されています。関数呼び出しを実行する際に残す必要のある変数だけがスタックに保存されます。

.. BEAM is a threaded-code interpreter. Each instruction is word
   pointing directly to executable C-code, making instruction
   dispatching very fast.

BEAMはダイレクトスレデッドコード [#threadedcode]_ という方法で高速化されており、命令分岐は非常に高速です。

.. [#threadedcode] (訳注) インタプリタの最適化の手法の一つ。結果として命令キャッシュのヒット率が上がり、現代的な命令予測をするCPUでの実行効率が高まります。Rubyist Magazineに解説があります。 http://jp.rubyist.net/magazine/?0008-YarvManiacs

.. 2.8 Myth: Use '_' to speed up your program when a variable is not used

.. index::
  pair: 未使用変数; _

2.8 都市伝説: 変数を使用しない時は、'_'を使用することでプログラムの速度が上がる
-------------------------------------------------------------------------------

.. That was once true, but since R6B the BEAM compiler is quite capable
   of seeing itself that a variable is not used.

これはかつての実装では正しい話でした。しかし、R6BのBEAMコンパイラからは、ほとんどのケースで、変数が使用されているか、そうではないのか、というのが検知できるようになっています。

    Copyright (c) 1991-2009 Ericsson AB

