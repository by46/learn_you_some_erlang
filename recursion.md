Recursion
===
Hello recursion!
---

![](/images/ch4/recurse.png)

Some readers accustomed with imperative and object-oriented programming languages might be wondering why loops weren't shown already. The answer to this is "what is a loop?" Truth is, functional programming languages usually do not offer looping constructs like for and while. Instead, functional programmers rely on a silly concept named recursion.

一些习惯于命令式和面向对象编程语言的读者也许会惊讶为什么循环。对于这个问题的答案来源于“什么是循环”。真相是，函数式编程语言通常不提供类似for 和 while 的循环关键字。 函数式程序员依赖名为递归的简单概念。

I suppose you remember how invariable variables were explained in the intro chapter. If you don't, you can give them more attention! Recursion can also be explained with the help of mathematical concepts and functions. A basic mathematical function such as the factorial of a value is a good example of a function that can be expressed recursively. The factorial of a number n is the product of the sequence 1 x 2 x 3 x ... x n, or alternatively n x (n-1) x (n-2) x ... x 1. To give some examples, the factorial of 3 is 3! = 3 x 2 x 1 = 6. The factorial of 4 would be 4! = 4 x 3 x 2 x 1 = 24. Such a function can be expressed the following way in mathematical notation:

我假设你还记得在前面章节不变的变量。如果你不记得了， 那你需要关注他们。递归可以借助数学概念和函数来解释。一些基本的数学函数例如阶乘就是很好例子说明函数可以递归地表达。n的阶乘就是1 x 2 x 3 x 4 x...x n或者n x (n -1) x (n - 2) x …x 1的乘积。具一些例子， 3的阶乘 3! = 3 x 2 x 1 = 6 。 4的阶乘4! = 4 x 3 x 2 x 1 = 24 。阶乘可用下列数据公式表达：

![](/images/ch4/fac.png)

What this tells us is that if the value of n we have is 0, we return the result 1. For any value above 0, we return n multiplied by the factorial of n-1, which unfolds until it reaches 1:

这个函数表示， 如果n等于0， 函数就返回1.  否则， 函数函数n 乘以 n - 1的阶乘， 一直展开表达式直到到达1：

```
4! = 4 x 3!
4! = 4 x 3 x 2!
4! = 4 x 3 x 2 x 1!
4! = 4 x 3 x 2 x 1 x 1
```

How can such a function be translated from mathematical notation to Erlang? The conversion is simple enough. Take a look at the parts of the notation: n!, 1 and n((n-1)!) and then the ifs. What we've got here is a function name (n!), guards (the ifs) and a the function body (1 and n((n-1)!)). We'll rename n! to fac(N) to restrict our syntax a bit and then we get the following:

像这样一个函数，如何才能从数学符号翻译成Erlang代码？转换非常简单。看一看公式的各个部分：n!， 1 ，n((n - 1)!) 和if语句。 那么我们就此等到一个函数名(n!), guard子句（if语句）和 函数体（1 和 n((n - 1)!)。我们可以fac(N)来作为函数名， 然后我们会得到下列函数：

``` erlang
-module(recursive).
-export([fac/1]).
 
fac(N) when N == 0 -> 1;
fac(N) when N > 0  -> N*fac(N-1).
```

And this factorial function is now done! It's pretty similar to the mathematical definition, really. With the help of pattern matching, we can shorten the definition a bit:
阶乘函数已经完成！它和数学定义非常相像。使用模式匹配，我们可以让它定义更简单一些：

``` erlang
fac(0) -> 1;
fac(N) when N > 0 -> N*fac(N-1).
```

So that's quick and easy for some mathematical definitions which are recursive in nature. We looped! A definition of recursion could be made short by saying "a function that calls itself." However, we need to have a stopping condition (the real term is base case), because we'd otherwise loop infinitely. In our case, the stopping condition is when n is equal to 0. At that point we no longer tell our function to call itself and it stops its execution right there.

对于一些自身就具有递归性的数学函数定义， 这样的转换是很快，很容易的。我们已经循环了。递归的定义可以简单的概括为一个调用自己的函数。但是，我们也需要有一个终止条件，不然就会无限循环。在我们的例子中， 这终止条件就是n等于0。满足终止条件是，我们就不再调用自己， 它将会正确的停止。

Length
---

Let's try to make it slightly more practical. We'll implement a function to count how many elements a list contains. So we know from the beginning that we will need:

那我们来做一些简单的实践。 我们将会实现一个函数计算一个列表里面有多少个元素。在开始之前，我们需要知道如下节点：

    - a base case;
    - a function that calls itself;
    - a list to try our function on.

  - 终止条件
  - 一个调用自己的函数
  - 一个列表


With most recursive functions, I find the base case easier to write first: what's the simplest input we can have to find a length from? Surely an empty list is the simplest one, with a length of 0. So let's make a mental note that [] = 0 when dealing with lengths. Then the next simplest list has a length of 1: [_] = 1. This sounds like enough to get going with our definition. We can write this down:

很大多数递归函数一样， 我首先完成比较容易的终止条件。能计算出长度的最简单的输入是什么？无疑是空列表， 它的长度为0. 所以我们需要记下[]的长度为0. 然后第二简单的情况当然要属包含一个元素的类表：[_] = 1。我可以这样实现函数：

``` erlang
len([]) -> 0;
len([_]) -> 1.
```

Awesome! We can calculate the length of lists, given the length is either 0 or 1! Very useful indeed. Well of course it's useless, because it's not yet recursive, which brings us to the hardest part: extending our function so it calls itself for lists longer than 1 or 0. It was mentioned earlier that lists are defined recursively as [1 | [2| ... [n | []]]]. This means we can use the [H|T] pattern to match against lists of one or more elements, as a list of length one will be defined as [X|[]] and a list of length two will be defined as [X|[Y|[]]]. Note that the second element is a list itself. This means we only need to count the first one and the function can call itself on the second element. Given each value in a list counts as a length of 1, the function can be rewritten the following way:

我可以计算长度为0或者1的列表的长度了！的确很有用。同样这样的函数也没有多大用处， 因为它们不能递归，那么让我们来见识一下最困难的部分：为了应对长度超过1的列表，我们应该扩展函数，使函数自己调用自己。正如我们之前提到的， 列表可以递归的定义：[1|[2|..[n]]]。这就意味着我们可以使用[H|T]的模式匹配包含多个元素的列表，因为包含一个元素的列表可以定义为[X|[]]， 包含两个元素的列表可以定义为[X|[Y|[]]]。注意第二个元素本身就是一个列表。列表中给定的值被计数为1， 函数可以重写为：

``` erlang
len([]) -> 0;
len([_|T]) -> 1 + len(T).
```

And now you've got your own recursive function to calculate the length of a list. To see how len/1 would behave when ran, let's try it on a given list, say [1,2,3,4]:

现在你已经得到了你自己的递归函数用于计算列表的长度。看看len/1函数式如何运作的。让我使用[1,2,3,4]作为输入:

``` erlang
len([1,2,3,4]) = len([1 | [2,3,4])
               = 1 + len([2 | [3,4]])
               = 1 + 1 + len([3 | [4]])
               = 1 + 1 + 1 + len([4 | []])
               = 1 + 1 + 1 + 1 + len([])
               = 1 + 1 + 1 + 1 + 0
               = 1 + 1 + 1 + 1
               = 1 + 1 + 2
               = 1 + 3 
               = 4
```

Which is the right answer. Congratulations on your first useful recursive function in Erlang!

这就是正确答案。祝贺你已经完成Erlang中的第一个有用的递归函数。

Length of a Tail Recursion
---

![](/images/ch4/tail-recursion.png)

You might have noticed that for a list of 4 terms, we expanded our function call to a single chain of 5 additions. While this does the job fine for short lists, it can become problematic if your list has a few million values in it. You don't want to keep millions of numbers in memory for such a simple calculation. It's wasteful and there's a better way. Enter tail recursion.

你也许已经注意到刚才我们使用了包含四个元素的列表， 那我们现在用包含5个函数的列表来调用函数。这个函数在计算小列表长度事工作得很好， 如果你的列表包含10万个元素时，它将会用问题。对于如此简单的计算，你不会希望在内存中持有者10万的数据吧。这十分浪费资源，有一个很好的办法可以解决这样的问题。就是尾递归。

Tail recursion is a way to transform the above linear process (it grows as much as there are elements) to an iterative one (there is not really any growth). To have a function call being tail recursive, it needs to be 'alone'. Let me explain: what made our previous calls grow is how the answer of the first part depended on evaluating the second part. The answer to 1 + len(Rest) needs the answer of len(Rest) to be found. The function len(Rest) itself then needed the result of another function call to be found. The additions would get stacked until the last one is found, and only then would the final result be calculated. Tail recursion aims to eliminate this stacking of operation by reducing them as they happen.

尾递归是把上面提到的问题从线性过程转换为递归的方式。一个函数能被称为尾递归，他需要是“单独的”。让我们来解释一下：导致我们之前的函数调用资源增长的原因是计算第一部分却依赖第二部分。计算 1 + len(Rest)需要我们计算出len(Rest)的值。函数len(Rest)的计算又依赖于另外一个函数调用。在计算出最后一部分之前，所有的中间结果都必须入栈保存。尾递归只要用于消除操作栈的消耗。

In order to achieve this, we will need to hold an extra temporary variable as a parameter in our function. I'll illustrate the concept with the help of the factorial function, but this time defining it to be tail recursive. The aforementioned temporary variable is sometimes called accumulator and acts as a place to store the results of our computations as they happen in order to limit the growth of our calls:

为了完成这个目标，在我们的函数中，我需要持有一个额外的临时变量。我会重新演示关于阶乘函数的概念，但是这次我会用尾递归实现它。前面提到的临时变量有时被称为累加器， 扮演存储我们计算的结果的地方，它可以限制堆栈的增长：

``` erlang
tail_fac(N) -> tail_fac(N,1).
 
tail_fac(0,Acc) -> Acc;
tail_fac(N,Acc) when N > 0 -> tail_fac(N-1,N*Acc).
```

Here, I define both tail_fac/1 and tail_fac/2. The reason for this is that Erlang doesn't allow default arguments in functions (different arity means different function) so we do that manually. In this specific case, tail_fac/1 acts like an abstraction over the tail recursive tail_fac/2 function. The details about the hidden accumulator of tail_fac/2 don't interest anyone, so we would only export tail_fac/1 from our module. When running this function, we can expand it to:

这里，我同时定义二楼tail_fac/1 和tail_fac/2两个函数。 原因是Erlang不允许在函数中定义默认值(不同的参数数目意味着是不同的函数)，所以我们手动完成定义。在这个特殊的例子中， tail_fac/1类似是在尾递归函数tail_fac/2之上的抽象。tail_fac/2隐藏的累加器的细节对任何人都没有吸引力，所以我们仅仅需要导出tail_fac/2函数。当我们运行这个函数时， 它会按照下面的方式展开执行：

``` erlang
tail_fac(4)    = tail_fac(4,1)
tail_fac(4,1)  = tail_fac(4-1, 4*1)
tail_fac(3,4)  = tail_fac(3-1, 3*4)
tail_fac(2,12) = tail_fac(2-1, 2*12)
tail_fac(1,24) = tail_fac(1-1, 1*24)
tail_fac(0,24) = 24
```

See the difference? Now we never need to hold more than two terms in memory: the space usage is constant. It will take as much space to calculate the factorial of 4 as it will take space to calculate the factorial of 1 million (if we forget 4! is a smaller number than 1M! in its complete representation, that is).

看见不同之处了吗？我们不需要在内存中持有超过两个项元的内存：内存使用量将是恒定的。计算4!和计算1000000!所使用的内存是一样多的。

With an example of tail recursive factorials under your belt, you might be able to see how this pattern could be applied to our len/1 function. What we need is to make our recursive call 'alone'. If you like visual examples, just imagine you're going to put the +1 part inside the function call by adding a parameter:

通过尾递归阶乘函数的例子，你已经明白如何将这种模式应用到我们的len/1函数中。我们需要做的就是让我们的递归调用变成“单独”.如果你喜欢栩栩如生的例子， 你只需要想象一下把+1的部分放在额外参数里面:

``` erlang
len([]) -> 0;
len([_|T]) -> 1 + len(T).
```

becomes:

``` erlang
tail_len(L) -> tail_len(L,0).
 
tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T,Acc+1).
```

And now your length function is tail recursive.
现在len/1函数已经是尾递归函数了。

More recursive functions
---

![](/images/ch4/rock-paper-scissors.png)

We'll write a few more recursive functions, just to get in the habit a bit more. After all, recursion being the only looping construct that exists in Erlang (except list comprehensions), it's one of the most important concepts to understand. It's also useful in every other functional programming language you'll try afterwards, so take notes!

我们会编写更多的递归函数，只是仅仅养成这样一个习惯。递归是Erlang中唯一存在的循环子句(除了列表表达式)。这是一个很重要的概念。在每一种函数式编程语言中， 递归都是很有用的!

The first function we'll write will be duplicate/2. This function takes an integer as its first parameter and then any other term as its second parameter. It will then create a list of as many copies of the term as specified by the integer. Like before, thinking of the base case first is what might help you get going. For duplicate/2, asking to repeat something 0 time is the most basic thing that can be done. All we have to do is return an empty list, no matter what the term is. Every other case needs to try and get to the base case by calling the function itself. We will also forbid negative values for the integer, because you can't duplicate something -n times:

我们第一个要写的例子是duplicate/2函数。 这个函数接受一个整数N， 一个任意类型的项元Term作为参数。它会创建一个包含N个Term的列表。像以前一样，首先找出base case。 对于duplicate/2， 回答计算零次重复最容易。我们只需要返回一个空列表， 不关心Term是什么数据， 其他的情况都需要根据base case进行计算。我们同样会禁止负数，因为没有-n次的概念：

``` erlang
duplicate(0,_) ->
    [];
duplicate(N,Term) when N > 0 ->
    [Term|duplicate(N-1,Term)].
```

Once the basic recursive function is found, it becomes easier to transform it into a tail recursive one by moving the list construction into a temporary variable:
当我们找到基础递归函数之后，把它变成尾递归就很容易了， 只要把列表构造的部分放入临时变量中就可以了：

``` erlang
tail_duplicate(N,Term) ->
tail_duplicate(N,Term,[]).
 
tail_duplicate(0,_,List) ->
    List;
tail_duplicate(N,Term,List) when N > 0 ->
tail_duplicate(N-1, Term, [Term|List]).
```

Success! I want to change the subject a little bit here by drawing a parallel between tail recursion and a while loop. Our tail_duplicate/2 function has all the usual parts of a while loop. If we were to imagine a while loop in a fictional language with Erlang-like syntax, our function could look a bit like this:

成功！我想稍微改变一下主题， 对比一下尾递归和循环的区别。我们的tail_duplicate/2函数拥有所有的通用部分。如果我们用虚拟的函数，它看上去会像这样：

``` erlang
function(N, Term) ->
    while N > 0 ->
        List = [Term|List],
        N = N-1
    end,
    List.
```

Note that all the elements are there in both the fictional language and in Erlang. Only their position changes. This demonstrates that a proper tail recursive function is similar to an iterative process, like a while loop.

记住这里提到的所有提到的元素，即是在虚拟语言和Erlang中， 只是她们的位置不同而已。这个示例只是为了说明尾递归和迭代过程非常相似，就像while循环一样。

There's also an interesting property that we can 'discover' when we compare recursive and tail recursive functions by writing a reverse/1 function, which will reverse a list of terms. For such a function, the base case is an empty list, for which we have nothing to reverse. We can just return an empty list when that happens. Every other possibility should try to converge to the base case by calling itself, like with duplicate/2. Our function is going to iterate through the list by pattern matching [H|T] and then putting H after the rest of the list:

当我们比较递归和尾递归的时候，我们也发现一些有趣的属性。对于这个函数，base case 是一个空列表， 所以我们没有任何东西需要reserve。当传入空列表时，函数返回空列表。其他情况，我们可以通过base case 来计算出来， 就像duplicate/2函数。 我们的函数通过[H|T]模式遍历整个列表，把H放到剩余的列表后面:

``` erlang
reverse([]) -> [];
reverse([H|T]) -> reverse(T)++[H].
```

On long lists, this will be a true nightmare: not only will we stack up all our append operations, but we will need to traverse the whole list for every single of these appends until the last one! For visual readers, the many checks can be represented as:

对于长列表， 这将变成噩梦：每一次append操作都需要进行压栈操作，不仅如此每一次append操作都需要遍历整个列表，整个过程大概如下：

``` erlang
reverse([1,2,3,4]) = [4]++[3]++[2]++[1]
                      ↑    ↵
                   = [4,3]++[2]++[1]
                      ↑ ↑    ↵
                   = [4,3,2]++[1]
                      ↑ ↑ ↑    ↵
                   = [4,3,2,1]
```

This is where tail recursion comes to the rescue. Because we will use an accumulator and will add a new head to it every time, our list will automatically be reversed. Let's first see the implementation:

如果我们用一种更为人所熟知的方式，正如下所示:

``` erlang
tail_reverse(L) -> tail_reverse(L,[]).
 
tail_reverse([],Acc) -> Acc;
tail_reverse([H|T],Acc) -> tail_reverse(T, [H|Acc]).
```

If we represent this one in a similar manner as the normal version, we get:

``` erlang
tail_reverse([1,2,3,4]) = tail_reverse([2,3,4], [1])
                        = tail_reverse([3,4], [2,1])
                        = tail_reverse([4], [3,2,1])
                        = tail_reverse([], [4,3,2,1])
                        = [4,3,2,1]   
```

Which shows that the number of elements visited to reverse our list is now linear: not only do we avoid growing the stack, we also do our operations in a much more efficient manner!

上面的代码显示了我们遍历列表的代价已经是线性：不仅避免了栈的增长，而且我们的操作变得更高效了。

Another function to implement could be sublist/2, which takes a list L and an integer N, and returns the N first elements of the list. As an example, sublist([1,2,3,4,5,6],3) would return [1,2,3]. Again, the base case is trying to obtain 0 elements from a list. Take care however, because sublist/2 is a bit different. You've got a second base case when the list passed is empty! If we do not check for empty lists, an error would be thrown when calling recursive:sublist([1],2). while we want [1] instead. Once this is defined, the recursive part of the function only has to cycle through the list, keeping elements as it goes, until it hits one of the base cases:

另外一个实现的函数是sublist/2, 接受一个列表参数L和一个整数N，返回列表L中头N个元素。 例如， sublist([1,2,3,4,5,6],3)将会返回[1,2,3]。同样的， base case 是尝试获取从列表中获取0个元素。注意， 因为sublist/2有一些不同之处。你会发现有第二个base base，那就是列表元素是一个空列表。如果我们不检查空列表， 一个错误将会发生，例如sublist([1],2)。 在这种情况下，我们希望返回列表[1]。那么接下来的事情就简单了，只是简单的遍历整个列表，确保每个元素都会被遍历，直到获取到足够多的元素:

``` erlang
sublist(_,0) -> [];
sublist([],_) -> [];
sublist([H|T],N) when N > 0 -> [H|sublist(T,N-1)].
```

Which can then be transformed to a tail recursive form in the same manner as before:
我们可以采用同样的方法，把 它转换为尾递归方法:

``` erlang
tail_sublist(L, N) -> tail_sublist(L, N, []).
 
tail_sublist(_, 0, SubList) -> SubList;
tail_sublist([], _, SubList) -> SubList;
tail_sublist([H|T], N, SubList) when N > 0 ->
tail_sublist(T, N-1, [H|SubList]).
```

There's a flaw in this function. A fatal flaw! We use a list as an accumulator in exactly the same manner we did to reverse our list. If you compile this function as is, sublist([1,2,3,4,5,6],3) would not return [1,2,3], but [3,2,1]. The only thing we can do is take the final result and reverse it ourselves. Just change the tail_sublist/2 call and leave all our recursive logic intact:

在这个函数中也有一些瑕疵。一个致命的瑕疵！我们用同样的方式，使用一个列表作为加速器，来反转我们的列表。当你编译函数，并执行sublist([1,2,3,4,5,6],3)将不会返回[1,2,3]，而是返回的是[3,2,1]。我们唯一能做的是在返回最后结果之前颠倒列表的顺序。简单的按照如下方式更改tail_sublist/2函数:

``` erlang
tail_sublist(L, N) -> reverse(tail_sublist(L, N, [])).
```

The final result will be ordered correctly. It might seem like reversing our list after a tail recursive call is a waste of time and you would be partially right (we still save memory doing this). On shorter lists, you might find your code is running faster with normal recursive calls than with tail recursive calls for this reason, but as your data sets grow, reversing the list will be comparatively lighter.

最后的结果就是顺序正确的。对尾递归调用返回的结果执行reserve操作看上似乎是在浪费时间，但是你只说对了一半（这样做始终节省了内存）。对于短列表来说，常规递归调用的速度会比尾递归执行得更快，但是当你的数据集增长时，反转列表的操作还是比较快的。

Note: instead of writing your own reverse/1 function, you should use lists:reverse/1. It's been used so much for tail recursive calls that the maintainers and developers of Erlang decided to turn it into a BIF. Your lists can now benefit from extremely fast reversal (thanks to functions written in C) which will make the reversal disadvantage a lot less obvious. The rest of the code in this chapter will make use of our own reversal function, but after that you should not use it ever again.

注意:应避免编写你自己的reverse 函数，取而代之的是使用lists:reverse/1。对于尾递归它已经使用得如此之多，所以Erlang的维护者和开发者决定把它转换为BIF。所以list也得益于此（多亏用C语言写的函数）。接下来的例子我们还是会继续使用我们的反转函数，但是你在接下来的使用不应该在使用它们。

To push things a bit further, we'll write a zipping function. A zipping function will take two lists of same length as parameters and will join them as a list of tuples which all hold two terms. Our own zip/2 function will behave this way:

为了推动事情更远一点， 我们将写一个zip函数。zip函数需要接受两个相同长度的列表作为参数，把他们连接成一个包含两个元素的元组的列表。我们的zip函数将像下面的方式工作：

``` erlang
1> recursive:zip([a,b,c],[1,2,3]).
[{a,1},{b,2},{c,3}]
```

Given we want our parameters to both have the same length, the base case will be zipping two empty lists:

``` erlang
zip([],[]) -> [];
zip([X|Xs],[Y|Ys]) -> [{X,Y}|zip(Xs,Ys)].
```

However, if you wanted a more lenient zip function, you could decide to have it finish whenever one of the two list is done. In this scenario, you therefore have two base cases:

如果假设我们的两个参数长度相同，那么base case就是接受两个空列表:

``` erlang
lenient_zip([],_) -> [];
lenient_zip(_,[]) -> [];
lenient_zip([X|Xs],[Y|Ys]) -> [{X,Y}|lenient_zip(Xs,Ys)].
```

Notice that no matter what our base cases are, the recursive part of the function remains the same. I would suggest you try and make your own tail recursive versions of zip/2 and lenient_zip/2, just to make sure you fully understand how to make tail recursive functions: they'll be one of the central concepts of larger applications where our main loops will be made that way.

无论我们的base case是什么，函数的递归部分都是一样的。

If you want to check your answers, take a look at my implementation of recursive.erl, more precisely the tail_zip/2 and tail_lenient_zip/3 functions.

我鼓励你尝试实现你自己的尾递归版本的zip函数和lenient_zip函数， 只是为了确保你已经完全掌握了尾递归函数：他们将是一个大型应用的中心概念。

Note: tail recursion as seen here is not making the memory grow because when the virtual machine sees a function calling itself in a tail position (the last expression to be evaluated in a function), it eliminates the current stack frame. This is called tail-call optimisation (TCO) and it is a special case of a more general optimisation named Last Call Optimisation (LCO).

注意：尾递归没有是内存增长是因为当虚拟机看见一个函数在最后一步调用自己的时候（函数中的最后一个被求值的表达式），它将丢弃当前堆栈， 这个叫做尾调用优化(TCO)，是相较于称作最后调用优化（LCO）的常见优化更特殊的情况。

LCO is done whenever the last expression to be evaluated in a function body is another function call. When that happens, as with TCO, the Erlang VM avoids storing the stack frame. As such tail recursion is also possible between multiple functions. As an example, the chain of functions a() -> b(). b() -> c(). c() -> a(). will effectively create an infinite loop that won't go out of memory as LCO avoids overflowing the stack. This principle, combined with our use of accumulators is what makes tail recursion useful.

LCO在函数中最后一个被求值的表达式是调用其他函数时发生的。当TCO发生时， erlang虚拟机将避免存储栈帧。同样很多递归调用也可以在多个函数之间调用。例如， 这个调用链： a()->b().b()->c(). c() ->a(). 这样的调用链会有效地创建无限循环， 又不至于耗尽内存， 因为LCO避免堆栈溢出。

Quick, Sort!
---

![](/images/ch4/quicksort.png)

I can (and will) now assume recursion and tail recursion make sense to you, but just to make sure, I'm going to push for a more complex example, quicksort. Yes, the traditional "hey look I can write short functional code" canonical example. A naive implementation of quicksort works by taking the first element of a list, the pivot, and then putting all the elements smaller or equal to the pivot in a new list, and all those larger in another list. We then take each of these lists and do the same thing on them until each list gets smaller and smaller. This goes on until you have nothing but an empty list to sort, which will be our base case. This implementation is said to be naive because smarter versions of quicksort will try to pick optimal pivots to be faster. We don't really care about that for our example though.

我现在假定你已经对递归和尾递归有一定认识，但是为了保险起见，我将会列举一个更复杂的例子。 快速排序的一个稚嫩的实现，使用列表的第一个元素作为枢轴，大于或者等于枢轴的所有元素作为一个新的列表，其他的大于枢轴的作为另一个列表。然后我们对这两个列表做同样的操作，直到列表变得越来越小。当有列表为空的时候， 就是我们的base case。这实现之所以被称为稚嫩的， 因为其他更智能的版本会尝试挑选一个优化的枢轴，使得排序更快。对于我们的例子不需要关心这个。

We will need two functions for this one: a first function to partition the list into smaller and larger parts and a second function to apply the partition function on each of the new lists and to glue them together. First of all, we'll write the glue function:

我们需要两个函数：第一个函数用于把列表分成分别包含“小”元素和“大”元素两个部分，第二个函数用于把两个列表合并成一个。首先我来写第二个函数：

``` erlang
quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
    {Smaller, Larger} = partition(Pivot,Rest,[],[]),
    quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).
```

This shows the base case, a list already partitioned in larger and smaller parts by another function, the use of a pivot with both lists quicksorted appended before and after it. So this should take care of assembling lists. Now the partitioning function:

``` erlang
partition(_,[], Smaller, Larger) -> {Smaller, Larger};
partition(Pivot, [H|T], Smaller, Larger) ->
    if H =< Pivot -> partition(Pivot, T, [H|Smaller], Larger);
        H >  Pivot -> partition(Pivot, T, Smaller, [H|Larger])
    end.
```

And you can now run your quicksort function. If you've looked for Erlang examples on the Internet before, you might have seen another implementation of quicksort, one that is simpler and easier to read, but makes use of list comprehensions. The easy to replace parts are the ones that create new lists, the partition/4 function:

``` erlang
lc_quicksort([]) -> [];
lc_quicksort([Pivot|Rest]) ->
    lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot])
    ++ [Pivot] ++
    lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).
```

The main differences are that this version is much easier to read, but in exchange, it has to traverse the list twice to partition it in two parts. This is a fight of clarity against performance, but the real loser here is you, because a function lists:sort/1 already exists. Use that one instead.

Don't drink too much Kool-Aid:
All this conciseness is good for educational purposes, but not for performance. Many functional programming tutorials never mention this! First of all, both implementations here need to process values that are equal to the pivot more than once. We could have decided to instead return 3 lists: elements smaller, larger and equal to the pivot in order to make this more efficient.

Another problem relates to how we need to traverse all the partitioned lists more than once when attaching them to the pivot. It is possible to reduce the overhead a little by doing the concatenation while partitioning the lists in three parts. If you're curious about this, look at the last function (bestest_qsort/1) of recursive.erl for an example.

A nice point about all of these quicksorts is that they will work on lists of any data type you've got, even tuples of lists and whatnot. Try them, they work!

More than lists
---
By reading this chapter, you might be starting to think recursion in Erlang is mainly a thing concerning lists. While lists are a good example of a data structure that can be defined recursively, there's certainly more than that. For the sake of diversity, we'll see how to build binary trees, and then read data from them.

![](/images/ch4/tree.png)

First of all, it's important to define what a tree is. In our case, it's nodes all the way down. Nodes are tuples that contain a key, a value associated to the key, and then two other nodes. Of these two nodes, we need one that has a smaller and one that has a larger key than the node holding them. So here's recursion! A tree is a node containing nodes, each of which contains nodes, which in turn also contain nodes. This can't keep going forever (we don't have infinite data to store), so we'll say that our nodes can also contain empty nodes.

To represent nodes, tuples are an appropriate data structure. For our implementation, we can then define these tuples as {node, {Key, Value, Smaller, Larger}} (a tagged tuple!), where Smaller and Larger can be another similar node or an empty node ({node, nil}). We won't actually need a concept more complex than that.

Let's start building a module for our very basic tree implementation. The first function, empty/0, returns an empty node. The empty node is the starting point of a new tree, also called the root:

``` erlang
-module(tree).
-export([empty/0, insert/3, lookup/2]).
```
 
empty() -> {node, 'nil'}.
By using that function and then encapsulating all representations of nodes the same way, we hide the implementation of the tree so people don't need to know how it's built. All that information can be contained by the module alone. If you ever decide to change the representation of a node, you can then do it without breaking external code.

To add content to a tree, we must first understand how to recursively navigate through it. Let's proceed in the same way as we did for every other recursion example by trying to find the base case. Given that an empty tree is an empty node, our base case is thus logically an empty node. So whenever we'll hit an empty node, that's where we can add our new key/value. The rest of the time, our code has to go through the tree trying to find an empty node where to put content.

To find an empty node starting from the root, we must use the fact that the presence of Smaller and Larger nodes let us navigate by comparing the new key we have to insert to the current node's key. If the new key is smaller than the current node's key, we try to find the empty node inside Smaller, and if it's larger, inside Larger. There is one last case, though: what if the new key is equal to the current node's key? We have two options there: let the program fail or replace the value with the new one. This is the option we'll take here. Put into a function all this logic works the following way:

``` erlang
insert(Key, Val, {node, 'nil'}) ->
    {node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
    {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
    {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
insert(Key, Val, {node, {Key, _, Smaller, Larger}}) ->
    {node, {Key, Val, Smaller, Larger}}.
```

Note here that the function returns a completely new tree. This is typical of functional languages having only single assignment. While this can be seen as inefficient, most of the underlying structures of two versions of a tree sometimes happen to be the same and are thus shared, copied by the VM only when needed.

What's left to do on this example tree implementation is creating a lookup/2 function that will let you find a value from a tree by giving its key. The logic needed is extremely similar to the one used to add new content to the tree: we step through the nodes, checking if the lookup key is equal, smaller or larger than the current node's key. We have two base cases: one when the node is empty (the key isn't in the tree) and one when the key is found. Because we don't want our program to crash each time we look for a key that doesn't exist, we'll return the atom 'undefined'. Otherwise, we'll return {ok, Value}. The reason for this is that if we only returned Value and the node contained the atom 'undefined', we would have no way to know if the tree did return the right value or failed to find it. By wrapping successful cases in such a tuple, we make it easy to understand which is which. Here's the implemented function:

``` erlang
lookup(_, {node, 'nil'}) ->
    undefined;
lookup(Key, {node, {Key, Val, _, _}}) ->
    {ok, Val};
lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey ->
lookup(Key, Smaller);
lookup(Key, {node, {_, _, _, Larger}}) ->
lookup(Key, Larger).
```

And we're done. Let's test it with by making a little email address book. Compile the file and start the shell:

``` erlang
1> T1 = tree:insert("Jim Woodland", "jim.woodland@gmail.com", tree:empty()).
{node,{"Jim Woodland","jim.woodland@gmail.com",
    {node,nil},
    {node,nil}}}
2> T2 = tree:insert("Mark Anderson", "i.am.a@hotmail.com", T1).
{node,{"Jim Woodland","jim.woodland@gmail.com",
    {node,nil},
    {node,{"Mark Anderson","i.am.a@hotmail.com",
        {node,nil},
        {node,nil}}}}}
3> Addresses = tree:insert("Anita Bath", "abath@someuni.edu", tree:insert("Kevin Robert", "myfairy@yahoo.com", tree:insert("Wilson Longbrow", "longwil@gmail.com", T2))).
    {node,{"Jim Woodland","jim.woodland@gmail.com",
        {node,{"Anita Bath","abath@someuni.edu",
            {node,nil},
            {node,nil}}},
        {node,{"Mark Anderson","i.am.a@hotmail.com",
            {node,{"Kevin Robert","myfairy@yahoo.com",
                {node,nil},
                {node,nil}}},
            {node,{"Wilson Longbrow","longwil@gmail.com",
                {node,nil},
                {node,nil}}}}}}}

```

And now you can lookup email addresses with it:

``` erlang
4> tree:lookup("Anita Bath", Addresses).
{ok, "abath@someuni.edu"}
5> tree:lookup("Jacques Requin", Addresses).
undefined
```

That concludes our functional address book example built from a recursive data structure other than a list! Anita Bath now...

Note: Our tree implementation is very naive: we do not support common operations such as deleting nodes or rebalancing the tree to make the following lookups faster. If you're interested in implementing and/or exploring these, studying the implementation of Erlang's gb_trees module (otp_src_R<version>B<revision>/lib/stdlib/src/gb_trees.erl) is a good idea. This is also the module you should use when dealing with trees in your code, rather than reinventing your own wheel.

Thinking recursively
---
f you've understood everything in this chapter, thinking recursively is probably becoming more intuitive. A different aspect of recursive definitions when compared to their imperative counterparts (usually in while or for loops) is that instead of taking a step-by-step approach ("do this, then that, then this, then you're done"), our approach is more declarative ("if you get this input, do that, this otherwise"). This property is made more obvious with the help of pattern matching in function heads.

If you still haven't grasped how recursion works, maybe reading this will help you.

Joking aside, recursion coupled with pattern matching is sometimes an optimal solution to the problem of writing concise algorithms that are easy to understand. By subdividing each part of a problem into separate functions until they can no longer be simplified, the algorithm becomes nothing but assembling a bunch of correct answers coming from short routines (that's a bit similar to what we did with quicksort). This kind of mental abstraction is also possible with your everyday loops, but I believe the practice is easier with recursion. Your mileage may vary.

And now ladies and gentlemen, a discussion: the author vs. himself

— Okay, I think I understand recursion. I get the declarative aspect of it. I get it has mathematical roots, like with invariable variables. I get that you find it easier in some cases. What else?
— It respects a regular pattern. Find the base cases, write them down, then every other cases should try to converge to these base cases to get your answer. It makes writing functions pretty easy.
— Yeah, I got that, you repeated it a bunch of times already. My loops can do the same.
— Yes they can. Can't deny that!
— Right. A thing I don't get is why you bothered writing all these non-tail recursive versions if they're not as good as tail recursive ones.
— Oh it's simply to make things easier to grasp. Moving from regular recursion, which is prettier and easier to understand, to tail recursion, which is theoretically more efficient, sounded like a good way to show all options.
— Right, so they're useless except for educational purposes, I get it.
— Not exactly. In practice you'll see little difference in the performance between tail recursive and normal recursive calls. The areas to take care of are in functions that are supposed to loop infinitely, like main loops. There's also a type of functions that will always generate very large stacks, be slow and possibly crash early if you don't make them tail recursive. The best example of this is the Fibonacci function, which grows exponentially if it's not iterative or tail recursive. Function calls expanded to create the sequence '0,1,1,2,3,5,8...' You should profile your code (I'll show how to do that at a later point, I promise), see what slows it down, and fix it.
— But loops are always iterative and make this a non-issue.
— Yes, but... but... my beautiful Erlang...
— Well isn't that great? All that learning because there is no 'while' or 'for' in Erlang. Thank you very much I'm going back to programming my toaster in C!
— Not so fast there! Functional programming languages have other assets! If we've found some base case patterns to make our life easier when writing recursive functions, a bunch of smart people have found many more to the point where you will need to write very few recursive functions yourself. If you stay around, I'll show you how such abstractions can be built. But for this we will need more power. Let me tell you about higher order functions...

![](/images/ch4/fib.png)