Higher Order Functions
===
Let's get functional
---
![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch5/lambda.png?raw=true)

An important part of all functional programming languages is the ability to take a function you defined and then pass it as a parameter to another function. This in turn binds that function parameter to a variable which can be used like any other variable within the function. A function that can accept other functions transported around that way is named a higher order function. Higher order functions are a powerful means of abstraction and one of the best tools to master in Erlang.

所有函数式编程语言很重要的一部分是接受自定义函数，并把它作为参数传递给其他函数的能力。这个过程会绑定函数参数到一个变量，并可以像该函数中其他变量一样使用。一个函数接受其他函数作为输入参数， 该函数被称为高阶函数。高阶函数是很强大的抽象方法， 同时也是Erlang中非常强大的工具之一。

Again, this a concept rooted in mathematics, mainly lambda calculus. I won't go into much detail about lambda calculus because some people have a hard time grasping it and it's a bit out of scope. However, I'll define it briefly as a system where everything is a function, even numbers. Because everything is a function, functions must accept other functions as parameters and can operate on them with even more functions!

此外， 来源于数学的概念， λ演算。我不会探究太多关于λ演算的细节， 因为有读者可能需要花费很多时间才能理解它，而且它也超出来我们的范围。 然而，我将简短地把它定义为一个系统，任何东西都是函数，甚至是数字也是函数的系统。由于任何东西都是函数， 所以函数必须接受其他函数作为参数，并且和更多函数一起工作。

Alright, this might be a little bit weird, so let's start with an example:

好吧， 这听起来有些不可思议，让我们来看一个例子：

```
-module(hhfuns).
-compile(export_all).
 
one() -> 1.
two() -> 2.
 
add(X,Y) -> X() + Y().
```

Now open the Erlang shell, compile the module and get going:

打开Erlang shell， 编译模块，开始下面的输入：

```
1> c(hhfuns).
{ok, hhfuns}
2> hhfuns:add(one,two).
    ** exception error: bad function one
        in function  hhfuns:add/2
3> hhfuns:add(1,2).
    ** exception error: bad function 1
        in function  hhfuns:add/2
4> hhfuns:add(fun hhfuns:one/0, fun hhfuns:two/0).
3
```

Confusing? Not so much, once you know how it works (isn't that always the case?) In command 2, the atoms one and two are passed to add/2, which then uses both atoms as function names (X() + Y()). If function names are written without a parameter list then those names are interpreted as atoms, and atoms can not be functions, so the call fails. This is the reason why expression 3 also fails: the values 1 and 2 can not be called as functions either, and functions are what we need!

困惑？一旦你知道第二条命令是如何工作，你就不会再困惑了。在第二条命令里， 原子one 和two 被传递给add/2函数，然后这个两个原子都被作为函数名进行调用(X() + Y())。如果函数名没有写参数列表，那么这些名字被解释为原子，原子不能作为函数，所以调用失败。第三条命令调用失败的原因是：数值1和2同样不能作为函数来调用， 所以函数才是我们所需要的。

This is why a new notation has to be added to the language in order to let you pass functions from outside a module. This is what fun Module:Function/Arity is: it tells the VM to use that specific function, and then bind it to a variable.

为了让你能从模块之外传递函数，所以一个新的标记被加入erlang中。就是fun Module:Function/Arity, 它告诉VM使用那个函数，并绑定到一个变量。

So what are the gains of using functions in that manner? Well a little example might be needed in order to understand it. We'll add a few functions to hhfuns that work recursively over a list to add or subtract one from each integer of a list:

我们能从这种使用函数的方式中获得什么？为了理解它，我们需要一个小小的例子。我们向hhfuns模块中添加几个函数，用于递归地对一个整数列表中的每个元素进行加1或减1操作。

```
increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].
 
decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].
```

See how similar these functions are? They basically do the same thing: they cycle through a list, apply a function on each element (+ or -) and then call themselves again. There is almost nothing changing in that code: only the applied function and the recursive call are different. The core of a recursive call on a list like that is always the same. We'll abstract all the similar parts in a single function (map/2) that will take another function as an argument:

这些函数是不是如此相似？他们基本完成同样的事情：他们以此遍历整个列表， 对每个元素应用一个函数(+ 或 -)，然后调用自己。两段代码基本上没有什么变化：除应用的函数和递归调用不同意外。像那样递归遍历列表的代码看上总是一样的。 我们将会抽象所有相似的部分到一个单独函数(map/2)，它接受另外一个函数作为参数：

```
map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].
 
incr(X) -> X + 1.
decr(X) -> X - 1.
```

Which can then be tested in the shell:


```
1> c(hhfuns).
{ok, hhfuns}
2> L = [1,2,3,4,5].
[1,2,3,4,5]
3> hhfuns:increment(L).
[2,3,4,5,6]
4> hhfuns:decrement(L).
[0,1,2,3,4]
5> hhfuns:map(fun hhfuns:incr/1, L).
[2,3,4,5,6]
6> hhfuns:map(fun hhfuns:decr/1, L).
[0,1,2,3,4]
```

Here the results are the same, but you have just created a very smart abstraction! Every time you will want to apply a function to each element of a list, you only have to call map/2 with your function as a parameter. However, it is a bit annoying to have to put every function we want to pass as a parameter to map/2 in a module, name it, export it, then compile it, etc. In fact it's plainly unpractical. What we need are functions that can be declared on the fly...

虽然两种方式的结果是一样，但是你已经创建了一个非常智能的抽象！任何时候，你想对列表中每个元素应用某个函数，你只要使用你的函数作为参数调用map/2.  但是， 让人讨厌的是必须把要作为参数传递的函数放入一个模块中，为他命名，导出它，编译它等等。 实际上，这根本是不切实际的。我们所需要的函数可以很简单的被定义。

Anonymous functions
---

Anonymous functions, or funs, address that problem by letting you declare a special kind of function inline, without naming them. They can do pretty much everything normal functions can do, except calling themselves recursively (how could they do it if they are anonymous?) Their syntax is:

匿名函数或者函数，通过让你声明一种特殊的不用命名的内联函数来解决上面提到的问题。他们几乎可以完成正常函数所能完成的所有事情，除了递归调用自己以外(如果他们是匿名的，他们如何能调用自己呢？)，他们语法：

```
fun(Args1) ->
        Expression1, Exp2, ..., ExpN;
    (Args2) ->
        Expression1, Exp2, ..., ExpN;
    (Args3) ->
        Expression1, Exp2, ..., ExpN
    end
```

And can be used the following way:

```
7> Fn = fun() -> a end.
#Fun<erl_eval.20.67289768>
8> Fn().
a
9> hhfuns:map(fun(X) -> X + 1 end, L).
[2,3,4,5,6]
10> hhfuns:map(fun(X) -> X - 1 end, L).
[0,1,2,3,4]
```

And now you're seeing one of the things that make people like functional programming so much: the ability to make abstractions on a very low level of code. Basic concepts such as looping can thus be ignored, letting you focus on what is done rather than how to do it.

你现在已经看到让人们如此喜欢函数式编程的事情：在很低的级别，对代码做抽象的能力。 例如循环这样的基本概念可以被忽略，让你关注是做些什么,而不是如何去做。

Anonymous functions are already pretty dandy for such abstractions but they still have more hidden powers:

匿名函数已经是相当高级的抽象，但他们仍然有更多隐藏的能力。

```
11> PrepareAlarm = fun(Room) ->
11>                     io:format("Alarm set in ~s.~n",[Room]),
11>                     fun() -> io:format("Alarm tripped in ~s! Call Batman!~n",[Room]) end
11>                   end.
#Fun<erl_eval.20.67289768>
12> AlarmReady = PrepareAlarm("bathroom").
Alarm set in bathroom.
#Fun<erl_eval.6.13229925>
13> AlarmReady().
Alarm tripped in bathroom! Call Batman!
ok
```

![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch5/batman.png?raw=true)

Hold the phone Batman! What's going on here? Well, first of all, we declare an anonymous function assigned to PrepareAlarm. This function has not run yet: it only gets executed when PrepareAlarm("bathroom"). is called. Batman with a manly mustache At that point, the call to io:format/2 is evaluated and the "Alarm set" text is output. The second expression (another anonymous function) is returned to the caller and then assigned to AlarmReady. Note that in this function, the variable Room's value is taken from the 'parent' function (PrepareAlarm). This is related to a concept called closures.

“蝙蝠侠快接电话！” 到底发生什么事了？好的， 首先， 我们声明了一个匿名函数，并赋值给PrepareAlarm变量。该函数还还有运行: 当调用PrepareAlarm("bothroom")该函数才会执行。 当执行PrepareAlarm函数是， io:format/2被求值，并且"Alarm set"文本被输出。 第二个表达式(另外一个匿名函数)被返回给调用者， 并且赋值给AlarmReady变量。注意，在这个函数中，变量Room的值是从父函数(PrepareAlarm)中获取的。这个被称为闭包。

To understand closures, one must first understand scope. A function's scope can be imagined as the place where all the variables and their values are stored. In the function base(A) -> B = A + 1., A and B are both defined to be part of base/1's scope. This means that anywhere inside base/1, you can refer to A and B and expect a value to be bound to them. And when I say 'anywhere', I ain't kidding, kid; this includes anonymous functions too:

为了理解闭包，首先必须理解作用域。一个函数的作用域可以被想象为保存所有变量及其变量值的地方。 在函数`base(A) -> B = A + 1.`中， 变量A和变量B都被定义在base/1的作用域中。这意味着在`base/1`函数中的任何地方， 你都可以引用变量A和变量B，并获取绑定到他们的值。我说"在任何地方"，不是在开玩笑；在匿名函数中同样可以访问:

```
base(A) ->
    B = A + 1,
    F = fun() -> A * B end,
    F().
```

B and A are still bound to base/1's scope, so the function F can still access them. This is because F inherits base/1's scope. Like most kinds of real-life inheritance, the parents can't get what the children have:

变量B和变量A始终绑定到`base/1`的作用域，所以函数F也可以访问他们。 这是因为函数F继承了`base/1`的作用域。像所有类型的现实继承一样， 父类不能获取子类所拥有的:

```
base(A) ->
    B = A + 1,
    F = fun() -> C = A * B end,
    F(),
    C.
```

In this version of the function, B is still equal to A + 1 and F will still execute fine. However, the variable C is only in the scope of the anonymous function in F. When base/1 tries to access C's value on the last line, it only finds an unbound variable. In fact, had you tried to compile this function, the compiler would have thrown a fit. Inheritance only goes one way.

在这个版本的函数中， 变量B始终等于变量A + 1，并且函数F始终执行良好。 然而， 变量C只包含在匿名函数F的作用域中。当函数`base/1`在最后一行代码中尝试访问变量C的值是，只找到了一个未绑定的变量。事实上，如果你尝试编译该函数，编译goes one way器会发出警告。继承是单向的。

It is important to note that the inherited scope follows the anonymous function wherever it is, even when it is passed to another function:

注意：继承的作用域会和匿名函数关联，即使当匿名函数被传递给其他函数:

```
a() ->
    Secret = "pony",
    fun() -> Secret end.
 
b(F) ->
    "a/0's password is "++F().
```

Then if we compile it:
如果我们编译它：

```
14> c(hhfuns).
{ok, hhfuns}
15> hhfuns:b(hhfuns:a()).
"a/0's password is pony"
```

Who told a/0's password? Well, a/0 did. While the anonymous function has a/0's scope when it's declared in there, it can still carry it when executed in b/1, as explained above. This is very useful because it lets us carry around parameters and content out of its original context, where the whole context itself are not needed anymore (exactly like we did with Batman in a previous example).

谁公布了`a/0`的密码？好的，是`a/0`自己公布的。 当匿名函数被定义是，它就拥有了`a/0`的作用域。正如上面解释的，直到该匿名函数在 `b/1`中执行时，它一直持有该作用域。这个非常有用，因为它可以让我们
把一些参数和内容搬离它原始的上下文，为什么只是有一些呢， 是因为匿名函数不需要整个上下文(实际上就像上一个蝙蝠侠的例子中展现的一样)。

You're most likely to use anonymous functions to carry state around when you have functions defined that take many arguments, but you have a constant one:

当你定义了一个接受多个参数的函数， 但是其中一个参数是一个常量，那么你很有可能利用匿名函数来保持状态：

```
16> math:pow(5,2).
25.0
17> Base = 2.
2
18> PowerOfTwo = fun(X) -> math:pow(Base,X) end.
#Fun<erl_eval.6.13229925>
17> hhfuns:map(PowerOfTwo, [1,2,3,4]).
[2.0,4.0,8.0,16.0]
```

By wrapping the call to math:pow/2 inside an anonymous function with the Base variable bound in its scope, we made it possible to have each of the calls to PowerOfTwo in hhfuns:map/2 use the integers from the list as the exponents of our base.

*need to adjust*
通过将对`math:pow/2`的调用和Base变量的绑定包装在匿名函数中，我就可以使用列表中的元素来对Base变量做幂计算。

A little trap you might fall into when writing anonymous functions is when you try to redefine the scope:

当你编写一个尝试重定义作用域的匿名函数时， 你很有可能掉入一个陷阱：

```
base() ->
    A = 1,
    (fun() -> A = 2 end)().
```

This will declare an anonymous function and then run it. As the anonymous function inherits base/0's scope, trying to use the = operator compares 2 with the variable A (bound to 1). This is guaranteed to fail. However it is possible to redefine the variable if it's done in the nested function's head:

上面的代码表示要声明一个匿名函数，紧接着运行它。由于匿名函数继承了`base/0`的作用域， 尝试使用`=`操作符比较数值2和变量A(绑定了数值1)。它一定会失败。然而是有可能重定义变量的，只是需要在内嵌的函数头中完成：

```
base() ->
    A = 1,
    (fun(A) -> A = 2 end)(2).
```

And this works. If you try to compile it, you'll get a warning about shadowing ("Warning: variable 'A' shadowed in 'fun'"). Shadowing is the term used to describe the act of defining a new variable that has the same name as one that was in the parent scope. This is there to prevent some mistakes (usually rightly so), so you might want to consider renaming your variables in these circumstances.

这样就可以重定义变量，如果你尝试编译它， 你会收到一条关于shadowing的警告("Warning: variable 'A' shadowed in 'fun'"). 
shadowing是用于描述一种行为的词汇，这种行为是定义了一个新的变量，但是这个变量和父作用域的变量拥有相同的名字。这是为了防止一些错误，所以你应该考虑在这种场景下重命名你的变量。


Update:
Starting with version 17.0, the language supports using anonymous functions with an internal name. That's right, anonymous but named functions.

从opt 17.0开始，erlang支持使用一个内部名字来命名匿名函数，没错，匿名却有名字的函数。

The trick is that the name is visible only within the function's scope, not outside of it. The main advantage of this is that it makes it possible to define anonymous recursive functions. For example, we could make an anonymous function that keeps being loud forever:

窍门是函数名只在函数作用域内可见， 在函数作用域就不可见。主要的好处是使定义匿名递归函数成为可能。你可以顶一个永远运行的匿名函数：

```
18> f(PrepareAlarm), f(AlarmReady).
ok
19> PrepareAlarm = fun(Room) ->
19>    io:format("Alarm set in ~s.~n",[Room]),
19>     fun Loop() ->
19>        io:format("Alarm tripped in ~s! Call Batman!~n",[Room]),
19>        timer:sleep(500),
19>         Loop()
19>     end
19> end.
#Fun<erl_eval.6.71889879>
20> AlarmReady = PrepareAlarm("bathroom").
Alarm set in bathroom.
#Fun<erl_eval.44.71889879>
21> AlarmReady().
Alarm tripped in bathroom! Call Batman!
Alarm tripped in bathroom! Call Batman!
Alarm tripped in bathroom! Call Batman!
...
```

The Loop variable refers to the anonymous function itself, and within that scope, will be usable as any other similar variable pointing to an anonymous function. This should generally make a lot of operations in the shell a lot less painful moving on forward.

变量Loop引用匿名函数本身，并在该作用域中，同其他类似的变量，Loop指向匿名函数。这将减少很多shell的操作。

We'll set the anonymous function theory aside a bit and we'll explore more common abstractions to avoid having to write more recursive functions, like I promised at the end of the previous chapter.

把匿名函数理论放置一边，我们将浏览更多常见的抽象，以避免编写更多的递归函数， 就像我在上周结尾是做出的承诺。

Maps, filters, folds and more
---
![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch5/erland.png?raw=true)

At the beginning of this chapter, I briefly showed how to abstract away two similar functions to get a map/2 function. I also affirmed that such a function could be used for any list where we want to act on each element. The function was the following:

在本章开始， 我简单地展示了如何抽象两个类似的函数为map/2函数。 我可以保证这样一个函数可以使用任何一个列表。函数如下：

```
map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].
```

However, there are many other similar abstractions to build from commonly occurring recursive functions. Let's first take a look at these two functions:

然而， 可以从常见递归函数构建更多的类似抽象函数。让我们先看看这两个函数：

```
%% only keep even numbers
even(L) -> lists:reverse(even(L,[])).
 
even([], Acc) -> Acc;
even([H|T], Acc) when H rem 2 == 0 ->
    even(T, [H|Acc]);
even([_|T], Acc) ->
    even(T, Acc).
 
%% only keep men older than 60
old_men(L) -> lists:reverse(old_men(L,[])).
 
old_men([], Acc) -> Acc;
old_men([Person = {male, Age}|People], Acc) when Age > 60 ->
    old_men(People, [Person|Acc]);
old_men([_|People], Acc) ->
    old_men(People, Acc).
```

The first one takes a list of numbers and returns only those that are even. The second one goes through a list of people of the form {Gender, Age} and only keeps those that are males over 60. The similarities are a bit harder to find here, but we've got some common points. Both functions operate on lists and have the same objective of keeping elements that succeed some test (also a predicate) and then drop the others. From this generalization we can extract all the useful information we need and abstract them away:

第一个函数接受一个整型列表， 只返回所有的偶数。第二个函数遍历由{Gender, Age}元组组成的列表，并只返回大于60的女性记录。也许相似之处很难发现，但是我们会发现一些共同之处。两个函数都是操作一个列表，留下那些条件测试成功的元素， 丢弃其他的。从这个概述中我们提取出我们需要的有用信息，并抽象他们：

```
filter(Pred, L) -> lists:reverse(filter(Pred, L,[])).
 
filter(_, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
    case Pred(H) of
        true  -> filter(Pred, T, [H|Acc]);
        false -> filter(Pred, T, Acc)
    end.
```

To use the filtering function we now only need to get the test outside of the function. Compile the hhfuns module and try it:

通过使用过滤函数，我现在只需要在函数外部做条件测试。 编译这hhfuns模块并测试它：

```
1> c(hhfuns).
{ok, hhfuns}
2> Numbers = lists:seq(1,10).
[1,2,3,4,5,6,7,8,9,10]
3> hhfuns:filter(fun(X) -> X rem 2 == 0 end, Numbers).
[2,4,6,8,10]
4> People = [{male,45},{female,67},{male,66},{female,12},{unknown,174},{male,74}].
[{male,45},{female,67},{male,66},{female,12},{unknown,174},{male,74}]
5> hhfuns:filter(fun({Gender,Age}) -> Gender == male andalso Age > 60 end, People).
[{male,66},{male,74}]
```

These two examples show that with the use of the filter/2 function, the programmer only has to worry about producing the predicate and the list. The act of cycling through the list to throw out unwanted items is no longer necessary to think about. This is one important thing about abstracting functional code: try to get rid of what's always the same and let the programmer supply in the parts that change.

这两个例子展示了通过使用`filter/2`函数，程序员仅仅需要关心如何产生谓词函数和列表。遍历整个并丢弃不需要的元素的行为已经不再需要考虑了。抽象功能代码最重要的事情是：尝试剔除所有不变的地方，只让程序员编程变换的部分。

In the previous chapter, another kind of recursive manipulation we applied on lists was to look at every element of a list one after the other and reduce them to a single answer. This is called a fold and can be used on the following functions:

在前一章中， 我们应用在列表的另外一类递归操作是，依次遍历列表中的每个元素，减少到一个元素。这个过程称为fold， 可以在下面函数中使用：

```
%% find the maximum of a list
max([H|T]) -> max2(T, H).
 
max2([], Max) -> Max;
max2([H|T], Max) when H > Max -> max2(T, H);
max2([_|T], Max) -> max2(T, Max).
 
%% find the minimum of a list
min([H|T]) -> min2(T,H).
 
min2([], Min) -> Min;
min2([H|T], Min) when H < Min -> min2(T,H);
min2([_|T], Min) -> min2(T, Min).
 
%% sum of all the elements of a list
sum(L) -> sum(L,0).

sum([], Sum) -> Sum;
sum([H|T], Sum) -> sum(T, H+Sum).
```

To find how the fold should behave, we've got to find all the common points of these actions and then what is different. As mentioned above, we always have a reduction from a list to a single value. Consequently, our fold should only consider iterating while keeping a single item, no list-building needed. Then we need to ignore the guards, because they're not always there: these need to be in the user's function. In this regard, our folding function will probably look a lot like sum.

为了找出`fold`是如何运作的，我们不得不找出这些动作的所有共同点，然后确定他们不同之处。正如上文提到的， 我们总是从列表缩减为单值。因此，我们fold过程只需要考虑进行迭代时如何持有一个单值，而不是构建一个列表。我们需要忽略所有的Guard， 因为他们不总是需要：他们应该包含在用户定义的函数中。就这样一点而言，我们的fold函数很像是求和运算。

![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch5/foldr.png?raw=true)

A subtle element of all three functions that wasn't mentioned yet is that every function needs to have an initial value to start counting with. In the case of sum/2, we use 0 as we're doing addition and given X = X + 0, the value is neutral and we can't mess up the calculation by starting there. If we were doing multiplication we'd use 1 given X = X * 1. The functions min/1 and max/1 can't have a default starting value: if the list was only negative numbers and we started at 0, the answer would be wrong. As such, we need to use the first element of the list as a starting point. Sadly, we can't always decide this way, so we'll leave that decision to the programmer. By taking all these elements, we can build the following abstraction:

这三个函数另外一个共同之处没有提到，那就是每个函数同需要一个初始值。在`sum/2`中， 我们使用数值0来参与加法计算，数值0是中性的，使用它我们不会污染计算结果。如果是乘法计算，我们就使用数值1作为初始值。`min/1`和`max/1`函数不需要一个默认值：如果列表中只包含负数，并使用数值0作为起始值，那么这个结果就不正确了。所以，我们需要使用列表首元素作为起始点。很遗憾，我们不是总能通过这种方式确定初始值，所以我们把这个判断留给程序自己确定。通过总结这些所有要素，我们可以构建如下抽象：

```
fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(H,Start), T).
```

And when tried:

```
6> c(hhfuns).
{ok, hhfuns}
7> [H|T] = [1,7,3,5,9,0,2,3].   
[1,7,3,5,9,0,2,3]
8> hhfuns:fold(fun(A,B) when A > B -> A; (_,B) -> B end, H, T).
9
9> hhfuns:fold(fun(A,B) when A < B -> A; (_,B) -> B end, H, T).
0
10> hhfuns:fold(fun(A,B) -> A + B end, 0, lists:seq(1,6)).
21
```

Pretty much any function you can think of that reduces lists to 1 element can be expressed as a fold.

几乎任何将列表归纳为单值的函数都可以表示为fold。

What's funny there is that you can represent an accumulator as a single element (or a single variable), and an accumulator can be a list. Therefore, we can use a fold to build a list. This means fold is universal in the sense that you can implement pretty much any other recursive function on lists with a fold, even map and filter:

有趣的是你可以把累加器看作是一个单值，也可以看作是一个列表。因此，我们可以使用fold来构建列表。这就意味着在这类场景中fold是通用的， 你可以使用fold来实现作用于列表的几乎任何其他递归函数，甚至包括map 和 filter:

```
reverse(L) ->
    fold(fun(X,Acc) -> [X|Acc] end, [], L).
 
map2(F,L) ->
    reverse(fold(fun(X,Acc) -> [F(X)|Acc] end, [], L)).
 
filter2(Pred, L) ->
    F = fun(X,Acc) ->
        case Pred(X) of
            true  -> [X|Acc];
            false -> Acc
        end
    end,
    reverse(fold(F, [], L)).
```

And they all work the same as those written by hand before. How's that for powerful abstractions?

和前面的实现一样，他们工作良好。

Map, filters and folds are only one of many abstractions over lists provided by the Erlang standard library (see lists:map/2, lists:filter/2, lists:foldl/3 and lists:foldr/3). Other functions include all/2 and any/2 which both take a predicate and test if all the elements return true or if at least one of them returns true, respectively. Then you have dropwhile/2 that will ignore elements of a list until it finds one that fit a certain predicate, its opposite, takewhile/2, that will keep all elements until there is one that doesn't return true to the predicate. A complimentary function to the two previous ones is partition/2, which will take a list and return two: one that has the terms which satisfy a given predicate, and one list for the others. Other frequently used lists functions include flatten/1, flatlength/1, flatmap/2, merge/1, nth/2, nthtail/2, split/2 and a bunch of others.

map, filter和folds仅仅是Erlang标准库提供的适用于列表的众多抽象之一(`lists:map/2`, `lists:filter/2`, `lists:foldl/3` 和 `lists:foldr/3`)。还包括`all/2`和`any/2`，他们都是接受一个谓词，如果列表中每个元素都通过测试， `all/2`就返回true，或者如果至少有一个元素通过测试，`any/2`就返回true。`dropwhile/2`函数会丢弃所有元素，直到找到满足谓词条件的，相反地， `tablewhile/2`保留所有元素，直到遇到不满足谓词条件的元素。`partition/2`函数会接受一个列表，然后返回两个列表：其中一个只包含满足谓词的元素，另外一个包含剩下的。其他经常使用的函数包括`flatten/1`, `flatlength/1`, `flatmap/2`, `merge/1`, `nth/2`, `nthtail/2`, `split/2`等等。

You'll also find other functions such as zippers (as seen in last chapter), unzippers, combinations of maps and folds, etc. I encourage you to read the documentation on lists to see what can be done. You'll find yourself rarely needing to write recursive functions by using what's already been abstracted away by smart people.

你将会发现其他函数，例如zippers(见最后一章), unzippers, 结合maps和folds的等等。我鼓励你阅读关于lists模块的文档。你会发现你很少需要编写递归函数， 完全可以使用一些聪明的家伙已经抽象的函数。