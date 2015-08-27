Syntax in functions
===
Pattern Matching
---
![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch2/snail.png?raw=true)

Now that we have the ability to store and compile our code, we can begin to write more advanced functions. Those that we have written so far were extremely simple and a bit underwhelming. We'll get to more interesting stuff. The first function we'll write will need to greet someone differently according to gender. In most languages you would need to write something similar to this:

既然我们已经能保存和编译代码，那么我就 可以编写更多 高级函数。到目前 为止， 我们编写的函数都非常简单，有点平淡无奇。我们会接触更多有意思的东西。 我将要编写的第一个函数需要根据性别来进行不同欢迎。在大多数语言中， 你可能会编写出和下列代码类似的代码：

```
function greet(Gender,Name)
    if Gender == male then
        print("Hello, Mr. %s!", Name)
    else if Gender == female then
        print("Hello, Mrs. %s!", Name)
    else
        print("Hello, %s!", Name)
end
```

With pattern-matching, Erlang saves you a whole lot of boilerplate code. A similar function in Erlang would look like this:

使用模式匹配， Erlang减少很多代码。 一个类似的函数在Erlang看上去会像这样：

```
greet(male, Name) ->
    io:format("Hello, Mr. ~s!", [Name]);
greet(female, Name) ->
    io:format("Hello, Mrs. ~s!", [Name]);
greet(_, Name) ->
    io:format("Hello, ~s!", [Name]).
```

I'll admit that the printing function is a lot uglier in Erlang than in many other languages, but that is not the point. The main difference here is that we used pattern matching to define both what parts of a function should be used and bind the values we need at the same time. There was no need to first bind the values and then compare them! So instead of:

我必须承认Erlang中的答应函数比其他语言中的更难看一些，但是这不是重点。这里主要不同之处是：我们使用了模式匹配定义了那个函数分支会被使用，同时绑定我们需要的值。所以我们不需要绑定这些，然后再比较它们！ 可以替换下列代码：

```
function(Args)
    if X then
        Expression
    else if Y then
        Expression
    else
        Expression
```

We write:

```
function(X) ->
    Expression;
function(Y) ->
    Expression;
function(_) ->
    Expression.
```

in order to get similar results, but in a much more declarative style. Each of these function declarations is called a function clause. Function clauses must be separated by semicolons (;) and together form a function declaration. A function declaration counts as one larger statement, and it's why the final function clause ends with a period. It's a "funny" use of tokens to determine workflow, but you'll get used to it. At least you'd better hope so because there's no way out of it!

为了得到类似的结果，却采用了更多声明的编程方式。每一个了函数声明被称为一个函数子句(function clause)。 函数子句之间使用分号分隔，合在一起被称为函数声明。函数声明被视为一个大语句， 这就为什么最后一个函数子句以句号结尾的原因。这是一个使用令牌来决定工作流的有趣的应用，但是你要适应使用它。最后你最要能适应，因为没有其他方法避开他。

Note: io:format's formatting is done with the help of tokens being replaced in a string. The character used to denote a token is the tilde (~). Some tokens are built-in such as ~n, which will be changed to a line-break. Most other tokens denote a way to format data. The function call io:format("~s!~n",["Hello"]). includes the token ~s, which accepts strings and bitstrings as arguments, and ~n. The final output message would thus be "Hello!\n". Another widely used token is ~p, which will print an Erlang term in a nice way (adding in indentation and everything).

注意：io:format 是利用字符串替换占位符来完成格式化。使用~表示一个占位符。有些占位符是内置的， 例如~n， 他会被替换为换行。其他的占位符都是表示格式化数据的方式。Io:format("~s!~n", ["hello"]). 包含一个~s占位符 ， 他表示接受一个字符串或者位串为参数， 还有一个~n占位符。最终的输出信息将会是"hello!\n"。另外一个广泛使用的是~p， 它会以一种不错的方式打印Erlang 项元(添加必要的缩进)。

The io:format function will be seen in more details in later chapters dealing with input/output with more depth, but in the meantime you can try the following calls to see what they do: `io:format("~s~n",[<<"Hello">>])`, `io:format("~p~n",[<<"Hello">>])`, `io:format("~~~n")`, `io:format("~f~n", [4.0])`, `io:format("~30f~n", [4.0])`. They're a small part of all that's possible and all in all they look a bit like printf in many other languages. If you can't wait until the chapter about I/O, you can read the online documentation to know more.

我们会在后面的章节中了解关于io:format函数处理输入/输出的更多信息， 但是你也可以在运行时中执行下列代码，并观察他的结果: `io:format("~s~n",[<<"Hello">>]), io:format("~p~n",[<<"Hello">>]), io:format("~~~n"), io:format("~f~n", [4.0]), io:format("~30f~n", [4.0])`, 他们看上去很像其他语言中的printf。 你查看erlang文档了解更多关于io:format的信息。

Pattern matching in functions can get more complex and powerful than that. As you may or may not remember from a few chapters ago, we can pattern match on lists to get the heads and tails. Let's do this! Start a new module called functions in which we'll write a bunch of functions to explore many pattern matching avenues available to us:

在上面这些函数中的模式匹配可以更复杂和更强大。我们可以模式匹配列表， 获取表头和表尾。开始一个叫functions的新模块， 我们将编写多个函数用于探索对我们可用的模式匹配方法。

```
-module(functions).
-compile(export_all). %% replace with -export() later, for God's sake!
```

The first function we'll write is head/1, acting exactly like erlang:hd/1 which takes a list as an argument and returns its first element. It'll be done with the help of the cons operator (|):

第一个编写的函数head/1， 更erlang:hd/1函数的作用相同， 接受一个list作为参数，并返回第一个元素。我们会借助Cons operator(|)的帮助。

```
head([H|_]) -> H.
```

If you type functions:head([1,2,3,4]). in the shell (once the module is compiled), you can expect the value '1' to be given back to you. Consequently, to get the second element of a list you would create the function:

如果你在shell中输入function:head([1,2,3,4]).(当然模块被编译之后), 你可以预料到返回结果是1。因此， 你可以创建一个函数获取列表中的第二元素。

```
second([_,X|_]) -> X.
```

The list will just be deconstructed by Erlang in order to be pattern matched. Try it in the shell!

下面的例子只是为了用于演示而已。

```
1> c(functions).
{ok, functions}
2> functions:head([1,2,3,4]).
1
3> functions:second([1,2,3,4]).
2
```

This could be repeated for lists as long as you want, although it would be impractical to do it up to thousands of values. This can be fixed by writing recursive functions, which we'll see how to do later on. For now, let's concentrate on more pattern matching. The concept of free and bound variables we discussed in Starting Out (for real) still holds true for functions: we can then compare and know if two parameters passed to a function are the same or not. For this, we'll create a function same/2 that takes two arguments and tells if they're identical:

只要你愿意你可以按照上面的方法获取列表中的每个一个元素， 只是这样的方式对于拥有上千个元素列表有些不切实际。可以通过编写递归函数解决这个问题，我们会在后面看到该如何做。

```
same(X,X) ->
    true;
same(_,_) ->
    false.
```

And it's that simple. Before explaining how the function works, we'll go over the concept of bound and unbound variables again, just in case:

这个非常简单， 在解释函数怎么样工作以前， 我们会再一次复习绑定和未绑定变量的概念。

![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch2/musical-chair.png?raw=true)

If this game of musical chairs was Erlang, you'd want to sit on the empty chair. Sitting on one already occupied wouldn't end well! Joking aside, unbound variables are variables without any values attached to them (like our empty chair). Binding a variable is simply attaching a value to an unbound variable. In the case of Erlang, when you want to assign a value to a variable that is already bound, an error occurs unless the new value is the same as the old one. Let's imagine our snake on the right: if another snake comes around, it won't really change much to the game. You'll just have more angry snakes. If a different animal comes to sit on the chair (a honey badger, for example), things will go bad. Same values for a bound variable are fine, different ones are a bad idea. You can go back to the subchapter about Invariable Variables if this concept is not clear to you.

如果音乐椅游戏是Erlang，你需要坐在空椅子上。坐在一个已经被占领的椅子将不会有好结局。 不开玩笑了，未绑定变量就是没有任何值依附于他们(就像我们的空椅子一样)。绑定一个变量就是简单地附加一个值到未绑定变量。在Erlang中，当你想分配一个值到已经绑定值的变量上，除非新值和旧值一样，否者会发生错误。让我们想象一下右边的蛇：如果另外一条蛇出现， 这不会太多的改变游戏的局面。你只会拥有更多的愤怒的蛇。如果有另外的动物来坐在这个椅子上（例如一直蜜罐）， 那么事情将变得非常糟糕。相同的值对于一个已绑定变量非常友好，但是不同的值却是一个坏主意。 如果你不清楚变量的概念，可以回到前面关于不可变变量进行复习。

Back to our code: what happens when you call same(a,a) is that the first X is seen as unbound: it automatically takes the value a. Then when Erlang goes over to the second argument, it sees X is already bound. It then compares it to the a passed as the second argument and looks to see if it matches. The pattern matching succeeds and the function returns true. If the two values aren't the same, this will fail and go to the second function clause, which doesn't care about its arguments (when you're the last to choose, you can't be picky!) and will instead return false. Note that this function can effectively take any kind of argument whatsoever! It works for any type of data, not just lists or single variables. As a rather advanced example, the following function prints a date, but only if it is formatted correctly:

回到我们的代码： 当你调用same(a, a)函数时，第一个X被视为未绑定变量，它会自动绑定为a，然后当Erlang检查这个第二个变量时，然后X变量已经绑定值了。比较两个值然后查看是否匹配。这模式匹配成功， 函数返回true。 如果两个值不相等， 匹配失败，第二个函数子句被执行， 这个函数并不关心参数的内容， 然后返回false。这个函数高效地接受任意类型的参数！ 不管是lists还是一个变量值，对任何类型的数据函数都能很好工作。做为高阶的例子，但是只有在才格式正确的情况下，下列函数才打印时间。

```
valid_time({Date = {Y,M,D}, Time = {H,Min,S}}) ->
    io:format("The Date tuple (~p) says today is: ~p/~p/~p,~n",[Date,Y,M,D]),
    io:format("The time tuple (~p) indicates: ~p:~p:~p.~n", [Time,H,Min,S]);
valid_time(_) ->
    io:format("Stop feeding me wrong data!~n").
```

Note that it is possible to use the = operator in the function head, allowing us to match both the content inside a tuple ({Y,M,D}) and the tuple as a whole (Date). The function can be tested the following way:

在一个函数中那样， 可以用 = 操作符匹配整个整个tuple内容。 这个函数可以按照下列按时测试。

```
4> c(functions).
{ok, functions}
5> functions:valid_time({{2011,09,06},{09,04,43}}).
The Date tuple ({2011,9,6}) says today is: 2011/9/6,
The time tuple ({9,4,43}) indicates: 9:4:43.
ok
6> functions:valid_time({{2011,09,06},{09,04}}).
Stop feeding me wrong data!
ok
```

There is a problem though! This function could take anything for values, even text or atoms, as long as the tuples are of the form {{A,B,C}, {D,E,F}}. This denotes one of the limits of pattern matching: it can either specify really precise values such as a known number of atom, or abstract values such as the head|tail of a list, a tuple of N elements, or anything (_ and unbound variables), etc. To solve this problem, we use guards.

这有一个问题， 这个函数可以接受任意类型的值，不管是文本，原子，或者{{A, B, C}, {D, E, F}}形式的元组。这其实是模式匹配的一个限制：它就可以指定精确值， 例如数值原子， 或者抽象值，例如一个列表值， N个元素的元组，或者其他任何值（_ 和未绑定变量）。 为了解决这个问题， 我们可以用guards.

Guards, Guards!
---

![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch2/driving-age.png?raw=true)

Guards are additional clauses that can go in a function's head to make pattern matching more expressive. As mentioned above, pattern matching is somewhat limited as it cannot express things like a range of value or certain types of data. A concept we couldn't represent is counting: is this 12 years old basketball player too short to play with the pros? Is this distance too long to walk on your hands? Are you too old or too young to drive a car? You couldn't answer these with simple pattern matching. I mean, you could represent the driving question such as:

Guard是额外子句，可以放在函数的头部，使模式匹配更具表达性。正如上面提到的，模式匹配的限制在于，它不能表达区间值和特定数据类型的匹配。有一个概念是我们无法用模式匹配表达的， 这就是计数： 12岁的篮球选手参加俱乐部联会是不是太矮了？对于你来说，一步跨过相当手臂长度的距离是不是太长了？你是不是太老或者太年轻而无法开车？你不能用模式匹配回答这些问题。 我的意思是你不能像下面这样来回答开车的问题：

```
old_enough(0) -> false;
old_enough(1) -> false;
old_enough(2) -> false;
...
old_enough(14) -> false;
old_enough(15) -> false;
old_enough(_) -> true.
```

But it would be incredibly impractical. You can do it if you want, but you'll be alone to work on your code forever. If you want to eventually make friends, start a new guards module so we can type in the "correct" solution to the driving question:

但是这将是非常不切实际。 你确实可以这样做，但是你可以能会写非常多的代码。如果你想改变注意，那可以开始一个叫guards的新模块来正确解决开车的问题。

```
old_enough(X) when X >= 16 -> true;
old_enough(_) -> false.
```

And you're done! As you can see, this is much shorter and cleaner. Note that a basic rule for guard expression is they must return true to succeed. The guard will fail if it returns false or if it throws an exception. Suppose we now forbid people who are over 104 years old to drive. Our valid ages for drivers is now from 16 years old up to 104 years old. We need to take care of that, but how? Let's just add a second guard clause:

正如你所见的， 代码变得更短，也更清楚了。记住，对于guard表达式的一条基本规则就是它们必须返回true来表示成功。如果guard表达式返回false或者抛出一个异常，那它将会失败。假设现在我们禁止104岁以上的人开车。现在对于开车合法的年龄是从16岁到104岁。 我们应该怎么做呢， 那就让我们加入第二个guard分支。

```
right_age(X) when X >= 16, X =< 104 ->
    true;
right_age(_) ->
    false.
```

The comma (,) acts in a similar manner to the operator andalso and the semicolon (;) acts a bit like orelse (described in "Starting Out (for real)"). Both guard expressions need to succeed for the whole guard to pass. We could also represent the function the opposite way:

逗号的作用和andalso操作符一样， 分号和orelse的作用一样。每个子guard表达式都通过才能使整个guard表达式通过。我们可以像下列这样写代码。

```
wrong_age(X) when X < 16; X > 104 ->
    true;
wrong_age(_) ->
    false.
```

![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch2/guard.png?raw=true)

And we get correct results from that too. Test it if you want (you should always test stuff!). In guard expressions, the semi-colon (;) acts like the orelse operator: if the first guard fails, it then tries the second, and then the next one, until either one guard succeeds or they all fail.

我们可以得到同样的结果。 你可以自己测试一下(你应该总是测试文中提到的例子)。在guard表达式中，这个分号扮演着orelse操作符的角色：如果第一个guard失败，然后尝试第二个， 然后是下一个， 直到有一个guard表达式成功或者全部都失败。

You can use a few more functions than comparisons and boolean evaluation in functions, including math operations (A*B/C >= 0) and functions about data types, such as is_integer/1, is_atom/1, etc. (We'll get back on them in the following chapter). One negative point about guards is that they will not accept user-defined functions because of side effects. Erlang is not a purely functional programming language (like Haskell is) because it relies on side effects a lot: you can do I/O, send messages between actors or throw errors as you want and when you want. There is no trivial way to determine if a function you would use in a guard would or wouldn't print text or catch important errors every time it is tested over many function clauses. So instead, Erlang just doesn't trust you (and it may be right to do so!)

你可以适当的多一些函数，在函数中少一些比较和boolean运算，包括算术运算符(A*B/C >= 0)， 数据类型函数， 例如is_integer/1, is_atom/1等。我们会在接下来一章中从新启动他们。guard表达式的一个缺点是出于性能的考虑，他们不接受用户自定义函数。Erlang不是纯函数式编程语言（不像Haskell），因为它很依赖副作用：你可以进行IO， 在不同个actor之间发送消息， 根据你的需要抛出异常。因为没有有效的方式判断一个你在guard表达式中使用的函数会或者不会打印文本， 或者在任何时候捕获重要的错误，包括所有函数分支。所以， Erlang选择不相信你（或许你可能是正确的）。

That being said, you should be good enough to understand the basic syntax of guards to understand them when you encounter them.

话虽如此， 你也应该充分理解guard表示的基本语法。

Note: I've compared , and ; in guards to the operators andalso and orelse. They're not exactly the same, though. The former pair will catch exceptions as they happen while the latter won't. What this means is that if there is an error thrown in the first part of the guard X >= N; N >= 0, the second part can still be evaluated and the guard might succeed; if an error was thrown in the first part of X >= N orelse N >= 0, the second part will also be skipped and the whole guard will fail.

注意：我们把guard表达式中的"," 和";"比作andalso 和 orelse操作符。严格意义上他们不一样。前者会捕获异常，而后者这不会。这意味着如果在guard "X >= N; N >= 0"的第一部分中抛出异常， 那么第二部分任然会继续执行，并且guard会执行成功。如果guard表示"X >= N orelse N >= 0"的第一部分抛出异常，那么第二部分将会被跳过， 并且整个guard表达式将会失败。

However (there is always a 'however'), only andalso and orelse can be nested inside guards. This means (A orelse B) andalso C is a valid guard, while (A; B), C is not. Given their different use, the best strategy is often to mix them as necessary.

但是(总是有一个“但是”)， 只有andalso 和orelse才可以在guard表示中嵌套使用。这就意味着"(A orelse B) andaslo C"这样的表达式是合法的， 然后"(A; B), C"却不是合法。使用它们的最好策略是按照需要混合使用他们。

What the If!?
---

Ifs act like guards and share guards' syntax, but outside of a function clause's head. In fact, the if clauses are called Guard Patterns. Erlang's ifs are different from the ifs you'll ever encounter in most other languages; compared to them they're weird creatures that might have been more accepted had they had a different name. When entering Erlang's country, you should leave all you know about ifs at the door. Take a seat because we're going for a ride.

If表达式跟guard表达式的行为相似，语法也很相似， 但是他不在函数子句的头部定义。 实际上， if子句被称为guard模式。Erlang的if表示和你在其他语言中遇到的if语句有些不一样。和其他语言中的if语句比起来，他们非常奇怪， 如果他们拥有一个不同名字，或许才可以被人们接受。当你进入Erlang的世界，你就应该丢到所有关于if的认识。找好位置， 就下来的旅程将会很长。

To see how similar to guards the if expression is, look at the following examples:

为了了解guard表达式和if表示究竟有多相似， 看看下面的例子。

```
-module(what_the_if).
-export([heh_fine/0]).
 
 
heh_fine() ->
    if 1 =:= 1 ->
        works
    end,
    if 1 =:= 2; 1 =:= 1 ->
        works
    end,
    if 1 =:= 2, 1 =:= 1 ->
        fails
    end.
```

Save this as what_the_if.erl and let's try it:

```
1> c(what_the_if).
./what_the_if.erl:12: Warning: no clause will ever match
./what_the_if.erl:12: Warning: the guard for this clause evaluates to 'false'
{ok,what_the_if}
2> what_the_if:heh_fine().
** exception error: no true branch found when evaluating an if expression
        in function  what_the_if:heh_fine/0
```

Uh oh! the compiler is warning us that no clause from the if on line 12 (1 =:= 2, 1 =:= 1) will ever match because its only guard evaluates to false. Remember, in Erlang, everything has to return something, and if expressions are no exception to the rule. As such, when Erlang can't find a way to have a guard succeed, it will crash: it cannot not return something. As such, we need to add a catch-all branch that will always succeed no matter what. In most languages, this would be called an 'else'. In Erlang, we use 'true' (this explains why the VM has thrown "no true branch found" when it got mad):

编译器警告我们， if语句没有一个子句被匹配， 因为唯一的guard表示返回false。 记住， 在Erlang中， 所有东西都有返回值， if表达式也不例外。 例如， 当Erlang没有找到一条guard成功的路径，那么它就会崩溃：因为它不能返回一些值。正因为如此， 我们需要添加一个捕获全部的分支， 在任何情况下都可以执行。 在大多数语言中， 这被成为"else"。 在erlang中， 我用'true'（这也解释了为什么虚拟机抛出一个“no true branch found”）。

![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch2/labyrinth.png?raw=true)

```
oh_god(N) ->
    if N =:= 2 -> might_succeed;
        true -> always_does  %% this is Erlang's if's 'else!'
    end.
```

And now if we test this new function (the old one will keep spitting warnings, ignore them or take them as a reminder of what not to do):

如果我们测试这个新的函数（这旧的函数依然会抛出警告， 忽略它，或者把他们当作一个提醒）

```
3> c(what_the_if).
./what_the_if.erl:12: Warning: no clause will ever match
./what_the_if.erl:12: Warning: the guard for this clause evaluates to 'false'
{ok,what_the_if}
4> what_the_if:oh_god(2).
might_succeed
5> what_the_if:oh_god(3).
always_does
```

Here's another function showing how to use many guards in an if expression. The function also illustrates how any expression must return something: Talk has the result of the if expression bound to it, and is then concatenated in a string, inside a tuple. When reading the code, it's easy to see how the lack of a true branch would mess things up, considering Erlang has no such thing as a null value (ie.: lisp's nil, C's NULL, Python's None, etc):

这里有另外一个展示如何在if表达式中使用多个guard表达式。这个例子展示了任何表达式必须返回一些值：变量Talk绑定了if表达式的结果， 然后在元祖中拼接成一个字符串。当阅读代码时，非常容易就会发现，如果没有true分支，事情将变得多糟糕， erlang没有null 值（例如lisp 的nil， C语言的NULL， Python的None）。

%% note, this one would be better as a pattern match in function heads!
%% I'm doing it this way for the sake of the example.

```
help_me(Animal) ->
    Talk = if Animal == cat  -> "meow";
        Animal == beef -> "mooo";
        Animal == dog  -> "bark";
        Animal == tree -> "bark";
        true -> "fgdadfgna"
    end,
    {Animal, "says " ++ Talk ++ "!"}.
```

And now we try it:

```
6> c(what_the_if).
./what_the_if.erl:12: Warning: no clause will ever match
./what_the_if.erl:12: Warning: the guard for this clause evaluates to 'false'
{ok,what_the_if}
7> what_the_if:help_me(dog).
{dog,"says bark!"}
8> what_the_if:help_me("it hurts!").
{"it hurts!","says fgdadfgna!"}
```

You might be one of the many Erlang programmers wondering why 'true' was taken over 'else' as an atom to control flow; after all, it's much more familiar. Richard O'Keefe gave the following answer on the Erlang mailing lists. I'm quoting it directly because I couldn't have put it better:

你也许会惊讶为什么会用来true代替else用于控制流程。Richard O'Keefe在Erlang邮件列表中给出了答案。我直接引用了，因为我不能给出更好的回答。

It may be more FAMILIAR, but that doesn't mean 'else' is a good thing. I know that writing '; true ->' is a very easy way to get 'else' in Erlang, but we have a couple of decades of psychology-of-programming results to show that it's a bad idea. I have started to replace:

这会更友好， 但是这并不意味着else就是一件好事。我知道在Erlang中，书写'; true ->'代替'else'是一件非常容易的事，但是这是一件非常糟糕的事情。只是在写代码的时候比较让人厌烦， 但是对读代码或多或少有些帮助。

```
                          by
    if X > Y -> a()     if X > Y  -> a()
     ; true  -> b()      ; X =< Y -> b()
    end             end

    if X > Y -> a()     if X > Y -> a()
     ; X < Y -> b()      ; X < Y -> b()
     ; true  -> c()      ; X ==Y -> c()
    end         end
```

which I find mildly annoying when _writing_ the code but enormously helpful when _reading_ it.

'Else' or 'true' branches should be "avoided" altogether: ifs are usually easier to read when you cover all logical ends rather than relying on a "catch all" clause.

'Else' 和'true'分支都应该尽量避免。你覆盖所有逻辑比依赖"catch all"分支更容易阅读。

As mentioned before, there are only a limited set of functions that can be used in guard expressions (we'll see more of them in Types (or lack thereof)). This is where the real conditional powers of Erlang must be conjured. I present to you: the case expression!

正如前面提到的， 这里只有小部分函数可以用于guard表达式。

Note: All this horror expressed by the function names in what_the_if.erl is expressed in regards to the if language construct when seen from the perspective of any other languages' if. In Erlang's context, it turns out to be a perfectly logical construct with a confusing name.

注意：其他语言中的角度来看，在what_the_if.erl中有关if的结构显得非常令人恐怖。在Erlang中， 结果是if语句是一个非常完美地逻辑构造， 却拥有一个令人产生混淆的名字。

In Case ... of
---

If the if expression is like a guard, a case ... of expression is like the whole function head: you can have the complex pattern matching you can use with each argument, and you can have guards on top of it!

如果if表达式像guard子句，那么case子句就就相当于整个函数头部：你可以有使用每个参数的复杂的模式匹配， 同时还可以应用guard子句。

As you're probably getting pretty familiar with the syntax, we won't need too many examples. For this one, we'll write the append function for sets (a collection of unique values) that we will represent as an unordered list. This is possibly the worst implementation possible in terms of efficiency, but what we want here is the syntax:

由于你可能已经很熟悉这个语法了，所以我们没有比较使用例子。对于这个， 我们将会写一个向集合（唯一值的集合）中添加元素的函数，这个集合包含一个未排序的列表。就高效而言，这个例子可能是最糟糕实现， 但是我们像展示的是语言:

```
insert(X,[]) ->
    [X];
insert(X,Set) ->
    case lists:member(X,Set) of
        true  -> Set;
        false -> [X|Set]
    end.
```

If we send in an empty set (list) and a term X to be added, it returns us a list containing only X. Otherwise, the function lists:member/2 checks whether an element is part of a list and returns true if it is, false if it is not. In the case we already had the element X in the set, we do not need to modify the list. Otherwise, we add X as the list's first element.

如果我们把项元X添加到一个空集合里面，函数返回一个只包含X的列表。否则， 使用lists:member/2检查一个元素是否是列表的一部分，如果是列表的一部分，则返回true， 否则返回false。在这种情况下， 集合中已经包含了元素X， 所以我们不需要修改这个列表。 否则，我们将X添加到列表的头部， 作为第一个元素。

In this case, the pattern matching was really simple. It can get more complex (you can compare your code with mine):

这个例子中， 模式匹配真的很简单。 它可以变得更复杂一些:

```
beach(Temperature) ->
    case Temperature of
        {celsius, N} when N >= 20, N =< 45 ->
            'favorable';
        {kelvin, N} when N >= 293, N =< 318 ->
            'scientifically favorable';
        {fahrenheit, N} when N >= 68, N =< 113 ->
            'favorable in the US';
        _ ->
            'avoid beach'
    end.
```

Here, the answer of "is it the right time to go to the beach" is given in 3 different temperature systems: Celsius, Kelvins and Fahrenheit degrees. Pattern matching and guards are combined in order to return an answer satisfying all uses. As pointed out earlier, case ... of expressions are pretty much the same thing as a bunch of function heads with guards. In fact we could have written our code the following way:

这里有一个关于“是否适合去海滩”的例子， 例子中的函数接受三种温度系统作为输入参数： 摄氏，绝对温标和华氏温标。为了返回一个合适的回答，模式匹配和guard子句被一起使用。正如之前指出的case表达式和一堆有用guard的函数非常相似。事实上， 我们可以按照下列方式书写代码:

```
beachf({celsius, N}) when N >= 20, N =< 45 ->
    'favorable';
...
beachf(_) ->
    'avoid beach'.
```

This raises the question: when should we use if, case ... of or functions to do conditional expressions?

这带出一个问题： 什么时候该使用if，case 和带有额外表达式的函数?

Which to use?
---

![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch2/coppertone.png?raw=true)

Which to use is rather hard to answer. The difference between function calls and case ... of are very minimal: in fact, they are represented the same way at a lower level, and using one or the other effectively has the same cost in terms of performance. One difference between both is when more than one argument needs to be evaluated: function(A,B) -> ... end. can have guards and values to match against A and B, but a case expression would need to be formulated a bit like:

该使用那个非常难回答。 函数调用和case子句之间的区别非常小：事实上， 在底层处理中，他们的呈现方式是一样，就执行效率来说， 他们的开销是一样。他们之间的一个不同之处是， 对于一个参数时，函数是需要求值的：funcation(A, B)-> … end. 可以针对A和B的guard子句和模式匹配， 但是case表达式需要被规范化， 如下所示：

```
case {A,B} of
    Pattern Guards -> ...
end.
```

This form is rarely seen and might surprise the reader a bit. In similar situations, using a function call might be more appropriate. On the other hand the insert/2 function we had written earlier is arguably cleaner the way it is rather than having an immediate function call to track down on a simple true or false clause.

这种例子很少见，也许会让读者吓一跳。 在同样的情形下， 使用函数调用可能会更合适。另一方面， 我们之前写的insert/2函数就是一个很好的例证：不是直接调用函数，而是处理简单的true和false子句。

Then the other question is why would you ever use if, given cases and functions are flexible enough to even encompass if through guards? The rationale behind if is quite simple: it was added to the language as a short way to have guards without needing to write the whole pattern matching part when it wasn't needed.

然后另外一个问题是为什么你应该使用if表达式， 对于要完成if携带guard所完成，case 和 函数都能很好的适应。if子句背后的原理非常简单：它是作为能够拥有guard子句的一种简单方式而被加入语言中，你不需要写一个完整的模式匹配。

Of course, all of this is more about personal preferences and what you may encounter more often. There is no good solid answer. The whole topic is still debated by the Erlang community from time to time. Nobody's going to go try to beat you up because of what you've chosen, as long as it is easy to understand. As Ward Cunningham once put it, "Clean code is when you look at a routine and it's pretty much what you expected."