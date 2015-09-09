Errors and Exceptions
===
Not so fast!
---
![](/images/ch6/cyclist.png)

There's no right place for a chapter like this one. By now, you've learned enough that you're probably running into errors, but not yet enough to know how to handle them. In fact we won't be able to see all the error-handling mechanisms within this chapter. That's a bit because Erlang has two main paradigms: functional and concurrent. The functional subset is the one I've been explaining since the beginning of the book: referential transparency, recursion, higher order functions, etc. The concurrent subset is the one that makes Erlang famous: actors, thousands and thousands of concurrent processes, supervision trees, etc.

太难给这样一章安排合适的位置。到目前为止，你已经学习了很多关于erlang的知识，可能已经遭遇了error， 但是获取你还不知道如何处理他们。事实上，我们不可能仅仅用一章内容就能讲清楚处理错误的所有机制。那是因为Erlang有两个主要内容：函数式和并发。函数式部分已经在本书开始部分解释过了：引用透明， 递归，高阶函数等等。并发部分使得Erlang家喻户晓：actor模型， 成千上万的并发编程，监听树等等。

Because I judge the functional part essential to know before moving on to the concurrent part, I'll only cover the functional subset of the language in this chapter. If we are to manage errors, we must first understand them.

由于我觉得函数式部分更为基础，所以需要比并发部分先了解， 所以在这一章只会覆盖Erlang函数式相关的错误处理策略。如果我们要管理错误，我们首先应该了解它们。


Note: Although Erlang includes a few ways to handle errors in functional code, most of the time you'll be told to just let it crash. I hinted at this in the Introduction. The mechanisms that let you program this way are in the concurrent part of the language.

尽管在函数式代码中，Erlang有多种处理错误的方式，也许你听到最多的应该是崩溃。这个我在第一章也介绍过。这种机制是让你在Erlang关于并发部分编程中使用。

A Compilation of Errors
---

There are many kinds of errors: compile-time errors, logical errors, run-time errors and generated errors. I'll focus on compile-time errors in this section and go through the others in the next sections.

这里有几类错误：编译时错误， 逻辑错误，运行时错误和产生的错误。在这一节中，我会更多的介绍运行时错误，其他类型的错误会在接下来几节中介绍。

Compile-time errors are often syntactic mistakes: check your function names, the tokens in the language (brackets, parentheses, periods, commas), the arity of your functions, etc. Here's a list of some of the common compile-time error messages and potential resolutions in case you encounter them:

编译错误经常是语法错误：会检查你的函数名，语言语法（方括号，圆括号，句号，逗号），函数的参数个数等等。这有一些常见编译时错误的错误消息列表，和一些可能用得上的解决方案。

- module.beam: Module name 'madule' does not match file name 'module'
The module name you've entered in the -module attribute doesn't match the filename.
- 模块名和文件名不一致

- ./module.erl:2: Warning: function some_function/0 is unused
You have not exported a function, or the place where it's used has the wrong name or arity. It's also possible that you've written a function that is no longer needed. Check your code!
- 你没有导出该函数，或者某处使用错误参数个数调用函数。 也有可能是你编写了一个未使用的函数。检查你的代码。

- ./module.erl:2: function some_function/1 undefined
The function does not exist. You've written the wrong name or arity either in the -export attribute or when declaring the function. This error is also output when the given function could not be compiled, usually because of a syntax error like forgetting to end a function with a period.
- 该函数不存在。在`-export`属性中或者定义函数时，书写了错误的函数名或者错误参数个数。这个错误也会在某个函数不能被编译时被抛出，通常是语法错误，例如忘记了函数结尾处的句号。

- ./module.erl:5: syntax error before: 'SomeCharacterOrWord'
This happens for a variety of reason, namely unclosed parentheses, tuples or wrong expression termination (like closing the last branch of a case with a comma). Other reasons might include the use of a reserved atom in your code or unicode characters getting weirdly converted between different encodings (I've seen it happen!)
- 有很多可能性， 未结束的括号，元组和错误的表达式结束（使用逗号结束case的分支语句）。其他原因可能包括在代码中使用保留字原子，unicode字符在不同的字符编码间转换。

- ./module.erl:5: syntax error before:
All right, that one is certainly not as descriptive! This usually comes up when your line termination is not correct. This is a specific case of the previous error, so just keep an eye out.
- 这是一个不确定的描述。通常是因为代码行不正确的结束导致的。这是前一个错误的特例，留意就行了。

- ./module.erl:5: Warning: this expression will fail with a 'badarith' exception
Erlang is all about dynamic typing, but remember that the types are strong. In this case, the compiler is smart enough to find that one of your arithmetic expressions will fail (say, llama + 5). It won't find type errors much more complex than that, though.
- Erlang采用动态类型系统，但是记住是强类型。在这个例子中，编译器非常聪明，它发现一个算术表达式会失败。他不能找到更为复杂的类型错误了。

- ./module.erl:5: Warning: variable 'Var' is unused
You declared a variable and never use it afterwards. This might be a bug with your code, so double-check what you have written. Otherwise, you might want to switch the variable name to _ or just prefix it with an underscore (something like _Var) if you feel the name helps make the code readable.
- 变量定义未使用。在你的代码中，这也许是一个问题，所以复核一下你的代码。如果不存在问题，你可以使用`_`变量代替，如果你觉得变量名可以帮助理解代码，可以添加下划线作为前缀(例如_Var)

- ./module.erl:5: Warning: a term is constructed, but never used
In one of your functions, you're doing something such as building a list, declaring a tuple or an anonymous function without ever binding it to a variable or returning it. This warning tells you you're doing something useless or that you have made some mistake.
- 在某个函数中， 你构建了一个列表，声明了一个元组，或者一个匿名函数，但却没有当定到某个变量或者将它作为返回值。

- ./module.erl:5: head mismatch
It's possible your function has more than one head, and each of them has a different arity. Don't forget that different arity means different functions, and you can't interleave function declarations that way. This error is also raised when you insert a function definition between the head clauses of another function.
- 函数包含多个函数头，他们却拥有不同的参数个数。记住，不同的参数个数意味着是不同函数， 所以你不能交错定义函数。这错误也可能来之你在某个函数定义插入到另一个函数的函数头列表中。

- ./module.erl:5: Warning: this clause cannot match because a previous clause at line 4 always matches
A function defined in the module has a specific clause defined after a catch-all one. As such, the compiler can warn you that you'll never even need to go to the other branch.
- 匹配所有的函数分支之后还定义了其他函数分支，导致该分支永远不会执行，编译器发出警告信息


- ./module.erl:9: variable 'A' unsafe in 'case' (line 5)
You're using a variable declared within one of the branches of a case ... of outside of it. This is considered unsafe. If you want to use such variables, you'd be better of doing MyVar = case ... of...
- 在case分支中定了某变量，但却在case分支之外使用了该变量。这样的代码被认为是不安全的。如果你希望使用该变量，你最好这样：MyVar = case ... of

This should cover most errors you get at compile-time at this point. There aren't too many and most of the time the hardest part is finding which error caused a huge cascade of errors listed against other functions. It is better to resolve compiler errors in the order they were reported to avoid being misled by errors which may not actually be errors at all. Other kinds of errors sometimes appear and if you've got one I haven't included, send me an email and I'll add it along with an explanation as soon as possible.

在编译时，你应该解决所有错误。错误类型也不会太多，大部分时间都花费在最难的部分，找出引起一大堆级联错误列表的那个关键错误。最好是根据报告的顺序解决编译错误，这样可以避免被那些不是真正错误的错误所误导。有时候会出现其他类型的编译错误，如果你发现了我没有列出的编译错误，请给发送邮件，我会尽快添加到这个列表中。

No, YOUR logic is wrong!
---

Logical errors are the hardest kind of errors to find and debug. They're most likely errors coming from the programmer: branches of conditional statements such as 'if's and 'case's that don't consider all the cases, mixing up a multiplication for a division, etc. They do not make your programs crash but just end up giving you unseen bad data or having your program work in an unintended manner.

逻辑错误是最难寻找和调试的错误类型。他们大部分来自程序员：来自一大堆没有考虑所有情况的条件语句， 例如`if`和`case`子句， 也可能来自混合了乘除法的语句。虽然他们不会导致你的程序崩溃，但是以给出了未预见的错误数据，或者使程序以某种非期望方式运行而结束。

![](/images/ch6/exam.png)

You're most likely on your own when it comes to this, but Erlang has many facilities to help you there, including test frameworks, TypEr and Dialyzer (as described in the types chapter), a debugger and tracing module, etc. Testing your code is likely your best defense. Sadly, there are enough of these kinds of errors in every programmer's career to write a few dozen books about so I'll avoid spending too much time here. It's easier to focus on those that make your programs crash, because it happens right there and won't bubble up 50 levels from now. Note that this is pretty much the origin of the 'let it crash' ideal I mentioned a few times already.

当错误发生时，也许你只能独自面对，但是Erlang有很多可以帮到你的工具，包括 测试框架， TypEr 和Dialyzer(types章节中提到的)，调试器和跟踪模块等等。测试代码也许是最好的防御手段。很遗憾， 在每个程序员的职业生涯中，他会遇到很多类型的错误，所以我不想花太多的时间在防御这些错误类型上。相比而言，关注那些使程序崩溃的错误更容易，因为当这些异常发生时， 他们不会被冒泡50级。就是几乎就是“让它崩溃”说法的原始来源吧。

Run-time Errors
---
Run-time errors are pretty destructive in the sense that they crash your code. While Erlang has ways to deal with them, recognizing these errors is always helpful. As such, I've made a little list of common run-time errors with an explanation and example code that could generate them.

就使程序崩溃而言，运行时错误是非常觉有破坏性的。虽然Erlang有办法可以处理运行时异常， 但是识别这些运行时异常总是有益处的。同样， 我列举了一些常见的运行时错误及说明，还有一些产生他们的示例代码。

**function_clause**

``` erlang
1> lists:sort([3,2,1]).
[1,2,3]
2> lists:sort(fffffff).
** exception error: no function clause matching lists:sort(fffffff)
```

All the guard clauses of a function failed, or none of the function clauses' patterns matched.

函数所有guard子句匹配失败， 或者没有任何函数子句的模式匹配成功。

**case_clause**
``` erlang
3> case "Unexpected Value" of
3>    expected_value -> ok;
3>    other_expected_value -> 'also ok'
3> end.
** exception error: no case clause matching "Unexpected Value"
```

Looks like someone has forgotten a specific pattern in their case, sent in the wrong kind of data, or needed a catch-all clause!

看起来像是在他们的case子句中忘记了一个特殊的模式匹配，并输入了错误类型的数据， 也可以使用一个匹配所有的子句。

**if_clause**
``` erlang
4> if 2 > 4 -> ok;
4>    0 > 1 -> ok
4> end.
** exception error: no true branch found when evaluating an if expression
```

This is pretty similar to case_clause errors: it can not find a branch that evaluates to true. Ensuring you consider all cases or add the catch-all true clause might be what you need.

这类错误和case_clause 子句非常相似：没有永真子句。确认你考虑了所有情况，或者加入匹配所有情况的子句。


**badmatch**
``` erlang
5> [X,Y] = {4,5}.
** exception error: no match of right hand side value {4,5}
```

Badmatch errors happen whenever pattern matching fails. This most likely means you're trying to do impossible pattern matches (such as above), trying to bind a variable for the second time, or just anything that isn't equal on both sides of the = operator (which is pretty much what makes rebinding a variable fail!). Note that this error sometimes happens because the programmer believes that a variable of the form _MyVar is the same as _. Variables with an underscore are normal variables, except the compiler won't complain if they're not used. It is not possible to bind them more than once.

Badmatch异常发生在模式匹配失败时。这很有可能意味着你正尝试做一些不可能完成的模式匹配（如上所示）、试图多次绑定某个变量或者`=`操作符两边的值不相等（很可能是从新绑定变量失败导致的）。**missing**。以下划线开头的变量也是正常的变量，是因为当有变量未使用时，编译器会发出警告。

**badarg**
``` erlang
6> erlang:binary_to_list("heh, already a list").
** exception error: bad argument
    in function  binary_to_list/1
        called as binary_to_list("heh, already a list")
```

This one is really similar to function_clause as it's about calling functions with incorrect arguments. The main difference here is that this error is usually triggered by the programmer after validating the arguments from within the function, outside of the guard clauses. I'll show how to throw such errors later in this chapter.

这类异常和function_clause异常很类似，使用不正确的参数调用函数而产生。主要不同之处是：badarg异常一般由程序员自己触发，程序员在函数体中验证参数的合法性，如果非法通过erlang:error(badarg)触发异常。我会在稍后的章节中演示如何触发这类异常。

**undef**
``` erlang
7> lists:random([1,2,3]).
** exception error: undefined function lists:random/1
```

This happens when you call a function that doesn't exist. Make sure the function is exported from the module with the right arity (if you're calling it from outside the module) and double check that you did type the name of the function and the name of the module correctly. Another reason to get the message is when the module is not in Erlang's search path. By default, Erlang's search path is set to be in the current directory. You can add paths by using code:add_patha/1 or code:add_pathz/1. If this still doesn't work, make sure you compiled the module to begin with!

当你调用某个不存在的函数是就会触发该异常。确认该函数是否从模块中导出，并且函数参数个数也正确(当你从模块之外调用)，复查函数名和函数所属模块名是否正确。另外的一个原因可能是模块不在Erlang的搜索路径中。缺省情况下，Erlang的搜索路径被设置为当前工作目录。你可以通过`code:add_patha/1`和`code:add_pathz/1`函数添加其他的搜索路径。如果仍然不工作，那确认是否编译了模块。

**badarith**
``` erlang
8> 5 + llama.
** exception error: bad argument in an arithmetic expression
    in operator  +/2
        called as 5 + llama
```

This happens when you try to do arithmetic that doesn't exist, like divisions by zero or between atoms and numbers.

当你尝试进行非法的算术运算时，就会触发该异常，例如：出零或者在原子和数值之间进行算术运算。

**badfun**
``` erlang
9> hhfuns:add(one,two).
** exception error: bad function one
    in function  hhfuns:add/2
```

The most frequent reason why this error occurs is when you use variables as functions, but the variable's value is not a function. In the example above, I'm using the hhfuns function from the previous chapter and using two atoms as functions. This doesn't work and badfun is thrown.

发生该异常常见的原因是把非函数变量的变量当作函数使用，例如上面例子所示，hhfuns函数中使用两个原子当作函数使用，但是这通常不会有效，并且会触发badfun异常。

**badarity**
``` erlang
10> F = fun(_) -> ok end.
#Fun<erl_eval.6.13229925>
11> F(a,b).
** exception error: interpreted function with arity 1 called with two arguments
```

The badarity error is a specific case of badfun: it happens when you use higher order functions, but you pass them more (or fewer) arguments than they can handle.

badarity异常是一种特殊的badfun异常：当你使用高阶函数是，传递了过多或过少的参数时， 就会触发该异常。

**system_limit**
There are many reasons why a system_limit error can be thrown: too many processes (we'll get there), atoms that are too long, too many arguments in a function, number of atoms too large, too many nodes connected, etc. To get a full list in details, read the Erlang Efficiency Guide on system limits. Note that some of these errors are serious enough to crash the whole VM.

触发system_limit异常的原因有多种：过多的进程， 原子过长，函数的参数过多，原子个数过多，链接的节点过多等等。阅读Erlang 高效指南的系统限制文档，以获得详细的列表。注意有一些异常会非常严重，以至于导致整个虚拟机崩溃。

Raising Exceptions
---

![](/images/ch6/stop.png)

In trying to monitor the execution of code and protect against logical errors, it's often a good idea to provoke run-time crashes so problems will be spotted early.

在试图监听代码的执行和防止逻辑错误时，引发运行时异常而是问题及早发现是不错的注意。

There are three kinds of exceptions in Erlang: errors, throws and exits. They all have different uses (kind of):

在Erlang中有三种异常：errors， throws 和exits。他们都有不同的用途：

**Errors**

Calling erlang:error(Reason) will end the execution in the current process and include a stack trace of the last functions called with their arguments when you catch it. These are the kind of exceptions that provoke the run-time errors above.

调用`erlang:error(Reason)`将会终止当前进程的执行，当你捕获异常时，该异常会包含最后一个调用函数的堆栈信息以及调用参数。这些可触发的异常就如上所示。

Errors are the means for a function to stop its execution when you can't expect the calling code to handle what just happened. If you get an if_clause error, what can you do? Change the code and recompile, that's what you can do (other than just displaying a pretty error message). An example of when not to use errors could be our tree module from the recursion chapter. That module might not always be able to find a specific key in a tree when doing a lookup. In this case, it makes sense to expect the user to deal with unknown results: they could use a default value, check to insert a new one, delete the tree, etc. This is when it's appropriate to return a tuple of the form {ok, Value} or an atom like undefined rather than raising errors.

当你不再期望调用代码处理异常，Errors是函数终止执行的一种手段。如果你捕获到if_clause异常，你会做什么？你可以修改代码，重新编译(或者显示一个友好的错误信息)。在迭代章节中的tree例子，tree模块不是总能在tree中找到某个key。在当前例子中，期望用户自己来处理未知结果是有意义的：他们可以使用一个默认值，插入一个新值，或者删除该tree等等。返回{ok, Value}形式的元组或者返回例如undefined的原子比触发异常更合适。

Now, errors aren't limited to the examples above. You can define your own kind of errors too:

现在，上面例子的errors没有做限制。你也可以自定义错误类型:


``` erlang
1> erlang:error(badarith).
** exception error: bad argument in an arithmetic expression
2> erlang:error(custom_error).
** exception error: custom_error
```

Here, custom_error is not recognized by the Erlang shell and it has no custom translation such as "bad argument in ...", but it's usable in the same way and can be handled by the programmer in an identical manner (we'll see how to do that soon).

此时， custom_error不能被Erlang shell识别，它不能被翻译为更为可读的异常，当时它也可以按照同样方式使用，按照同样的方式被程序员处理。

**Exits**

There are two kinds of exits: 'internal' exits and 'external' exits. Internal exits are triggered by calling the function exit/1 and make the current process stop its execution. External exits are called with exit/2 and have to do with multiple processes in the concurrent aspect of Erlang; as such, we'll mainly focus on internal exits and will visit the external kind later on.

有两种类型的exits：'internal' exits 和 'external' exits. 调用函数`exit/1`会触发内部退出，并且会使当前进程终止执行。在并发编程多进程中，调用函数`exit/2`将会触发外部异常。同样的，我们将会主要关注内部退出，在稍后章节中我们会看到外部异常。

Internal exits are pretty similar to errors. In fact, historically speaking, they were the same and only exit/1 existed. They've got roughly the same use cases. So how to choose one? Well the choice is not obvious. To understand when to use one or the other, there's no choice but to start looking at the concepts of actors and processes from far away.

内部退出非常类似errors。事实上，从历史来讲， 他们都是一样的，只存在`exit/1`。他们的用法大致相同。所以怎么选择他们呢？选择不是很明显。为了理解什么时候用errors，什么时候用exits，除了查看actor模型和进程的概念，没有其他选择。

In the introduction, I've compared processes as people communicating by mail. There's not a lot to add to the analogy, so I'll go to diagrams and bubbles.

在第一章节，我曾把进程比作公民通过信件通信。没有必要增加更多的类比，所以我们来看图示。

![](/images/ch6/a-b-msg.png)

Processes here can send each other messages. A process can also listen for messages, wait for them. You can also choose what messages to listen to, discard some, ignore others, give up listening after a certain time etc.

进程间可以相互发送消息。进程可以接收、等待消息。你也可以选择接收那些消息，丢弃那些消息，忽略那些消息，放弃接收消息等等。

![](/images/ch6/a-b-c-hello.png)

These basic concepts let the implementors of Erlang use a special kind of message to communicate exceptions between processes. They act a bit like a process' last breath; they're sent right before a process dies and the code it contains stops executing. Other processes that were listening for that specific kind of message can then know about the event and do whatever they please with it. This includes logging, restarting the process that died, etc.

由于这些基础概念， 所以使得Erlang的实现者使用某种特殊消息在进程间传达异常。他们的角色有点像进程的心跳；如果进程没有终止，这些特殊消息就会被正确发送。监听了这些特殊消息的其他进程可以收到这些事件，并进行合理的处理。包括记录日志，重启刚死掉的进程等等。

![](/images/ch6/a-b-dead.png)

With this concept explained, the difference in using erlang:error/1 and exit/1 is easier to understand. While both can be used in an extremely similar manner, the real difference is in the intent. You can then decide whether what you've got is 'simply' an error or a condition worthy of killing the current process. This point is made stronger by the fact that erlang:error/1 returns a stack trace and exit/1 doesn't. If you were to have a pretty large stack trace or lots of arguments to the current function, copying the exit message to every listening process would mean copying the data. In some cases, this could become unpractical.

有了这些概念的解释， 使用`erlang:error/1` 和 `erlang:exit/1`的不同之处就更容易理解。他们都可以以某种非常相似的方式使用，正真的不同在于使用意图。你可以决定接收一个简洁的异常，还是携带有额外信息的异常。`erlang:error/1`会返回堆栈信息，而`erlang:exit/1`不会返回堆栈信息。如果当前函数有很大的堆栈信息和调用参数，拷贝这些退出消息给其他进程就意味着拷贝数据。 在有些情况下，这些将是不切实际的。

**Throws**

A throw is a class of exceptions used for cases that the programmer can be expected to handle. In comparison with exits and errors, they don't really carry any 'crash that process!' intent behind them, but rather control flow. As you use throws while expecting the programmer to handle them, it's usually a good idea to document their use within a module using them.

`throw`是异常的一种，用于那些程序员期望处理的异常情况。相较于`exit`和`error`， 他不会有"崩溃进程"的意图，而只是用于流程控制。你使用了`throw`，而又期望程序员处理这些异常，那么在文档中记录这些使用情况是一个很好的注意。

The syntax to throw an exception is:

`throw`异常的语法：

``` erlang
1> throw(permission_denied).
** exception throw: permission_denied
```

Where you can replace permission_denied by anything you want (including 'everything is fine', but that is not helpful and you will lose friends).

你可以用你希望的任何东西代替permission_denied。（包括'everything is fine'， 但是这些不是有用的信息）。

Throws can also be used for non-local returns when in deep recursion. An example of that is the ssl module which uses throw/1 as a way to push {error, Reason} tuples back to a top-level function. This function then simply returns that tuple to the user. This lets the implementer only write for the successful cases and have one function deal with the exceptions on top of it all.

当递归调用层数很深时，throw可以用于非本地返回。举个例子，在ssl模块中，就使用`throw/1`来推送{error, Reason}元组到最上层函数。最上层函数仅向用户返回一个元组。这可以让实现者仅仅编写正常情况的代码，而在顶层使用一个函数来处理所有的异常情况。

Another example could be the array module, where there is a lookup function that can return a user-supplied default value if it can't find the element needed. When the element can't be found, the value default is thrown as an exception, and the top-level function handles that and substitutes it with the user-supplied default value. This keeps the programmer of the module from needing to pass the default value as a parameter of every function of the lookup algorithm, again focusing only on the successful cases.

另外一个例子是`array`模块的lookup函数，当找不到需要的元素是，该函数仅仅返回一个用户定义的默认值。当元素找不到时，某个默认值被当作异常被抛出，顶层函数负责处理该异常，然后使用用户定义的默认值替代该异常。这样就可以是模块的编写者不需要向所有查找逻辑函数传递该默认值。

As a rule of thumb, try to limit the use of your throws for non-local returns to a single module in order to make it easier to debug your code. It will also let you change the innards of your module without requiring changes in its interface.

作为经验之谈，尽量限制在单个模块中使用throws来为做非本地返回，因为这样会使你的代码更容易调试。这样也使得当你改变模块的内部结构时，不至于修改模块的接口。

Dealing with Exceptions
---

I've already mentioned quite a few times that throws, errors and exits can be handled. The way to do this is by using a try ... catch expression.

我已经提到过多次，throw，error和exit是可以被处理。是通过`tryy...catch`表达式来处理。

A try ... catch is a way to evaluate an expression while letting you handle the successful case as well as the errors encountered. The general syntax for such an expression is:

`try..catch`可以对摸个表达式求值，这样即可以处理正常情况，也可以处理异常情况。

``` erlang
try Expression of
    SuccessfulPattern1 [Guards] ->
        Expression1;
    SuccessfulPattern2 [Guards] ->
        Expression2
catch
    TypeOfError:ExceptionPattern1 ->
        Expression3;
    TypeOfError:ExceptionPattern2 ->
        Expression4
end.
```

The Expression in between try and of is said to be protected. This means that any kind of exception happening within that call will be caught. The patterns and expressions in between the try ... of and catch behave in exactly the same manner as a case ... of. Finally, the catch part: here, you can replace TypeOfError by either error, throw or exit, for each respective type we've seen in this chapter. If no type is provided, a throw is assumed. So let's put this in practice.

`Expression` 表达式放在`try`和`if`之间，就表示该表达式被保护起来了。这就意味，在该表达式中所有触发的异常都会被捕获到。在`try...of`和`catch`之间的 `patterns`和`expressions`的行为更`case ... of`中的表达式一样。最后，在catch部分中， 你可以使用error，throw和exit代替`TypeOfError`表达式。我们会在这一章中详细介绍每个部分。`throw`为默认值。那让我们实践一下吧。

First of all, let's start a module named exceptions. We're going for simple here:

首先， 编写一个名为exceptions的模块，列举如下：

``` erlang
-module(exceptions).
-compile(export_all).
 
throws(F) ->
    try F() of
        _ -> ok
    catch
        Throw -> {throw, caught, Throw}
    end.
```

We can compile it and try it with different kinds of exceptions:

我们编译该模块，尝试不同类型的异常：

``` erlang
1> c(exceptions).
{ok,exceptions}
2> exceptions:throws(fun() -> throw(thrown) end).
{throw,caught,thrown}
3> exceptions:throws(fun() -> erlang:error(pang) end).
** exception error: pang
```

As you can see, this try ... catch is only receiving throws. As stated earlier, this is because when no type is mentioned, a throw is assumed. Then we have functions with catch clauses of each type:

正如你所见，`try...catch`表达式仅接收throw。如前所述， 如果catch子句中没有指定错误类型，那么throw被设置为默认值。然后，我们写一个函数，在catch子句中分别指定每种错误类型。

``` erlang
errors(F) ->
    try F() of
        _ -> ok
    catch
        error:Error -> {error, caught, Error}
    end.
 
exits(F) ->
    try F() of
        _ -> ok
    catch
        exit:Exit -> {exit, caught, Exit}
    end.
```

And to try them:

然后测试这些代码：

``` erlang
4> c(exceptions).
{ok,exceptions}
5> exceptions:errors(fun() -> erlang:error("Die!") end).
{error,caught,"Die!"}
6> exceptions:exits(fun() -> exit(goodbye) end).
{exit,caught,goodbye}
```

The next example on the menu shows how to combine all the types of exceptions in a single try ... catch. We'll first declare a function to generate all the exceptions we need:

下一个例子是怎么把所有类型的异常结合在一起，放在单个`try...catch`语句。 我们首先声明一个函数，来生成我们需要的所有类型的异常：

``` erlang
sword(1) -> throw(slice);
sword(2) -> erlang:error(cut_arm);
sword(3) -> exit(cut_leg);
sword(4) -> throw(punch);
sword(5) -> exit(cross_bridge).
 
black_knight(Attack) when is_function(Attack, 0) ->
    try Attack() of
        _ -> "None shall pass."
    catch
        throw:slice -> "It is but a scratch.";
        error:cut_arm -> "I've had worse.";
        exit:cut_leg -> "Come on you pansy!";
        _:_ -> "Just a flesh wound."
    end.
```

Here is_function/2 is a BIF which makes sure the variable Attack is a function of arity 0. Then we add this one for good measure:

`is_function/2`是一个内建函数，可以用来确认Attack变量是一个接收0个参数的函数。添加一个函数用于测试：

``` erlang
talk() -> "blah blah".
```

And now for something completely different:

现在来一些复杂的例子：

``` erlang
7> c(exceptions).
{ok,exceptions}
8> exceptions:talk().
"blah blah"
9> exceptions:black_knight(fun exceptions:talk/0).
"None shall pass."
10> exceptions:black_knight(fun() -> exceptions:sword(1) end).
"It is but a scratch."
11> exceptions:black_knight(fun() -> exceptions:sword(2) end).
"I've had worse."
12> exceptions:black_knight(fun() -> exceptions:sword(3) end).
"Come on you pansy!"
13> exceptions:black_knight(fun() -> exceptions:sword(4) end).
"Just a flesh wound."
14> exceptions:black_knight(fun() -> exceptions:sword(5) end).
"Just a flesh wound."
```

The expression on line 9 demonstrates normal behavior for the black knight, when function execution happens normally. Each line that follows that one demonstrates pattern matching on exceptions according to their class (throw, error, exit) and the reason associated with them (slice, cut_arm, cut_leg).

第九行的表达式演示了：当`exceptions:talk/0`正确执行时，黑骑士就表现为正常行为。剩下的所有示例代码演示了：根据类型(throw, error, exit)及相关的异常原因(slice, cut_arm, cut_leg)来匹配异常。

One thing shown here on expressions 13 and 14 is a catch-all clause for exceptions. The _:_ pattern is what you need to use to make sure to catch any exception of any type. In practice, you should be careful when using the catch-all patterns: try to protect your code from what you can handle, but not any more than that. Erlang has other facilities in place to take care of the rest.

表达式13和表达式14展示了匹配所有异常的子句的用法。你需要使用`_:_`模式来匹配任何类型的任何异常。 在实践中，当使用匹配所有情况的模式时，你应该非常小心：为了保护你的代码，所以你需要你能处理的异常，而不是所有异常。Erlang有其他工具来处理剩下的情况。


![](/images/ch6/black-knight.png)

There's also an additional clause that can be added after a try ... catch that will always be executed. This is equivalent to the 'finally' block in many other languages:

可以向`try...catch`添加一个额外的子句，它会被总是能被执行。 这等同于其他语言中的`finally`语言块：

``` erlang
try Expr of
    Pattern -> Expr1
catch
    Type:Exception -> Expr2
    after % this always gets executed
        Expr3
end
```

No matter if there are errors or not, the expressions inside the after part are guaranteed to run. However, you can not get any return value out of the after construct. Therefore, after is mostly used to run code with side effects. The canonical use of this is when you want to make sure a file you were reading gets closed whether exceptions are raised or not.

不管有没有异常发生， 在after部分中的代码被确保一定会执行。然而，你不会从after子句中得到任何返回值（译者注：因为返回值由`try`和`catch`及`catch`和`after`之间的子句决定）。因此，after主要用于运行具有副作用的代码。一种权威的用法是确保有错误发生时，你可以关闭已经打开的文件句柄。

We now know how to handle the 3 classes of exceptions in Erlang with catch blocks. However, I've hidden information from you: it's actually possible to have more than one expression between the try and the of!

现在，在Erlang中，我们已经可以通过catch代码块来处理三种异常。然而，我也隐藏了一些信息：在`try` 和 `of`之间可以有多个表达式！

``` erlang
whoa() ->
    try
        talk(),
        _Knight = "None shall Pass!",
        _Doubles = [N*2 || N <- lists:seq(1,100)],
        throw(up),
        _WillReturnThis = tequila
    of
        tequila -> "hey this worked!"
    catch
        Exception:Reason -> {caught, Exception, Reason}
    end.
```

By calling exceptions:whoa(), we'll get the obvious {caught, throw, up}, because of throw(up). So yeah, it's possible to have more than one expression between try and of...

通过调用`exceptions:whoa/0`函数，因为`throw(up)`，我们会收到`{caught, throw, up}`元组。所以在`try`和`of`之间可以有多条语句。

What I just highlighted in exceptions:whoa/0 and that you might have not noticed is that when we use many expressions in that manner, we might not always care about what the return value is. The of part thus becomes a bit useless. Well good news, you can just give it up:

当我们对多个表达式使用`try...of...catch`语句时，很有可能你不用关心返回值是什么，所以`of`部分就变得很多余。好消息是，你可以去掉`of`部分：

``` erlang
im_impressed() ->
    try
        talk(),
        _Knight = "None shall Pass!",
        _Doubles = [N*2 || N <- lists:seq(1,100)],
        throw(up),
        _WillReturnThis = tequila
    catch
        Exception:Reason -> {caught, Exception, Reason}
    end.
```

And now it's a bit leaner!

现在，看上去简洁多了！

Note: It is important to know that the protected part of an exception can't be tail recursive. The VM must always keep a reference there in case there's an exception popping up.

注意：有一点非常重要，在`try`和`of`之间的保护代码不能进行尾递归。当有异常发生时，VM必须总是保持引用。

Because the try ... catch construct without the of part has nothing but a protected part, calling a recursive function from there might be dangerous for programs supposed to run for a long time (which is Erlang's niche). After enough iterations, you'll go out of memory or your program will get slower without really knowing why. By putting your recursive calls between the of and catch, you are not in a protected part and you will benefit from Last Call Optimisation.

因为`try..catch`结构除了保护部分没有其他代码， 如果就从保护部分递归调用函数，并且该函数会运行很长时间，那么这会很危险。递归调用足够多次后，你可能会耗尽内存，或者你的程序会运行得越来越慢，而你却不知道什么原因。把你递归调用的部分放在`of` 和 `catch`之间，而不是在保护部分。你会因为LCO(最后调用优化)受益的。

Some people use try ... of ... catch rather than try ... catch by default to avoid unexpected errors of that kind, except for obviously non-recursive code with results that won't be used by anything. You're most likely able to make your own decision on what to do!

默认情况下， 有些人使用`try...of..catch`代替`try...catch`来避免意外的错误类型，除了明显地非递归调用代码，并且它的结果不会在被使用。 你可以自己决定使用哪种。


Wait, there's more!
---

As if it wasn't enough to be on par with most languages already, Erlang's got yet another error handling structure. That structure is defined as the keyword catch and basically captures all types of exceptions on top of the good results. It's a bit of a weird one because it displays a different representation of exceptions:

``` erlang
1> catch throw(whoa).
whoa
2> catch exit(die).
{'EXIT',die}
3> catch 1/0.
{'EXIT',{badarith,[{erlang,'/',[1,0]},
{erl_eval,do_apply,5},
{erl_eval,expr,5},
{shell,exprs,6},
{shell,eval_exprs,6},
{shell,eval_loop,3}]}}
4> catch 2+2.
4
```

What we can see from this is that throws remain the same, but that exits and errors are both represented as {'EXIT', Reason}. That's due to errors being bolted to the language after exits (they kept a similar representation for backwards compatibility).

The way to read this stack trace is as follows:

``` erlang
5> catch doesnt:exist(a,4).             
{'EXIT',{undef,[{doesnt,exist,[a,4]},
    {erl_eval,do_apply,5},
    {erl_eval,expr,5},
    {shell,exprs,6},
    {shell,eval_exprs,6},
    {shell,eval_loop,3}]}}
```

The type of error is undef, which means the function you called is not defined (see the list at the beginning of this chapter)
The list right after the type of error is a stack trace
The tuple on top of the stack trace represents the last function to be called ({Module, Function, Arguments}). That's your undefined function.
The tuples after that are the functions called before the error. This time they're of the form {Module, Function, Arity}.
That's all there is to it, really.
You can also manually get a stack trace by calling erlang:get_stacktrace/0 in the process that crashed.

You'll often see catch written in the following manner (we're still in exceptions.erl):

``` erlang
catcher(X,Y) ->
    case catch X/Y of
        {'EXIT', {badarith,_}} -> "uh oh";
        N -> N
    end.
```

And as expected:

``` erlang
6> c(exceptions).
{ok,exceptions}
7> exceptions:catcher(3,3).
1.0
8> exceptions:catcher(6,3).
2.0
9> exceptions:catcher(6,0).
"uh oh"
```

This sounds compact and easy to catch exceptions, but there are a few problems with catch. The first of it is operator precedence:

``` erlang
10> X = catch 4+2.
* 1: syntax error before: 'catch'
10> X = (catch 4+2).
6
```

That's not exactly intuitive given that most expressions do not need to be wrapped in parentheses this way. Another problem with catch is that you can't see the difference between what looks like the underlying representation of an exception and a real exception:

``` erlang
11> catch erlang:boat().
{'EXIT',{undef,[{erlang,boat,[]},
    {erl_eval,do_apply,5},
    {erl_eval,expr,5},
    {shell,exprs,6},
    {shell,eval_exprs,6},
    {shell,eval_loop,3}]}}
12> catch exit({undef, [{erlang,boat,[]}, {erl_eval,do_apply,5}, {erl_eval,expr,5}, {shell,exprs,6}, {shell,eval_exprs,6}, {shell,eval_loop,3}]}).
{'EXIT',{undef,[{erlang,boat,[]},
    {erl_eval,do_apply,5},
    {erl_eval,expr,5},
    {shell,exprs,6},
    {shell,eval_exprs,6},
    {shell,eval_loop,3}]}}
```

And you can't know the difference between an error and an actual exit. You could also have used throw/1 to generate the above exception. In fact, a throw/1 in a catch might also be problematic in another scenario:

``` erlang
one_or_two(1) -> return;
one_or_two(2) -> throw(return).
```

And now the killer problem:

``` erlang
13> c(exceptions).
{ok,exceptions}
14> catch exceptions:one_or_two(1).
return
15> catch exceptions:one_or_two(2).
return
```

Because we're behind a catch, we can never know if the function threw an exception or if it returned an actual value! This might not really happen a whole lot in practice, but it's still a wart big enough to have warranted the addition of the try ... catch construct in the R10B release.

Try a try in a tree
---
To put exceptions in practice, we'll do a little exercise requiring us to dig for our tree module. We're going to add a function that lets us do a lookup in the tree to find out whether a value is already present in there or not. Because the tree is ordered by its keys and in this case we do not care about the keys, we'll need to traverse the whole thing until we find the value.

The traversal of the tree will be roughly similar to what we did in tree:lookup/2, except this time we will always search down both the left branch and the right branch. To write the function, you'll just need to remember that a tree node is either {node, {Key, Value, NodeLeft, NodeRight}} or {node, 'nil'} when empty. With this in hand, we can write a basic implementation without exceptions:

``` erlang
%% looks for a given value 'Val' in the tree.
has_value(_, {node, 'nil'}) ->
    false;
has_value(Val, {node, {_, Val, _, _}}) ->
    true;
has_value(Val, {node, {_, _, Left, Right}}) ->
    case has_value(Val, Left) of
        true -> true;
        false -> has_value(Val, Right)
    end.
```

The problem with this implementation is that every node of the tree we branch at has to test for the result of the previous branch:

![](/images/ch6/tree-case.png)

This is a bit annoying. With the help of throws, we can make something that will require less comparisons:

``` erlang
has_value(Val, Tree) ->
    try has_value1(Val, Tree) of
        false -> false
    catch
        true -> true
    end.
 
has_value1(_, {node, 'nil'}) ->
    false;
has_value1(Val, {node, {_, Val, _, _}}) ->
    throw(true);
has_value1(Val, {node, {_, _, Left, Right}}) ->
has_value1(Val, Left),
has_value1(Val, Right).
```

The execution of the code above is similar to the previous version, except that we never need to check for the return value: we don't care about it at all. In this version, only a throw means the value was found. When this happens, the tree evaluation stops and it falls back to the catch on top. Otherwise, the execution keeps going until the last false is returned and that's what the user sees:

![](/images/ch6/tree-throw.png)

Of course, the implementation above is longer than the previous one. However, it is possible to realize gains in speed and in clarity by using non-local returns with a throw, depending on the operations you're doing. The current example is a simple comparison and there's not much to see, but the practice still makes sense with more complex data structures and operations.

That being said, we're probably ready to solve real problems in sequential Erlang.