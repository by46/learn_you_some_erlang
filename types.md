Types (or lack thereof)
===
Dynamite-strong Typing
---

As you might have noticed when typing in examples from Starting Out (for real), and then modules and functions from Modules and Syntax in Functions, we never needed to write the type of a variable or the type of a function. When pattern matching, the code we had written didn't have to know what it would be matched against. The tuple {X,Y} could be matched with {atom, 123} as well as {"A string", <<"binary stuff!">>}, {2.0, ["strings","and",atoms]} or really anything at all.

在前面的例子中，我不需要为一个变量或函数添加类型。当使用模式匹配的时候， 我们所写的代码不需要知道自己所匹配的是什么。{X, Y}元素可以匹配{atom, 123} ， 同样也可以匹配{"A string", <<"binary stuff!">>},  也可以匹配{2.0, ["strings", "and", atoms]}。

When it didn't work, an error was thrown in your face, but only once you ran the code. This is because Erlang is dynamically typed: every error is caught at runtime and the compiler won't always yell at you when compiling modules where things may result in failure, like in Starting Out (for real)'s "llama + 5" example.

只有当你运行代码的时候，并且如果代码没有正常工作，你才能收到错误信息。这也说明Erlang是动态类型：所有错误都是在运行时捕获到的，当编译一个可能存在错误的模块时，编译器不会做出任何反应。

![](/images/ch2/ham.png)


One classic friction point between proponents of static and dynamic typing has to do with the safety of the software being written. A frequently suggested idea is that good static type systems with compilers enforcing them with fervor will catch most errors waiting to happen before you can even execute the code. As such, statically typed languages are to be seen as safer than their dynamic counterparts. While this might be true when comparing with many dynamic languages, Erlang begs to differ and certainly has a track record to prove it. The best example is the often reported nine nines (99.9999999%) of availability offered on the Ericsson AXD 301 ATM switches, consisting of over 1 million lines of Erlang code. Please note that this is not an indication that none of the components in an Erlang-based system failed, but that a general switch system was available 99.9999999% of the time, planned outages included. This is partially because Erlang is built on the notion that a failure in one of the components should not affect the whole system. Errors coming from the programmer, hardware failures or [some] network failures are accounted for: the language includes features which will allow you to distribute a program over to different nodes, handle unexpected errors, and never stop running.

动态类型和静态类型支持者之间的最大的争论点必须处理软件的安全性。一个最常见的建议是拥有良好的静态类型的编译器， 并强制编译器在错误发生之前捕获错误， 在你运行代码之前。就其本身而论，静态类型语言看上去比对应的动态类型实现的版本更安全。通过和几种动态语言比较，这种理论可能才是真的，但是Erlang不同意这样的理论，也有确实的证据可以证明这种理论在erlang这里是行不通的。最好的例子是Ericsson AXD 301 ATM交换机上运行的程序，拥有超过一百万行的代码，它提供了99.9999999%的可靠性。请记住这并不是表明没有一个以erlang为基础的组件失败，而是指一个通用交换机系统在99.9999999%的时间里是可用的，包括计划中的停电。这一部分应该归功于Erlang是构建在一个组件失败不应该影响整个系统的理念上的。来自程序员，硬件故障和一些网络故障的异常发生原因是：编程语言包含允许你在不同的节点上分发程序，处理意外错误和永远运行的特性。

To make it short, while most languages and type systems aim to make a program error-free, Erlang uses a strategy where it is assumed that errors will happen anyway and makes sure to cover these cases: Erlang's dynamic type system is not a barrier to reliability and safety of programs. This sounds like a lot of prophetic talking, but you'll see how it's done in the later chapters.

简单说来， 当打不语言和类型系统旨在使程序准确无误的运行时， Erlang却采用了另外一种策略， 它假设错误会在任何地方发生，并会确保以下情况：Erlang的动态类型不是构建一个可靠的安全的系统的障碍。这听起来像一个预言， 但是你们会在接下来的章节发现它确实已经实现了。

Note: Dynamic typing was historically chosen for simple reasons; those who implemented Erlang at first mostly came from dynamically typed languages, and as such, having Erlang dynamic was the most natural option to them.

注意：选择动态类型的原因很简单， 最先实现Erlang的开发者都是来之动态类型语言的开发者， 所以在实现Erlang时，很自然就选择了动态类型作为他的类型系统。

Erlang is also strongly typed. A weakly typed language would do implicit type conversions between terms. If Erlang were to be weakly typed we could possibly do the operation 6 = 5 + "1". while in practice, an exception for bad arguments will be thrown:

Erlang同样是强类型语言。弱类型语言在类型之间进行隐式类型转换。如果Erlang是一种若类型语言的话，那么我们可以执行6=5+"1"这样的操作。实际上， 它会导致一个异常：

``` erlang
1> 6 + "1".
** exception error: bad argument in an arithmetic expression
        in operator  +/2
        called as 6 + "1"
```

Of course, there are times when you could want to convert one kind of data to another one: changing regular strings into bit strings to store them or an integer to a floating point number. The Erlang standard library provides a number of functions to do it.

当然，有时你想把一种类型转换为另外一种：普通字符串转换为位串来保存它们， 或者把整形转换为浮点型。Erlang标准库提供一些完成这样功能的函数。

Type conversions
---

Erlang, like many languages, changes the type of a term by casting it into another one. This is done with the help of built-in functions, as many of the conversions could not be implemented in Erlang itself. Each of these functions take the form <type>_to_<type> and are implemented in the erlang module. Here are a few of them:

更其他语言类似， Erlang通过转换把一种类型变为另外一种类型。是通过一组内置的函数，因为很多转换不再由Erlang代码完成。这些函数都有类似的形式：<type>_to_<type>类型， 都是erlang模块中实现。这有一些例子：

``` erlang
1> erlang:list_to_integer("54").
54
2> erlang:integer_to_list(54).
"54"
3> erlang:list_to_integer("54.32").
** exception error: bad argument
    in function  list_to_integer/1
        called as list_to_integer("54.32")
4> erlang:list_to_float("54.32").
54.32
5> erlang:atom_to_list(true).
"true"
6> erlang:list_to_bitstring("hi there").
<<"hi there">>
7> erlang:bitstring_to_list(<<"hi there">>).
"hi there"
```

And so on. We're hitting on a language wart here: because the scheme <type>_to_<type> is used, every time a new type is added to the language, a whole lot of conversion BIFs need to be added! Here's the whole list already there:

这样的方式也是一种编程语言的缺点： 因为<type>_to_<type>的形式被使用， 一旦有新的类型被加入语言中， 那也同样的需要添加一系列关于新类型的转换内置函数。Erlang所有的转换函数列在下面：

``` erlang
atom_to_binary/2, atom_to_list/1, binary_to_atom/2, binary_to_existing_atom/2, binary_to_list/1, bitstring_to_list/1, binary_to_term/1, float_to_list/1, fun_to_list/1, integer_to_list/1, integer_to_list/2, iolist_to_binary/1, iolist_to_atom/1, list_to_atom/1, list_to_binary/1, list_to_bitstring/1, list_to_existing_atom/1, list_to_float/1, list_to_integer/2, list_to_pid/1, list_to_tuple/1, pid_to_list/1, port_to_list/1, ref_to_list/1, term_to_binary/1, term_to_binary/2 and tuple_to_list/1.
```

That's a lot of conversion functions. We'll see most if not all of these types through this book, although we probably won't need all of these functions.

我们会在本书中遇见大部分的转换函数， 也有一些不会用到。

To Guard a Data Type
---

Erlang basic data types are easy to spot, visually: tuples have the curly brackets, lists the square brackets, strings are enclosed in double quotation marks, etc. Enforcing a certain data type has thus been possible with pattern matching: a function head/1 taking a list could only accept lists because otherwise, the matching ([H|_]) would have failed.

Erlang的基础数据类型很容易被发现， 从表面上看：元组使用花括号， 列表使用中括号， 字符串使用双引号包围。可以通过模式匹配来强制使用一种确定的数据类型：head/1函数只能接受一个列表作为参数，因为其他数据类型会导致([H|_])模式匹配失败。

![](/images/ch2/my-name-is.png)

However, we've had a problem with numeric values because we couldn't specify ranges. Consequently, we used guards in functions about temperature, the age to drive, etc. We're hitting another roadblock now. How could we write a guard that ensures that patterns match against data of a single specific type, like numbers, atoms or bitstrings?

不管怎样， 在处理数值上都存在问题， 因为我们不能匹配具体的范围。因此， 我在关于温度的问题的函数中使用guard。我现在正好遇到另外一个障碍。 我们要怎么样构造一个guard来确保模式匹配是针对单个特定的类型， 例如数字， 原子或者位串?

There are functions dedicated to this task. They will take a single argument and return true if the type is right, false otherwise. They are part of the few functions allowed in guard expressions and are named the type test BIFs:

这里存在一些函数专门解决这类问题。他们接受一个参数，如果参数是正确的类型，则返回true，否则返回false。他们是能应用在guard表达式中的少数几个函数中的一部分，用类型命名的内置函数:

``` erlang
is_atom/1           is_binary/1        
is_bitstring/1      is_boolean/1        is_builtin/3       
is_float/1          is_function/1       is_function/2      
is_integer/1        is_list/1           is_number/1        
is_pid/1            is_port/1           is_record/2        
is_record/3         is_reference/1      is_tuple/1         
```

They can be used like any other guard expression, wherever guard expressions are allowed. You might be wondering why there is no function just giving the type of the term being evaluated (something akin to type_of(X) -> Type). The answer is pretty simple. Erlang is about programming for the right cases: you only program for what you know will happen and what you expect. Everything else should cause errors as soon as possible. Although this might sound insane, the explanations you'll get in Errors and Exceptions will hopefully make things clearer. Until then, just trust me on that.

他们能他们guard表达式一样使用，只要是允许guard表达式的地方都可以使用。 你也许会惊讶，为什么不存在返回项元的数据类型的函数，例如type_of(X) -> Type。答案非常简单。Erlang是对正确情况进行编程：你只会为你知道将会发生和你所期望的情况编程。一切都应该尽快引起错误。虽然这听起来很疯狂，你始终会捕获错误和异常的解释会让事情变得清晰。请相信我。

Note: type test BIFs constitute more than half of the functions allowed in guard expressions. The rest are also BIFs, but do not represent type tests. These are: 

注意：在guard表达式中允许使用的内置函数中，有超过半数都是类型测试函数。剩下的内置函数不是用于类型测试的， 他们列在下面：

``` erlang
abs(Number), bit_size(Bitstring), byte_size(Bitstring), element(N, Tuple), float(Term), hd(List), length(List), node(), node(Pid|Ref|Port), round(Number), self(), size(Tuple|Bitstring), tl(List), trunc(Number), tuple_size(Tuple).
```

The functions node/1 and self/0 are related to distributed Erlang and processes/actors. We'll eventually use them, but we've still got other topics to cover before then.

Node/1 和self/0函数和分布式和processes/actors模型有关。我们会在后面使用它们，并在另外的主题中覆盖这两个函数。

It may seem like Erlang data structures are relatively limited, but lists and tuples are usually enough to build other complex structures without worrying about anything. As an example the basic node of a binary tree could be represented as {node, Value, Left, Right}, where Left and Right are either similar nodes or empty tuples. I could also represent myself as:

这看上去Erlang的数据结构相对有限， 其实列表和元组通常足以构造其他复杂结构， 并且没有任何后顾之忧。例如：二叉树的节点可以用{node, Value, Left, Right}来表示， Left 和Right既可以是类似节点，或者是空元组。我可以使用下列结构来表示我自己:

``` erlang
{person, {name, <<"Fred T-H">>},
{qualities, ["handsome", "smart", "honest", "objective"]},
{faults, ["liar"]},
{skills, ["programming", "bass guitar", "underwater breakdancing"]}}.
```

Which shows that by nesting tuples and list and filling them with data, we can obtain complex data structures and build functions to operate on them.

通过上面的例子表明，通过嵌套元组和列表，并填充数据，我们可以获得复杂的数据结构，并可以构建函数来操作他们。

Update:
The release R13B04 saw the addition of the BIF binary_to_term/2, which lets you unserialize data the same way binary_to_term/1 would, except the second argument is an option list. If you pass in [safe], the binary won't be decoded if it contains unknown atoms or anonymous functions, which could exhaust memory

更新：R13B04变笨发布了额外的内置函数binary_to_term/2函数， 它允许你反序列化数据， 跟binary_to_term/1的方式一样， 只是需要多添加一个选项列表作为第二个参数。如果你传递[safe], 当二进制数据中包含会耗尽内存的未知原子或者匿名函数时，他不会不会被解码。

For Type Junkies
---

![](/images/ch2/type-dance.png)


This section is meant to be read by programmers who can not live without a static type system for one reason or another. It will include a little bit more advanced theory and everything may not be understood by everyone. I will briefly describe tools used to do static type analysis in Erlang, defining custom types and getting more safety that way. These tools will be described for anyone to understand much later in the book, given that it is not necessary to use any of them to write reliable Erlang programs. Because we'll show them later, I'll give very little details about installing, running them, etc. Again, this section is for those who really can't live without advanced type systems.

这一节本打算是给那些没有静态类型系统而不能生活的程序员阅读的。它将稍微包含一些进阶理论，不是所有的都能被每一个人理解。我会简略的介绍一下Erlang中用于静态类型分析的工具，定义自定义类型和写更安全的代码。对于每个人，这些工具将会在本书后面的章节进行更详细的描述， 因为要写出可靠的Erlang 程序不要依赖他们中的任何一个。因为我会在后面介绍他们， 所以我只会简单的介绍如何安装， 运行。此外， 这一节也是为他们确实离了高级类型系统不能活的人准备的。

Through the years, there were some attempts to build type systems on top of Erlang. One such attempt happened back in 1997, conducted by Simon Marlow, one of the lead developers of the Glasgow Haskell Compiler, and Philip Wadler, who worked on Haskell's design and has contributed to the theory behind monads (Read the paper on said type system). Joe Armstrong later commented on the paper:

这些年来，仍然有人企图在Erlang之上构建类型系统。其中之一就发生在1997年，由Simon Marlow和Philip Wadler牵头， 最后Joe Armstrong发表了评论：

One day Phil phoned me up and announced that a) Erlang needed a type system, b) he had written a small prototype of a type system and c) he had a one year’s sabbatical and was going to write a type system for Erlang and “were we interested?” Answer —“Yes.”

一天Phil 打电话给我， 并且宣称：1. Erlang需要一个类型系统， 2.他已经类型系统的原型，3.他有一年的休假，正打算为Erlang写一个类型系统， 并问“我们是否有兴趣”。 回答：“有”。

Phil Wadler and Simon Marlow worked on a type system for over a year and the results were published in [20]. The results of the project were somewhat disappointing. To start with, only a subset of the language was type-checkable, the major omission being the lack of process types and of type checking inter-process messages.

Phil 和Simon整整一年都在为完成一个类型系统而工作， 最后还是发布了。但是项目的结果却让人有些失望。首先， 编程语言中只有一部分代码是类型可检查的，大部分是缺少处理类型和缺少类型检查。

Processes and messages both being one of the core features of Erlang, it may explain why the system was never added to the language. Other attempts at typing Erlang failed. The efforts of the HiPE project (attempts to make Erlang's performances much better) produced Dialyzer, a static analysis tool still in use today, with its very own type inference mechanism.

进程和消息都是Erlang的核心特性之一，他们正好可以解释为什么类型系统没有被加入到Erlang中。另一个关于类型系统的尝试也失败了。HiPE项目(试图提高Erlang的执行效率)的努力却催生了Dialyzer，一种今天仍在使用的静态分析工具，它具有一种类型推断机制。

The type system that came out of it is based on success typings, a concept different from Hindley-Milner or soft-typing type systems. Success types are simple in concept: the type-inference will not try to find the exact type of every expression, but it will guarantee that the types it infers are right, and that the type errors it finds are really errors.

类型系统产生于基于success typings，一种不同于软类型化系统的概念。Success types在概念很简单：类型推断不会试图找到所有表达式的确切类型， 但是它会保证它推断的类型一定是正确的， 它发现的类型错误也一定是他们真的错了。

The best example would come from the implementation of the function and, which will usually take two Boolean values and return 'true' if they're both true, 'false' otherwise. In Haskell's type system, this would be written and :: bool -> bool -> bool. If the and function had to be implemented in Erlang, it could be done the following way:

最好的例子来自函数的实现，要实现一个接受两个boolean类型的值，如果他们都为true，则返回true，否则返回false。 在 haskell的类型系统中， 函数应该写成这样：and::bool -> bool -> bool.如果在erlang中要实现同样的功能， 它可以写成这样：

``` erlang
and(false, _) -> false;
and(_, false) -> false;
and(true,true) -> true.
```

Under success typing, the inferred type of the function would be and(_,_) -> bool(), where _ means 'anything'. The reason for this is simple: when running an Erlang program and calling this function with the arguments false and 42, the result would still be 'false'. The use of the _ wildcard in pattern matching made it that in practice, any argument can be passed as long as one of them is 'false' for the function to work. ML types would have thrown a fit (and its users had a heart attack) if you had called the function this way. Not Erlang. It might make more sense to you if you decide to read the paper on the implementation of success types, which explains the rationale behind the behavior. I really encourage any type junkies out there to read it, it's an interesting and practical implementation definition.

在success typing中， 函数的类型推断会是这样：and(_, _) -> bool(), "_" 意味着可以使任意值。这样做的原因很简单：当运行Erlang程序的时候， 使用false和42作为参数调用函数， 结果是'false'。在模式匹配中使用"_"通配符的作用是：只要任意一个参数为false，那么函数的返回值也就是false。如果你按照这样的方式调用这个函数，ML 的类型系统会抛出一个异常(他的用户像是被电击了一样)。如果你决定阅读文档上关于实现Success Type的部分，那么文档可能对你更有意义。我鼓励任何类型追随者离开这里， 去阅读文档，它是一个有趣和实践性的实现。

The details about type definitions and function annotations are described in the Erlang Enhancement Proposal 8 (EEP 8). If you're interested in using success typings in Erlang, check out the TypEr application and Dialyzer, both part of the standard distribution. To use them, type in $ typer --help and $ dialyzer --help (typer.exe --help and dialyzer.exe --help for Windows, if they're accessible from the directory you are currently in).

关于类型定义和函数注释的细节在Erlang Enhancement Proposal 8 (EEP 8)中有详细描述。如果你对在Erlang中使用Success Type， 检查TypEr application 和Dialyzer， 他们都是标准模块。使用$ typer --help 和$ dialyzer --help获得帮助信息。

TypEr will be used to generate type annotations for functions. Used on this small FIFO implementation, it spits the following type annotations:

TypEr用来为函数生成类型注释。这有一个关于FIFO实现的例子， 它会推断出下面的类型声明：

``` erlang
%% File: fifo.erl
%% --------------
-spec new() -> {'fifo',[],[]}.
-spec push({'fifo',_,_},_) -> {'fifo',nonempty_maybe_improper_list(),_}.
-spec pop({'fifo',_,maybe_improper_list()}) -> {_,{'fifo',_,_}}.
-spec empty({'fifo',_,_}) -> bool().
```

Implementation of fifo (queues): made out of two stacks (last-in first-out).
Which is pretty much right. Improper lists should be avoided because lists:reverse/1 doesn't support them, but someone bypassing the module's interface would be able to get through it and submit one. In this case, the functions push/2 and pop/2 might still succeed for a few calls before they cause an exception. This either tells us to add guards or refine our type definitions manually. Suppose we add the signature -spec push({fifo,list(),list()},_) -> {fifo,nonempty_list(),list()}. and a function that passes an improper list to push/2 to the module: when scanning it in Dialyzer (which checks and matches the types), the error message "The call fifo:push({fifo,[1|2],[]},3) breaks the contract '<Type definition here>' is output.


![](/images/ch2/fifo.png)

Dialyzer will complain only when code will break other code, and if it does, it'll usually be right (it will complain about more stuff too, like clauses that will never match or general discrepancies). Polymorphic data types are also possible to write and analyze with Dialyzer: the hd() function could be annotated with -spec([A]) -> A. and be analyzed correctly, although Erlang programmers seem to rarely use this type syntax.

Don't drink too much Kool-Aid:
Some of the things you can't expect Dialyzer and TypEr to do is type classes with constructors, first order types and recursive types. The types of Erlang are only annotations without effects or restrictions on actual compiling unless you enforce them yourself. The type checker will never tell you a program that can run right now (or has run for two years) has a type bug when it effectively causes no error when running (although you could have buggy code running correctly...)

While recursive types are something that would be really interesting to have, they're unlikely to ever appear in the current forms of TypEr and Dialyzer (the paper above explains why). Defining your own types to simulate recursive types by adding one or two levels manually is the best you can do at the moment.

It's certainly not a full-blown type system, not as strict or powerful as what languages like Scala, Haskell or Ocaml propose. Its warning and error messages are also usually a bit cryptic and not really user friendly. However, it's still a very good compromise if you really can't live in a dynamic world or wish for additional safety; just expect it to be a tool in your arsenal, not too much more.

Update:
Since version R13B04, recursive types are now available as an experimental feature for Dialyzer. This makes the previous Don't drink too much Kool-aid partially wrong. Shame on me.

Note that the type documentation has also become official (although it remains subject to change) and is more complete than what can be found in EEP8.