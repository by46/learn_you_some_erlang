Modules
===
What are modules
---

![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch1/modules.png?raw=true)

Working with the interactive shell is often considered a vital part of using dynamic programming languages. It is useful to test all kinds of code and programs. Most of the basic data types of Erlang were used without even needing to open a text editor or saving files. You could drop your keyboard, go play ball outside and call it a day, but you would be a terrible Erlang programmer if you stopped right there. Code needs to be saved somewhere to be used!

为了使用交互式shell通常被认为是使用动态编程语言的一个重要部分。这对于测试所有类型的代码和程序很有有用。Erlang大部分的基础数据结构不需要打开一个文本编辑，或者保存为文件才能运行。你可以丢掉键盘， 在户外玩一整天球，如果你就此停下你前进的步伐， 你将会成为一个可怕的Erlang程序员。代码应该保存，因为不知道什么地方就会使用它。

This is what modules are for. Modules are a bunch of functions regrouped in a single file, under a single name. Additionally, all functions in Erlang must be defined in modules. You have already used modules, perhaps without realizing it. The BIFs mentioned in the previous chapter, like hd or tl, actually belong to the erlang module, as well as all of the arithmetic, logic and Boolean operators. BIFs from the erlang module differ from other functions as they are automatically imported when you use Erlang. Every other function defined in a module you will ever use needs to be called with the form Module:Function(Arguments).

这就是模块的作用。模块就是在一个文件中重组一些列函数。此外，在Erlang中，所有的函数必须定义在模块中。你已经在使用模块了，可能只是你没有意识到而已。在前面几章提到的BIF， 例如hd 或者tl， 实际上是属于erlang模块，不仅如此，所有的算术， 逻辑和布尔运算符也都是属于erlang模块。erlang模块中的BIF不同于其他函数的地方是，当你使用erlang的时， 他们已经被自动导入了。在其他模块中定义的函数，你需要通过`Module:Function(Arguments)`的形式进行调用。

You can see for yourself:

你可以自己试一试：

```
1> erlang:element(2, {a,b,c}).
b
2> element(2, {a,b,c}).
b
3> lists:seq(1,4).
[1,2,3,4]
4> seq(1,4).
** exception error: undefined shell command seq/2
```

Here, the seq function from the list module was not automatically imported, while element was. The error 'undefined shell command' comes from the shell looking for a shell command like f() and not being able to find it. There are some functions from the erlang module which are not automatically imported, but they're not used too frequently.

这里， list模块的seq函数没有被自动导入， 但是element函数却被自动导入了。"undefined shell command"是因为shell尝试搜寻类似f()的shell命令，却没有找到而引起的错误。erlang模块中也有一些函数因为不常用，而没有被自动导入。

Logically, you should put functions about similar things inside a single module. Common operations on lists are kept in the lists module, while functions to do input and output (such as writing to the terminal or in a file) are regrouped in the io module. One of the only modules you will encounter which doesn't respect that pattern is the aforementioned erlang module that has functions which do math, conversions, deal with multiprocessing, fiddle with the virtual machine's settings, etc. They have no point in common except being built-in functions. You should avoid creating modules like erlang and instead focus on clean logical separations.

逻辑上， 你可以把所有类似的函数放置到一个模块中。lists的公共操作纳入lists模块， 输入输出操作的函数放入io模块。 唯一的例外，是上述提到的erlang模块，它包含了math(数学)， 转换， 处理多进程， 处理虚拟机的设置等一系列函数。唯一的共通之处就是它们都是内置函数。 你应该避免像erlang模块这样创建新的模块， 取而代之的是关注清晰逻辑分离。

Module Declaration
---

![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch1/declaration.png?raw=true)

When writing a module, you can declare two kinds of things: functions and attributes. Attributes are metadata describing the module itself such as its name, the functions that should be visible to the outside world, the author of the code, and so on. This kind of metadata is useful because it gives hints to the compiler on how it should do its job, and also because it lets people retrieve useful information from compiled code without having to consult the source.

在编写一个模块时，你可以声明两种事物：函数(functions)和属性(attributes)。属性是描述模块本身的元数据， 例如名字， 外部可见的函数， 代码的作者等。这类元数据很有用， 因为它可以帮助编译器更好的完成编译工作，在没有可参考的源代码情况下，可以从经过编译的代码中获取有用的信息。

There is a large variety of module attributes currently used in Erlang code across the world; as a matter of fact, you can even declare your own attributes for whatever you please. There are some pre-defined attributes that will appear more frequently than others in your code. All module attributes follow the form -Name(Attribute).. Only one of them is necessary for your module to be compilable:

如今，在Erlang代码中使用了大量的模块属性，事实上， 你甚至可以任意定义你专属的属性。也有一些出现频率很高的预定义属性。 所有的模块属性都按照-Name(Attribute).的形式。 其中只有下列属性是可编译模块所必须的：

```
-module(Name).
```

This is always the first attribute (and statement) of a file, and for good reason: it's the name of the current module, where Name is an atom. This is the name you'll use to call functions from other modules. The calls are made with the M:F(A) form, where M is the module name, F the function, and A the arguments.
It's time to code already! Our first module will be very simple and useless. Open your text editor and type in the following, then save it under useless.erl:

他总是文件的第一个属性， 对于这种情况， 有一个很好的原因：它是当前模块的名字，也是用于从其他模块调用函数的名字， M:F(A)的形式调用.
是时候开始编程了，我们的第一个模块将会非常简单，并且没有多大用处。 打开你的文本编辑器， 输入如下代码， 然后保存在userless.erl:

```
-module(useless).
```

This line of text is a valid module. Really! Of course it's useless without functions. Let's first decide what functions will be exported from our 'useless' module. To do this, we will use another attribute:

这行代码就构成了一个合法的模块。 真的！这个模块没有函数。首先，让我们来决定什么函数需要从useless模块导出。要完成这件事， 我们将用到另外一个属性:

```
-export([Function1/Arity, Function2/Arity, ..., FunctionN/Arity]).
```

This is used to define what functions of a module can be called by the outside world. It takes a list of functions with their respective arity. The arity of a function is an integer representing how many arguments can be passed to the function. This is critical information, because different functions defined within a module can share the same name if and only if they have a different arity. The functions add(X,Y) and add(X,Y,Z) would thus be considered different and written in the form add/2 and add/3 respectively.

这用于定义模块的那些函数可以从外部调用。它接受一个包含参数数目的函数列表清单。函数的Arity是表明该函数可以接受几个参数。这是一个至关重要的信息，因为， 只有当同名函数拥有不同的Arity， 才可以在一个模块中定义为不同的函数。函数add(x, y) 和add(x, y, z) 是不同的， 可以简写为add/2 和add/3。

Note: Exported functions represent a module's interface. It is important to define an interface revealing strictly what is necessary for it to be used and nothing more. Doing so lets you fiddle with all the other [hidden] details of your implementation without breaking code that might depend on your module.

注意：被导出的函数代表模块的接口，定义一个清晰的接口对于使用来说非常重要， 所以尽量隐藏不必要的细节。

Our useless module will first export a useful function named 'add', which will take two arguments. The following -export attribute can be added after the module declaration:

我们的useless模块将首先导出一个名叫add的有用的函数，这个函数接受两个参数。下面的-export属性可以被添加到模块声明的后面:

```
-export([add/2]).
```

And now write the function:
然后，实现这个函数：

```
add(A,B) ->
    A + B.
```

The syntax of a function follows the form Name(Args) -> Body., where Name has to be an atom and Body can be one or more Erlang expressions separated by commas. The function is ended with a period. Note that Erlang doesn't use the 'return' keyword. 'Return' is useless! Instead, the last logical expression of a function to be executed will have its value returned to the caller automatically without you having to mention it.

函数的语法遵循这样的形式：Name(Args) -> Body.， Name必须是原子(atom)， Body可以使以逗号分隔的多条语句。 函数以句号结尾。注意erlang不使用return关键字。函数最后执行的表达式将被作为函数的返回值， 自动地返回给调用者。

Add the following function (why yes, every tutorial needs a 'Hello world' example! Even at the fourth chapter!), without forgetting to add it to the -export attribute.

添加下列函数， 并且不要忘了把它加入-export属性中。

```
%% Shows greetings.
%% io:format/1 is the standard function used to output text.
hello() ->
    io:format("Hello, world!~n").
```

What we see from this function is that comments are single-line only and begin with a % sign (using %% is purely a question of style.) The hello/0 function also demonstrates how to call functions from foreign modules inside yours. In this case, io:format/1 is the standard function to output text, as written in the comments.

从greeting函数中， 我们可以看到只能通过前导%单行注释， 使用%%，纯属编程风格不同而已。hello/0函数展示了如何调用外部模块函数。这个例子中， io:format/1是一个用于输出文本的准函数。

A last function will be added to the module, using both functions add/2 and hello/0:

```
greet_and_add_two(X) ->
    hello(),
    add(X,2).
```

![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch1/imports.png?raw=true)

Do not forget to add greet_and_add_two/1 to the exported function list. The calls to hello/0 and add/2 don't need to have the module name prepended to them because they were declared in the module itself.

不要忘记把greet_and_add_two加入导出函数列表中，调用hello/0和add/2不要使用模块的名字，因为他们是在同一个模块中声明的。

Had you wanted to be able to call io:format/1 in the same manner as add/2 or any other function defined within the module, you could have added the following module attribute at the beginning of the file: -import(io, [format/1]).. Then you could have called format("Hello, World!~n"). directly. More generally, the -import attribute follows this recipe:

如果你想以调用`add/2`的方式使用`io:format`, 那你必须在文件开始处，使用`-import(io, [format/1]).`的方式导入外部函数。然后， 你就可以类似`format("hello, world!~n")`方式使用函数。-import属性遵照如下的格式:

```
-import(Module, [Function1/Arity, ..., FunctionN/Arity]).
```

Importing a function is not much more than a shortcut for programmers when writing their code. Erlang programmers are often discouraged from using the -import attribute as some people find it reduces the readability of code. In the case of io:format/2, the function io_lib:format/2 also exists. Finding which one is used means going to the top of the file to see from which module it was imported. Consequently, leaving the module name in is considered good practice. Usually, the only functions you'll see imported come from the lists module: its functions are used with a higher frequency than those from most other modules.

导入一个函数非常简单。Erlang程序员通常不使用import属性，因为有人觉得它降低了可读性。在io:format的例子中， 同时也存在io_lib:format/2函数。要确定使用的那个函数，必须要回到文件的顶部查看到底导入是那个函数。因此，不使用模块被认为是一种不错的实践。通常， 唯一可以放入import属性中的函数来自lists模块， 是因为他们使用非常频繁。

Your useless module should now look like the following file:
现在useless模块看上去像下面这个文件：

```
-module(useless).
-export([add/2, hello/0, greet_and_add_two/1]).
 
add(A,B) ->
    A + B.
 
%% Shows greetings.
%% io:format/1 is the standard function used to output text.
hello() ->
    io:format("Hello, world!~n").
 
greet_and_add_two(X) ->
    hello(),
    add(X,2).
```

We are done with the "useless" module. You can save the file under the name useless.erl. The file name should be the module name as defined in the -module attribute, followed by '.erl', which is the standard Erlang source extension.

我们已经完成了useless，你可以保存为useless.erl, 文件的名字必须和模块的名字一样，然后加上".erl"作为文件扩展名。

Before showing how to compile the module and finally try all its exciting functions, we will see how to define and use macros. Erlang macros are really similar to C's '#define' statements, mainly used to define short functions and constants. They are simple expressions represented by text that will be replaced before the code is compiled for the VM. Such macros are mainly useful to avoid having magic values floating around your modules. A macro is defined as a module attribute of the form: -define(MACRO, some_value). and is used as ?MACRO inside any function defined in the module. A 'function' macro could be written as -define(sub(X,Y), X-Y). and used like ?sub(23,47), later replaced by 23-47 by the compiler. Some people will use more complex macros, but the basic syntax stays the same.

在演示如何编译模块之前， 最后尝试一下所有令人兴奋的函数，我们会明白如何定义和使用宏。Erlang的宏和C语言的'#define'语句非常类似， 主要用于定义短函数和常量值。 在代码编译之前，他们以文本的方式被简单替换。对于在模块中避免大量使用模数值很有用。宏作为模块属性被定义， 形式如：-define(MACRO, some_value). 并且以?MACRO的方式在模块中任何函数中使用。一个“函数”宏可以定义为-define(sub(X, Y), X-Y). 并且以?sub(23, 47)的形式使用， 它会被编译器替换为23-47。一些会使用更为复杂的宏，但是基本语法都是一样。

Compiling the code
---

Erlang code is compiled to bytecode in order to be used by the virtual machine. You can call the compiler from many places: $ erlc flags file.erl when in the command line, compile:file(FileName) when in the shell or in a module, c() when in the shell, etc.

为了能够被Erlang虚拟机使用，Erlang源码会被编译为字节码。你可以采用多种方式调用编译：在命令行中使用$ erlc flags file.erl， 在一个模块内或者在shell中使用compile:file(FileName)， 在shell中使用c().

It's time to compile our useless module and try it. Open the Erlang shell, type in:
是时候该尝试编译我们的useless模块了。打开Erlang shell，输入：

```
1> cd("/path/to/where/you/saved/the-module/").
"Path Name to the directory you are in"
ok
```

By default, the shell will only look for files in the same directory it was started in and the standard library: cd/1 is a function defined exclusively for the Erlang shell, telling it to change the directory to a new one so it's less annoying to browse for our files. Windows users should remember to use forward slashes. When this is done, do the following:

默认情况下，shell只会在当前工作目录下寻找文件，你也可以用Erlang shell提供的cd/1, 进行目录切换。Windows用户需要注意的是使用正斜杠("/")代替路径中的反斜杠("\"）。如果一切就绪，请跟着做:

```
2> c(useless).
{ok,useless}
```

If you have another message, make sure the file is named correctly, that you are in the right directory and that you've made no mistake in your module. Once you successfully compile code, you'll notice that a useless.beam file was added next to useless.erl in your directory. This is the compiled module. Let's try our first functions ever:

如果你得到的是另外的消息， 首先确认文件的名字是否正确、当前工作目录是否正确、确保在模块中没有错误。如果你成功地编译了代码， 你会发现在useless.erl同目录中多出一个useless.beam文件。这就是被编译的模块。让我们来验证函数：

```
3> useless:add(7,2).
9
4> useless:hello().
Hello, world!
ok
5> useless:greet_and_add_two(-3).
Hello, world!
-1
6> useless:not_a_real_function().
** exception error: undefined function useless:not_a_real_function/0
```

The functions work as expected: add/2 adds numbers, hello/0 outputs "Hello, world!", and greet_and_add_two/1 does both! Of course, you might be asking why hello/0 returns the atom 'ok' after outputting text. This is because Erlang functions and expressions must always return something, even if they would not need to in other languages. As such, io:format/1 returns 'ok' to denote a normal condition, the absence of errors.

Add/2 、greet_and_add_two/1和hello/0 都工作正常。 当然你可能会问为什么hello/0函数会在输出文本之后返回'ok'原子？虽然在其他语言中不一定需要返回值， 但是Erlang的函数和表达式必须有返回值。同样的， io:format/2返回ok表示一切正常， 没有错误发生。

Expression 6 shows an error being thrown because a function doesn't exist. If you have forgotten to export a function, this is the kind of error message you will have when trying it out.

表达式 6(译者注：useless:not_a_real_function())抛出了一个错误，因为模块没有这个函数。 如果你忘记了导出这个函数， 当你尝试调用它的时候，你就会收到这样的异常。

Note: If you were ever wondering, '.beam' stands for Bogdan/Björn's Erlang Abstract Machine, which is the VM itself. Other virtual machines for Erlang exist, but they're not really used anymore and are history: JAM (Joe's Abstract Machine, inspired by Prolog's WAM and old BEAM, which attempted to compile Erlang to C, then to native code. Benchmarks demonstrated little benefits in this practice and the concept was given up.

注意: ".beam" 是Bogdan/Björn's Erlang Abstract Machine的缩写， 就是VM本身。也存在其他Erlang虚拟机， 他们已经成为历史，不再使用了， 例如： JAM(Joe's Abstract Machine), 是受到Prolog的WAM 和Old BEAM的启发，JAM首先把Erlang 源码编译为C， 然后再原生代码(native code)， 在实际的性能结果表明这样的流程对性能的提升不够明显， 所以最后放弃了。

There are a whole lot of compilation flags existing to get more control over how a module is compiled. You can get a list of all of them in the Erlang documentation. The most common flags are:

有一些选项可以更细致的控制如何编译一个模块。你可以在Erlang的文档中找到， 其中最常用的列下下面：


-debug_info
    Erlang tools such as debuggers, code coverage and static analysis tools will use the debug information of a module in order to do their work.
-{outdir,Dir}
    By default, the Erlang compiler will create the 'beam' files in the current directory. This will let you choose where to put the compiled file.
-export_all
    Will ignore the -export module attribute and will instead export all functions defined. This is mainly useful when testing and developing new code, but should not be used in production.
-{d,Macro} or {d,Macro,Value}
    Defines a macro to be used in the module, where Macro is an atom. This is more frequently used when dealing when unit-testing, ensuring that a module will only have its testing functions created and exported when they are explicitly wanted. By default, Value is 'true' if it's not defined as the third element of the tuple.

To compile our useless module with some flags, we could do one of the following:
可以选择一些选项进行编译useless模块， 按照如下方法：

```
7> compile:file(useless, [debug_info, export_all]).
{ok,useless}
8> c(useless, [debug_info, export_all]).
{ok,useless}
```

You can also be sneaky and define compile flags from within a module, with a module attribute. To get the same results as from expressions 7 and 8, the following line could be added to the module:

你可以在模块中使用模块属性定义编译标志。如果要和上面的编译效果一样， 可以在模块中加入下面这行代码：

```
-compile([debug_info, export_all]).
```

Then just compile and you'll get the same results as if you manually passed flags. Now that we're able to write down functions, compile them and execute them, it's time to see how far we can take them!

然后只是单单的编译它，就可以达到上面一样的效果。 现在你可以编写函数， 编译 他们，然后执行他们!

Note: another option is to compile your Erlang module to native code. Native code compiling is not available for every platform and OS, but on those that support it, it can make your programs go faster (about 20% faster, based on anecdotal evidence). To compile to native code, you need to use the hipe module and call it the following way: hipe:c(Module,OptionsList). You could also use c(Module,[native]). when in the shell to achieve similar results. Note that the .beam file generated will contain both native and non-native code, and the native part will not be portable across platforms.

注意：另外一些选项是把erlang 模块编译为本地代码。 不是在所有平台和操作上都支持本地代码编译， 但是那些支持的平台上， 它可以使你的程序更快(根据 坊间流传，可能会快20%)。为了能编译成本地代码， 你需要使用hipe模块， 并使用如下方式调用：hipe:c(Module, OptionList)。你也可以用c(Module, [native])。 作为结果， .beam文件将包含本地代码和字节码， 并且本地代码将不是跨平台的。

More About Modules
---

Before moving on to learning more about writing functions and barely useful snippets of code, there are a few other miscellaneous bits of information that might be useful to you in the future that I'd like to discuss.

在继续学习编写函数和代码之前， 有一些繁杂的信息也许对你之后的学习有帮助。

The first one concerns metadata about modules. I mentioned in the beginning of this chapter that module attributes are metadata describing the module itself. Where can we find this metadata when we don't have an access to the source? Well the compiler plays nice with us: when compiling a module, it will pick up most module attributes and store them (along with other information) in a module_info/0 function. You can see the metadata of the useless module the following way:

第一个需要关注的模块元数据。在本章一开始，我提到模块属性是描述模块自身的元数据。当我们没有源代码时，可以在哪里找到这些元数据？编译器对我们 非常友好： 当它编译一个模块的时候， 它会收集所有的模块属性，并且保存在module_info/0函数里面。 你可以用以下方式查看useless模块的属性：

```
9> useless:module_info().
[{exports,[{add,2},
                    {hello,0},
                    {greet_and_add_two,1},
                    {module_info,0},
                    {module_info,1}]},
    {imports,[]},
    {attributes,[{vsn,[174839656007867314473085021121413256129]}]},
    {compile,[{options,[]},
                    {version,"4.6.2"},
                    {time,{2009,9,9,22,15,50}},
                    {source,"/home/ferd/learn-you-some-erlang/useless.erl"}]}]
10> useless:module_info(attributes).
[{vsn,[174839656007867314473085021121413256129]}]
```

The snippet above also shows an additional function, module_info/1 which will let you grab one specific piece of information. You can see exported functions, imported functions (none in this case!), attributes (this is where your custom metadata would go), and compile options and information. Had you decided to add -author("An Erlang Champ"). to your module, it would have ended up in the same section as vsn. There are limited uses to module attributes when it comes to production stuff, but they can be nice when doing little tricks to help yourself out: I'm using them in my testing script for this book to annotate functions for which unit tests could be better; the script looks up module attributes, finds the annotated functions and shows a warning about them.

上面的输出也显示了另外一个函数， module_info/1, 允许你获取特定属性的信息。你可以查看exported function， imported functions， attributes(保存自定义元数据)和编译选项和信息。如果你决定增加-author("An Erlang Champ").到你的模块中， 你可以在module_info/0中找到它。在生产环境中，他们只有有限的用处， 但是有些使用技巧是他可以帮到你自己：我在这本的testing script中使用它来注释哪个单元测试更好。这些script 查找模块属性，找到被注释的函数， 显示关于他们的警告。

Note: vsn is an automatically generated unique value differentiating each version of your code, excluding comments. It is used in code hot-loading (upgrading an application while it runs, without stopping it) and by some tools related to release handling. You can also specify a vsn value yourself if you want: just add -vsn(VersionNumber) to your module.

注意：vsn是自动产生的唯一值， 代码(不包括注释)的每个版本的唯一值都是不同的。他被用于热加载(不宕机更新代码)， 一些工具也会使用它。你也可以通过-vsn(VersionNumber)自己定义vsn值。

A small graph with three nodes: Mom, Dad and You. Mom and Dad are parents of You, and You is brother of Dad. Text under: 'If circular dependencies are digusting in real life, maybe they should be disgusting in your programs too'
Another point that would be nice to approach regards general module design: avoid circular dependencies! A module A should not call a module B that also calls module A. Such dependencies usually end up making code maintenance difficult. In fact, depending on too many modules even if they're not in a circular dependency can make maintenance harder. The last thing you want is to wake up in the middle of the night only to find a maniac software engineer or computer scientist trying to gouge your eyes out because of terrible code you have written.

另外一点关于通用模块设计的方法：避免循环依赖！模块A不应该调用模块B，同时模块B也会调用模块A。 这样的依赖关系是的代码很难维护。 事实上， 即使不存在循环依赖， 如果依赖太多模块也会使得代码难于维护。不要因为你所写的糟糕的代码，而且半夜叫醒。

For similar reasons (maintenance and fear for your eyes), it is usually considered a good practice to regroup functions that have similar roles close together. Starting and stopping an application or creating and deleting a record in some database are examples of such a scenario.

因为相似的原因(便于维护和保护你的眼睛)， 把角色相同的函数组织在一起被认为是一个不错的实践。启动和停止应用， 创建和删除记录就是这样的例子。

Well, that's enough for the pedantic moralizations. How about we explore Erlang a little more?

好吧，教条式说教已经足够多了， 让我们更进一步的探究Erlang，如何？



![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch1/circular-dependencies.png?raw=true)