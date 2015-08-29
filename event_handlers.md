Event Handlers(translate imcompletely)
===
Handle This! *pumps shotgun*
---

There is a certain thing that I've avoided getting into in a few of the previous examples. If you look back at the reminder app, you'll see that I somehow mentioned that we could notify clients, whether they were IM, mail, etc. In the previous chapter, our trading system used io:format/2 to notify people of what was going on.

我们回顾一下前面的一个例子。如果你回顾一下reminder app的例子， 你就会发现我以某种方式提示过我们可以通知客户端，不管他们是IM，还是Mail, 或者其他。 在前一章，我们的trading system使用io:format
/2的方式告诉人们发生了什么事。

You can probably see the common link between both cases. They're all about letting people (or some process or application) know about an event that happened at some point in time. In one case, we only output the results while in the other, we took the Pid of subscribers before sending them a message.

你也许会发现这两个例子的相似性。他们都是关于在某一时刻让人们知道发生了什么事。在这个例子， 我们仅仅输出了结果， 在另外一个例子中，在发送他们消息之前，我们记下了订阅者的PID信息。

The output approach is minimalist and can not be extended with ease. The one with subscribers is certainly valid. In fact, it's pretty useful when each of the subscribers has a long-running operation to do after receiving an event. In simpler cases, where you do not necessarily want a standby process waiting for events for each of the callbacks, a third approach can be taken.

输出的途径是最小的，并且不能容易的扩展。事实上， 当每一个订阅者在接收到事件之后，需要很长一段时间进行处理，这样的方式是很有用的。在一个简单的例子中， 如果你不需要一个备用的进程等待处理进程， 有第三种方式可以选择。

This third approach simply takes a process which accepts functions and lets them run on any incoming event. This process is usually called an event manager and it might end up looking a bit like this:

这第三种选择是简单用一个进程接受所有的输入事件。 这个进程通常被称为了事件管理器和它看起来可能会像这样：

![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch16/event-manager.png?raw=true)

Doing things that way has a few advantages:
我这样做有以下几个优点：

- If your server has many subscribers, it can keep going because it only needs to forward events once
- 如果你的服务有多个订阅者，它仍然能很好的工作，因为他们只需要发送事件一次。

- If there is a lot of data to be transferred, it's only done once and all callbacks operate on that same instance of the data
- 如果要传输很多数据， 它只会传递一次，并且所有的callback只会在同一个数据实例上执行。

- You don't need to spawn processes for short lived tasks
- 你不需要产生进程对于那些短暂生命任务。

And of course there are a few downsides too:
同样也有一些缺点：

- If all functions need to run for a long time, they're going to block each other. This can be prevented by actually having the function forward the event to a process, basically turning the event manager as an event forwarder (something similar to what we did for the reminder app)
- 如果所有的函数需要运行很长一段时间，他们会柱塞其他的函数。这可以通过这个函数把事件发送给一个进程，简单的把事件管理器当作一个事件转发器。
- In fact, a function that loops indefinitly can prevent any new event from being handled until something crashes.
- 事实上，对于一个不确定循环的函数可以任何新事件被处理，直到发生崩溃。

There is a way to solve these downsides, which is a bit underwhelming. Basically, you have to turn the event manager approach into the subscriber one. Luckily, the event manager approach is flexible enough to do it with ease and we'll see how to do it later in this chapter.

有一个让人影响深刻的方法可以解决这些缺点。简单地，你必须把时间管理器方式改变为订阅方式，幸运的是， 事件管理器方式足够简单，足够灵活，我们会在这章后面部分看到。

I usually write a very basic version of the OTP behaviour we'll see in pure Erlang beforehand, but in this case, I'll instead come straight to the point. Here comes gen_event.

有一个让人影响深刻的方法可以解决这些缺点。简单地，你必须把时间管理器方式改变为订阅方式，幸运的是， 事件管理器方式足够简单，足够灵活，我们会在这章后面部分看到。

Generic Event Handlers
---
The gen_event behaviour differs quite a bit from the gen_server and gen_fsm behaviours in that you are never really starting a process. The whole part I've described above about 'accepting a callback' is the reason for this. The gen_event behaviour basically runs the process that accepts and calls functions, and you only provide a module with these functions. This is to say, you have nothing to do with regards to event manipulation except give your callback functions in a format that pleases the event manager. All managing is done for free; you only provide what's specific to your application. This is not really surprising given OTP is, again, all about separating what's generic from specific.

Gen_event行为模式和gen_server和gen_fsm只有一些细微的不同， gen_event不需要真正的启动一个进程。上面的描述中已经很好的解释了。gen_event主要运行一个接受和调用函数的进程，你只需要提供一个export这些函数的模块。也就是说， 你只要提供一些要求格式的回调函数。所有的管理都完成，并且都是免费的。

This separation, however, means that the standard spawn -> init -> loop -> terminate pattern will only be applied to event handlers. Now if you recall what has been said before, event handlers are a bunch of functions running in the manager. This means the currently presented model:

Spawn-> init->loop->terminate的模式只会应用到事件处理器。现在如果你回想之前我们提到， 事件处理只是一些在manager中运行的函数。这就意味着当前模型:

![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch16/common-pattern.png?raw=true)

Switches to something more like this for the programmer:
 可能会变成这样：

![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch16/event-handler-pattern.png?raw=true)

Each event handler can hold its own state, carried around by the manager for them. Each event handler can then take the form:
每个event handler 都可以持有自己的状态， 可以被manager随身携带。每个event handler看起来像这样：

![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch16/event-handler-pattern-local.png?raw=true)

This is nothing too complex so let's get it on with the event handlers' callbacks themselves.
那我们开始解释一下关于event handler的callback的含义。

init and terminate

The init and terminate functions are similar to what we've seen in the previous behaviours with servers and finit state machines. init/1 takes a list of arguments and returns {ok, State}. Whatever happens in init/1 should have its counterpart in terminate/2.
init和terminate函数和gen_server和gen_fsm含义很相似。 Init/1接受一个list并且返回{ok，State}.  Terminate 和init很相似

handle_event

The handle_event(Event, State) function is more or less the core of gen_event's callback modules. It works like gen_server's handle_cast/2 in that it works asynchronously. It differs with regards to what it can return though:
是gen_event的核心。他和gen_server的handle_cast很像，都是异步工作的。只是他的返回值略显不同：

{ok, NewState}
{ok, NewState, hibernate}, which puts the event manager itself into hibernation until the next event
remove_handler
{swap_handler, Args1, NewState, NewHandler, Args2}
A sleeping polar bear with a birthday hat

![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch16/hibernate.png?raw=true)

The tuple {ok, NewState} works in a way similar to what we've seen with gen_server:handle_cast/2; it simply updates its own state and doesn't reply to anyone. In the case of {ok, NewState, hibernate} it is to be noted that the whole event manager is going to be put in hibernation. Remember that event handlers run in the same process as their manager. Then remove_handler drops the handler from the manager. This can be useful whenever your event handler knows its done and it has nothing else to do. Finally, there's {swap_handler, Args1, NewState, NewHandler, Args2}. This one is not used too frequently, but what it does is remove the current event handler and replace it with a new one. This is done by first calling CurrentHandler:terminate(Args1, NewState) and removing the current handler, then adding a new one by calling NewHandler:init(Args2, ResultFromTerminate). This can be useful in the cases where you know some specific event happened and you're better of giving control to a new handler. This is likely the kind of thing where you'll simply know when you need it. Again, it's not that frequently used.

{ok, NewState}只是简单的更新自己的状态和不回答任何人。{ok, NewState, hibernate}导致event manager进入休眠状态。记住所有的event handler中在同一个进程中运行。Remove_handler表示丢弃该handler。对于handler知道自己的使命已经完成并且没有其他事情可做的时候，这个值会派上用场。{swap_handler, Args1, NewState, NewHander, Args2}不是很常用，它的作用是一个新的handler代替当前handler。它是通过首先调用CurrentHandler:terminate(Args1, NewState)，然后删除当前handler， 最后调用NewHandler:init(Args2, ResulteFromTerminate)来完成。它适用于你知道一些特别的事件发生， 并且有更合适的handler来处理这些事件。

All incoming events can come from gen_event:notify/2 which is asynchronous like gen_server:cast/2 is. There is also gen_event:sync_notify/2 which is synchronous. This is a bit funny to say, because handle_event/2 remains asynchronous. The idea here is that the function call only returns once all event handlers have seen and treated the new message. Until then, the event manager will keep blocking the calling process by not replying.

handle_call

This is similar to a gen_server's handle_call callback, except that it can return {ok, Reply, NewState}, {ok, Reply, NewState, hibernate}, {remove_handler, Reply} or {swap_handler, Reply, Args1, NewState, Handler2, Args2}. The gen_event:call/3-4 function is used to make the call.

该回调函数于gen_server的handle_call回调函数很相似，除了它的返回不一样外， 它的返回可能值为{ok, Reply, NewState}, {ok, Reply, NewState, hiberate}, {remove_handler, Reply}, {swap_handler, Reply, Args1, NewState, Handler2, Args2}。 gen_event:call/3-4函数用于发出请求。

This raises a question. How does this work when we have something like 15 different event handlers? Do we expect 15 replies or just one that contains them all? Well, in fact we'll be forced to choose only one handler to reply to us. We'll get into the details of how this is done when we actually see how to attach handlers to our event manager, but if you're impatient, you can look at how the function gen_event:add_handler/3 works to try to figure it out.

那么问题来了。当我们有15种不同event handlers，它是如何运转的。是我们分别进行15返回， 或者返回包含所有的一个Reply？实现上，我们将强制选择一个来进行Reply。当我们亲眼看见如何向我们的事件管理附加event handler的时候，你就会了解细节。如果你实在等不及， 你可以先阅读gen_event:add_handler/3的文档。

handle_info

The handle_info/2 callback is pretty much the same as handle_event (same return values and everything), with the exception that it only treats out of band messages, such as exit signals, messages sent directly to the event manager with the ! operator, etc. It has use cases similar to those of handle_info in gen_server and in gen_fsm.

handle_info/2 回调函数和handle_event非常相似（返回值都一样），唯一不同的地方是它只接受没有匹配到消息， 例如退出信号， 通过！操作符直接向event manager发送的消息等。使用方式和gen_server和gen_fsm中的handle_info类似。

code_change

Code change works in exactly the same manner as it does for gen_servers, except it's for each individual event handler. It takes 3 arguments, OldVsn, State, and Extra, which are in order, the version number, the current handler's state and data we can ignore for now. All it needs to do is return {ok, NewState}.

同gen_server中code change方式一样，但是在event handler 中他们独立的。 它接受3个参数， OldVsn， State 和Extra， 需要返回{ok, NewState}。

It's Curling Time!
---

With the callbacks seen, we can start looking at implementing something with gen_event. For this part of the chapter, I've chosen to make a set of event handlers used to track game updates of one of the most entertaining sports in the world: curling.
If you've never seen or played curling before (which is a shame!), the rules are relatively simple:

作为回顾，我们开始使用gen_event来实现例子。这部分，我选择实现一些event handler来跟踪一项世界级的娱乐比赛的比赛进程：冰壶。
如果你不了解冰壶运动， 规则相对简单：

![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch16/curling-ice.png?raw=true)

You have two teams and they try to send a curling stone sliding on the ice, trying to get to the middle of the red circle. They do this with 16 stones and the team with the stone closest to the center wins a point at the end of the round (called an end). If the team has the two closest stones, it earns two points, and so on. There are 10 ends and the team with the most points at the end of the 10 ends wins the game.

两个团队竞赛， 他们在冰上射出一个冰壶，尽量停在红圈的中央。他们用16次机会，最接近中心的队伍赢得一分，如果一队中有两个冰壶最接近中心，那么将赢得两分。赢得分数最多的获胜。

There are more rules making the game more fascinating, but this is a book on Erlang, not extremely fascinating winter sports. If you want to learn more about the rules, I suggest you head up to the Wikipedia article on curling.

当然，还有很多其他规则，但是在一本Erlang的书籍中不太合适详细介绍，所以如果需要更详细规则可以从Wikipedia 上获取。

For this entirely real-world-relevant scenario, we'll be working for the next winter Olympic Games. The city where everything happens are just done building the arena where the matches will take place and they're working on getting the scoreboard ready. It turns out that we have to program a system that will let some official enter game events, such as when a stone has been thrown, when a round ends or when a game is over, and then route these events to the scoreboard, a stats system, news reporters' feeds, etc.

对于现实世界的相关场景， 我们将为下一届冬季奥运会工作。 比赛城市的比赛场地已全面完工，但是出了比分显示牌还没有完工。结果是，我们必须编写一个程序，好让一些工作人员可以输入一些相关事件， 例如一只冰壶被投出，或者一个轮比赛结束，或者整个比赛结束，可以把这些事件显示在比分牌上，或者发送给另外的系统， 或者是一个新闻专栏等。

Being as clever as we are, we know this is a chapter on gen_event and deduce we will likely use it to accomplish our task. We won't implement all the rules given this is more of an example, but feel free to do so when we're done with the chapter. I promise not to be mad.

如果我们足够聪明， 我们知道这章是关于gen_event，可以推断出我们将很可能使用gen_event来完成该任务。我们不需要实现所有的规则，当这章结束的时候，我们会感觉很轻松。我保证这个不是发疯了。

We'll start with the scoreboard. Because they're installing it right now, we'll make use of a fake module that would usually let us interact with it, but for now it'll only use standard output to show what's going on. This is where curling_scoreboard_hw.erl comes in:

我们将开始我们的积分牌项目，因为它们已经安装好了， 所以需要先创建一些假的模块，并与他们交互，并且也是简单的在标准输出上打印这些消息。以下是curling_scoreboard_hw.erl模块的内容：

```
-module(curling_scoreboard_hw).
-export([add_point/1, next_round/0, set_teams/2, reset_board/0]).
 
%% This is a 'dumb' module that's only there to replace what a real hardware
%% controller would likely do. The real hardware controller would likely hold
%% some state and make sure everything works right, but this one doesn't mind.
 
%% Shows the teams on the scoreboard.
set_teams(TeamA, TeamB) ->
    io:format("Scoreboard: Team ~s vs. Team ~s~n", [TeamA, TeamB]).
 
next_round() ->
    io:format("Scoreboard: round over~n").
 
add_point(Team) ->
    io:format("Scoreboard: increased score of team ~s by 1~n", [Team]).
 
reset_board() ->
    io:format("Scoreboard: All teams are undefined and all scores are 0~n").
```

So this is all the functionality the scoreboard has. They usually have a timer and other awesome functionalities, but whatever. Seems like the Olympics committee didn't feel like having us implementing trivialities for a tutorial.

这就是积分牌的所有函数。只有一个计时器和其他了不起的函数。似乎美国奥林匹克委员会不想拥有实现琐事教程。

This hardware interface lets us have a little bit of design time to ourselves. We know that there are a few events to handle for now: adding teams, going to the next round, setting the number of points. We will only use the reset_board functionality when starting a new game and won't need it as part of our protocol. The events we need might take the following form in our protocol:

硬件接口导致我们需要花一点时间用于设计。现在我们需要处理一些事件：添加比赛队伍，进行下一轮比赛， 设置比分。当开始新一轮比赛时，我们用reset_board函数来表示， 并且不需要把它作为我们协定的一部分。需要处理的事件按照下列形式列在我们的协议里面：

{set_teams, TeamA, TeamB}, where this is translated to a single call to curling_scoreboard_hw:set_teams(TeamA, TeamB);
{add_points, Team, N}, where this is translated to N calls to curling_scoreboard_hw:add_point(Team);
next_round, which gets translated to a single call with the same name.
We can start our implementation with this basic event handler skeleton:

```
-module(curling_scoreboard).
-behaviour(gen_event).
 
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
terminate/2]).
 
init([]) ->
    {ok, []}.
 
handle_event(_, State) ->
    {ok, State}.
 
handle_call(_, State) ->
    {ok, ok, State}.
 
handle_info(_, State) ->
    {ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(_Reason, _State) ->
    ok.
```

This is a skeleton that we can use for every gen_event callback module out there. For now, the scoreboard event handler itself won't need to do anything special except forward the calls to the hardware module. We expect the events to come from gen_event:notify/2, so the handling of the protocol should be done in handle_event/2. The file curling_scoreboard.erl shows the updates:

对于所有gen_event 回调模块我们都可以采用这个代码框架结构。现在，我们积分牌事件处理器只是简单的向前调用硬件模块。可以使用gen_event:notify/2 来产生事件，所以协定的事件处理函数只需要在handle_event/2中处理。 curling_scoreboard.erl代码更新如下：

```
-module(curling_scoreboard).
-behaviour(gen_event).
 
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
terminate/2]).
 
init([]) ->
    {ok, []}.
 
handle_event({set_teams, TeamA, TeamB}, State) ->
    curling_scoreboard_hw:set_teams(TeamA, TeamB),
    {ok, State};
handle_event({add_points, Team, N}, State) ->
    [curling_scoreboard_hw:add_point(Team) || _ <- lists:seq(1,N)],
    {ok, State};
handle_event(next_round, State) ->
    curling_scoreboard_hw:next_round(),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.
 
handle_call(_, State) ->
    {ok, ok, State}.
 
handle_info(_, State) ->
    {ok, State}.
```

You can see the updates done to the handle_event/2 function. Trying it:
你可以发现在handle_event/2函数中的变更:

```
1> c(curling_scoreboard_hw).
{ok,curling_scoreboard_hw}
2> c(curling_scoreboard).
{ok,curling_scoreboard}
3> {ok, Pid} = gen_event:start_link().
{ok,<0.43.0>}
4> gen_event:add_handler(Pid, curling_scoreboard, []).
ok
5> gen_event:notify(Pid, {set_teams, "Pirates", "Scotsmen"}).
Scoreboard: Team Pirates vs. Team Scotsmen
ok
6> gen_event:notify(Pid, {add_points, "Pirates", 3}).
ok
Scoreboard: increased score of team Pirates by 1
Scoreboard: increased score of team Pirates by 1
Scoreboard: increased score of team Pirates by 1
7> gen_event:notify(Pid, next_round).
Scoreboard: round over
ok
8> gen_event:delete_handler(Pid, curling_scoreboard, turn_off).
ok
9> gen_event:notify(Pid, next_round).
ok
```

A few things are going on here. The first of them is that we're starting the gen_event process as a standalone thing. We then attach our event handler to it dynamically with gen_event:add_handler/3. This can be done as many times as you want. However, as mentioned in the handle_call part earlier, this might cause problems when you want to work with a particular event handler. If you want to call, add or delete a specific handler when there's more than one instance of it, you'll have to find a way to uniquely identify it. My favorite way of doing it (one that works great if you don't have anything more specific in mind) is to just use make_ref() as a unique value. To give this value to the handler, you add it by calling add_handler/3 as gen_event:add_handler(Pid, {Module, Ref}, Args). From this point on, you can use {Module, Ref} to talk to that specific handler. Problem solved.

有几件事需要交代一下 ，第一我们启动了一个gen_event 进程， 我们通过gen_event:add_handler/2动态的向该进程附加了一个事件处理器。 你可以附加同一个时间处理器多次。当操作特定事件处理器时，这将会引起一些问题。当存在一个事件处理器的多个时，你需要调用，添加或者删除该特定事件处理， 你需要找到一个唯一标识它的途径。我最喜欢的方式是通过make_ref()产生一个唯一值。使用gen_event:add_handler(Pid,{Module,Ref}, Args)来添加事件处理器。通过{Module, Ref}可以和特定的处理器通信。 所以问题得以解决。

![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch16/curling-stone.png?raw=true)

Anyway, you can then see that we send messages to the event handler, which successfully calls the hardware module. We then remove the handler. Here, turn_off is an argument to the terminate/2 function, which our implementation currently doesn't care about. The handler is gone, but we can still send events to the event manager. Hooray.

不管怎么样， 我可以在屏幕上看到我们发给事件处理器的消息， 他们表示我们成功调用了硬件模块。然后我们删除事件处理器。这里， turn_off 将会传递给terminate/2函数，并作为一个参数， 但我们的实现忽略了这个参数。当handler删除之后， 我们仍然可以向事件管理器发送消息。

One awkward thing with the code snippet above is that we're forced to call the gen_event module directly and show everyone what our protocol looks like. A better option would be to provide an abstraction module on top of it that just wraps all we need. This will look a lot nicer to everyone using our code and will, again, let us change the implementation if (or when) we need to do it. It will also let us specify what handlers are necessary to include for a standard curling game:

上面代码有一件尴尬的事情，我们必须直接调用gen_event模块，并且必须向所有人公布我们的协议。更好的选择是我们提供一个抽象的模块来封装这些所有的细节。这也许会对我有使用我们的代码的人会更好一些。如果我们要修改内部协议结构，这样做也会有所帮助。这样也是我们可以指定哪个handler是必须要包含在我们标准的冰壶比赛中:

```
-module(curling).
-export([start_link/2, set_teams/3, add_points/3, next_round/1]).
 
start_link(TeamA, TeamB) ->
    {ok, Pid} = gen_event:start_link(),
    %% The scoreboard will always be there
    gen_event:add_handler(Pid, curling_scoreboard, []),
    set_teams(Pid, TeamA, TeamB),
    {ok, Pid}.
 
set_teams(Pid, TeamA, TeamB) ->
    gen_event:notify(Pid, {set_teams, TeamA, TeamB}).
 
add_points(Pid, Team, N) ->
    gen_event:notify(Pid, {add_points, Team, N}).
 
next_round(Pid) ->
    gen_event:notify(Pid, next_round).
```

And now running it:

```
1> c(curling).
{ok,curling}
2> {ok, Pid} = curling:start_link("Pirates", "Scotsmen").
Scoreboard: Team Pirates vs. Team Scotsmen
{ok,<0.78.0>}
3> curling:add_points(Pid, "Scotsmen", 2).
Scoreboard: increased score of team Scotsmen by 1
Scoreboard: increased score of team Scotsmen by 1
ok
4> curling:next_round(Pid).
Scoreboard: round over
ok
```

Some kind of weird looking alien sitting on a toilet, surprised at the newspapers it is reading
This doesn't look like much of an advantage, but it's really about making it nicer to use that code (and reduces the possibilities of writing the messages wrong).

这看上不是很高级，但是这确实使我们使用那段代码很优雅了。

Alert the Press!
---

![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch16/alert.png?raw=true)

We've got the basic scoreboard done, now we want international reporters to be able to get live data from our official in charge of updating our system. Because this is an example program, we won't go through the steps of setting up a socket and writing a protocol for the updates, but we'll put the system in place to do it by putting an intermediary process in charge of it.

我们已经完成了基础的记分牌， 现在我么需要一个国际的报告器，能够从主办方办公室接收现场数据来更新系统。由于这是一个实例程序， 我们不打算完全实现socket 和编写与更新相关的协议，而是简单的向标准输出中打印消息来模拟这个过程。

Basically, whenever a news organization feels like getting into the game feed, they'll register their own handler that just forwards them the data they need. We'll effectively going to turn our gen_event server into some kind of message hub, just routing them to whoever needs them.

The first thing to do is update the curling.erl module with the new interface. Because we want things to be easy to use, we'll only add two functions, join_feed/2 and leave_feed/2. Joining the feed should be doable just by inputting the right Pid for the event manager and the Pid to forward all the events to. This should return a unique value that can then be used to unsubscribe from the feed with leave_feed/2:

```
%% Subscribes the pid ToPid to the event feed.
%% The specific event handler for the newsfeed is
%% returned in case someone wants to leave
join_feed(Pid, ToPid) ->
    HandlerId = {curling_feed, make_ref()},
    gen_event:add_handler(Pid, HandlerId, [ToPid]),
    HandlerId.
 
leave_feed(Pid, HandlerId) ->
    gen_event:delete_handler(Pid, HandlerId, leave_feed).
```

Note that I'm using the technique described earlier for multiple handlers ({curling_feed, make_ref()}). You can see that this function expects a gen_event callback module named curling_feed. If I only used the name of the module as a HandlerId, things would have still worked fine,except that we would have no control on which handler to delete when we're done with one instance of it. The event manager would just pick one of them in an undefined manner. Using a Ref makes sure that some guy from the Head-Smashed-In Buffalo Jump press leaving the place won't disconnect a journalist from The Economist (no idea why they'd do a report on curling, but what do you know). Anyway, here is the implementation I've made of the curling_feed module:

```
-module(curling_feed).
-behaviour(gen_event).
 
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
terminate/2]).
 
init([Pid]) ->
    {ok, Pid}.
 
handle_event(Event, Pid) ->
    Pid ! {curling_feed, Event},
    {ok, Pid}.
 
handle_call(_, State) ->
    {ok, ok, State}.
 
handle_info(_, State) ->
    {ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(_Reason, _State) ->
    ok.
```

The only interesting thing here is still the handle_event/2 function, which blindly forwards all events to the subscribing Pid. Now when we use the new modules:

```
1> c(curling), c(curling_feed).
{ok,curling_feed}
2> {ok, Pid} = curling:start_link("Saskatchewan Roughriders", "Ottawa Roughriders").
Scoreboard: Team Saskatchewan Roughriders vs. Team Ottawa Roughriders
{ok,<0.165.0>}
3> HandlerId = curling:join_feed(Pid, self()).
{curling_feed,#Ref<0.0.0.909>}
4> curling:add_points(Pid, "Saskatchewan Roughriders", 2).
Scoreboard: increased score of team Saskatchewan Roughriders by 1
ok
Scoreboard: increased score of team Saskatchewan Roughriders by 1
5> flush().
Shell got {curling_feed,{add_points,"Saskatchewan Roughriders",2}}
ok
6> curling:leave_feed(Pid, HandlerId).
ok
7> curling:next_round(Pid).
Scoreboard: round over
ok
8> flush().
ok
```

And we can see that we added ourselves to the feed, got the updates, then left and stopped receiving them. You can actually try to add many processes many times and it will work fine.

This introduces a problem though. What if one of the curling feed subscribers crashes? Do we just keep the handler going on there? Ideally, we wouldn't have to. In fact, we don't have to. All that needs to be done is to change the call from gen_event:add_handler/3 to gen_event:add_sup_handler/3. If you crash, the handler is gone. Then on the opposite end, if the gen_event manager crashes, the message {gen_event_EXIT, Handler, Reason} is sent back to you so you can handle it. Easy enough, right? Think again.

Don't Drink Too Much Kool-Aid

![](https://github.com/by46/learn_you_some_erlang/blob/master/images/ch16/leash.png?raw=true)

It might have happened at some time in your childhood that you went to your aunt or grandmother's place for a party or something. If you were mischievous in any way, you would have several adults looking over you, on top of your parents. Now if you ever did something wrong, you would get scolded by your mom, dad, aunt, grandmother and then everyone would keep telling you after that even though you already clearly knew you had done something wrong. Well gen_event:add_sup_handler/3 is a bit like that; no, seriously.

Whenever you use gen_event:add_sup_handler/3, a link is set up between your process and the event manager so both of them are supervised and the handler knows if its parent process fails. If you recall the Errors and Processes chapter and its section on monitors, I have mentioned that monitors are great for writing libraries which need to know what's going on with other processes because they can be stacked, at the opposite of links. Well gen_event predates the appearance of monitors in Erlang and a strong commitment to backwards compatibility introduced this pretty bad wart. Basically, because you could have the same process acting as the parent of many event handlers, so the library doesn't ever unlink the processes (except when it terminates for good) just in case. Monitors would actually solve the problem, but they aren't being used there.

This mean that everything goes alright when your own process crashes: the supervised handler is terminated (with the call to YourModule:terminate({stop, Reason}, State)). Everything goes alright when your handler itself crashes (but not the event manager): you will receive {gen_event_EXIT, HandlerId, Reason}. When the event manager is shut down though, you will either:

Receive the {gen_event_EXIT, HandlerId, Reason} message then crash because you're not trapping exits;
Receive the {gen_event_EXIT, HandlerId, Reason} message, then a standard 'EXIT' message that is either superfluous or confusing.
That's quite a wart, but at least you know about it. You can try and switch your event handler to a supervised one if you feel like it. It'll be safer even if it risks being more annoying in some cases. Safety first.

We're not done yet! what happens if some member of the media is not there on time? We need to be able to tell them from the feed what the current state of the game is. For this, we'll write an additional event handler named curling_accumulator. Again, before writing it entirely, we might want to add it to the curling module with the few calls we want:

```
-module(curling).
-export([start_link/2, set_teams/3, add_points/3, next_round/1]).
-export([join_feed/2, leave_feed/2]).
-export([game_info/1]).
 
start_link(TeamA, TeamB) ->
    {ok, Pid} = gen_event:start_link(),
    %% The scoreboard will always be there
    gen_event:add_handler(Pid, curling_scoreboard, []),
    %% Start the stats accumulator
    gen_event:add_handler(Pid, curling_accumulator, []),
    set_teams(Pid, TeamA, TeamB),
    {ok, Pid}.
 
%% skipping code here
 
%% Returns the current game state.
game_info(Pid) ->
    gen_event:call(Pid, curling_accumulator, game_data).
```

A thing to notice here is that the game_info/1 function uses only curling_accumulator as a handler id. In the cases where you have many versions of the same handler, the hint about using make_ref() (or any other means) to ensure you write to the right handler still holds. Also note that I made the curling_accumulator handler start automatically, much like the scoreboard. Now for the module itself. It should be able to hold state for the curling game: so far we have teams, score and rounds to track. This can all be held in a state record, changed on each event received. Then, we will only need to reply to the game_data call, as below:

```
-module(curling_accumulator).
-behaviour(gen_event).
 
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
terminate/2]).
 
-record(state, {teams=orddict:new(), round=0}).
 
init([]) ->
    {ok, #state{}}.
 
handle_event({set_teams, TeamA, TeamB}, S=#state{teams=T}) ->
    Teams = orddict:store(TeamA, 0, orddict:store(TeamB, 0, T)),
    {ok, S#state{teams=Teams}};
handle_event({add_points, Team, N}, S=#state{teams=T}) ->
    Teams = orddict:update_counter(Team, N, T),
    {ok, S#state{teams=Teams}};
handle_event(next_round, S=#state{}) ->
    {ok, S#state{round = S#state.round+1}};
handle_event(_Event, Pid) ->
    {ok, Pid}.
 
handle_call(game_data, S=#state{teams=T, round=R}) ->
    {ok, {orddict:to_list(T), {round, R}}, S};
handle_call(_, State) ->
    {ok, ok, State}.
 
handle_info(_, State) ->
    {ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(_Reason, _State) ->
    ok.
```

So you can see we basically just update the state until someone asks for game details, at which point we'll be sending them back. We did this in a very basic way. A perhaps smarter way to organize the code would have been to simply keep a list of all the events to ever happen in the game so we could send them back at once each time a new process subscribes to the feed. This won't be needed here to show how things work, so let's focus on using our new code:

```
1> c(curling), c(curling_accumulator).
{ok,curling_accumulator}
2> {ok, Pid} = curling:start_link("Pigeons", "Eagles").
Scoreboard: Team Pigeons vs. Team Eagles
{ok,<0.242.0>}
3> curling:add_points(Pid, "Pigeons", 2).
Scoreboard: increased score of team Pigeons by 1
ok
Scoreboard: increased score of team Pigeons by 1
4> curling:next_round(Pid).
Scoreboard: round over
ok
5> curling:add_points(Pid, "Eagles", 3).
Scoreboard: increased score of team Eagles by 1
ok
Scoreboard: increased score of team Eagles by 1
Scoreboard: increased score of team Eagles by 1
6> curling:next_round(Pid).
Scoreboard: round over
ok
7> curling:game_info(Pid).
{[{"Eagles",3},{"Pigeons",2}],{round,2}}
```

Enthralling! Surely the Olympic committee will love our code. We can pat ourselves on the back, cash in a fat check and go play videogames all night now.

We haven't seen all there is to do with gen_event as a module. In fact, we haven't seen the most common use of event handlers: logging and system alarms. I decided against showing them because pretty much any other source on Erlang out there uses gen_event strictly for that. If you're interested in going there, check out error_logger first.

Even if we've not seen the most common uses of gen_event, it's important to say that we've seen all the concepts necessary to understanding them, building our own and integrating them into our applications. More importantly, we've finally covered the three main OTP behaviours used in active code development. We still have a few behaviours left to visit—those that act as a bunch of glue between all of our worker processes—such as the supervisor.