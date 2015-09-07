Learn you some erlang
===
Content
---
* [Modules](modules.md)
* [Syntax in Functions](syntax_in_functions.md)
* [Types](types.md)
* [Recursion](recursion.md)
* [Higher Order Functions](higher_order_functions.md)
* [Errors and Exceptions](errors_and_exceptions.md)
* [Event Handlers](event_handlers.md)

#! /bin/bash

if [ $# != 1 ]
then
	echo "usage: mem.sh [pid]"
	exit 1
fi

FNAME=mem.txt
FIVEM=300

ps u -p 1 | head -1 > $FNAME

while true
do
	ps h u -p $1 >> $FNAME
	sleep $FIVEM
done
