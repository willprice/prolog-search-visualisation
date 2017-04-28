# Development

## Environment setup

Dependencies:

* [npm](https://www.npmjs.com/)
* [yarn](https://yarnpkg.com/en/)
* [swipl](http://www.swi-prolog.org/)
* [webpack](https://webpack.github.io/)

Set up development environment
```
# Install frontend packages
$ yarn

# Terminal 1: Launch prolog server with debugging topics enabled
$ ./serve-debug.sh

# Terminal 2: start module bundler (watches files and rebundles)
$ webpack

# Open browser: http://localhost:4000
```

## Frontend

Currently the frontend is fundamentally flawed in its architecture. Bottom
level objects need to run asynchronous methods, which means asynchronicity
permeates the whole call stack, except it doesn't because I didn't realise this
until halfway through implementation, so it is buggy, we invoke asynchronous
methods that return promises without waiting for them to resolve. The
implementation is meant to animate the path, however due to this "run async
methods and don't wait approach" we end up running all queued animations at
once which makes it appear as if no animation is happening. I think the whole
thing needs rethinking and building from scratch with careful consideration of
where asynchronous behaviour takes place, ideally contained rather than
everywhere. 

Konva.js is used for animation, we create tweens; tweens can only be executed
asynchronously, but they have a `onFinish` callback, we wrap this in a promise
interface so that a UI component can queue an animation, receive a promise that
will resolve on animation completion. The idea of `AnimationQueue` was for
a bunch of concurrent animations to be queued then run simultaneously, but
I don't think I've used it that way.

The knife in the heart of the current implementation is the use of promises in
`PubSub` class which all low level classes (`Agent`, `Cell`) depend upon--this
means the promises propagate up through the stack, and the lack of types makes
it incredibly easy to forget to wait on a promise and instead carry on like you
just invoked a synchronous method.

An architecture like I have implemented could be tolerable if the `async` and
`await` keyword were used, but I wanted to avoid transpilation 

## Backend

The architecture is quite nice, I'm pleased with it, some parameters could be
wrapped up into records, the `cost/6` predicate in the `search_strategy` record
in `search.pl` needs refactoring, I think it knows about too much; its interface could
be reduced.

### Tests

There are some tests in `search.plt` which can be run using `make test`

### Docs

There are some API docs, they can be viewed by running `swipl -f doc_server.pl`
and visiting http://localhost:5000
