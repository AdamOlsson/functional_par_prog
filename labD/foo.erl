-module(foo).
-compile(export_all).

foo() -> io:format(user, "Hello Node!",[]).

main() -> foo().