-module(pow).
-author("piotrek").

-export([power/2]).


power(_, 0) -> 1;
power(0, _) -> 0;
power(X, Y) -> X * power(X, Y - 1).
