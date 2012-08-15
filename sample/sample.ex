defmodule Hello do
 def world do
  Erlang.io.format("Hello Elixer.~n", [])
 end
end
Hello.world
