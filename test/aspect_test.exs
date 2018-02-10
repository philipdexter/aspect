defmodule AspectTest do
  use ExUnit.Case
  doctest Aspect

  test "greets the world" do
    assert Aspect.hello() == :world
  end
end
