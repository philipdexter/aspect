defmodule AspectTest do
  use ExUnit.Case
  doctest Aspect

  import Aspect.Compiler

  setup do
    on_exit fn ->
      :code.purge(:hi)
      :code.delete(:hi)
    end
  end

  test "can compile and load an empty module and then unload it" do
    {:module, :hi} = load(compile_string(""))
    assert {:file, 'hi.as'} == :code.is_loaded(:hi)
  end

  test "can perform arithmetic" do
    load(compile_string("""
    : add ( -- x ) 2 1 + ;
    : subtract ( -- x ) 2 1 - ;
    """))
    assert :hi.add() == 3
    assert :hi.subtract() == 1
  end

  test "can call defined words" do
    load(compile_string("""
    : push2 ( -- x y ) 1 2 ;
    : call ( -- x ) push2 + ;
    """))
    assert :hi.call() == 3
  end

  test "can define words with arguments" do
    load(compile_string("""
    : add1 ( x -- x+1 ) 1 + ;
    : add2 ( x -- x ) add1 add1 ;
    """))
    assert :hi.add1(2) == 3
    assert :hi.add2(2) == 4
  end

end
