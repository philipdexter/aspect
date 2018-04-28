defmodule AspectTest do
  use ExUnit.Case
  doctest Aspect

  import Aspect.Compiler

  setup do
    on_exit fn ->
      :code.purge(:scratchpad)
      :code.delete(:scratchpad)
    end
  end

  test "can compile and load an empty module and then unload it" do
    {:module, :scratchpad} = load(compile_string(""))
    assert {:file, 'hi.as'} == :code.is_loaded(:scratchpad)
  end

  test "can perform arithmetic" do
    load(compile_string("""
    : add ( -- x ) 2 1 + ;
    : subtract ( -- x ) 2 1 - ;
    """))
    assert :scratchpad.add() == 3
    assert :scratchpad.subtract() == 1
  end

  test "can call defined words" do
    load(compile_string("""
    : push2 ( -- x y ) 1 2 ;
    : call ( -- x ) push2 + ;
    """))
    assert :scratchpad.call() == 3
  end

  test "can define words with arguments" do
    load(compile_string("""
    : add1 ( x -- x+1 ) 1 + ;
    : add2 ( x -- x ) add1 add1 ;
    """))
    assert :scratchpad.add1(2) == 3
    assert :scratchpad.add2(2) == 4
  end

  test "can set the module name" do
    load(compile_string("""
    M: test
    : it ( -- x ) 1 ;
    """))
    assert {:file, 'hi.as'} == :code.is_loaded(:test)
    assert :test.it == 1
  end

end
