defmodule AspectTest do
  use ExUnit.Case
  import ExUnit.CaptureIO
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

  test "ordering of aspect stack to elixir tuples" do
    load(compile_string("""
    : give2 ( -- x x ) 1 2 ;
    : drop1 ( x y -- x ) drop ;
    """))
    assert {2, 1} == :scratchpad.give2()
    assert 1 == :scratchpad.drop1(2, 1)
  end

  test "empty function" do
    load(compile_string("""
    : blank ( -- ) ;
    """))
    assert :ok == :scratchpad.blank()
  end

  test "carry-through functions" do
    load(compile_string("""
    : carry ( x -- x ) ;
    : carry ( x y -- x y ) ;
    : carry ( x y z -- x y z ) ;
    : withcarry3 ( -- x ) 1 2 3 - - ;
    """))
    assert 1 == :scratchpad.carry(1)
    assert {1, 2} == :scratchpad.carry(1, 2)
    assert {1, 2, 3} == :scratchpad.carry(1, 2, 3)
    assert 2 == :scratchpad.withcarry3()
  end

  test "can set the module name" do
    load(compile_string("""
    M: test
    : it ( -- x ) 1 ;
    """))
    assert {:file, 'hi.as'} == :code.is_loaded(:test)
    assert :test.it == 1
  end

  test "dup" do
    load(compile_string("""
    : d ( -- x x ) 1 dup ;
    """))
    assert {1, 1} == :scratchpad.d()
  end

  test "eval string" do
    f = fn ->
      eval_string(": h ( -- 1 ) 1 ; h 2 + .")
    end
    assert "Compiling string\n3\n" == capture_io(f)
  end

  test "if" do
    load(compile_string("""
    : true ( -- x ) :true [ 1 ] [ 2 ] if ;
    : false ( -- x ) :false [ 1 ] [ 2 ] if ;
    : check ( x -- y ) [ 1 ] [ 2 ] if ;
    """))
    assert 1 == :scratchpad.true()
    assert 2 == :scratchpad.false()
    assert 1 == :scratchpad.check(:true)
    assert 2 == :scratchpad.check(:false)
  end
end
