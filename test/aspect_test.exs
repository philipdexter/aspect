defmodule AspectTest do
  use ExUnit.Case
  import ExUnit.CaptureIO
  doctest Aspect


  import Aspect.Compiler

  def unload(module) do
    :code.purge(module)
    :code.delete(module)
  end

  setup do
    on_exit fn ->
      unload(:scratchpad)
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
    : pass2 ( x y -- x y ) ;
    """))
    assert {2, 1} == :scratchpad.give2()
    assert 1 == :scratchpad.drop1(2, 1)
    assert {1, 2} == :scratchpad.pass2(1, 2)
  end

  test "passthrough stack" do
    load(compile_string("""
    : and1 ( x -- x 1 ) 1 ;
    : and2 ( x y -- x y 1 2 ) 1 2 ;
    """))
    assert {1, 2} == :scratchpad.and1(2)
    assert {2, 1, 3, 4} == :scratchpad.and2(3, 4)
  end

  test "parse token" do
    load(compile_string("""
    : pt ( -- x ) parse-token 2 drop 1 ;
    """))
    assert 1 == :scratchpad.pt()
  end

  test "parse token in quot" do
    f = fn ->
      eval_string("[ parse-token 2 drop 1 ] call( -- x ) .")
    end
    assert "Compiling string\n1\n" == capture_io(f)
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

  test "quot and call" do
    load(compile_string("""
    : hello ( -- x x ) [ 1 2 + ] call( -- x ) [ dup 1 + ] call( x -- x x ) ;
    """))
    assert {4, 3} == :scratchpad.hello()
  end

  test "return quot" do
    load(compile_string("""
    : quot1 ( -- x ) [ 1 + ] ;
    : quot2 ( -- x ) [ - ] ;
    """))
    assert 5 == :scratchpad.quot1().(4)
    assert 1 == :scratchpad.quot1().(3, 2)
  end

  test "if" do
    load(compile_string("""
    : true ( -- x ) :true [ 1 ] [ 2 ] if ;
    : false ( -- x ) :false [ 1 ] [ 2 ] if ;
    : check ( x -- y ) [ 1 ] [ 2 ] if ;
    : quot ( -- x ) :true [ [ 1 ] ] [ [ 2 ] ] if ;
    """))
    assert 1 == :scratchpad.true()
    assert 2 == :scratchpad.false()
    assert 1 == :scratchpad.check(:true)
    assert 2 == :scratchpad.check(:false)
    assert 1 == :scratchpad.quot().()
  end

  test "infer" do
    load(compile_string("""
    : a ( -- x ) [ 1 ] infer ;
    : b ( -- x ) [ :hi ] infer ;
    : c ( -- x ) [ a b ] infer ;
    : d ( -- x ) [ + + ] infer ;
    : e ( -- x ) [ 1 + 2 + ] infer ;
    : f ( -- x ) [ + 2 + ] infer ;
    : g ( -- x ) [ [ 1 2 + ] ] infer ;
    : h ( -- x ) [ 1 [ 1 2 + ] ] infer ;
    """))
    assert {0, 1} == :scratchpad.a()
    assert {0, 1} == :scratchpad.b()
    assert {0, 2} == :scratchpad.c()
    assert {3, 1} == :scratchpad.d()
    assert {2, 1} == :scratchpad.f()
    assert {0, 1} == :scratchpad.g()
    assert {0, 2} == :scratchpad.h()
  end

  test "empty and cons" do
    load(compile_string("""
    : a ( -- x ) empty ;
    : b ( -- x ) 1 empty | ;
    : c ( -- x ) empty 1 swap | ;
    : d ( -- x ) 1 2 3 empty | | | ;
    """))
    assert [] == :scratchpad.a()
    assert [1] == :scratchpad.b()
    assert [1] == :scratchpad.c()
    assert [1,2,3] == :scratchpad.d()
  end

  test "syntax" do
    load(compile_string("""
    M: syntax SYNTAX: hi '1' swap | empty ;
    """))
    load(compile_string("""
    DEP: syntax : test ( -- x ) hi ;
    """))
    unload(:syntax)
    assert 1 == :scratchpad.test()
  end

  test "macro" do
    load(compile_string("""
    M: macros MACRO: hi ( -- x ) '1' empty | ;
    """))
    load(compile_string("""
    DEP: macros : test ( -- x ) hi ;
    """))
    unload(:macros)
    assert 1 == :scratchpad.test()
  end

  test "macro 2" do
    load(compile_string("""
    M: macros MACRO: hi ( x y -- x ) drop empty | ;
    """))
    load(compile_string("""
    DEP: macros : test ( -- x ) 1 2 hi ;
    """))
    unload(:macros)
    assert 1 == :scratchpad.test()
  end

  test "macro 3" do
    load(compile_string("""
    M: macros MACRO: hi ( x y -- x ) empty | | ;
    """))
    load(compile_string("""
    DEP: macros : test ( -- x y ) 1 2 hi ;
    """))
    unload(:macros)
    assert {2, 1} == :scratchpad.test()
  end

  test "static call" do
    load(compile_string("""
    : f ( -- x ) [ 1 2 + ] call ;
    : g ( -- x ) [ [ 1 ] ] call call ;
    """))
    assert 3 == :scratchpad.f()
    assert 1 == :scratchpad.g()
  end
end
