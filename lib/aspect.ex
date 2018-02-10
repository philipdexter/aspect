defmodule Aspect do
  def compile(file) do
    forms = Aspect.Compiler.compile file
    IO.inspect forms
  end
end
