defmodule Aspect.Run do
  def run(ast) do
    run(ast, [1, 2])
  end

  def run([], stack), do: stack

  def run(['swap' | ast], [a, b | stack]) do
    run(ast, [b, a | stack])
  end

  def run(['+' | ast], [a, b | stack]) do
    run(ast, [a + b | stack])
  end
end
