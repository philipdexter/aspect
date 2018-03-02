defmodule Aspect.CLI do
  def main([file]) do
    Aspect.compile(file)
  end
end
