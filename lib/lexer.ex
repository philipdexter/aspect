defmodule Aspect.Lexer do
  alias Aspect.Lexer, as: Lexer

  @opaque t :: %Lexer{}

  defstruct [:words]

  def parse_token(%Lexer{words: [w | ws]} = lexer) do
    {%Lexer{lexer | words: ws}, w}
  end

  @spec remaining_words(t()) :: Aspect.Compiler.AST.t()
  def remaining_words(%Lexer{words: words}), do: words

  @spec from_file(String.t()) :: Lexer.t()
  def from_file(file) do
    words =
      file
      |> File.stream!()
      |> Stream.zip(Stream.iterate(1, &(&1 + 1)))
      |> Stream.flat_map(fn {s, l} ->
        Enum.map(String.split(String.replace_trailing(s, "\n", ""), " "), &{&1, l})
      end)
      |> Stream.filter(fn {s, _} -> s != "" end)
      |> Stream.map(fn {s, _} -> s end)
      |> Enum.to_list()
      |> expand_macros

    %Lexer{words: words}
  end

  defp expand_macros(x), do: x
end
