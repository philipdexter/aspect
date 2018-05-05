defmodule Aspect.Lexer do
  alias Aspect.Lexer, as: Lexer

  @parsing_words %{
    ":" => &Aspect.Compiler.Builtins.colon/3,
    "M:" => &Aspect.Compiler.Builtins.set_module/3,
    "parse-token" => &Aspect.Compiler.Builtins.parse_token/3,
  }

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

    %Lexer{words: words}
  end

  @spec from_string(String.t()) :: Lexer.t()
  def from_string(string) do
    {:ok, s} = StringIO.open(string)

    words =
      s
      |> IO.binstream(:line)
      |> Stream.zip(Stream.iterate(1, &(&1 + 1)))
      |> Stream.flat_map(fn {s, l} ->
        Enum.map(String.split(String.replace_trailing(s, "\n", ""), " "), &{&1, l})
      end)
      |> Stream.filter(fn {s, _} -> s != "" end)
      |> Stream.map(fn {s, _} -> s end)
      |> Enum.to_list()

    %Lexer{words: words}
  end

  def parsing_word(word) do
    Map.fetch(@parsing_words, word)
  end

  def parse_words(ast, stack, ctx) do
    parse_words_aux([], ast, stack, ctx)
  end
  def parse_words_aux(code, [], stack, ctx) do
    {code, stack, ctx}
  end
  def parse_words_aux(code, ast, stack, ctx) do
    {code_, ast_, stack_, ctx_} = parse_word(ast, stack, ctx)
    parse_words_aux(code ++ code_, ast_, stack_, ctx_)
  end

  def parse_word([], [], ctx), do: {[], [], [], ctx}
  def parse_word([word | ast], stack, ctx) do
    case parsing_word(word) do
      :error ->
        # TODO handle numbers properly here maybe
        {[], ast, [word | stack], ctx}

      {:ok, f} ->
        f.(ast, stack, ctx)
    end
  end

end
