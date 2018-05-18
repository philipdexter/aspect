# TODO introduce lexer object to ... lex
# TODO build abstractions
# TODO allow erlang/elixir calls
# TODO distinguish from immediates and vars better
#      (allow passing immediates from stack to erlang functions for example)
# TODO save immedaites to variables?

# TODO my functions can't return anything yet, whoops

defmodule Aspect.Compiler do
  @builtins %{
    # TODO these stack effects are hacks, especially for call(
    "+" => {&Aspect.Compiler.Builtins.plus/3, {2, 1}},
    "-" => {&Aspect.Compiler.Builtins.minus/3, {2, 1}},
    "swap" => {&Aspect.Compiler.Builtins.swap/3, {2, 2}},
    "dup" => {&Aspect.Compiler.Builtins.dup/3, {1, 2}},
    "drop" => {&Aspect.Compiler.Builtins.drop/3, {1, 0}},
    "." => {&Aspect.Compiler.Builtins.pp/3, {1, 0}},
    "if" => {&Aspect.Compiler.Builtins.if/3, {3, 1}},
    "call(" => {&Aspect.Compiler.Builtins.call/3, {0, 0}},
    "infer" => {&Aspect.Compiler.Builtins.infer/3, {1, 1}},
    "|" => {&Aspect.Compiler.Builtins.cons/3, {2,1}},
    "empty" => {&Aspect.Compiler.Builtins.empty/3, {0,1}},
  }

  defmodule Ctx do
    defstruct fresh: 0, words: %{}, syntax: MapSet.new(), macros: MapSet.new(), module_name: "scratchpad",
      parsing_words: %{
        ":" => &Aspect.Compiler.Builtins.colon/3,
        "M:" => &Aspect.Compiler.Builtins.set_module/3,
        "SYNTAX:" => &Aspect.Compiler.Builtins.syntax/3,
        "MACRO:" => &Aspect.Compiler.Builtins.macro/3,
        "parse-token" => &Aspect.Compiler.Builtins.parse_token/3,
        "DEP:" => &Aspect.Compiler.Builtins.dep/3,
        "[" => &Aspect.Compiler.Builtins.quot/3,
      },
      macro_words: %{}
  end

  defmodule AST do
    @type t :: [word]
    @type word :: String.t()
  end

  @type stack :: [atom]

  @spec mfa_call(atom, atom, [tuple]) :: tuple
  def mfa_call(module, function, args) do
    {:call, 1, {:remote, 1, {:atom, 1, module}, {:atom, 1, function}}, args}
  end

  def local_call(function, args) do
    {:call, 1, {:atom, 1, function}, args}
  end

  def match(lhs, rhs) do
    {:match, 1, lhs, rhs}
  end

  def var(x) do
    {:var, 1, x}
  end

  def tuple(elems) do
    {:tuple, 1, elems}
  end

  def compile_string(string) do
    IO.puts("Compiling string")

    string
    |> Aspect.Lexer.from_string()
    |> to_eaf
  end

  def compile(file) do
    IO.puts("Compiling #{file}")

    file
    |> Aspect.Lexer.from_file()
    |> to_eaf
  end

  def load(forms) do
    case :compile.forms(forms, [:return_errors, :return_warnings]) do
      {:ok, mod, binary, _warnings} -> :code.load_binary(mod, 'hi.as', binary)
    end
  end

  def to_file(forms) do
    case :compile.forms(forms, [:return_errors, :return_warnings]) do
      {:ok, mod, binary, _warnings} ->
        {:ok, file} = File.open(Atom.to_string(mod) <> ".beam", [:write])
        :ok = IO.binwrite(file, binary)
        :ok = File.close(file)
    end
  end

  def cs(file) do
    to_file(compile(file))
  end

  def css(string) do
    to_file(compile_string(string))
  end

  def cl(file) do
    load(compile(file))
  end

  def fresh(0, ctx), do: {[], ctx}

  def fresh(num, %Ctx{fresh: fresh} = ctx) do
    fresh..(fresh + num - 1)
    |> Enum.map(fn x -> :erlang.list_to_atom('X' ++ :erlang.integer_to_list(x)) end)
    |> (fn x -> {x, %Ctx{ctx | fresh: fresh + num}} end).()
  end

  def define_with_effect(function_name, num_args, num_ret, %Ctx{words: words} = ctx) do
    %Ctx{ctx | words: Map.put(words, function_name, {num_args, num_ret})}
  end

  def add_parsing_words(syntax_atom_list, mod_atom, %Ctx{parsing_words: parsing_words} = ctx) do
    %Ctx{ctx | parsing_words: List.foldl(syntax_atom_list, parsing_words, fn s, map ->
            Map.put(map, Atom.to_string(s), fn a, b, c -> apply(mod_atom, s, [a, b, c]) end)
          end)}
  end

  def add_macros(macros_atom_list, mod_atom, %Ctx{macro_words: macro_words} = ctx) do
    %Ctx{ctx | macro_words: List.foldl(macros_atom_list, macro_words, fn {s, args}, map ->
            Map.put(map, Atom.to_string(s), {mod_atom, s, args})
          end)}
  end

  def define_syntax(name, %Ctx{syntax: syntax} = ctx) do
    %Ctx{ctx | syntax: MapSet.put(syntax, name)}
  end

  def define_macro(name, args, %Ctx{macros: macros} = ctx) do
    %Ctx{ctx | macros: MapSet.put(macros, {name, args})}
  end

  @spec to_eaf(Aspect.Lexer.t()) :: [tuple()]
  def to_eaf(lexer) do
    Aspect.Lexer.remaining_words(lexer)
    |> ast_to_eaf_module()
  end

  def ast_to_eaf_module(ast) do
    {code, [], ctx} = Aspect.Lexer.parse_words(ast, [], %Ctx{})

    [
      {:attribute, 1, :file, {'hi.as', 1}},
      {:attribute, 1, :module, String.to_atom(ctx.module_name)},
      {:attribute, 2, :compile, :export_all},
      {:attribute, 3, :words, Enum.map(ctx.words, fn ({word, _}) -> String.to_atom(word) end)},
      {:attribute, 3, :syntax, Enum.map(ctx.syntax, fn (word) -> String.to_atom(word) end)},
      {:attribute, 3, :macros, Enum.map(ctx.macros, fn ({word, args}) -> {String.to_atom(word), args} end)},
    ] ++ code ++ [{:eof, 7}]
  end

  def eval_string(string) do
    IO.puts("Compiling string")

    code =
      string
      |> Aspect.Lexer.from_string()
      |> to_eaf_words

    load(code)

    # use apply to avoid the 'module is not available' warning
    apply(:_eval_expr, :_eval_expr, [])

    :code.purge(:_eval_expr)
    :code.delete(:_eval_expr)

    :ok
  end

  def to_eaf_words(lexer) do
    Aspect.Lexer.remaining_words(lexer)
    |> ast_to_eaf_words()
  end

  def ast_to_eaf_words(ast) do
    {code, stack, ctx} = Aspect.Lexer.parse_words(ast, [], %Ctx{})

    # TODO need way to infer stack effects

    {stack_func, [], [], _} =
      Aspect.Compiler.Builtins.colon(
        ["_eval_expr", "(", "--", ")" | Enum.reverse(stack) ++ [";"]],
        [],
        ctx
      )

    [
      {:attribute, 1, :file, {'hi.as', 1}},
      {:attribute, 1, :module, :_eval_expr},
      {:attribute, 2, :compile, :export_all}
    ] ++ code ++ stack_func ++ [{:eof, 7}]
  end

  @spec word_type(String.t()) :: :atom | :call_setup | :func_call | :number | :builtin
  def word_type(word) do
    # if it's a list, it must (maybe? hopefully) be a quotation
    case is_list(word) do
      true -> :quot
      false ->
        case builtin(word) do
          {:ok, _} -> :builtin
          _ ->
            num? =
              try do
                String.to_integer(word)
              rescue
                ArgumentError -> :error
              end

            case num? do
              :error -> case String.starts_with?(word, ":") do
                          true -> :atom
                          false -> case String.contains?(word, "/") do
                                     true -> :call_setup
                                     false -> case String.starts_with?(word, "'") do
                                                true -> :charlist
                                                false -> :func_call
                                              end
                                   end
                        end
              _ -> :number
            end
        end
    end
  end

  def quot_to_eaf([]), do: {:nil, 1}
  def quot_to_eaf([x | xs]), do: {:cons, 1, quot_to_eaf_single(x), quot_to_eaf(xs)}
  def quot_to_eaf_single(x), do: {:bin, 1, [{:bin_elelment, 1, {:string, 1, String.to_charlist(x)}, :default, :default}]}

  def eaf_to_quot({:nil, _}), do: []
  def eaf_to_quot({:cons, _, x, xs}), do: [eaf_to_quot_single(x) | eaf_to_quot(xs)]
  def eaf_to_quot_single({:bin, _, [{:bin_elelment, _, {:string, _, x}, :default, :default}]}), do: :erlang.list_to_binary(x)

  def compile_word(word, ast, stack, ctx) do
    case word_type(word) do
      :quot ->
        # TODO need some way to return quots
        # maybe put tagged stuff onto stack
        # e.g., {:number, 3}
        #       {:var,    :X0}
        #       {:quot,   {:cons, blah}}
        # that way we can react differently and don't have to match everything
        # to vars
        {[], ast, [word | stack], ctx}
      :builtin ->
        {:ok, {w, _}} = builtin(word)
        w.(ast, stack, ctx)
      :atom ->
        {[x], ctxx} = fresh(1, ctx)
        {[match(var(x), {:atom, 1, String.to_atom(String.slice(word, 1..-1))})], ast, [x | stack], ctxx}
      :call_setup ->
        # call setup
        [arg_count, return_count] =
          word
          |> String.split("/")
          |> Enum.map(&elem(Integer.parse(&1), 0))

        [func | ast_next] = ast
        # TODO maybe borrow from elixir, use dot and somehow
        # distinguish beween elixir, erlang, and aspect calls
        [m, f] = String.split(func, ":")
        {args, stack_next} = Enum.split(stack, arg_count)
        ^arg_count = length(args)

        case return_count do
          0 ->
            {[
              mfa_call(String.to_atom(m), String.to_atom(f), Enum.map(args, &var/1))
            ], ast_next, stack_next, ctx}

          1 ->
            {[x], ctxx} = fresh(1, ctx)

            {[
              match(
                var(x),
                mfa_call(String.to_atom(m), String.to_atom(f), Enum.map(args, &var/1))
              )
            ], ast_next, [x | stack_next], ctxx}

          _ ->
            {xs, ctxx} = fresh(return_count, ctx)

            {[
              match(
                tuple(Enum.map(xs, &var/1)),
                mfa_call(String.to_atom(m), String.to_atom(f), Enum.map(args, &var/1))
              )
            ], ast_next, xs ++ stack_next, ctxx}
        end
      :func_call ->
        case Map.get(ctx.words, word) do
          nil ->
            throw({:undefined_function, word})

          {arg_count, ret_count} ->
            {args, stack_next} = Enum.split(stack, arg_count)
            ^arg_count = length(args)

            call = local_call(String.to_atom(word), Enum.map(args, &var/1))

            {code, stack_, ctx_} =
              case ret_count do
                0 ->
                  {call, stack_next, ctx}

                1 ->
                  {[x], ctxx} = fresh(1, ctx)
                  {match(var(x), call), [x | stack_next], ctxx}

                _ ->
                  {xs, ctxx} = fresh(ret_count, ctx)
                  {match(tuple(Enum.map(xs, &var/1)), call), xs ++ stack_next, ctxx}
              end

            {[code], ast, stack_, ctx_}
        end
      :number ->
        {[x], ctxx} = fresh(1, ctx)
        {[match(var(x), {:integer, 1, String.to_integer(word)})], ast, [x | stack], ctxx}
      :charlist ->
        # TODO
        # fix string handling, should do something in the parser which
        # when i sees ' (or "?) it parses until the closing one and treats
        # that as a word
        # TODO
        # charlist vs string? if we return a charlist here then the parser pukes
        {[x], ctxx} = fresh(1, ctx)
        {[match(var(x), {:string, 1, String.trim(word, "'")})], ast, [x | stack], ctxx}
    end
  end

  def builtin(word) do
    Map.fetch(@builtins, word)
  end

  @spec gen_code(AST.t(), stack, %Ctx{}) :: {[tuple()], stack, %Ctx{}}
  def gen_code(ast, stack, ctx) do
    gen_code_aux([], expand_macros(ast, ctx), stack, ctx)
  end
  def gen_code_aux(code, [], stack, ctx) do
    {code, stack, ctx}
  end
  def gen_code_aux(code, ast, stack, ctx) do
    {code_, ast_, stack_, ctx_} = compile_forms(ast, stack, ctx)
    gen_code_aux(code ++ code_, ast_, stack_, ctx_)
  end

  # TODO if quots become own item on stack, need to recurse into them

  def expand_macros(ast, ctx), do: expand_macros_aux([], ast, ctx)

  def expand_macros_aux(l, [], _), do: Enum.reverse(l)
  def expand_macros_aux(l, [x | ast], %Ctx{macro_words: macro_words} = ctx) do
    case Map.get(macro_words, x) do
      nil ->
        expand_macros_aux([x | l], ast, ctx)
      spec ->
        {new_l, added} = expand_macro(l, spec)
        expand_macros_aux(new_l, added ++ ast, ctx)
    end
  end

  def expand_macro(l, {mod_atom, f, arg_count}) do
    {args, rest} = Enum.split(l, arg_count)
    # TODO no really a quote, just an array of words
    quot = apply(mod_atom, f, args)
    {rest, quot}
  end

  @spec compile_forms(AST.t(), stack, %Ctx{}) :: {[tuple()], AST.t(), stack, %Ctx{}}
  def compile_forms([x | ast], stack, ctx) do
    compile_word(x, ast, stack, ctx)
  end

  def compile_forms([], [], ctx), do: {[], [], [], ctx}
end
