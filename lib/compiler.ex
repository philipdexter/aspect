# TODO introduce lexer object to ... lex
# TODO build abstractions
# TODO allow erlang/elixir calls
# TODO distinguish from immediates and vars better
#      (allow passing immediates from stack to erlang functions for example)
# TODO save immedaites to variables?

# TODO my functions can't return anything yet, whoops

defmodule Aspect.Compiler do
  @builtins %{
    # TODO these stack effects are hacks, especially for : and call(
    # need to figure out how to compute stack effects
    # as the parsing goes
    # need to plan how we put stuff on the stack while parsing
    # and put the stack effect there
    "+" => {&Aspect.Compiler.Builtins.plus/3, {2, 1}},
    "-" => {&Aspect.Compiler.Builtins.minus/3, {2, 1}},
    "swap" => {&Aspect.Compiler.Builtins.swap/3, {2, 2}},
    "dup" => {&Aspect.Compiler.Builtins.dup/3, {1, 2}},
    "drop" => {&Aspect.Compiler.Builtins.drop/3, {1, 0}},
    "." => {&Aspect.Compiler.Builtins.pp/3, {1, 0}},
    "[" => {&Aspect.Compiler.Builtins.quot/3, {0, 0}},
    "if" => {&Aspect.Compiler.Builtins.if/3, {3, 1}},
    "call(" => {&Aspect.Compiler.Builtins.call/3, {0, 0}},
    "infer" => {&Aspect.Compiler.Builtins.infer/3, {1, 1}},
  }

  # TODO test empty function definition

  defmodule Ctx do
    defstruct fresh: 0, words: %{}, module_name: "scratchpad"
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
      {:attribute, 2, :compile, :export_all}
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
                                 false -> :func_call
                               end
                    end
          _ -> :number
        end
    end
  end

  def compile_word(word, ast, stack, ctx) do
    case word_type(word) do
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
    end
  end

  def builtin(word) do
    Map.fetch(@builtins, word)
  end


  # TODO compile_forms has to call the lexer directly to get new words
  # this will allow the lexer to call parsing words
  # because right now compile_forms bypasses the lexer

  # TODO macros?

  @spec gen_code(AST.t(), stack, %Ctx{}) :: {[tuple()], stack, %Ctx{}}
  def gen_code(ast, stack, ctx) do
    gen_code_aux([], ast, stack, ctx)
  end
  def gen_code_aux(code, [], stack, ctx) do
    {code, stack, ctx}
  end
  def gen_code_aux(code, ast, stack, ctx) do
    {code_, ast_, stack_, ctx_} = compile_forms(ast, stack, ctx)
    gen_code_aux(code ++ code_, ast_, stack_, ctx_)
  end

  @spec compile_forms(AST.t(), stack, %Ctx{}) :: {[tuple()], AST.t(), stack, %Ctx{}}

  def compile_forms([x | ast], stack, ctx) do
    compile_word(x, ast, stack, ctx)
  end

  def compile_forms([], [], ctx), do: {[], [], [], ctx}
end
