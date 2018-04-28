# TODO introduce lexer object to ... lex
# TODO build abstractions
# TODO allow erlang/elixir calls
# TODO distinguish from immediates and vars better
#      (allow passing immediates from stack to erlang functions for example)
# TODO save immedaites to variables?

# TODO my functions can't return anything yet, whoops

defmodule Aspect.Compiler do
  @builtins %{
    "+" => &Aspect.Compiler.Builtins.plus/3,
    "-" => &Aspect.Compiler.Builtins.minus/3,
    "swap" => &Aspect.Compiler.Builtins.swap/3,
    "dup" => &Aspect.Compiler.Builtins.dup/3,
    ":" => &Aspect.Compiler.Builtins.colon/3
  }

  @parsing_words %{
    ":" => &Aspect.Compiler.Builtins.colon/3,
    "M:" => &Aspect.Compiler.Builtins.set_module/3
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
    # f = fn f, code, ast, stack, ctx ->
    #   # the idea is not to call compile_forms here
    #   # we just want to lex it and put stuff on the stack
    #   # UNLESS we come across a parsing word,
    #   # at that point we should evaluate it
    #   case compile_forms(ast, stack, ctx) do
    #     {code_, [], [], ctx_} ->
    #       {code ++ code_, ctx_}

    #     {code_, ast_, stack_, ctx_} ->
    #       f.(f, code ++ code_, ast_, stack_, ctx_)
    #   end
    # end

    # {code, ctx} = f.(f, [], ast, [], %Ctx{})

    {code, [], ctx} = parse_words(ast, [], %Ctx{})

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

    # TODO need to uncomment this eventaully, when the feature is supported
    #:_eval_expr._eval_expr()

    :code.purge(:_eval_expr)
    :code.delete(:_eval_expr)
  end

  def to_eaf_words(lexer) do
    Aspect.Lexer.remaining_words(lexer)
    |> ast_to_eaf_words()
  end

  def ast_to_eaf_words(ast) do
    f = fn f, code, ast, stack, ctx ->
      case compile_forms(ast, stack, ctx) do
        {code_, [], stack_, ctx_} ->
          {code ++ code_, stack_, ctx_}

        {code_, ast_, stack_, ctx_} ->
          f.(f, code ++ code_, ast_, stack_, ctx_)
      end
    end

    {code, stack, ctx} = f.(f, [], ast, [], %Ctx{})

    # TODO need way to infer stack effects

    {stack_func, [], [], _} = Aspect.Compiler.Builtins.colon(["_eval_expr", "(", "--", ")" | (Enum.reverse(stack) ++ [";"])], [], ctx)

    [
      {:attribute, 1, :file, {'hi.as', 1}},
      {:attribute, 1, :module, :_eval_expr},
      {:attribute, 2, :compile, :export_all}
    ] ++ code ++ stack_func ++ [{:eof, 7}]
  end

  # TODO case

  def compile_word(word, ast, stack, ctx) do
    case builtin(word) do
      {:ok, w} ->
        w.(ast, stack, ctx)

      _ ->
        # other
        num =
          try do
            String.to_integer(word)
          rescue
            ArgumentError -> :error
          end

        case num do
          :error ->
            # TODO better organize
            case String.contains?(word, "/") do
              true ->
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

              false ->
                case Map.get(ctx.words, word) do
                  nil ->
                    throw {:undefined_function, word}
                  {arg_count, ret_count} ->
                    {args, stack_next} = Enum.split(stack, arg_count)
                    ^arg_count = length(args)

                    call = local_call(String.to_atom(word), Enum.map(args, &var/1))

                    {code, stack_, ctx_} = case ret_count do
                                     0 -> {call, stack_next, ctx}
                                     1 ->
                                       {[x], ctxx} = fresh(1, ctx)
                                       {match(var(x), call), [x|stack_next], ctxx}
                                     _ ->
                                       {xs, ctxx} = fresh(ret_count, ctx)
                                       {match(tuple(Enum.map(xs, &var/1)), call), xs ++ stack_next, ctxx}
                                   end

                    {[code], ast, stack_, ctx_}
                end
            end

          _ ->
            {[word], ctxx} = fresh(1, ctx)
            {[match(var(word), {:integer, 9, num})], ast, [word | stack], ctxx}
        end
    end
  end

  def builtin(word) do
    Map.fetch(@builtins, word)
  end

  def parsing_word(word) do
    Map.fetch(@parsing_words, word)
  end

  def parse_words(ast, stack, ctx) do
    f = fn f, code, ast, stack, ctx ->
          case parse_word(ast, stack, ctx) do
            {code_, [], stack_, ctx_} ->
              {code ++ code_, stack_, ctx_}
            {code_, ast_, stack_, ctx_} ->
              f.(f, code ++ code_, ast_, stack_, ctx_)
          end
    end

    f.(f, [], ast, stack, ctx)
  end

  def parse_word([word | ast], stack, ctx) do
    case parsing_word(word) do
        # TODO handle numbers properly here maybe
      :error -> {[], ast, [word | stack], ctx}
      {:ok, f} -> f.(ast, stack, ctx)
    end
  end
  def parse_word([], [], ctx), do: {[], [], [], ctx}

  @spec compile_forms(AST.t(), stack, %Ctx{}) :: {[tuple()], AST.t(), stack, %Ctx{}}

  def compile_forms(["drop" | ast], [_a | stack], ctx) do
    {[], ast, stack, ctx}
  end

  def compile_forms(["parse-token", token | ast], stack, ctx) do
    {[], ast, [token | stack], ctx}
  end

  def compile_forms(["[" | ast], stack, ctx) do
    {quot_r, ast_rest} = parse_quotation(ast, [])
    quot = Enum.reverse(quot_r)
    {[], ast_rest, [quot | stack], ctx}
  end

  def compile_forms(["." | ast], [x | stack], ctx) do
    {[
       mfa_call(:io, :format, [
         {:string, 15, [126, 112, 126, 110]},
         {:cons, 15, var(x), {nil, 15}}
       ])
     ], ast, stack, ctx}
  end

  def compile_forms(["call(" | ast], [quot | stack], ctx) do
    {num_args, num_ret, ast_rest} = parse_effect(["("|ast])

    {arg_vars, ctxx} = fresh(num_args, ctx)

    {args_for_call, stack_rest} = Enum.split(stack, num_args)

    f = fn f, code, ast, stack, ctx ->
      case compile_forms(ast, stack, ctx) do
        {code_, [], ret_args, ctx_} ->
          {code ++ code_, ctx_, ret_args}

        {code_, ast_, stack_, ctx_} ->
          f.(f, code ++ code_, ast_, stack_, ctx_)
      end
    end

    {code, ctxxx, ret_args} = f.(f, [], quot, arg_vars, ctxx)

    full_code = case ret_args do
                  [] -> code
                  [v] -> code ++ [var(v)]
                  s -> code ++ [tuple(Enum.map(s, &var/1))]
                end

    # assert the declared return stack effect is the same
    # as the number of values left on the stack
    ^num_ret = length(ret_args)

    # todo quots that return more than 1
    # todo can look at the effect of the called quot
    # for now let's assume it returns either 0 or 1 val
    call =
      {:call, 12, {:fun, 12, {:clauses, [{:clause, 12, Enum.map(arg_vars, &var/1), [], full_code}]}},
       Enum.map(args_for_call, &var/1)}

    {code_, stack_, ctx_} =
      case ret_args do
        [] ->
          {call, stack_rest, ctxxx}

        [_] ->
          {[v], c} = fresh(1, ctxxx)
          {match(var(v), call), [v | stack_rest], c}

        _ ->
          {vs, c} = fresh(length(ret_args), ctxxx)
          {match(tuple(Enum.map(vs, &var/1)), call), vs ++ stack_rest, c}
      end

    {[code_], ast_rest, stack_, ctx_}
  end

  def compile_forms([x | ast], stack, ctx) do
    compile_word(x, ast, stack, ctx)
  end

  def compile_forms([], [], ctx), do: {[], [], [], ctx}

  # by default we should parse stuff by putting it on the stack

  def parse_body([";" | ast], stack) do
    {stack, ast}
  end

  def parse_body([token | ast], stack) do
    parse_body(ast, [token | stack])
  end

  def parse_quotation(["]" | ast], stack) do
    {stack, ast}
  end

  def parse_quotation([token | ast], stack) do
    parse_quotation(ast, [token | stack])
  end

  def parse_effect(ast) do
    {["("|front], ["--"|rest]} = Enum.split_while(ast, fn x -> x != "--" end)
    {back, [")"|next]} = Enum.split_while(rest, fn x -> x != ")" end)
    {length(front), length(back), next}
  end
end
