# TODO introduce lexer object to ... lex
# TODO build abstractions
# TODO allow erlang/elixir calls
# TODO distinguish from immediates and vars better
#      (allow passing immediates from stack to erlang functions for example)
# TODO save immedaites to variables?

defmodule Aspect.Compiler do
  defmodule Ctx do
    defstruct fresh: 0
  end

  defp mfa_call(module, function, args) do
    {:call, 1, {:remote, 1, {:atom, 1, module}, {:atom, 1, function}}, args}
  end

  defp match(lhs, rhs) do
    {:match, 1, lhs, rhs}
  end

  def compile(file) do
    IO.puts("Compiling #{file}")

    file
    |> File.stream!()
    |> Stream.zip(Stream.iterate(1, &(&1 + 1)))
    |> Stream.flat_map(fn {s, l} ->
      Enum.map(String.split(String.replace_trailing(s, "\n", ""), " "), &{&1, l})
    end)
    |> Stream.filter(fn {s, _} -> s != "" end)
    |> Stream.map(fn {s, _} -> s end)
    |> Enum.to_list()
    |> to_erlang_abstract_format
  end

  def load(forms) do
    case :compile.forms(forms, [:return_errors, :return_warnings]) do
      {:ok, mod, binary, _warnings} -> :code.load_binary(mod, 'hi.as', binary)
    end
  end

  def cl(file) do
    load(compile(file))
  end

  defp fresh(0, ctx), do: {[], ctx}

  defp fresh(num, %Ctx{fresh: fresh} = ctx) do
    fresh..(fresh + num - 1)
    |> Enum.map(fn x -> :erlang.list_to_atom('X' ++ :erlang.integer_to_list(x)) end)
    |> (fn x -> {x, %Ctx{ctx | fresh: fresh + num}} end).()
  end

  def to_erlang_abstract_format(ast) do
    f = fn f, code, ast, stack, ctx ->
      case compile_forms(ast, stack, ctx) do
        {code_, [], [], _} ->
          code ++ code_

        {code_, ast_, stack_, ctx_} ->
          f.(f, code ++ code_, ast_, stack_, ctx_)
      end
    end

    code = f.(f, [], ast, [], %Ctx{})

    [
      {:attribute, 1, :file, {"hi.as", 1}},
      {:attribute, 1, :module, :hi},
      {:attribute, 2, :compile, :export_all}
    ] ++ code ++ [{:eof, 7}]
  end

  def compile_forms(["swap" | ast], [a, b | stack], ctx) do
    {[], ast, [b, a | stack], ctx}
  end

  def compile_forms(["+" | ast], [a, b | stack], ctx) do
    # TODO: this assumes + takes 2 arguments
    # and returns 1
    {[x], ctxx} = fresh(1, ctx)
    {[match({:var, 6, x}, {:op, 6, :+, {:var, 6, a}, {:var, 6, b}})], ast, [x | stack], ctxx}
  end

  def compile_forms([":", func_name | ast], stack, ctx) do
    {arg_count, ast_body} = parse_effect(ast)
    {arg_vars, ctxx} = fresh(arg_count, ctx)
    {body_r, ast_rest} = parse_body(ast_body, [])
    body = Enum.reverse(body_r)

    f = fn f, code, ast, stack, ctx ->
      case compile_forms(ast, stack, ctx) do
        {code_, [], [], ctx_} ->
          {code ++ code_, ctx_}

        {code_, ast_, stack_, ctx_} ->
          f.(f, code ++ code_, ast_, stack_, ctx_)
      end
    end

    {code, ctxxx} = f.(f, [], body, arg_vars, ctxx)

    # todo ensure arg_count number of variables are left
    # on the stack
    {[
       {:function, 5, String.to_atom(func_name), arg_count,
        [{:clause, 5, Enum.map(arg_vars, fn var -> {:var, 5, var} end), [], code}]}
     ], ast_rest, stack, ctxxx}
  end

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
    # TODO use mfa_call
    {[
       mfa_call(:io, :format, [
         {:string, 15, [126, 112, 126, 110]},
         {:cons, 15, {:var, 15, x}, {nil, 15}}
       ])
     ], ast, stack, ctx}
  end

  def compile_forms(["call(" | ast], [quot | stack], ctx) do
    {num_args, ast_rest} = parse_effect(ast)

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

    # todo quots that return more than 1
    # todo can look at the effect of the called quot
    # for now let's assume it returns either 0 or 1 val
    call =
      {:call, 12,
       {:fun, 12,
        {:clauses, [{:clause, 12, Enum.map(arg_vars, fn var -> {:var, 12, var} end), [], code}]}},
       Enum.map(args_for_call, fn var -> {:var, 12, var} end)}

    {code_, stack_} =
      case ret_args do
        [] ->
          {call, stack_rest}

        [_] ->
          {match({:var, 12, :A}, call), [:A | stack_rest]}
      end

    {[code_], ast_rest, stack_, ctxxx}
  end

  def compile_forms([x | ast], stack, ctx) do
    num =
      try do
        String.to_integer(x)
      rescue
        ArgumentError -> :error
      end

    case num do
      :error ->
        # TODO better organize
        case String.contains?(x, "/") do
          true ->
            # call setup
            [arg_count, return_count] =
              x
              |> String.split("/")
              |> Enum.map(&elem(Integer.parse(&1), 0))

            [func | ast_next] = ast
            [m, f] = String.split(func, ":")
            {args, stack_next} = Enum.split(stack, arg_count)
            # TODO capture return variables!!!
            # TODO if return greater than 1 then extract tuple
            {[
               {:call, 15,
                {:remote, 15, {:atom, 15, String.to_atom(m)}, {:atom, 15, String.to_atom(f)}},
                Enum.map(args, fn var -> {:var, 5, var} end)}
             ], ast_next, stack_next, ctx}

          false ->
            # TODO assuming takes 3 args and returns 0
            # fix!!
            [a, b, c | stack_rest] = stack

            {[
               {:call, 9, {:atom, 9, String.to_atom(x)},
                [{:integer, 9, a}, {:var, 9, b}, {:var, 9, c}]}
             ], ast, stack_rest, ctx}
        end

      _ ->
        {[var], ctxx} = fresh(1, ctx)
        {[match({:var, 6, var}, {:integer, 9, num})], ast, [var | stack], ctxx}
    end
  end

  def compile_forms([], [], _), do: []

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

  defp parse_effect(["(" | ast]) do
    parse_effect(ast)
  end

  defp parse_effect([")" | ast]), do: {0, ast}

  defp parse_effect(["--" | ast]) do
    {_, next} = parse_effect(ast)
    {0, next}
  end

  defp parse_effect([_ | ast]) do
    {count, next} = parse_effect(ast)
    {count + 1, next}
  end

  @builtins %{
    +: {&Aspect.Compiler.compile_plus/2, {2, 1}}
  }

  def infer(x) do
    case Map.get(@builtins, x) do
      # TODO: just assumming it's an integer here, bad!
      nil ->
        {0, 1}

      {_, io} ->
        io
    end
  end

  def compile_plus(ast, [a, b | stack]) do
    {[{:op, 6, :+, {:var, 6, a}, {:var, 6, b}}], ast, stack}
  end
end
