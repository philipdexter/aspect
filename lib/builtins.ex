defmodule Aspect.Compiler.Builtins do
  import Aspect.Compiler

  def plus(ast, [a, b | stack], ctx) do
    {[x], ctxx} = fresh(1, ctx)
    {[match(var(x), {:op, 6, :+, varize(b), varize(a)})], ast, [x | stack], ctxx}
  end

  def minus(ast, [a, b | stack], ctx) do
    {[x], ctxx} = fresh(1, ctx)
    {[match(var(x), {:op, 6, :-, varize(b), varize(a)})], ast, [x | stack], ctxx}
  end

  def swap(ast, [a, b | stack], ctx) do
    {[], ast, [b, a | stack], ctx}
  end

  def drop(ast, [_ | stack], ctx) do
    {[], ast, stack, ctx}
  end

  def dup(ast, [a | stack], ctx) do
    {[], ast, [a, a | stack], ctx}
  end

  def set_module([mod_name | ast], stack, ctx) do
    {[], ast, stack, %Aspect.Compiler.Ctx{ctx | module_name: mod_name}}
  end

  def parse_token([token | ast], stack, ctx) do
    {[], ast, [token | stack], ctx}
  end

  def pp(ast, [x | stack], ctx) do
    {[
       mfa_call(:io, :format, [
         {:string, 15, [126, 112, 126, 110]},
         {:cons, 15, varize(x), {nil, 15}}
       ])
     ], ast, stack, ctx}
  end

  def quot(ast, stack, ctx) do
    {code, ast_rest, quot_r, ctx} = parse_quotation(ast, [], ctx)
    {code, ast_rest, [Enum.reverse(quot_r) | stack], ctx}
  end

  def infer(ast, [quot | stack], ctx) do
    answer = infer_stack_effect(eaf_to_quot(quot), ctx)
    {[x], ctx} = fresh(1, ctx)

    {[match(var(x), {:tuple, 1, Enum.map(Tuple.to_list(answer), fn x -> {:integer, 1, x} end)})],
     ast, [x | stack], ctx}
  end

  def calc_stack_effect({a, b}, {0, 0}), do: {a, b}
  def calc_stack_effect({a, b}, {0, y}), do: {a, b + y}
  def calc_stack_effect({a, b}, {x, y}) when b > 0, do: calc_stack_effect({a, b - 1}, {x - 1, y})
  def calc_stack_effect({a, b}, {x, y}), do: calc_stack_effect({a + 1, b}, {x - 1, y})

  def infer_stack_effect(quot, ctx) do
    List.foldl(quot, {0, 0}, fn elem, {a, b} ->
      {x, y} = infer_stack_effect_single(elem, ctx)
      calc_stack_effect({a, b}, {x, y})
    end)
  end

  def infer_stack_effect_single(word, ctx) do
    case Aspect.Compiler.word_type(word) do
      :quot ->
        {0, 1}

      :builtin ->
        {:ok, {_, effect}} = Aspect.Compiler.builtin(word)
        effect

      :atom ->
        {0, 1}

      :number ->
        {0, 1}

      :call_setup ->
        :ok

      :func_call ->
        case Map.get(ctx.words, word) do
          nil ->
            throw({:undefined_function, word})

          effect ->
            effect
        end
    end
  end

  def cons(ast, [xs, x | stack], ctx) do
    {[y], ctx} = fresh(1, ctx)
    code = [match(varize(y), {:cons, 1, varize(x), varize(xs)})]
    {code, ast, [y | stack], ctx}
  end

  def empty(ast, stack, ctx) do
    {[x], ctx} = fresh(1, ctx)
    code = [match(var(x), {nil, 1})]
    {code, ast, [x | stack], ctx}
  end

  def if(ast, [fc, tc, b | stack], ctx) do
    # TODO consider changing this to match on :false
    # and then everything else is considered true

    # TODO assuming the branches don't take any args, bad!!!
    # need way to tell statically how many they need, then
    # give them that much

    # TODO assuming the branches all return one arg, bad!!!

    # TODO if probably shouldn't have to worry about
    # generating code
    {tc_code, tc_ret_args, ctx} = gen_code(eaf_to_quot(tc), [], ctx)
    {fc_code, fc_ret_args, ctx} = gen_code(eaf_to_quot(fc), [], ctx)

    1 = length(tc_ret_args)
    1 = length(fc_ret_args)

    tc_full_code = [
      {:call, 1,
       {:fun, 1,
        {:clauses,
         [
           {:clause, 1, [], [],
            case tc_ret_args do
              [] -> tc_code
              [v] -> tc_code ++ [varize(v)]
              s -> tc_code ++ [tuple(Enum.map(s, &varize/1))]
            end}
         ]}}, []}
    ]

    fc_full_code = [
      {:call, 1,
       {:fun, 1,
        {:clauses,
         [
           {:clause, 1, [], [],
            case fc_ret_args do
              [] -> fc_code
              [v] -> fc_code ++ [varize(v)]
              s -> fc_code ++ [tuple(Enum.map(s, &varize/1))]
            end}
         ]}}, []}
    ]

    {[ret_var], ctx} = fresh(1, ctx)

    # TODO just match on false, and everything else is true?
    code =
      match(
        var(ret_var),
        {:case, 1, var(b),
         [
           {:clause, 1, [{:atom, 1, true}], [], tc_full_code},
           {:clause, 1, [{:atom, 1, false}], [], fc_full_code}
         ]}
      )

    {[code], ast, [ret_var | stack], ctx}
  end

  def pure_call(quot) when is_list(quot) do
    quot
  end
  def pure_call(_, [x | _], _), do: throw({:not_a_quot, x})

  def call(ast, [quot | stack], ctx) do
    {{num_args, num_ret}, ast_rest} = parse_effect(["(" | ast])

    {arg_vars, ctx} = fresh(num_args, ctx)

    {args_for_call, stack_rest} = Enum.split(stack, num_args)

    # TODO call probably shouldn't have to worry
    # about generating code
    {code, ret_args, ctx} = gen_code(eaf_to_quot(quot), arg_vars, ctx)

    full_code =
      case ret_args do
        [] -> code
        [v] -> code ++ [varize(v)]
        s -> code ++ [tuple(Enum.map(s, &varize/1))]
      end

    # assert the declared return stack effect is the same
    # as the number of values left on the stack
    ^num_ret = length(ret_args)

    call =
      {:call, 12,
       {:fun, 12, {:clauses, [{:clause, 12, Enum.map(arg_vars, &var/1), [], full_code}]}},
       Enum.map(args_for_call, &var/1)}

    {code_, stack_, ctx} =
      case ret_args do
        [] ->
          {call, stack_rest, ctx}

        [_] ->
          {[v], c} = fresh(1, ctx)
          {match(var(v), call), [v | stack_rest], c}

        _ ->
          {vs, c} = fresh(length(ret_args), ctx)
          {match(tuple(Enum.map(vs, &var/1)), call), vs ++ stack_rest, c}
      end

    {[code_], ast_rest, stack_, ctx}
  end

  def colon_(ast, stack, ctx) do
    {func_name, ast} = parse_func_name(ast)
    {effect, ast} = parse_effect(ast)
    {code, ast, body_r, ctx} = parse_body(ast, [], ctx)
    {code, ast, [func_name, effect, Enum.reverse(body_r) | stack], ctx}
  end

  def define_declared(ast, [name, effect, body | stack], ctx, register \\ true) do
    # TODO check declared stack effect with def
    {arg_count, ret_count} = effect

    {arg_vars, ctx} = fresh(arg_count, ctx)
    {code, stack_rest, ctx} = gen_code(body, arg_vars, ctx)
    ^ret_count = length(stack_rest)

    full_code =
      code
      |> ret_stack(stack_rest)
      |> ensure_ret()

    ctx =
      case register do
        true -> define_with_effect(name, arg_count, ret_count, ctx)
        false -> ctx
      end

    function =
      {:function, 5, String.to_atom(name), arg_count,
       [{:clause, 5, Enum.map(arg_vars, &var/1), [], full_code}]}

    {[function], ast, stack, ctx}
  end

  def parse_func_name([func_name | ast]) do
    {func_name, ast}
  end

  def parse_effect(ast) do
    {["(" | front], ["--" | rest]} = Enum.split_while(ast, fn x -> x != "--" end)
    {back, [")" | next]} = Enum.split_while(rest, fn x -> x != ")" end)
    {{length(front), length(back)}, next}
  end

  def colon(ast, stack, ctx) do
    {code, ast, stack, ctx} = colon_(ast, stack, ctx)
    {code_, ast, stack, ctx} = define_declared(ast, stack, ctx)
    {code ++ code_, ast, stack, ctx}
  end

  def varize(x) when is_atom(x), do: var(x)
  def varize(x), do: x

  defp ret_stack(code, stack) do
    case stack do
      [] -> code
      [x] -> code ++ [varize(x)]
      _ -> code ++ [tuple(Enum.map(stack, &varize/1))]
    end
  end

  defp ensure_ret(code) do
    case code do
      [] -> [{:atom, 1, :ok}]
      _ -> code
    end
  end

  def dep(ast, stack, ctx) do
    {code, ast, [module | stack], ctx} = Aspect.Lexer.parse_word(ast, stack, ctx)

    mod_atom = String.to_atom(module)

    unless :code.is_loaded(mod_atom) do
      :code.load_file(mod_atom)
    end

    syntax = :proplists.get_value(:syntax, mod_atom.module_info(:attributes))
    ctx = add_parsing_words(syntax, mod_atom, ctx)

    macros = :proplists.get_value(:macros, mod_atom.module_info(:attributes))
    ctx = add_macros(macros, mod_atom, ctx)

    {code, ast, stack, ctx}
  end

  def syntax(ast, stack, ctx) do
    {name, ast} = parse_func_name(ast)
    {code, ast, body_r, ctx} = parse_body(ast, [], ctx)

    # TODO parsing words maybe shouldn't return code?
    # this would require us to change how
    # define_declared works
    # it should just add something to ctx which then
    # generates the actual code later
    # ( ctx stack ast -- ctx stack ast code )
    syntax_effect = {3, 4}

    {code_, ast, stack, ctx} =
      define_declared(ast, [name, syntax_effect, Enum.reverse(body_r) | stack], ctx, false)

    ctx = define_syntax(name, ctx)

    {code ++ code_, ast, stack, ctx}
  end

  def macro(ast, stack, ctx) do
    {name, ast} = parse_func_name(ast)
    {effect, ast} = parse_effect(ast)
    {code, ast, body_r, ctx} = parse_body(ast, [], ctx)

    {args, 1} = effect

    {code_, ast, stack, ctx} =
      define_declared(ast, [name, effect, Enum.reverse(body_r) | stack], ctx, false)

    ctx = define_macro(name, args, ctx)

    {code ++ code_, ast, stack, ctx}
  end

  # TODO parse_body and parse_quoation are the same almost
  # need `parse-until`

  def parse_body(ast, stack, ctx) do
    parse_body_aux([], ast, stack, ctx)
  end

  def parse_body_aux(code, ast, stack, ctx) do
    {code_, ast, stack, ctx} = Aspect.Lexer.parse_word(ast, stack, ctx)

    case stack do
      [] -> throw(:unexpected_eof)
      [";" | stack] -> {code ++ code_, ast, stack, ctx}
      _ -> parse_body_aux(code ++ code_, ast, stack, ctx)
    end
  end

  def parse_quotation(ast, stack, ctx) do
    parse_quotation_aux([], ast, stack, ctx)
  end

  def parse_quotation_aux(code, ast, stack, ctx) do
    {code_, ast, stack, ctx} = Aspect.Lexer.parse_word(ast, stack, ctx)

    case stack do
      ["]" | stack] -> {code ++ code_, ast, stack, ctx}
      _ -> parse_quotation_aux(code ++ code_, ast, stack, ctx)
    end
  end
end
