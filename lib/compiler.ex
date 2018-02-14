defmodule Aspect.Compiler do

  defmodule Ctx do
    defstruct fresh: 0
  end

  def compile(file) do

    IO.puts "Compiling #{file}"

    file
    |> File.read!
    |> :erlang.binary_to_list
    |> :string.tokens(' \n')
    |> compile_forms
  end

  def load(forms) do
    case :compile.forms(forms, [:return_errors, :return_warnings]) do
      {:ok, mod, binary, _warnings} -> :code.load_binary(mod, 'hi.as', binary)
    end
  end

  def cl(file) do
    load compile(file)
  end

  defp fresh(num, %Ctx{fresh: fresh} = ctx) do
    fresh..(fresh+num-1)
    |> Enum.map(fn(x) -> :erlang.list_to_atom('X' ++ :erlang.integer_to_list(x)) end)
    |> fn(x) -> {x, %Ctx{ctx | fresh: fresh + num}} end.()
  end

  # todo these should all return contexts (and stacks) as well!!!

  def compile_forms(ast) do
    [{:attribute,1,:file,{"hi.as",1}},
     {:attribute,1,:module,:hi},
     {:attribute,2,:compile,:export_all}] ++
      compile_forms(ast, [], %Ctx{}) ++
      [{:eof,7}]
  end
  def compile_forms(['swap'|ast], [a,b|stack], ctx) do
    compile_forms(ast, [b,a|stack], ctx)
  end
  def compile_forms(['+'|ast], [a,b|stack], ctx) do
    # TODO: this assumes + takes 2 arguments
    # and returns 1
    {[x], ctxx} = fresh(1, ctx)
    [{:match,6,{:var,6,x},{:op,6,:+,{:var,6,a},{:var,6,b}}}]
    ++ compile_forms(ast, [x|stack], ctxx)
  end
  def compile_forms([':',func_name|ast], stack, ctx) do
    {arg_count, ast_body} = parse_effect(ast)
    {arg_vars, ctxx} = fresh(arg_count, ctx)
    {body_r, ast_rest} = parse_body(ast_body, [])
    body = Enum.reverse body_r

    # todo ensure arg_count number of variables are left
    # on the stack
    [{:function,5,:erlang.list_to_atom(func_name),arg_count,
      [{:clause,5,
        Enum.map(arg_vars, fn var -> {:var, 5, var} end),
        [],
        compile_forms(body, arg_vars, ctxx)}]}
     |
     compile_forms(ast_rest, stack, ctxx)]
  end
  def compile_forms(['drop'|ast], [_a|stack], ctx) do
    compile_forms(ast, stack, ctx)
  end
  def compile_forms(['parse-token',token|ast], stack, ctx) do
    compile_forms(ast, [token|stack], ctx)
  end
  def compile_forms(['['|ast], stack, ctx) do
    {quot_r, ast_rest} = parse_quotation(ast, [])
    quot = Enum.reverse quot_r
    compile_forms(ast_rest, [quot|stack], ctx)
  end
  def compile_forms(['.'|ast], [x|stack], ctx) do
    [{:call, 15,
      {:remote, 15, {:atom, 15, :io}, {:atom, 15, :format}},
      [{:string, 15, [126, 112, 126, 110]},
       {:cons, 15, {:var, 15, x}, {:nil, 15}}]}
     |
     compile_forms(ast, stack, ctx)]
  end

  def compile_forms(['call('|ast], [quot|stack], ctx) do
    {num_args, ast_rest} = parse_effect(ast)

    {arg_vars, ctxx} = fresh(num_args, ctx)

    {args_for_call, stack_rest} = Enum.split(stack, num_args)

    # todo when calling, check if there are args left on stack
    # and then take them off and match them
    # (see how callprint doesn't compile)
    [{:match, 12,
      {:var, 12, :A},
      {:call, 12,
       {:fun, 12, {:clauses,
                   [{:clause, 12,
                     Enum.map(arg_vars, fn var -> {:var, 12, var} end),
                     [],
                     compile_forms(quot, arg_vars, ctxx)}]}},
       Enum.map(args_for_call, fn var -> {:var, 12, var} end)}}
     |
     compile_forms(ast_rest, stack_rest, ctxx)]
  end
  def compile_forms([x|ast], stack, ctx) do
    num = try do
            :erlang.list_to_integer(x)
          rescue
            ArgumentError -> :error
          end
    case num do
      :error ->
        [a,b,c|stack_rest] = stack
        [{:call,9,{:atom,9,:erlang.list_to_atom(x)},
          [{:integer,9,a},{:var,9,b},{:var,9,c}]}
         |
         compile_forms(ast, stack_rest, ctx)]
      _ -> compile_forms(ast, [num|stack], ctx)
    end
  end
  def compile_forms([], [], _), do: []

  def parse_body([';'|ast], stack) do
    {stack, ast}
  end
  def parse_body([token|ast], stack) do
    parse_body(ast, [token|stack])
  end

  def parse_quotation([']'|ast], stack) do
    {stack, ast}
  end
  def parse_quotation([token|ast], stack) do
    parse_quotation(ast, [token|stack])
  end

  defp parse_effect(['('|ast]) do
    parse_effect(ast)
  end
  defp parse_effect([')'|ast]), do: {0, ast}
  defp parse_effect(['--'|ast]) do
    {_, next} = parse_effect(ast)
    {0, next}
  end
  defp parse_effect([_|ast]) do
    {count, next} = parse_effect(ast)
    {count+1, next}
  end


  @builtins %{
    +: {&Aspect.Compiler.compile_plus/2, {2,1}}
  }

  def infer(x) do
    case Map.get(@builtins, x) do
      nil -> # TODO: just assumming it's an integer here, bad!
            {0,1}
      {_, io} -> io
    end
  end

  def compile_plus(ast, [a,b|stack]) do
    {[{:op,6,:+,{:var,6,a},{:var,6,b}}],
     ast,
     stack}
  end

end
