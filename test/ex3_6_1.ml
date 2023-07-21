open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval =
  [
    {
      input = "1 :: 2 :: 3 :: [];;";
      expected = ListV [ IntV 1; IntV 2; IntV 3 ];
    };
    {
      input = "(1 :: []) :: (1 :: 2 :: []) :: 3 :: [];;";
      expected = ListV [ ListV [ IntV 1 ]; ListV [ IntV 1; IntV 2 ]; IntV 3 ];
    };
    {
      input = "(fun x -> x :: x :: []) 4;;";
      expected = ListV [ IntV 4; IntV 4 ];
    };
    {
      input =
        "let rec length = fun l -> match l with [] -> 0\n\
        \         | x::rest -> 1 + length rest in length (1 :: 2 :: 3 :: []);;";
      expected = IntV 3;
    };
    {
      input =
        "let f = fun cons -> match cons with [] -> 0 | x :: y -> x in (f []) \
         :: (f (1 :: [])) :: [];;";
      expected = ListV [ IntV 0; IntV 1 ];
    };
    { input = "let x = [] in 0 :: x;;"; expected = ListV [ IntV 0 ] };
    {
      input =
        "let rec append = fun a -> fun b -> match a with [] -> b | hd :: tl -> \
         hd :: append tl b in append (1 :: 2 :: 3 :: []) (4 :: 5 :: 6 :: []);;";
      expected = ListV [ IntV 1; IntV 2; IntV 3; IntV 4; IntV 5; IntV 6 ];
    };
  ]

let () =
  ignore (run_test_tt_main ("ex3.6.1" >::: gen_eval_tests dataset_for_eval))
