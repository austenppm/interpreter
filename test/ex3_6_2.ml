open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval =
  [
    { input = "[1; 2; 3];;"; expected = ListV [ IntV 1; IntV 2; IntV 3 ] };
    {
      input = "4 :: [1; 2; 3];;";
      expected = ListV [ IntV 4; IntV 1; IntV 2; IntV 3 ];
    };
    {
      input = "[1] :: [2] :: [];;";
      expected = ListV [ ListV [ IntV 1 ]; ListV [ IntV 2 ] ];
    };
    {
      input = "[1] :: [2] :: [] :: [];;";
      expected = ListV [ ListV [ IntV 1 ]; ListV [ IntV 2 ]; ListV [] ];
    };
    {
      input = "[[1]; [1; 2]; 3];;";
      expected = ListV [ ListV [ IntV 1 ]; ListV [ IntV 1; IntV 2 ]; IntV 3 ];
    };
    {
      input = "[[[[1; 2]]]];;";
      expected = ListV [ ListV [ ListV [ ListV [ IntV 1; IntV 2 ] ] ] ];
    };
  ]

let () =
  ignore (run_test_tt_main ("ex3.6.2" >::: gen_eval_tests dataset_for_eval))
