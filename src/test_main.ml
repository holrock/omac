(* A module with functions to test *)
module To_test = struct
  let capit letter = Char.uppercase_ascii letter
      let plus int_list = List.fold_left (fun a b -> a + b) 0 int_list
end

(* The tests *)
let capit () =
  Alcotest.(check string) "same chars"  "A" (Omac.hmac "a")

let test_set = [
  "Capitalize" , `Quick, capit
]

(* Run it *)
let () =
  Alcotest.run "My first test" [
    "test_set", test_set;
]
