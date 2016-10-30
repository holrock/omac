let hmac () =
  Alcotest.(check string) "build hmac"
    "9b5240fdc062d27e3f4ba998d6c53c28818b10cd"
    (Omac.hmac "test")

let parse_uri1 () =
  match (Omac.parse_uri (Uri.of_string "http://example.org/")) with
  | None -> ()
  | Some (s, t) -> Alcotest.fail (s ^ t)

let parse_uri2 () =
  match (Omac.parse_uri (Uri.of_string "http://example.org/digest")) with
  | None -> ()
  | Some (s, t) -> Alcotest.fail (s ^ t)

let parse_uri3 () =
  match (Omac.parse_uri (Uri.of_string "http://example.org/digest/imageurl")) with
  | None -> Alcotest.fail "expect result"
  | Some (s, t) ->
      Alcotest.(check string) "digest" "digest" s;
      Alcotest.(check string) "url" "imageurl" t

let parse_uri4 () =
  match (Omac.parse_uri (Uri.of_string "http://example.org/digest?url=imageurl")) with
  | None -> Alcotest.fail "expect result"
  | Some (s, t) ->
      Alcotest.(check string) "digest" "digest" s;
      Alcotest.(check string) "url" "imageurl" t


let test_set = [
  "hmac" , `Quick, hmac;
  "parse_uri1" , `Quick, parse_uri1;
  "parse_uri2" , `Quick, parse_uri2;
  "parse_uri3" , `Quick, parse_uri3;
  "parse_uri4" , `Quick, parse_uri4;
]

let () =
  Alcotest.run "run test" ["test_set", test_set]
