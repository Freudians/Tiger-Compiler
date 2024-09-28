let parse filename =
  let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    let result = Grammar.program TigerLexer.token lexbuf in
    close_in ic;
    Prabsyn.PrintAbsyn.print (stdout, result)    
let%expect_test "1" = parse "/home/anthonydu/appel/tiger_compiler/sample_programs/test1.tig";
  [%expect {|
    LetExp([
     TypeDec[
      (arrtype,
       ArrayTy(int))],
     VarDec(arr1,true,Some(arrtype),
      ArrayExp(arrtype,
       IntExp(10),
       IntExp(0)))],
     SeqExp[
      VarExp(
       SimpleVar(arr1))]) |}]
let%expect_test "2" = parse "/home/anthonydu/appel/tiger_compiler/sample_programs/test2.tig";
  [%expect {|
    LetExp([
     TypeDec[
      (myint,
       NameTy(int)),
      (arrtype,
       ArrayTy(myint))],
     VarDec(arr1,true,Some(arrtype),
      ArrayExp(arrtype,
       IntExp(10),
       IntExp(0)))],
     SeqExp[
      VarExp(
       SimpleVar(arr1))]) |}]
let%expect_test "3" = parse "/home/anthonydu/appel/tiger_compiler/sample_programs/test3.tig";
  [%expect {|
    LetExp([
     TypeDec[
      (rectype,
       RecordTy[
        (name,true,string),
        (age,true,int)])],
     VarDec(rec1,true,Some(rectype),
      RecordExp(rectype,[

       (name,
        StringExp("Nobody")),
       (age,
        IntExp(1000))]))],
     SeqExp[
      AssignExp(
       FieldVar(
        SimpleVar(rec1),
        name),
       StringExp("Somebody")),
      VarExp(
       SimpleVar(rec1))]) |}]
