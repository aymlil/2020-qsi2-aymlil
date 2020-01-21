open Framework;
open QCheckRely;
open Generator.Fantasy;
open Lib.Troll;

let {describe} = extendDescribe(QCheckRely.Matchers.matchers);

describe("Troll Invariance", ({test}) => {
  test("Troll score should be 0 when all elves resurrected", ({expect}) => {
    QCheck.Test.make(
      ~count=1000,
      ~name="Troll score should be 0 when all elves resurrected",
      troll_arbitrary,
      troll =>
      all_elves_resurrected(troll) |> scoring == 0
    )
    |> expect.ext.qCheckTest;
    ();
  });
  test("Troll score should always be >= 0", ({expect}) => {
    QCheck.Test.make(
        ~count=1000,
        ~name="Troll score should always be >= 0",
        troll_arbitrary,
        troll =>
        scoring(troll) >= 0
      )
      |> expect.ext.qCheckTest;
      ();
    ()
  });
});

describe("Troll Inverse", ({test}) => {
  test("oops_he_survived should always be inverse of i_got_one", ({expect}) => {
    QCheck.Test.make(
      ~count=1000,
      ~name="oops_he_survived should always be inverse of i_got_one",
      troll_elf_arbitrary,
      ((troll, elf)) =>
      troll |> i_got_one(elf) |> oops_he_survived(elf) |> scoring == scoring(troll)
    )
    |> expect.ext.qCheckTest;
    ()
  })
});

describe("Troll Analogy", ({test}) => {
  test("i_got_one and i_got should be consistent", ({expect}) => {
    QCheck.Test.make(
      ~count=1000,
      ~name="i_got_one and i_got should be consistent",
      troll_elf_int_arbitrary,
      ((troll, elf, num)) =>{
        let trolls = ref(troll);
        for(i in 1 to num){
          trolls := i_got_one(elf,trolls^);
        };
        (trolls^ |> scoring) == (troll |> i_got(num,elf) |> scoring)
      }
    )
    |> expect.ext.qCheckTest;
  })
});

describe("Troll Idempotence", ({test}) => {
  test(
    "all_elves_of_a_kind_resurrected brings the Troll killing list to a stable state",
    ({expect}) => {
      QCheck.Test.make(
        ~count=1000,
        ~name="all_elves_of_a_kind_resurrected brings the Troll killing list to a stable state",
        troll_elf_int_arbitrary,
        ((troll, elf, num)) =>{
          let trollTmp = ref(troll);
          for(i in 1 to num){
            trollTmp := all_elves_of_a_kind_resurrected(elf,trollTmp^);
          };
          (trollTmp^ |> scoring) == (troll |> all_elves_of_a_kind_resurrected(elf) |> scoring)
        }
      )
      |> expect.ext.qCheckTest;
    ()
  })
});