module Errormsg =
  struct

    let anyErrors = ref false
    let fileName = ref ""
    let lineNum = ref 1
    let linePos = ref [1]

    let reset() = (
      anyErrors := false;
      fileName := "";
      lineNum := 1;
      linePos := [1])
  end;;
