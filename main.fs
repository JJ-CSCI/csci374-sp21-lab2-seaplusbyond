module Assignment

type AMPM = AM | PM

// This function checks if an hour value `h` is not in [1,12] range
let areHoursInvalid h =
  if h < 1 || h > 12 then true
  else false

// This function checks if a minute value `m` is not in [0,59] range
let areMinutesInvalid m =
  if m < 0 || m > 59 then true
  else false

// This function creates a valid time tuple
//      use above functions: areHoursInvalid & areMinutesInvalid
let time h m ampm :(int * int * AMPM) =
  let mutable h = 0
  let mutable m = 0
  if (areHoursInvalid h = true) then h = 12
  else h = h
  if (areMinutesInvalid m = true) then m = 0
  else m = m
  (h, m, ampm)

// This function compares two times in tuple format
let lessThan (time1: int * int * AMPM) (time2: int * int * AMPM) :bool =
  let h1, m1, ampm1 = time1
  let h2, m2, ampm2 = time2
  if(ampm2 = PM && ampm1 = AM) then true //why can i access am and pm values here
  elif(h2 = h1 && m2 > m1) then true //but the comparison doesn't work here?
  elif(m2 = m1 && h2 > h1) then true //that's kinda stupid.
  else false
