type planet = Mercury | Venus | Earth | Mars
            | Jupiter | Saturn | Neptune | Uranus

let oneEarthYearInSeconds = 31557600.0

let toEarthYears (factor: float) (secs: int) = (float_of_int secs /. ((factor *. oneEarthYearInSeconds)))

let age_on p = toEarthYears(match p with
  | Mercury -> 0.2408467
  | Venus -> 0.61519726
  | Earth -> 1.0
  | Mars -> 1.8808158
  | Jupiter -> 11.862615
  | Saturn -> 29.447498
  | Uranus -> 84.016846
  | Neptune -> 164.79132)
