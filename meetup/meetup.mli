type schedule = First | Second | Third | Fourth | Teenth | Last

type weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

type date = (int * int * int)

val meetup_day : schedule -> weekday -> year:int -> month:int -> date
