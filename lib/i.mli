type configuration

val default : configuration

type chunk =
    Chunk of { seek : int option
             ; payload : string
             ; rest : (unit, (int * string)) result }

val chunk_of_string : configuration -> string -> chunk
