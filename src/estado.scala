import scala.util.Random
//creamos un enum para jugador
enum jugador:
  case liebre, sabuesos
//creamos la funcion que sorteara el turno del jugador al comienzo de la partida
def sortearturno(): jugador =
  if scala.util.Random.nextBoolean() then jugador.liebre
  else jugador.sabuesos
//case class estado con toda la informacion necesaria del estado de la partida
case class estado(
  liebre: posicion,
  sabuesos: set[Posicion],
           
                 )