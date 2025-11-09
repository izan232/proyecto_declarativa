import scala.util.Random

enum Fila:
  case A,M,B
  def toInt:Int=this match
    case Fila.A => 1
    case Fila.M => 0
    case Fila.B => -1

enum Columna:
  case I2, I1, M, D1, D2
  def toInt:Int=this match
    case Columna.I2 => -2
    case Columna.I1 => -1
    case Columna.M => 0
    case Columna.D1 => 1
    case Columna.D2 => 2
enum Jugador:
  case Liebre, Sabuesos

case class Posicion(col:Columna, fila: Fila):
  def x: Int = col.toInt
  def y: Int = fila.toInt
  def manhattan(other: Posicion): Int =
    Math.abs(this.x - other.x) + Math.abs(this.y - other.y)
trait TableroJuego:
  def movimientosDesde(p:Posicion): Set[Posicion]

  def posicionInicialLiebre:Posicion
  def posicionesInicialesSabuesos:Set[Posicion]
  def posicionMetaLiebre:Posicion

  def pintarTablero(estado:Estado):Unit

  def esFinPartida(estado:Estado):Option[Jugador]
  
object TableroClasicoLyS extends TableroJuego:
  private val I1A = Posicion(Columna.I1, Fila.A)
  private val MA = Posicion(Columna.M, Fila.A)
  private val D1A = Posicion(Columna.D1, Fila.A)

  private val I2M = Posicion(Columna.I2, Fila.M)
  private val I1M = Posicion(Columna.I1, Fila.M)
  private val MM = Posicion(Columna.M, Fila.M)
  private val D1M = Posicion(Columna.D1, Fila.M)
  private val D2M = Posicion(Columna.D2, Fila.M)

  private val I1B = Posicion(Columna.I1, Fila.B)
  private val MB = Posicion(Columna.M, Fila.B)
  private val D1B = Posicion(Columna.D1, Fila.B)
  
  private val grafo: Map[Posicion, Set[Posicion]] = Map(
    I1A -> Set(MA,MM,I1M,I2M),
    MA -> Set(I1A,MM,D1A),
    D1A -> Set(MA,MM,D1M,D2M),

    I2M -> Set(I1A,I1M,I1B),
    I1M -> Set(I1A,MM,I1B,I2M),
    MM -> Set(I1A,MA,D1A,D1M,D1B,MB,I1B,I1M),
    D1M -> Set(D1A,D2M,D1B,MM),
    D2M -> Set(D1A,D1M,D1B),

    I1B -> Set(I2M,I1M,MM,MB),
    MB -> Set(I1B,MB,D1B),
    D1B -> Set(MB,MM,D1M,D2M)
  )

  override def movimientosDesde(p: Posicion): Set[Posicion] = grafo.getOrElse(p,Set())
  override def posicionInicialLiebre: Posicion = D2M
  override def posicionesInicialesSabuesos: Set[Posicion] = [I1A,I2M,I1B]
  override def posicionMetaLiebre: Posicion = I2M
  private def pintarNodo(p: Posicion, estado: Estado): String =

    val RESET = "\u001B[0m"
    val ROJO = "\u001B[31m"
    val AZUL = "\u001B[34m"
    val BLANCO = "\u001B[37m"
    
    if (estado.liebre == p) s"${ROJO}L${RESET}"
    else if (estado.sabuesos.contains(p)) s"${AZUL}S${RESET}"
    else s"${BLANCO}o${RESET}
  override def pintarTablero(estado: Estado): Unit =
    val s = pintarNodo(_, estado)
    println(s" ${s(I1A)}-----${s(MA)}-----${s(D1A)}")
    println(" ╱ | \\ | / | \\")
    println(s" ${s(I2M)}---${s(I1M)}-----${s(MM)}-----${s(D1M)}---${s(D2M)}")
    println(" \\ | / | \\ | /")
    println(s" ${s(I1B)}-----${s(MB)}-----${s(D1B)}")
  override def esFinPartida(estado: Estado): Option[Jugador] =
    if (estado.liebre == posicionMetaLiebre) then Some(Jugador.Liebre)
    else if (MovimientoLiebre.movimientosPosibles(this, estado).isEmpty) then Some(Jugador.Sabuesos)
    else None
def sortearTurno():Jugador=
  if Random.nextBoolean() then Jugador.Liebre
  else Jugador.Sabuesos

case class Estado(
                 liebre: Posicion,
                 sabuesos: Set[Posicion]
                 turno: Jugador
                 ):
  def ocupadas: Set[Posicion] = sabuesos + liebre
  def inicial(tablero: TableroJuego): Estado =
    Estado(
      liebre = posicionInicialLiebre,
      sabuesos = posicionesInicialesSabuesos,
      turno = sortearTurno(),
    )
sealed trait MovimientoFicha:
  def moviminetosPosibles(tableroJuego: TableroJuego,estado: Estado):Set[Posicion]
case object MovimientoLiebre extends MovimientoFicha:
  override def moviminetosPosibles(tableroJuego: TableroJuego,estado: Estado):Set[Posicion]= {
    val movimientos = tablero.movimientosDesde(Estado.liebre)
    movimientos = movimientos - Estado.ocupadas
  }
private def rebasaSabueso(liebrePosicion: Posicion, sabueso: Posicion): Boolean =
  liebrePosicion.x < sabueso.x

private def distanciaSabuesos(tablero: TableroJuego, estado: Estado, liebrePosicion: Posicion): Int =
  estado.sabuesos.map(s => liebrePosicion.manhattan(s)).sum

  private def hasRebasadoAlgun(estado: Estado): Boolean =
    estado.sabuesos.exists(s => estado.liebre.x < s.x)

  def evaluarMovimiento(tablero: TableroJuego, estado: Estado, destino: Posicion): (Int, Int) =
    val sabuesesRebasados = estado.sabuesos.count(s => rebasaSabueso(destino, s))
    val distanciaSabs = distanciaSabuesos(tablero, estado, destino)
    val distanciaAMeta = destino.manhattan(tablero.posicionMetaLiebre)

    if hasRebasadoAlgun(estado) then
      (-distanciaAMeta, distanciaSabs)
    else
      (sabuesesRebasados, distanciaSabs)
case object MovimientoSabueso:
  override def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion] =
    movimientosPosiblesPorSabueso(tablero, estado).map(_._2)
  def movimientosPosiblesPorSabueso(tablero: TableroJuego, estado: Estado): Set[(Posicion, Posicion)] =
    for
      sabueso <- estado.sabuesos
      movimiento <- tablero.movimientosDesde(sabueso)
      if movimiento.x >= sabueso.x
      if !estado.ocupadas.contains(movimiento)
    yield (sabueso, movimiento)
