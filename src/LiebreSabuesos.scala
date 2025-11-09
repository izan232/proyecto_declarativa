import Jugador.{Liebre, Sabuesos}

import scala.io.StdIn
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
case class Estado(
                   liebre: Posicion,
                   sabuesos: Set[Posicion],
                   turno : Jugador
                 ):
  def ocupadas: Set[Posicion] = sabuesos + liebre
case class Posicion(col:Columna, fila: Fila):
  def x: Int = col.toInt
  def y: Int = fila.toInt
  def manhattan(other: Posicion): Int =
    Math.abs(this.x - other.x) + Math.abs(this.y - other.y)
trait TableroJuego:
  def movimientosDesde(p:Posicion): Set[Posicion]

  def posicionInicialLiebre:Posicion
  def posicionesInicialesSabuesos:Set[Posicion]
  def posicionMetaLiebre: Posicion

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
    MB -> Set(I1B,MM,D1B),
    D1B -> Set(MB,MM,D1M,D2M)
  )

  override def movimientosDesde(p: Posicion): Set[Posicion] = grafo.getOrElse(p,Set())
  override def posicionInicialLiebre: Posicion = D2M
  override def posicionesInicialesSabuesos: Set[Posicion] = Set(I2M,I1A,I1B)
  override def posicionMetaLiebre: Posicion = I2M
  private def pintarNodo(p: Posicion, estado: Estado): String =

    val RESET = "\u001B[0m"
    val ROJO = "\u001B[31m"
    val AZUL = "\u001B[34m"
    val BLANCO = "\u001B[37m"
    
    if (estado.liebre == p) s"${ROJO}L${RESET}"
    else if (estado.sabuesos.contains(p)) s"${AZUL}S${RESET}"
    else s"${BLANCO}o${RESET}"
  override def pintarTablero(estado: Estado): Unit =
    val s = pintarNodo(_, estado)
    println(s" ${s(I1A)}-----${s(MA)}-----${s(D1A)}")
    println(" ╱ | \\ | / | \\")
    println(s" ${s(I2M)}---${s(I1M)}-----${s(MM)}-----${s(D1M)}---${s(D2M)}")
    println(" \\ | / | \\ | /")
    println(s" ${s(I1B)}-----${s(MB)}-----${s(D1B)}")
  override def esFinPartida(estado: Estado): Option[Jugador] =
    if (estado.liebre == posicionMetaLiebre) then Some(Liebre)
    else if (MovimientoLiebre.movimientosPosibles(this, estado).isEmpty) then Some(Sabuesos)
    else None
def sortearTurno():Jugador=
  if Random.nextBoolean() then Liebre
  else Sabuesos


sealed trait MovimientoFicha:
  def movimientosPosibles(tableroJuego: TableroJuego,estado: Estado):Set[Posicion]
case object MovimientoLiebre extends MovimientoFicha:
  override def movimientosPosibles(tableroJuego: TableroJuego,estado: Estado):Set[Posicion]= {
    val movimientos = tableroJuego.movimientosDesde(estado.liebre)
    movimientos -- estado.ocupadas
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
case object MovimientoSabueso extends MovimientoFicha:
  override def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion] =
    movimientosPosiblesPorSabueso(tablero, estado).map(_._2)
  def movimientosPosiblesPorSabueso(tablero: TableroJuego, estado: Estado): Set[(Posicion, Posicion)] =
    for
      sabueso <- estado.sabuesos
      movimiento <- tablero.movimientosDesde(sabueso)
      if movimiento.x >= sabueso.x
      if !estado.ocupadas.contains(movimiento)
    yield (sabueso, movimiento)
  def distanciaALiebre(sabueso: Posicion, liebre:Posicion): Int =
    sabueso.manhattan(liebre)

  def evaluarMovimiento(tablero:TableroJuego,estado:Estado,sabueso:Posicion,destino:Posicion):(Int,Int)=
    val distanciaActual= distanciaALiebre(sabueso, estado.liebre)
    val distanciaNueva= distanciaALiebre(destino,estado.liebre)
    val mejora =distanciaActual-distanciaNueva

    val posicionIzquierda = -destino.x

    (mejora,posicionIzquierda)

def bucleJuego (tablero:TableroJuego,estado: Estado,modoIA:Set[Jugador]):Jugador=
  tablero.pintarTablero(estado)
  println(s"Turno del: ${if estado.turno == Liebre then "LIEBRE" else "SABUESOS"}\n")

  estado.turno match
    case Liebre =>
      val movimientosPosibles = MovimientoLiebre.movimientosPosibles(tablero,estado)

      if (movimientosPosibles.isEmpty) then
        println("Los sabuesos han ganado")
        Sabuesos
      else
        val movimientosOrdenados = movimientosPosibles.toList.map(m=>(m,MovimientoLiebre.evaluarMovimiento(tablero,estado,m))).sortBy{case (_,(v1,v2)) => (-v1,-v2)}
        movimientosOrdenados.zipWithIndex.foreach {case ((pos,(v1,v2)),idx)=>println(f"$idx: ${pos} - Heurística: ($v1, $v2)")}

        val eleccion = if modoIA.contains(Liebre) then 0
        else
          print(s"Elige movimiento (0-${movimientosOrdenados.length - 1}): ")
          StdIn.readLine().toInt

        val nuevoEstado = Estado(
          liebre = movimientosOrdenados(eleccion)._1,
          sabuesos=estado.sabuesos,
          turno = Jugador.Sabuesos
        )
        tablero.esFinPartida(nuevoEstado) match {
          case Some(ganador) =>
            tablero.pintarTablero((nuevoEstado))
            println(s"La liebre ha ganado")
            ganador
          case None =>
            bucleJuego(tablero,nuevoEstado,modoIA)
        }
    case Sabuesos =>
      val movimientosPosibles= MovimientoSabueso.movimientosPosiblesPorSabueso(tablero,estado)
    if MovimientoSabueso.movimientosPosiblesPorSabueso(tablero,estado).isEmpty then
      println("La liebre ha ganado")
      Liebre
    else
      val movimientosOrdenados = {
        MovimientoSabueso.movimientosPosiblesPorSabueso(tablero, estado).toList
          .map { case (origen, destino) =>
            ((origen, destino), MovimientoSabueso.evaluarMovimiento(tablero, estado, origen, destino))
          }
          .sortBy { case (_, (v1, v2)) => (-v1, -v2) }
      }

      movimientosOrdenados.zipWithIndex.foreach { case (((origen, destino), (v1, v2)), idx) =>
        println(f"$idx: $origen -> $destino - Heurística: ($v1, $v2)")
      }
      val eleccion = if modoIA.contains(Sabuesos) then
        val mejor = movimientosOrdenados.head._1
        println(s"IA elige: ${mejor._1} -> ${mejor._2}")
        movimientosOrdenados.indexWhere(_._1 == mejor)
      else
        print(s"Elige movimiento (0-${movimientosOrdenados.length - 1}): ")
        StdIn.readLine().toInt

      val (sabueso, destino) = movimientosOrdenados(eleccion)._1
      val nuevosSabuesos = (estado.sabuesos - sabueso) + destino

      val nuevoEstado = Estado(
        liebre = estado.liebre,
        sabuesos = nuevosSabuesos,
        turno = Liebre
      )
      tablero.esFinPartida(nuevoEstado) match
        case Some(ganador) =>
          tablero.pintarTablero(nuevoEstado)
          println(s"¡Los sabuesos han ganado! ")
          ganador
        case None =>
          bucleJuego(tablero, nuevoEstado, modoIA)

object JuegoLiebreySabuesos extends App:
  println("=== BIENVENIDO AL JUEGO DE LA LIEBRE Y LOS SABUESOS ===\n")

  var seguirJugando = true

  while (seguirJugando) do
    println("\n¿Quién juega?")
    println("1. Jugador vs Jugador")
    println("2. Jugador (Liebre) vs IA (Sabuesos)")
    println("3. IA (Liebre) vs Jugador (Sabuesos)")
    println("4. IA (Liebre) vs IA (Sabuesos)")
    print("Elige opción (1-4): ")

    val opcion = StdIn.readLine().toInt

    val modoIA = opcion match
      case 1 => Set[Jugador]()
      case 2 => Set(Sabuesos)
      case 3 => Set(Liebre)
      case 4 => Set(Liebre, Sabuesos)
      case _ => Set[Jugador]()

    val turnoInicial = sortearTurno()
    println(s"\n¡La partida comienza! Comienza: ${if turnoInicial == Liebre then "LIEBRE" else "SABUESOS"}\n")

    val estadoInicial = Estado(
      liebre = TableroClasicoLyS.posicionInicialLiebre,
      sabuesos = TableroClasicoLyS.posicionesInicialesSabuesos,
      turno = turnoInicial
    )

    val ganador = bucleJuego(TableroClasicoLyS, estadoInicial, modoIA)

    println(s"\n¡¡¡ ${if ganador == Liebre then "¡La LIEBRE ha ganado!" else "¡Los SABUESOS han ganado!"} !!!\n")

    print("¿Jugar otra partida? (s/n): ")
    seguirJugando = StdIn.readLine().toLowerCase() == "s"

    println("\n¡Gracias por jugar!")