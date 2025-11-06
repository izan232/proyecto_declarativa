

/* ==================== ENUMERADOS ====================

enum Fila:
case A, M, B

def toInt: Int = this match
case A => 1
case M => 0
case B => -1

enum Columna:
case I2, I1, M, D1, D2

def toInt: Int = this match
case I2 => -2
case I1 => -1
case M => 0
case D1 => 1
case D2 => 2

enum Jugador:
case Liebre, Sabuesos

// ==================== POSICION ====================

case class Posicion(col: Columna, fila: Fila):
def x: Int = col.toInt
def y: Int = fila.toInt

def manhattan(other: Posicion): Int =
  Math.abs(this.x - other.x) + Math.abs(this.y - other.y)

override def toString: String = s"${col}${fila}"

// ==================== ESTADO ====================

case class Estado(
                   liebre: Posicion,
                   sabuesos: Set[Posicion],
                   turno: Jugador
                 ):
def ocupadas: Set[Posicion] = sabuesos + liebre

// ==================== TRAIT TABLERO ====================

trait TableroJuego:
def movimientosDesde(p: Posicion): Set[Posicion]

def posicionInicialLiebre: Posicion
def posicionesInicialesSabuesos: Set[Posicion]
def posicionMetaLiebre: Posicion

def pintarTablero(estado: Estado): Unit

def esFinPartida(estado: Estado): Option[Jugador]

// ==================== TABLERO CLASICO ====================

object TableroClasicoLyS extends TableroJuego:

// Definición de posiciones
val I1A = Posicion(Columna.I1, Fila.A)
val MA = Posicion(Columna.M, Fila.A)
val D1A = Posicion(Columna.D1, Fila.A)

val I2M = Posicion(Columna.I2, Fila.M)
val I1M = Posicion(Columna.I1, Fila.M)
val MM = Posicion(Columna.M, Fila.M)
val D1M = Posicion(Columna.D1, Fila.M)
val D2M = Posicion(Columna.D2, Fila.M)

val I1B = Posicion(Columna.I1, Fila.B)
val MB = Posicion(Columna.M, Fila.B)
val D1B = Posicion(Columna.D1, Fila.B)

// Lista de adyacencias del grafo hexagonal
private val grafo: Map[Posicion, Set[Posicion]] = Map(
  I1A -> Set(MA, I1M, MM),
  MA -> Set(I1A, D1A, I1M, D1M),
  D1A -> Set(MA, D1M, D2M),

  I2M -> Set(I1M, I1B, MB),
  I1M -> Set(I1A, I2M, MM, I1B, MB),
  MM -> Set(I1A, MA, D1A, I1M, D1M, I1B, MB, D1B),
  D1M -> Set(MA, D1A, MM, D2M, MB, D1B),
  D2M -> Set(D1A, D1M, D1B),

  I1B -> Set(I2M, I1M, MM, MB),
  MB -> Set(I2M, I1M, MM, D1M, I1B, D1B),
  D1B -> Set(MM, D1M, D2M, MB)
)

override def movimientosDesde(p: Posicion): Set[Posicion] =
  grafo.getOrElse(p, Set())

override def posicionInicialLiebre: Posicion = D2M

override def posicionesInicialesSabuesos: Set[Posicion] = Set(I2M, I1B, I1A)

override def posicionMetaLiebre: Posicion = I2M

override def pintarTablero(estado: Estado): Unit =
def pintarNodo(p: Posicion): String =
val RESET = "\u001B[0m"
val ROJO = "\u001B[31m"
val AZUL = "\u001B[34m"
val BLANCO = "\u001B[37m"

if (estado.liebre == p) s"${ROJO}L${RESET}"
else if (estado.sabuesos.contains(p)) s"${AZUL}S${RESET}"
else s"${BLANCO}o${RESET}"

println(s"         ${pintarNodo(I1A)}-----${pintarNodo(MA)}-----${pintarNodo(D1A)}")
println("      ╱  |  \\  |  /  |  \\")
println(s"     ${pintarNodo(I2M)}---${pintarNodo(I1M)}-----${pintarNodo(MM)}-----${pintarNodo(D1M)}---${pintarNodo(D2M)}")
println("      \\  |  /  |  \\  |  /")
println(s"         ${pintarNodo(I1B)}-----${pintarNodo(MB)}-----${pintarNodo(D1B)}")
println()

override def esFinPartida(estado: Estado): Option[Jugador] =
  // La liebre gana si alcanza su meta
  if (estado.liebre == posicionMetaLiebre) then Some(Jugador.Liebre)
  // Los sabuesos ganan si la liebre no puede moverse
  else if (MovimientoLiebre.movimientosPosibles(this, estado).isEmpty) then Some(Jugador.Sabuesos)
  else None

// ==================== MOVIMIENTOS ====================

sealed trait MovimientoFicha:
def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion]

case object MovimientoLiebre extends MovimientoFicha:
override def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion] =
val movimientos = tablero.movimientosDesde(estado.liebre)
movimientos.filterNot(estado.ocupadas.contains)

def movimientosPosiblesPorSabueso(tablero: TableroJuego, estado: Estado): Set[(Posicion, Posicion)] =
  for
    sabueso <- estado.sabuesos
movimiento <- tablero.movimientosDesde(sabueso)
if movimiento.x >= sabueso.x
if !estado.ocupadas.contains(movimiento)
yield (sabueso, movimiento)

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

private def distanciaALiebre(sabueso: Posicion, liebre: Posicion): Int =
  sabueso.manhattan(liebre)

def evaluarMovimiento(tablero: TableroJuego, estado: Estado, sabueso: Posicion, destino: Posicion): (Int, Int) =
// Estrategia: acercarse a la liebre y preferir movimientos que reduzcan la distancia
val distanciaActual = distanciaALiebre(sabueso, estado.liebre)
val distanciaNueva = distanciaALiebre(destino, estado.liebre)
val mejora = distanciaActual - distanciaNueva

// Segundo criterio: posición más a la izquierda (para cerrar paso)
val posicionIzquierda = -destino.x

(mejora, posicionIzquierda)

// ==================== UTILIDADES ====================

def sortearTurno(): Jugador =
  if Random.nextBoolean() then Jugador.Liebre else Jugador.Sabuesos

// ==================== BUCLE DE JUEGO ====================

def bucleJuego(tablero: TableroJuego, estado: Estado, modoIA: Set[Jugador]): Jugador =
  tablero.pintarTablero(estado)
println(s"Turno del: ${if estado.turno == Jugador.Liebre then "LIEBRE" else "SABUESOS"}\n")

estado.turno match
case Jugador.Liebre =>
val movimientosPosibles = MovimientoLiebre.movimientosPosibles(tablero, estado)

if (movimientosPosibles.isEmpty) then
println("¡Los sabuesos han ganado! La liebre está acorralada.")
Jugador.Sabuesos
else
val movimientosOrdenados = movimientosPosibles.toList
  .map(m => (m, MovimientoLiebre.evaluarMovimiento(tablero, estado, m)))
  .sortBy { case (_, (v1, v2)) => (-v1, -v2) }

movimientosOrdenados.zipWithIndex.foreach { case ((pos, (v1, v2)), idx) =>
  println(f"$idx: ${pos} - Heurística: ($v1, $v2)")
}

val eleccion = if modoIA.contains(Jugador.Liebre) then
val mejor = movimientosOrdenados.head._1
println(s"IA elige: $mejor")
movimientosOrdenados.indexWhere(_._1 == mejor)
else
print("Elige movimiento (0-${movimientosOrdenados.length - 1}): ")
StdIn.readLine().toInt

val nuevoEstado = Estado(
  liebre = movimientosOrdenados(eleccion)._1,
  sabuesos = estado.sabuesos,
  turno = Jugador.Sabuesos
)
bucleJuego(tablero, nuevoEstado, modoIA)

case Jugador.Sabuesos =>
val movimientosPosibles = MovimientoSabueso.movimientosPosiblesPorSabueso(tablero, estado)

if (movimientosPosibles.isEmpty) then
println("¡La liebre ha ganado!")
Jugador.Liebre
else
val movimientosOrdenados = movimientosPosibles.toList
  .map { case (origen, destino) =>
    ((origen, destino), MovimientoSabueso.evaluarMovimiento(tablero, estado, origen, destino))
  }
  .sortBy { case (_, (v1, v2)) => (-v1, -v2) }

movimientosOrdenados.zipWithIndex.foreach { case (((origen, destino), (v1, v2)), idx) =>
  println(f"$idx: $origen -> $destino - Heurística: ($v1, $v2)")
}

val eleccion = if modoIA.contains(Jugador.Sabuesos) then
val mejor = movimientosOrdenados.head._1
println(s"IA elige: ${mejor._1} -> ${mejor._2}")
movimientosOrdenados.indexWhere(_._1 == mejor)
else
print("Elige movimiento (0-${movimientosOrdenados.length - 1}): ")
StdIn.readLine().toInt

val (sabueso, destino) = movimientosOrdenados(eleccion)._1
val nuevosSabuesos = (estado.sabuesos - sabueso) + destino

val nuevoEstado = Estado(
  liebre = estado.liebre,
  sabuesos = nuevosSabuesos,
  turno = Jugador.Liebre
)

tablero.esFinPartida(nuevoEstado) match
case Some(ganador) =>
tablero.pintarTablero(nuevoEstado)
println(s"¡Los sabuesos han ganado! La liebre está acorralada.")
ganador
case None =>
  bucleJuego(tablero, nuevoEstado, modoIA)

// ==================== PROGRAMA PRINCIPAL ====================

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
case 2 => Set(Jugador.Sabuesos)
case 3 => Set(Jugador.Liebre)
case 4 => Set(Jugador.Liebre, Jugador.Sabuesos)
case _ => Set[Jugador]()

val turnoInicial = sortearTurno()
println(s"\n¡La partida comienza! Comienza: ${if turnoInicial == Jugador.Liebre then "LIEBRE" else "SABUESOS"}\n")

val estadoInicial = Estado(
  liebre = TableroClasicoLyS.posicionInicialLiebre,
  sabuesos = TableroClasicoLyS.posicionesInicialesSabuesos,
  turno = turnoInicial
)

val ganador = bucleJuego(TableroClasicoLyS, estadoInicial, modoIA)

println(s"\n¡¡¡ ${if ganador == Jugador.Liebre then "¡La LIEBRE ha ganado!" else "¡Los SABUESOS han ganado!"} !!!\n")

print("¿Jugar otra partida? (s/n): ")
seguirJugando = StdIn.readLine().toLowerCase() == "s"

println("\n¡Gracias por jugar!")
