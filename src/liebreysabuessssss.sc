

/*

override def toString: String = s"${col}${fila}"

// ==================== ESTADO ====================

case class Estado(
                   liebre: Posicion,
                   sabuesos: Set[Posicion],
                   turno: Jugador
                 ):
def ocupadas: Set[Posicion] = sabuesos + liebre


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
