/*@main def main(): Unit = {
  println("=== BIENVENIDO AL JUEGO DE LA LIEBRE Y LOS SABUESOS ===\n")
  var seguirJugando = true

  while (seguirJugando) do
    println("\n¿Quién juega?")
    println("1. Jugador vs Jugador")
    println("2. Jugador (Liebre) vs IA (Sabuesos)")
    println("3. IA (Liebre) vs Jugador (Sabuesos)")
    println("4. IA (Liebre) vs IA (Sabuesos)")
    print("Elige opción (1-4): ")

    val opcion = scala.io.StdIn.readLine().toInt

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
    seguirJugando = scala.io.StdIn.readLine().toLowerCase() == "s"
  println("\n¡Gracias por jugar!")
}
