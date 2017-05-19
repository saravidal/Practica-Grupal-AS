# Practica-Grupal-AS

![Image of Sevenfic](https://github.com/saravidal/Practica-Grupal-AS/blob/master/images/sevenfic.png)

**Descripción**  
Sistema que implementa el juego del Siete y Medio en Erlang. Permite jugar con uno o varios usuarios a la vez.

**Módulos**
- `sala.erl`: El "servidor" en el que se ejecutan las partidas. En él se realizan todas las acciones.
- `jugador.erl`: Correspondiente a los clientes que se comunican con el servidor. El cliente es ligero, envía todo el trabajo a la sala.
- `balanceador.erl`: Punto intermedio del sistema, encargado de la creación de salas y la gestión de jugadores entrantes.

**Cómo jugar**  
Antes de comenzar, compilar los archivos (erlc `balanceador.erl`/`sala.erl`/`jugador.erl`)

- En la misma máquina:   
1. Abrir terminales para jugadores y balanceador.  
2. En todos los terminales ejecutar `erl -sname <nombre>@localhost`. Nota: El nombre del balanceador debe ser conocido por todos los clientes, por lo que se recomienda llamarlo, por simplicidad, `erl -sname balanceador@localhost`, aunque podría tener cualquier otro nombre.  
3. Iniciar el balanceador con el comando `balanceador:start().`.
4. Iniciar los clientes con `jugador:empezar(<nombre del balanceador>@localhost).` y esperar a que la partida comience.
5. Se les indicará a los jugadores la acción que deben realizar, teniendo disponibles las siguientes:
    - `jugador:apostar(cantidad).`  
    - `jugador:carta("cubierta"/"descubierta").`  
    - `jugador:plantarse().`
    - `jugador:abandonar().`  
