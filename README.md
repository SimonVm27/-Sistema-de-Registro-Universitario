# 🎓 Sistema de Registro Universitario

> **ST0244 – Lenguajes de Programación** · EAFIT University · Escuela de Ciencias Aplicadas e Ingeniería

---

```
██████╗ ███████╗ ██████╗ ██╗███████╗████████╗██████╗  ██████╗ 
██╔══██╗██╔════╝██╔════╝ ██║██╔════╝╚══██╔══╝██╔══██╗██╔═══██╗
██████╔╝█████╗  ██║  ███╗██║███████╗   ██║   ██████╔╝██║   ██║
██╔══██╗██╔══╝  ██║   ██║██║╚════██║   ██║   ██╔══██╗██║   ██║
██║  ██║███████╗╚██████╔╝██║███████║   ██║   ██║  ██║╚██████╔╝
╚═╝  ╚═╝╚══════╝ ╚═════╝ ╚═╝╚══════╝   ╚═╝   ╚═╝  ╚═╝ ╚═════╝ 
         Universidad · Sistema de Control de Acceso
```

---

## 📋 Descripción

Este proyecto implementa un **sistema de registro de entrada y salida de estudiantes** para una universidad, desarrollado en **dos paradigmas de programación** distintos:

| Versión | Lenguaje | Paradigma |
|--------|----------|-----------|
| 🟣 **Versión A** | Haskell | Programación Funcional |
| 🔵 **Versión B** | Prolog | Programación Lógica |

Ambas versiones comparten el mismo comportamiento funcional y persisten los datos en el archivo `University.txt`.

---

## ✨ Funcionalidades

```
┌─────────────────────────────────────────────────────┐
│                    MENÚ PRINCIPAL                   │
├──────┬──────────────────────────────────────────────┤
│  1   │  📥  Check In  — Registrar entrada           │
│  2   │  🔍  Buscar    — Buscar estudiante por ID    │
│  3   │  ⏱️  Tiempo    — Calcular tiempo en campus   │
│  4   │  📄  Listar    — Ver todos los estudiantes   │
│  5   │  📤  Check Out — Registrar salida            │
│  6   │  🚪  Salir     — Finalizar el programa       │
└──────┴──────────────────────────────────────────────┘
```

### 📥 1. Check In — Registro de Entrada
- Solicita el **nombre** e **ID** del estudiante
- Registra la **hora actual** de entrada automáticamente (formato `HH:MM`)
- Valida que el estudiante no esté ya registrado dentro

### 🔍 2. Búsqueda por ID
- Permite buscar un estudiante por su **ID**
- Muestra nombre, hora de entrada y estado actual
- Indica si el estudiante **aún está** o **ya salió** de la universidad

### ⏱️ 3. Cálculo de Tiempo
- Calcula cuánto tiempo permaneció el estudiante en la universidad
- Resultado en formato `HH:MM`
- Solo disponible para estudiantes con **salida registrada**

### 📄 4. Lista de Estudiantes
- Carga los datos desde `University.txt`
- Muestra el listado completo con toda la información de cada estudiante
- Indica quién sigue actualmente en la universidad

### 📤 5. Check Out — Registro de Salida
- Solicita el **ID** del estudiante que va a salir
- Registra la **hora actual** de salida automáticamente
- Actualiza los datos en memoria y en el archivo

---

## 🗂️ Estructura del Proyecto

```
📁 registro-universitario/
├── 📄 University.txt          ← Base de datos (generada automáticamente)
├── 🟣 Main.hs                 ← Implementación en Haskell
├── 🔵 main.pl                 ← Implementación en Prolog
└── 📘 README.md               ← Este archivo
```

### 📄 Formato de `University.txt`

**Haskell** almacena cada estudiante como una línea CSV:
```
id,nombre,HH:MM entrada,HH:MM salida
```
> La salida se representa como `00:00` cuando el estudiante aún está dentro.

**Prolog** almacena cada estudiante como un término Prolog:
```prolog
estudiantes("id","nombre",'HH:MM entrada','sin_salida').
```

---

## 🟣 Versión Haskell

### 🛠️ Requisitos
- [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler)
- Módulos utilizados: `Data.List`, `Data.Time`, `System.IO`, `Data.Char`

### ▶️ Compilar y Ejecutar
```bash
ghc Main.hs -o registro
./registro
```
O directamente con el intérprete:
```bash
runghc Main.hs
```

### 🧩 Estructura del Código

| Función | Descripción |
|--------|-------------|
| `main` | Carga el archivo e inicia el bucle principal |
| `bucle` | Loop recursivo que mantiene el estado del programa |
| `cargar` | Lee `University.txt` y carga los datos en memoria |
| `guardar` | Persiste la lista actualizada en `University.txt` |
| `menu` | Muestra el menú y valida la opción ingresada |
| `ejecutar` | Despacha la opción seleccionada al handler correcto |
| `registro` | Registra la entrada de un estudiante |
| `registrarSalida` | Registra la salida de un estudiante |
| `buscar` | Busca un estudiante por ID |
| `calculoTiempo` | Calcula el tiempo de permanencia |
| `listaEstudiantes` | Lista todos los estudiantes registrados |
| `convertir` | Convierte `HH:MM` a minutos totales |
| `formato` | Convierte minutos totales a `HH:MM` |
| `dosDigitos` | Formatea un número con cero a la izquierda si es necesario |
| `actualizar` | Actualiza la hora de salida de un estudiante en la lista |
| `datos` | Muestra la información formateada de un estudiante |

### 💡 Decisiones de Diseño (Haskell)
- El estado de los estudiantes se representa como una **lista de `String`** en memoria, donde cada elemento es una línea CSV.
- El programa es **totalmente funcional**: cada función recibe la lista y retorna una nueva lista actualizada (`IO [String]`).
- El manejo del tiempo usa `getZonedTime` de `Data.Time` para obtener la hora local real del sistema.
- La persistencia se logra reescribiendo el archivo completo en cada operación de cambio.

---

## 🔵 Versión Prolog

### 🛠️ Requisitos
- [SWI-Prolog](https://www.swi-prolog.org/) versión 8+

### ▶️ Ejecutar
```bash
swipl main.pl
```
El programa inicia automáticamente gracias a `:- initialization(main).`

### 🧩 Estructura del Código

| Predicado | Descripción |
|-----------|-------------|
| `main/0` | Carga el archivo e inicia el menú |
| `menu/0` | Muestra el menú y procesa la opción |
| `ejecutar/1` | Despacha la opción seleccionada |
| `cargar/0` | Lee `University.txt` y aserta los hechos en la base de conocimiento |
| `guardar/0` | Persiste los hechos dinámicos en `University.txt` |
| `registro/0` | Registra la entrada de un estudiante |
| `registro_salida/0` | Registra la salida de un estudiante |
| `busqueda/0` | Busca un estudiante por ID |
| `calcular_tiempo/0` | Calcula el tiempo de permanencia |
| `listar/0` | Lista todos los estudiantes registrados |
| `tomar_hora/1` | Obtiene la hora actual del sistema en formato `HH:MM` |
| `minutos/2` | Convierte `HH:MM` a minutos totales |
| `formato/2` | Convierte minutos totales a `HH:MM` |
| `datos/1` | Muestra la información formateada de una lista de estudiantes |

### 💡 Decisiones de Diseño (Prolog)
- Los estudiantes se almacenan como **hechos dinámicos**: `estudiantes(Ide, Nombre, Entrada, Salida)`.
- Se usa `assertz/1` para agregar estudiantes y `retract/1` para modificarlos (salida).
- La hora actual se obtiene con `get_time/1` y `stamp_date_time/3` de la librería estándar de SWI-Prolog.
- La persistencia escribe los hechos con `format/3` como términos Prolog válidos para poder recargarlos con `read/2`.

---

## ⚖️ Comparativa de Paradigmas

| Aspecto | 🟣 Haskell (Funcional) | 🔵 Prolog (Lógico) |
|---------|----------------------|-------------------|
| **Estado** | Lista inmutable pasada entre funciones | Base de conocimiento dinámica (`assert`/`retract`) |
| **Estructura de datos** | `[String]` (líneas CSV) | Hechos: `estudiantes/4` |
| **Modificación** | Se genera una nueva lista | Se retracta y re-aserta el hecho |
| **Flujo de control** | Recursión + pattern matching | Unificación + backtracking |
| **I/O** | Mónada `IO` | Predicados built-in (`read_line`, `writeln`) |
| **Persistencia** | `writeFile` / `readFile'` | `open`/`read`/`write` + `forall` |
| **Tiempo** | `getZonedTime` de `Data.Time` | `get_time` + `stamp_date_time` |

---

## 👥 Información del Curso

| Campo | Detalle |
|-------|---------|
| 📚 **Materia** | ST0244 – Lenguajes de Programación |
| 🏫 **Institución** | Universidad EAFIT |
| 🏛️ **Escuela** | Ciencias Aplicadas e Ingeniería |
| 👨‍🏫 **Profesor** | Alexander Narváez Berrío |
| 📅 **Periodo** | Febrero 2026 |

---

## ⚠️ Notas Importantes

> 💾 **Persistencia automática**: El archivo `University.txt` se crea automáticamente si no existe al iniciar el programa.

> 🕐 **Manejo del tiempo**: Ambas versiones usan el **reloj del sistema** para registrar las horas de entrada y salida en formato `HH:MM`.

> 🔄 **Salida `00:00` / `sin_salida`**: Indica que el estudiante **aún no ha salido** de la universidad. No confundir con medianoche.

> 📝 **Sustentación presencial obligatoria**: La defensa del proyecto se realiza en clase, con ejecución en vivo de ambas implementaciones.

---

<div align="center">

**Hecho con ❤️ para ST0244 · EAFIT 2026**

`Haskell` · `Prolog` · `Programación Funcional` · `Programación Lógica`

</div>
