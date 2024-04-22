---
title: Laboratorio de Funcional
author: Sofía Desimone, Simon Lopez, Julián Chrispeels, Paula Pacheco.
---
La consigna del laboratorio está en https://tinyurl.com/funcional-2024-famaf

# 1. Tareas
Pueden usar esta checklist para indicar el avance.

## Verificación de que pueden hacer las cosas.
- [x] Haskell instalado y testeos provistos funcionando. (En Install.md están las instrucciones para instalar.)

## 1.1. Lenguaje
- [x] Módulo `Dibujo.hs` con el tipo `Dibujo` y combinadores. Puntos 1 a 3 de la consigna.
- [x] Definición de funciones (esquemas) para la manipulación de dibujos.
- [x] Módulo `Pred.hs`. Punto extra si definen predicados para transformaciones innecesarias (por ejemplo, espejar dos veces es la identidad).

## 1.2. Interpretación geométrica
- [x] Módulo `Interp.hs`.

## 1.3. Expresión artística (Utilizar el lenguaje)
- [x] El dibujo de `Dibujos/Feo.hs` se ve lindo.
- [x] Módulo `Dibujos/Grilla.hs`.
- [x] Módulo `Dibujos/Escher.hs`.
- [x] Listado de dibujos en `Main.hs`.

## 1.4 Tests
- [x] Tests para `Dibujo.hs`.
- [x] Tests para `Pred.hs`.

# 2. Experiencia
Aprendimos a programar en un lenguaje funcional y a utilizar git, que no habíamos usado anteriormente. Distribuimos las tareas entre los integrantes del grupo para entregar el trabajo en tiempo y forma.


# 3. Preguntas
Al responder tranformar cada pregunta en una subsección para que sea más fácil de leer.

1. ¿Por qué están separadas las funcionalidades en los módulos indicados? Explicar detalladamente la responsabilidad de cada módulo.

Las funcionalidades están separadas en módulos para dividir nuestro problema en otros más pequeños según las tareas de cada parte del código, y así lograr un código más fácil de entender y reutilizable en otros problemas. Facilita la comprensión del trabajo de cada módulo, y se pueden testear sus funcionalidades. 
 Los módulos son:
- Dibujo.hs: define la sintaxis de nuestro lenguaje de figuras, las palabras y nuestro lenguaje, sus funciones constructoras  y algunos combinadores. 

- Pred.hs: define predicados sobre figuras básicas.

- FloatingPic.hs: declara funciones auxiliares para la interpretación geométrica de las figuras, y brinda la interfaz para ello.

- Interp.hs: declara la semántica de nuestro lenguaje, es decir, brinda una interpretación geométrica de las operaciones del lenguaje utilizando módulos de la librería Gloss.

- Main.hs: integra los módulos anteriores y combina las distintas funcionalidades para efectivamente resolver lo que queremos.


2. ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y en vez de eso, es un parámetro del tipo?

Las figuras básicas no están incluidas en la definición del lenguaje porque éste es un tipo de datos que puede funcionar sobre distintos tipos de figuras básicas con diferentes interpretaciones. De acuerdo a lo que se quiera crear, se puede dar una definición de figura diferente.


3. ¿Qué ventaja tiene utilizar una función de `fold` sobre hacer pattern-matching directo?

La ventaja principal de utilizar una función de fold en lugar de hacer pattern-matching directo se basa en la abstracción y generalización que proporciona.
Pattern-matching es útil para manejar casos específicos y aplicar una lógica diferente a cada caso, sin embargo, puede volverse complicado cuando se tienen muchos patrones. En cambio, las funciones de tipo fold abstraen operaciones comunes y permiten procesar datos de forma más flexible y uniforme. A su vez, se trata de funciones genéricas, por lo que son reutilizables y pueden aplicarse a distintos tipos de datos, siendo útil para nuestro caso, ya que las funciones básicas son un parámetro de tipo.


4. ¿Cuál es la diferencia entre los predicados definidos en Pred.hs y los tests?

La diferencia principal se encuentra en el contexto en el que se definen y cómo se utilizan. Pred.hs proporciona una abstracción general para trabajar con predicados sobre tipos genéricos, que no están necesariamente relacionados con la representación de las figuras, por lo que se pueden utilizar en diferentes contextos. En cambio, TestPred.hs se enfoca en probar propiedades específicas de los dibujos creados con el tipo de datos Dibujo Picture, y los predicados están específicamente relacionados con las propiedades que se quieren verificar en los dibujos antes creados.


# 4. Extras
Agregamos las funciones:

- modDim que modifica las dimensiones de un dibujo;

- rotarAlpha que rota alpha grados una figura.

Creamos la imagen Caracoles (src/Dibujos/Caracoles.hs) en el que se utilizan las dos funciones antes mencionadas.

