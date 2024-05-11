import common._
import scala.collection.parallel.immutable.ParVector
import scala.util.Random

package object Matrices {
  val random = new Random()

  type Matriz = Vector[Vector[Int]]

  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    // Crea una matriz de enteros cuadrada de long x long,
    // con valores aleatorios entre 0 y vals
    val v = Vector.fill(long, long)(random.nextInt(vals))
    v
  }

  def vectorAlAzar(long: Int, vals: Int): Vector[Int] = {
    // Crea un vector de enteros de longitud long,
    // con valores aleatorios entre 0 y vals
    val v = Vector.fill(long)(scala.util.Random.nextInt(vals))
    v
  }

  def transpuesta(m: Matriz): Matriz = {
    val l = m.length
    Vector.tabulate(l, l)((i, j) => m(j)(i))
  }

  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map { case (i, j) => (i * j) }.sum
  }

  def prodPuntoParD(v1: ParVector[Int], v2: ParVector[Int]): Int = { // A ser usada en el punto 1.5
    (v1 zip v2).map { case (i, j) => (i * j) }.sum
  }

  // Ejercicio 1.1.1
  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    Vector.tabulate(n, n) { (i, j) =>
      prodPunto(m1(i), transpuesta(m2)(j))
    }
  }

  // Ejercicio 1.1.2
  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    Vector.tabulate(n, n) { (i, j) =>
      task {
        prodPunto(m1(i), transpuesta(m2)(j))
      }
    }.map(_.map(_.join()))
  }

  // Ejercicio 1.2.1
  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    m.slice(i, i + l).map(_.slice(j, j + l))
  }

  // Ejercicio 1.2.2
  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    m1.zip(m2).map { case (row1, row2) =>
      row1.zip(row2).map { case (x, y) => x + y }
    }
  }

  // Ejercicio 1.2.3
  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if (n == 1) {
      Vector(Vector(prodPunto(m1(0), m2(0))))
    } else {
      val halfSize = n / 2
      val a11 = subMatriz(m1, 0, 0, halfSize)
      val a12 = subMatriz(m1, 0, halfSize, halfSize)
      val a21 = subMatriz(m1, halfSize, 0, halfSize)
      val a22 = subMatriz(m1, halfSize, halfSize, halfSize)
      val b11 = subMatriz(m2, 0, 0, halfSize)
      val b12 = subMatriz(m2, 0, halfSize, halfSize)
      val b21 = subMatriz(m2, halfSize, 0, halfSize)
      val b22 = subMatriz(m2, halfSize, halfSize, halfSize)

      val p1 = multMatrizRec(a11, sumMatriz(b12, restaMatriz(b22, b11)))
      val p2 = multMatrizRec(sumMatriz(a11, a12), b22)
      val p3 = multMatrizRec(sumMatriz(a21, a22), b11)
      val p4 = multMatrizRec(a22, restaMatriz(b21, b11))
      val p5 = multMatrizRec(sumMatriz(a11, a22), sumMatriz(b11, b22))
      val p6 = multMatrizRec(restaMatriz(a12, a22), sumMatriz(b21, b22))
      val p7 = multMatrizRec(restaMatriz(a11, a21), sumMatriz(b11, b12))

      val c11 = sumMatriz(restaMatriz(sumMatriz(p5, p4), p2), p6)
      val c12 = sumMatriz(p1, p2)
      val c21 = sumMatriz(p3, p4)
      val c22 = restaMatriz(restaMatriz(sumMatriz(p5, p1), p3), p7)

      val result = Vector.tabulate(n, n) { (i, j) =>
        if (i < halfSize && j < halfSize) c11(i)(j)
        else if (i < halfSize && j >= halfSize) c12(i)(j - halfSize)
        else if (i >= halfSize && j < halfSize) c21(i - halfSize)(j)
        else c22(i - halfSize)(j - halfSize)
      }
      result
    }
  }

  // Ejercicio 1.2.4
  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if (n == 1) {
      Vector(Vector(prodPunto(m1(0), m2(0))))
    } else {
      val halfSize = n / 2
      val a11 = subMatriz(m1, 0, 0, halfSize)
      val a12 = subMatriz(m1, 0, halfSize, halfSize)
      val a21 = subMatriz(m1, halfSize, 0, halfSize)
      val a22 = subMatriz(m1, halfSize, halfSize, halfSize)
      val b11 = subMatriz(m2, 0, 0, halfSize)
      val b12 = subMatriz(m2, 0, halfSize, halfSize)
      val b21 = subMatriz(m2, halfSize, 0, halfSize)
      val b22 = subMatriz(m2, halfSize, halfSize, halfSize)

      val p1 = task {
        multMatrizRecPar(a11, sumMatriz(b12, restaMatriz(b22, b11)))
      }
      val p2 = task {
        multMatrizRecPar(sumMatriz(a11, a12), b22)
      }
      val p3 = task {
        multMatrizRecPar(sumMatriz(a21, a22), b11)
      }
      val p4 = task {
        multMatrizRecPar(a22, restaMatriz(b21, b11))
      }
      val p5 = task {
        multMatrizRecPar(sumMatriz(a11, a22), sumMatriz(b11, b22))
      }
      val p6 = task {
        multMatrizRecPar(restaMatriz(a12, a22), sumMatriz(b21, b22))
      }
      val p7 = task {
        multMatrizRecPar(restaMatriz(a11, a21), sumMatriz(b11, b12))
      }

      val c11 = sumMatriz(restaMatriz(sumMatriz(p5.join(), p4.join()), p2.join()), p6.join())
      val c12 = sumMatriz(p1.join(), p2.join())
      val c21 = sumMatriz(p3.join(), p4.join())
      val c22 = restaMatriz(restaMatriz(sumMatriz(p5.join(), p1.join()), p3.join()), p7.join())

      val result = Vector.tabulate(n, n) { (i, j) =>
        if (i < halfSize && j < halfSize) c11(i)(j)
        else if (i < halfSize && j >= halfSize) c12(i)(j - halfSize)
        else if (i >= halfSize && j < halfSize) c21(i - halfSize)(j)
        else c22(i - halfSize)(j - halfSize)
      }
      result
    }
  }

  // Ejercicio 1.3.1
  def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
    m1.zip(m2).map { case (row1, row2) =>
      row1.zip(row2).map { case (x, y) => x - y }
    }
  }

  // Ejercicio 1.3.2
  def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if (n <= 64) {
      multMatrizRec(m1, m2)
    } else {
      val halfSize = n / 2
      val a11 = subMatriz(m1, 0, 0, halfSize)
      val a12 = subMatriz(m1, 0, halfSize, halfSize)
      val a21 = subMatriz(m1, halfSize, 0, halfSize)
      val a22 = subMatriz(m1, halfSize, halfSize, halfSize)
      val b11 = subMatriz(m2, 0, 0, halfSize)
      val b12 = subMatriz(m2, 0, halfSize, halfSize)
      val b21 = subMatriz(m2, halfSize, 0, halfSize)
      val b22 = subMatriz(m2, halfSize, halfSize, halfSize)

      val s1 = restaMatriz(b12, b22)
      val s2 = sumMatriz(a11, a12)
      val s3 = sumMatriz(a21, a22)
      val s4 = restaMatriz(b21, b11)
      val s5 = sumMatriz(a11, a22)
      val s6 = sumMatriz(b11, b22)
      val s7 = restaMatriz(a12, a22)
      val s8 = sumMatriz(b21, b22)
      val s9 = restaMatriz(a11, a21)
      val s10 = sumMatriz(b11, b12)

      val p1 = multStrassen(a11, s1)
      val p2 = multStrassen(s2, b22)
      val p3 = multStrassen(s3, b11)
      val p4 = multStrassen(a22, s4)
      val p5 = multStrassen(s5, s6)
      val p6 = multStrassen(s7, s8)
      val p7 = multStrassen(s9, s10)

      val c11 = sumMatriz(restaMatriz(sumMatriz(p5, p4), p2), p6)
      val c12 = sumMatriz(p1, p2)
      val c21 = sumMatriz(p3, p4)
      val c22 = restaMatriz(restaMatriz(sumMatriz(p5, p1), p3), p7)

      val result = Vector.tabulate(n, n) { (i, j) =>
        if (i < halfSize && j < halfSize) c11(i)(j)
        else if (i < halfSize && j >= halfSize) c12(i)(j - halfSize)
        else if (i >= halfSize && j < halfSize) c21(i - halfSize)(j)
        else c22(i - halfSize)(j - halfSize)
      }
      result
    }
  }

  // Ejercicio 1.3.3
  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if (n <= 64) {
      multMatrizRecPar(m1, m2)
    } else {
      val halfSize = n / 2
      val a11 = subMatriz(m1, 0, 0, halfSize)
      val a12 = subMatriz(m1, 0, halfSize, halfSize)
      val a21 = subMatriz(m1, halfSize, 0, halfSize)
      val a22 = subMatriz(m1, halfSize, halfSize, halfSize)
      val b11 = subMatriz(m2, 0, 0, halfSize)
      val b12 = subMatriz(m2, 0, halfSize, halfSize)
      val b21 = subMatriz(m2, halfSize, 0, halfSize)
      val b22 = subMatriz(m2, halfSize, halfSize, halfSize)

      val s1 = restaMatriz(b12, b22)
      val s2 = sumMatriz(a11, a12)
      val s3 = sumMatriz(a21, a22)
      val s4 = restaMatriz(b21, b11)
      val s5 = sumMatriz(a11, a22)
      val s6 = sumMatriz(b11, b22)
      val s7 = restaMatriz(a12, a22)
      val s8 = sumMatriz(b21, b22)
      val s9 = restaMatriz(a11, a21)
      val s10 = sumMatriz(b11, b12)

      val p1 = task {
        multStrassenPar(a11, s1)
      }
      val p2 = task {
        multStrassenPar(s2, b22)
      }
      val p3 = task {
        multStrassenPar(s3, b11)
      }
      val p4 = task {
        multStrassenPar(a22, s4)
      }
      val p5 = task {
        multStrassenPar(s5, s6)
      }
      val p6 = task {
        multStrassenPar(s7, s8)
      }
      val p7 = task {
        multStrassenPar(s9, s10)
      }

      val c11 = sumMatriz(restaMatriz(sumMatriz(p5.join(), p4.join()), p2.join()), p6.join())
      val c12 = sumMatriz(p1.join(), p2.join())
      val c21 = sumMatriz(p3.join(), p4.join())
      val c22 = restaMatriz(restaMatriz(sumMatriz(p5.join(), p1.join()), p3.join()), p7.join())

      val result = Vector.tabulate(n, n) { (i, j) =>
        if (i < halfSize && j < halfSize) c11(i)(j)
        else if (i < halfSize && j >= halfSize) c12(i)(j - halfSize)
        else if (i >= halfSize && j < halfSize) c21(i - halfSize)(j)
        else c22(i - halfSize)(j - halfSize)
      }
      result
    }
  }
}