
/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * @author  John Miller
 * @version 1.0
 * @date    Sat May 22 12:57:45 EDT 2010
 * @see     LICENSE (MIT style license file).
 */

package scalation.advmath

import scalation.util.Error

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * The Matrix class is abstract and defines the operations to be defined by two
 * concrete subclasses: MatrixN and SparseMatrixN.
 * @param dim1  the first/row dimension
 * @param dim2  the second/column dimension
 */
abstract class Matrix [T <% Ordered [T]: ClassManifest] (val dim1: Int,
                                                         val dim2: Int)
         extends Error
{
    /** Range for the storage array on dimension 1 (rows)
     */
    protected val range1 = 0 until dim1

    /** Range for the storage array on dimension 2 (columns)
     */
    protected val range2 = 0 until dim2

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Get this matrix's element at the i,j-th index position.
     * @param i  the row index
     * @param j  the column index
     */
    def apply (i: Int, j: Int): T

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Get this matrix's vector at the i-th index position (i-th row).
     * @param i  the row index
     */
    def apply (i: Int): VectorN [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Set this matrix's element at the i,j-th index position to the scalar x.
     * @param i  the row index
     * @param j  the column index
     * @param x  the scalar value to assign
     */
    def update (i: Int, j: Int, x: T)

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Set this matrix's row at the i-th index position to the vector u.
     * @param i  the row index
     * @param u  the vector value to assign
     */
    def update (i: Int, u: VectorN [T])

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Set all the elements in this matrix to the scalar x.
     * @param x  the scalar value to assign
     */
    def set (x: T)
    {
        for (i <- range1; j <- range2) this(i, j) = x
    } // set

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Set the values in this matrix as copies of the values in 2D array u.
     * @param u  the 2D array of values to assign
     */
    def set (u: Array [Array [T]])
    {
        for (i <- range1; j <- range2) this(i, j) = u(i)(j)
    } // set

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Iterate over the matrix row by row.
     * @param f  the function to apply
     */
    def foreach [U] (f: Array [T] => U)
    {
        var i = 0
        while (i < dim1) { f (this(i)()); i += 1 }
    } // foreach

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Get row '_row' from the matrix, returning it as a vector.
     * @param _row  the row to extract from the matrix
     * @param from  the position to start extracting from
     */
    def row (_row: Int, from: Int = 0): VectorN [T] =
    {
        val c = VectorN [T] (dim2 - from)
        for (j <- from until dim2) c(j - from) = this(_row, j)
        c
    } // row

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Set row 'row' of the matrix to a vector.
     * @param row  the column to set
     * @param u    the vector to assign to the column
     */
    def setRow (row: Int, u: VectorN [T])
    {
        for (j <- range2) this(row, j) = u(j)
    } // setRow

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Get column '_col' from the matrix, returning it as a vector.
     * @param _col  the column to extract from the matrix
     * @param from  the position to start extracting from
     */
    def col (_col: Int, from: Int = 0): VectorN [T] =
    {
        val c = VectorN [T] (dim1 - from)
        for (i <- from until dim1) c(i - from) = this(i, _col)
        c
    } // col

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Set column 'col' of the matrix to a vector.
     * @param col  the column to set
     * @param u    the vector to assign to the column
     */
    def setColumn (col: Int, u: VectorN [T])
    {
        for (i <- range1) this(i, col) = u(i)
    } // setColumn

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Slice this matrix row-wise from to end.
     * @param from  the start of the slice
     * @param end   the end of the slice
     */
    def slice (from: Int, end: Int): Matrix [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Slice this matrix row-wise r_from to r_end and column-wise c_from to c_end.
     * @param r_from  the start of the row slice
     * @param r_end   the end of the row slice
     * @param c_from  the start of the column slice
     * @param c_end   the end of the column slice
     */
    def slice (r_from: Int, r_end: Int, c_from: Int, c_end: Int): Matrix [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Slice this matrix excluding the given row and column.
     * @param row  the row to exclude
     * @param col  the column to exclude
     */
    def sliceExclude (row: Int, col: Int): Matrix [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Return 1 if the condition is true else 0
     * @param cond  the condition to evaluate
     */
    def oneIf (cond: Boolean): Int = 
    {
        if (cond) 1 else 0
    } // oneIf

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Transpose this matrix (rows => columns).
     */
    def t: Matrix [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Add this matrix and matrix b.
     * @param b  the matrix to add (requires sameCrossDimensions)
     */
    def + (b: Matrix [T]) (implicit nu: Numeric [T]): Matrix [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Add inplace this matrix and matrix b.
     * @param b  the matrix to add (requires sameCrossDimensions)
     */
    def += (b: Matrix [T]) (implicit nu: Numeric [T])

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Add this matrix and scalar s.
     * @param s  the scalar to add
     */
    def + (s: T) (implicit nu: Numeric [T]): Matrix [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Add inplace this matrix and scalar s.
     * @param s  the scalar to add
     */
    def += (s: T) (implicit nu: Numeric [T])

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * From this matrix substract matrix b.
     * @param b  the matrix to subtract (requires sameCrossDimensions)
     */
    def - (b: Matrix [T]) (implicit nu: Numeric [T]): Matrix [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * From this matrix substract inplace matrix b.
     * @param b  the matrix to subtract (requires sameCrossDimensions)
     */
    def -= (b: Matrix [T]) (implicit nu: Numeric [T])

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * From this matrix subtract scalar s.
     * @param s  the scalar to subtract
     */
    def - (s: T) (implicit nu: Numeric [T]): Matrix [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * From this matrix subtract inplace scalar s.
     * @param s  the scalar to subtract
     */
    def -= (s: T) (implicit nu: Numeric [T])

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Multiply this matrix by matrix b.
     * @param b  the matrix to multiply by (requires sameCrossDimensions)
     */
    def * (b: Matrix [T]) (implicit nu: Numeric [T]): Matrix [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Multiply inplace this matrix by matrix b.
     * @param b  the matrix to multiply by (requires sameCrossDimensions)
     */
    def *= (b: Matrix [T]) (implicit nu: Numeric [T])

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Multiply this matrix by vector b.
     * @param b  the vector to multiply by
     */
    def * (b: VectorN [T]) (implicit nu: Numeric [T]): VectorN [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Multiply this matrix by scalar s.
     * @param s  the scalar to multiply by
     */
    def * (s: T) (implicit nu: Numeric [T]): Matrix [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Multiply inplace this matrix by scalar s.
     * @param s  the scalar to multiply by
     */
    def *= (s: T) (implicit nu: Numeric [T])

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Multiply this matrix by vector b to produce another matrix (a_ij * b_j)
     * @param b  the vector to multiply by
     */
    def ** (b: VectorN [T]) (implicit nu: Numeric [T]): Matrix [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Multiply inplace this matrix by vector b to produce another matrix (a_ij * b_j)
     * @param b  the vector to multiply by
     */
    def **= (b: VectorN [T]) (implicit nu: Numeric [T])

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Decompose this matrix into the product of lower and upper triangular
     * matrices (l, u) using the LU Decomposition algorithm.  This version uses
     * partial pivoting.
     */
    def lud (implicit nu: Fractional [T]): Tuple2 [Matrix [T], Matrix [T]]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Decompose inplace this matrix into the product of lower and upper triangular
     * matrices (l, u) using the LU Decomposition algorithm.  This version uses
     * partial pivoting.
     */
    def lud_ip (implicit nu: Fractional [T]): Tuple2 [Matrix [T], Matrix [T]]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Solve for x in the equation l*u*x = b (see lud above).
     * @param l  the lower triangular matrix
     * @param u  the upper triangular matrix
     * @param b  the constant vector
     */
    def solve (l: Matrix [T], u: Matrix [T], b: VectorN [T])
        (implicit nu: Fractional [T]): VectorN [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Solve for x in the equation l*u*x = b (see lud above).
     * @param lu  the lower and upper triangular matrices
     * @param b   the constant vector
     */
    def solve (lu: Tuple2 [Matrix [T], Matrix [T]], b: VectorN [T])
        (implicit nu: Fractional [T]): VectorN [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Solve for x in the equation a*x = b where a is this matrix (see lud above).
     * @param b  the constant vector.
     */
    def solve (b: VectorN [T])
        (implicit nu: Fractional [T]): VectorN [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Determine the rank of this m by n matrix by taking the upper triangular
     * matrix from the LU Decomposition and counting the number of non-zero
     * diagonal elements.
     */
    def rank (implicit nu: Fractional [T]): Double =
    {
        val max   = if (dim1 < dim2) dim1 else dim2   // rank <= min (m, n)
        val u     = lud._2                            // upper triangular matrix
        var count = 0
        for (i <- 0 until max if this(i, i) != 0.) count += 1
        count
    } // rank

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Combine this matrix with matrix b, placing them along the diagonal and
     * filling in the bottom left and top right regions with zeroes; [this, b].
     * @param b  the matrix to combine with this matrix
     */
    def diag (b: Matrix [T]) (implicit nu: Numeric [T]): Matrix [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Form a matrix [Ip, this, Iq] where Ir is a r by r identity matrix, by
     * positioning the three matrices Ip, this and Iq along the diagonal.
     * @param p  the size of identity matrix Ip
     * @param q  the size of identity matrix Iq
     */
    def diag (p: Int, q: Int) (implicit nu: Numeric [T]): Matrix [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Invert this matrix (requires a squareMatrix).  This version uses partial
     * pivoting.
     */
    def inverse (implicit nu: Fractional [T]): Matrix [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Invert inplace this matrix (requires a squareMatrix).  This version uses
     * partial pivoting.
     */
    def inverse_ip (implicit nu: Fractional [T]): Matrix [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Use Guass-Jordan reduction on this matrix to make the left part embed an
     * identity matrix.  A constraint on this m by n matrix is that n >= m.
     */
    def reduce (implicit nu: Fractional [T]): Matrix [T]

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Use Guass-Jordan reduction inplace on this matrix to make the left part
     * embed an identity matrix.  A constraint on this m by n matrix is that n >= m.
     */ 
    def reduce_ip (implicit nu: Fractional [T])

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Compute the (right) nullspace of this m by n matrix (requires n = m + 1)
     * by performing Guass-Jordan reduction and extracting the negation of the
     * last column augmented by 1.  The nullspace of matrix a is "this vector v
     * times any scalar s", i.e., s*v*a = 0.  The left nullspace of matrix a is
     * the same as the right nullspace of a.t (a transpose).
     */
    def nullspace (implicit nu: Fractional [T]): VectorN [T] =
    {
        if (dim2 != dim1 + 1) flaw ("nullspace", "requires n (columns) = m (rows) + 1")
        val _1 = nu.one
        reduce.col(dim2 - 1) * nu.negate (_1) ++ new VectorN (_1)
    } // nullspace

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Compute the (right) nullspace inplace of this m by n matrix (requires n = m + 1)
     * by performing Guass-Jordan reduction and extracting the negation of the
     * last column augmented by 1.  The nullspace of matrix a is "this vector v
     * times any scalar s", i.e., s*v*a = 0.  The left nullspace of matrix a is
     * the same as the right nullspace of a.t (a transpose).
     */
    def nullspace_ip (implicit nu: Fractional [T]): VectorN [T] =
    {
        if (dim2 != dim1 + 1) flaw ("nullspace", "requires n (columns) = m (rows) + 1")
        val _1 = nu.one
        reduce_ip
        col(dim2 - 1) * nu.negate (_1) ++ new VectorN (_1)
    } // nullspace_ip

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Compute the trace of this matrix, i.e., the sum of the elements on the
     * main diagonal.  Should also equal the sum of the eigenvalues.
     * @see Eigen.scala
     */
    def trace (implicit nu: Numeric [T]): T =
    {
        if ( ! isSquare) flaw ("trace", "trace only works on square matrices")
        val _0  = nu.zero
        var sum = _0
        for (i <- range1) sum = nu.plus (sum, this(i, i))
        sum
    } // trace

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Compute the determinant of this matrix.
     */
    def det (implicit nu: Numeric [T]): T

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Check whether this matrix and the other Matrix have the same dimensions.
     * @param b  the other matrix
     */
    def sameDimensions (b: Matrix [T]): Boolean =
    {
        dim1 == b.dim1 && dim2 == b.dim2
    } // sameDimensions

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Check whether this matrix and the other matrix have the same cross dimensions.
     * @param b  the other matrix
     */
    def sameCrossDimensions (b: Matrix [T]): Boolean =
    {
        dim2 == b.dim1
    } // sameCrossDimensions

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Check whether this matrix is rectangular (all rows have the same number
     * of columns).
     */
    def isRectangular: Boolean

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Check whether this matrix is square (same row and column dimensions).
     */
    def isSquare: Boolean =
    {
        dim1 == dim2 && isRectangular
    } // isSquare

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Check whether this matrix is nonnegative (has no negative elements).
     */
    def isNonnegative (implicit nu: Numeric [T]): Boolean =
    {
        val _0 = nu.zero
        for (i <- range1; j <- range2 if nu.lt (this(i, j), _0)) return false
        true
    } // isNonegative

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Check whether this matrix is symmetric.
     */
    def isSymmetric: Boolean =
    {
        for (i <- 0 to dim1 - 2; j <- i + 1 until dim2 if this(i, j) != this(j, i)) return false
        true
    } // isSymmetric

} // Matrix class

