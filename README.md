# vecmath

A simple vector maths library for graphics programming.

## Quickstart

Add the following to your build.sbt:

```scala
libraryDependencies += "com.scryetek" %% "vecmath" % "0.3.1"
```

Or if you're using ScalaJS

```scala
libraryDependencies += "com.scryetek" %%% "vecmath" % "0.3.1"
```

Proper documentation to follow shortly

### Scope

Currently, `vecmath` supports common operations on the following classes of matrices of Floats:

* 4x4 matrix (`Mat4`)
* 3x3 matrix (`Mat3`)
* 2x2 matrix (`Mat2`)
* 2x3 matrix (`Mat2d`) - a 3x3 matrix with a fixed bottom row of `0 0 1`

The following vector sizes are supported

`Vec4`, `Vec3`, `Vec2`

And of course, no graphics library would be complete without Quaternions, in the `Quat` package.

Matrices are stored in *column major* order- that is, how you would write them.  Angles are always assumed to be
given in radians. A 4x4 matrix (`Mat4`) has the following positions:

```
m00 m01 m02 m03
m10 m11 m12 m13
m20 m21 m22 m23
m30 m31 m32 m33
```

Quaternions are defined with imaginary components `x`, `y`, `z`, and real component `w`, in that order. 

Every class supports a `.copy()` method, which works like a case class copy method (but these classes are not case
classes, since they contain mutable members).

Every class also supports an analogous `.set()` command, with two variants.  The first variant has the same signature
as `.copy()`, and destructively re-assigns fields, and the second takes a single parameter of the same type, and
sets the object to an exact copy of the parameter.

## Mutable vs Immutable usage.

There are usually two ways of doing things (particularly for common operations) with the vecmath library.  You can use
the pleasant to use and read operator way, which I'd recommend you stick with until it's too slow:

```scala
val v = Vec2(1,2) + Vec2(3,4) * 5
```

Or you can use the hairier but potentially far more efficient mutable way:

```scala
val v2 = Vec2(1,2)
v2.add(Vec2(3,4)) // or even v2.add(3,4)
// v2 is now Vec(4,6)
```

Most mutating methods take an optional 'out' parameter, which stores the result in another object entirely.

```scala
val v2 = Vec2(1,2)
val out = Vec2()
v2.add(Vec2(3,4), out) // or v2.add(3,4, out)
// out is now Vec(4,6), but v2 is unchanged
```

Nearly all methods are marked `@inline`, but since neither `scalac` nor `proguard` dare inline constructors, we sadly
can't expect to get purely stack allocated code like you might in C++. However- inlining using the mutable
methods does work, if you care to compile with `-optimise`.

#### Common Sense Alert

You must look after what is mutable and what is not yourself, don't go mutating other people's Vec2's, and for heaven's
sake don't place vectors as keys in hashtables and then mutate them unless you like losing them forever. For less pain,
create a defensive copy of the object you want to mangle using the `.copy()` method.

### Matrix Operations

Assume we have `Mat4` matrices, M1 and M2.  We can say:

```scala
M3 = M1 * M2
```

This is equivalent to:

```scala
M1.postMultiply(M2, M3)
```

The opposite direction is also supported:

```scala
M3 = M2 * M1
```

Is the same as:

```scala
M1.preMultiply(M2, M3)
```

Note sometimes we have verb and adjective variations of the same method.

```scala
M1.invert(M2)
```

The verb form is mutating, while the adjective creates a copy:

```scala
M2 = M1.inverted
```

## Documentation

See the [ScalaDoc](http://mseddon.github.io/vecmath/api/#scryetek.vecmath.package)

### Test coverage/correctness

The methods employed in this library are verified using Mathematica 10, (primarily to quickly ensure implementations
conform to the same conventions etc, since there is some variation) and nearly every method is property tested
using the excellent [Scalacheck](https://www.scalacheck.org/) based on the derived laws, and standard definitions.

Speed is always considered over precision, and numerically this library is not overly stable.

Occasionally we get unlucky and numerically a test fails once in a while.  This is not considered a huge loss unless
the error is way above expectations.  Each property is tested 5000 times, though the generators could probably be
made more evil, particularly with respect to triggering singularities.

### Future Plans

Currently, this library is simply a 'vector math library', whatever that means.  It includes enough linear algebra to
get things done conveniently with WebGL for example.  In future I will extend it to support raytracing primitives,
spatial partitioning and probably things like triangulation.  At which point, maybe 'vecmath' will be a fairly bad name.

Such is life.

### Acknowledgements

This library contains implementations based on the excellent [Euclidean Space](http://www.euclideanspace.com/) site,
and probably more things from the [Graphics Gems](http://www.graphicsgems.org) series than I realise.
