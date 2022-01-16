# elm-review-no-unsafe-division

This package provides an [elm-review](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule that forbids most usages of the native division functions and operators (`/`, `//`, `modBy` and `remainderBy`), while still allowing the trivially correct cases where the divisor is a non-zero literal.

## Rationale

When the divisor is 0, the native division functions and operators produce results that can lead to undesired behavior.

The `/` operator can produce values like `NaN` and `Infinity`

```elm
0 / 0 -> NaN : Float
2 / 0 -> Infinity : Float
-2 / 0 -> -Infinity : Float
```

The `//` operator produces 0, which is a somewhat arbitrary result that might lead to domain logic errors

```elm
2 // 0 -> 0 : Int
```

The `modBy` function throws a runtime exception

```elm
modBy 0 2 -> "Error: Cannot perform mod 0. Division by zero error."
```

And the `remainderBy` function produces a `NaN` with type `Int`, of all things

```elm
remainderBy 0 2 -> NaN : Int
```

Using safe alternatives from the [Basics.Extra](https://package.elm-lang.org/packages/elm-community/basics-extra) package forces us to handle these cases explicitly, which reduces the possibility of errors.

## Configuration

```elm
module ReviewConfig exposing (config)

import NoUnsafeDivision
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoUnsafeDivision.rule
    ]
```

## Behavior

### Rejects

Division operations where the divisor is a zero literal

```elm
a : Float
a = 1 / 0

b : Float
b = (/) 1 0

c : Int
c = 1 // 0

d : Int
d = (//) 1 0

e : Int
e = modBy 0 1

f : Int
e = remainderBy 0 1
```

Division operations where the divisor is a non-literal value

```elm
a : Float
a = 1 / x

b : Float
b = (/) 1 x

c : Int
c = 1 // y

d : Int
d = (//) 1 y

e : Int
e = modBy y 1

f : Int
f = remainderBy y 1
```

Partial applications of `modBy` and `remainderBy` where the divisor is a zero literal or a non-literal value

```elm
a : Int -> Int
a = modBy 0

b : Int -> Int
b = remainderBy 0

c : Int -> Int
c = modBy x

d : Int -> Int
d = remainderBy x
```

Partial applications of `(/)` and `(//)` in prefix form

```elm
a : Float -> Float
a = (/) 1

b : Int -> Int
b = (//) 1

c : Float -> Float
c = (/) x

d : Int -> Int
d = (//) y
```

Usages without application

```elm
a : Float -> Float -> Float
a = (/)

b : Int -> Int -> Int
b = remainderBy

c : a -> Int -> Int -> Int
c = always modBy
```

### Accepts

Division operations where the divisor is a non-zero literal

```elm
a : Float
a = 1 / 0.3

b : Float
b = (/) 1 1.6

c : Int
c = 1 // 3

d : Int
d = (//) 1 2

e : Int
e = modBy 4 1

f : Int
e = remainderBy 2 1
```

Partial applications of `modBy` and `remainderBy` where the divisor is a non-zero literal

```elm
a : Int -> Int
a = modBy 3

b : Int -> Int
b = remainderBy 2
```

## Thanks

@jfmengels - for the amazing tool that is [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/)

@VladimirLogachev - for mentorship and advice
