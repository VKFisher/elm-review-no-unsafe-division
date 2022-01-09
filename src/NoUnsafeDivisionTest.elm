module NoUnsafeDivisionTest exposing (tests)

import NoUnsafeDivision exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


expectedDivisionError : String -> Review.Test.ExpectedError
expectedDivisionError under =
    Review.Test.error
        { message = "Use `Basics.Extra.safeDivide` instead of the native `/`"
        , details =
            [ "Using the native division operator can result in values like `NaN` or `Infinity`, which may lead to unwanted behavior."
            ]
        , under = under
        }


expectedIntegerDivisionError : String -> Review.Test.ExpectedError
expectedIntegerDivisionError under =
    Review.Test.error
        { message = "Use `Basics.Extra.safeIntegerDivide` instead of the native `//`"
        , details =
            [ "`x // 0` produces 0, which is a somewhat arbitrary result and may lead to unwanted behavior in your code."
            , "In cases where you do want the result to be 0, it is better to define it explicitly through `Maybe.withDefault 0 <| safeIntegerDivide x y`."
            ]
        , under = under
        }


expectedModByError : String -> Review.Test.ExpectedError
expectedModByError under =
    Review.Test.error
        { message = "Use `Basics.Extra.safeModBy` instead of the native `modBy`"
        , details =
            [ "`modBy 0 x` is one of the very few ways to cause a runtime exception in Elm."
            ]
        , under = under
        }


expectedRemainderByError : String -> Review.Test.ExpectedError
expectedRemainderByError under =
    Review.Test.error
        { message = "Use `Basics.Extra.safeRemainderBy` instead of the native `remaiderBy`"
        , details =
            [ "`remainderBy 0 x` produces `NaN`, which is probably not what you want."
            ]
        , under = under
        }


tests : Test
tests =
    describe "NoUnsafeDivision"
        [ describe "`/` operator"
            [ describe "infix form"
                [ test "should not report anything when dividing by a non-zero float literal" <|
                    \() ->
                        """module A exposing (..)
a : Float
a = 3 / 2
"""
                            |> Review.Test.run rule
                            |> Review.Test.expectNoErrors
                , test "should report error when dividing by a zero float literal" <|
                    \() ->
                        """module A exposing (..)
a : Float
a = 3 / 0
"""
                            |> Review.Test.run rule
                            |> Review.Test.expectErrors
                                [ expectedDivisionError "3 / 0"
                                ]
                , test "should report error when dividing by a non-literal float value" <|
                    \() ->
                        """module A exposing (..)
b : Float
b = 1

a : Float
a = 3 / b
"""
                            |> Review.Test.run rule
                            |> Review.Test.expectErrors
                                [ expectedDivisionError "3 / b"
                                ]
                ]
            , describe "prefix form"
                [ test "should not report anything when dividing by a non-zero float literal" <|
                    \() ->
                        """module A exposing (..)
a : Float
a = (/) 3 2
"""
                            |> Review.Test.run rule
                            |> Review.Test.expectNoErrors
                , test "should report error when dividing by a zero float literal" <|
                    \() ->
                        """module A exposing (..)
a : Float
a = (/) 3 0
"""
                            |> Review.Test.run rule
                            |> Review.Test.expectErrors
                                [ expectedDivisionError "(/) 3 0"
                                ]
                , test "should report error when dividing by a non-literal float value" <|
                    \() ->
                        """module A exposing (..)
b : Float
b = 1

a : Float
a = (/) 3 b
"""
                            |> Review.Test.run rule
                            |> Review.Test.expectErrors
                                [ expectedDivisionError "(/) 3 b"
                                ]
                , test "should report error when used as an argument" <|
                    \() ->
                        """module A exposing (..)

f : a -> (Float -> Float -> Float)
f = always (/)
"""
                            |> Review.Test.run rule
                            |> Review.Test.expectErrors
                                [ expectedDivisionError "(/)"
                                ]
                , test "should report error on partial application" <|
                    \() ->
                        """module A exposing (..)

f : Float -> Float
f = (/) 4
"""
                            |> Review.Test.run rule
                            |> Review.Test.expectErrors
                                [ expectedDivisionError "(/)"
                                ]
                ]
            ]
        , describe "`//` operator"
            [ describe "infix form"
                [ test "should not report anything when dividing by a non-zero integer literal" <|
                    \() ->
                        """module A exposing (..)
a : Int
a = 3 // 2
"""
                            |> Review.Test.run rule
                            |> Review.Test.expectNoErrors
                , test "should report error when dividing by a zero integer literal" <|
                    \() ->
                        """module A exposing (..)
a : Int
a = 3 // 0
"""
                            |> Review.Test.run rule
                            |> Review.Test.expectErrors
                                [ expectedIntegerDivisionError "3 // 0"
                                ]
                , test "should report error when dividing by a non-literal integer value" <|
                    \() ->
                        """module A exposing (..)
b : Int
b = 1

a : Int
a = 3 // b
"""
                            |> Review.Test.run rule
                            |> Review.Test.expectErrors
                                [ expectedIntegerDivisionError "3 // b"
                                ]
                ]
            , describe "prefix form"
                [ test "should not report anything when dividing by a non-zero integer literal" <|
                    \() ->
                        """module A exposing (..)
a : Int
a = (//) 3 2
"""
                            |> Review.Test.run rule
                            |> Review.Test.expectNoErrors
                , test "should report error when dividing by a zero integer literal" <|
                    \() ->
                        """module A exposing (..)
a : Int
a = (//) 3 0
"""
                            |> Review.Test.run rule
                            |> Review.Test.expectErrors
                                [ expectedIntegerDivisionError "(//) 3 0"
                                ]
                , test "should report error when dividing by a non-literal integer value" <|
                    \() ->
                        """module A exposing (..)
b : Int
b = 1

a : Int
a = (//) 3 b
"""
                            |> Review.Test.run rule
                            |> Review.Test.expectErrors
                                [ expectedIntegerDivisionError "(//) 3 b"
                                ]
                , test "should report error when used as an argument" <|
                    \() ->
                        """module A exposing (..)

f : a -> (Float -> Float -> Float)
f = always (//)
"""
                            |> Review.Test.run rule
                            |> Review.Test.expectErrors
                                [ expectedIntegerDivisionError "(//)"
                                ]
                , test "should report error on partial application" <|
                    \() ->
                        """module A exposing (..)

f : Float -> Float
f = (//) 4
"""
                            |> Review.Test.run rule
                            |> Review.Test.expectErrors
                                [ expectedIntegerDivisionError "(//)"
                                ]
                ]
            ]
        , describe "`modBy` function"
            [ test "should not report anything when dividing by a non-zero integer literal" <|
                \() ->
                    """module A exposing (..)
a : Int
a = modBy 2 3
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectNoErrors
            , test "should report error when dividing by a zero integer literal" <|
                \() ->
                    """module A exposing (..)
a : Int
a = modBy 0 3
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ expectedModByError "modBy 0 3"
                            ]
            , test "should report error when dividing by a non-literal integer value" <|
                \() ->
                    """module A exposing (..)
b : Int
b = 1

a : Int
a = modBy b 3
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ expectedModByError "modBy b 3"
                            ]
            , test "should report error when used as an argument" <|
                \() ->
                    """module A exposing (..)

f : a -> (Int -> Int -> Int)
f = always modBy
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ expectedModByError "modBy"
                            ]
            , test "should report error on partial application to zero literal" <|
                \() ->
                    """module A exposing (..)

f : Int -> Int
f = modBy 0
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ expectedModByError "modBy 0"
                            ]
            , test "should not report error on partial application to non-zero literal" <|
                \() ->
                    """module A exposing (..)

f : Int -> Int
f = modBy 4
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectNoErrors
            ]
        , describe "`remainderBy` function"
            [ test "should not report anything when dividing by a non-zero integer literal" <|
                \() ->
                    """module A exposing (..)
a : Int
a = remainderBy 2 3
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectNoErrors
            , test "should report error when dividing by a zero integer literal" <|
                \() ->
                    """module A exposing (..)
a : Int
a = remainderBy 0 3
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ expectedRemainderByError "remainderBy 0 3"
                            ]
            , test "should report error when dividing by a non-literal integer value" <|
                \() ->
                    """module A exposing (..)
b : Int
b = 1

a : Int
a = remainderBy b 3
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ expectedRemainderByError "remainderBy b 3"
                            ]
            , test "should report error when used as an argument" <|
                \() ->
                    """module A exposing (..)

f : a -> (Int -> Int -> Int)
f = always remainderBy
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ expectedRemainderByError "remainderBy"
                            ]
            , test "should report error on partial application" <|
                \() ->
                    """module A exposing (..)

f : Int -> Int
f = remainderBy 0
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ expectedRemainderByError "remainderBy 0"
                            ]
            , test "should not report error on partial application to non-zero literal" <|
                \() ->
                    """module A exposing (..)

f : Int -> Int
f = remainderBy 4
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectNoErrors
            ]
        ]
